unit Parse.FuncCall;

interface
uses Def.Functions, Def.VarTypes,
  Parse.Errors, Parse.Literals, Parse.Expr;

//Parse a procedure call, or a function call with the result being ignored.
function DoParseProcedureCall(Func: PFunction): TQuicheError;

//Parse a function call (which returns a value)
function DoParseFunctionCall(Func: PFunction;var Slug: TExprSlug): TQuicheError;


implementation
uses SysUtils,
  Def.IL, Def.Operators, Def.Variables, Def.UserTypes, Def.TypeData, Def.Consts, Def.Scopes,
  Lib.Primitives,
  Parse.Base, Parse.Eval, Parse.TypeChecker,
  Z80.Hardware;

//Bytes required for return stack pointer, previous IX etc.
//Probably should be somewhere else. CPU specific?
const iFunctionMetaByteSize = 4;

type TSlugArray= array[0..MaxFunctionParams] of TExprSlug;

//===================================== ARGUMENTS

//Read an argument from the source code, and return it in a Slug
function ParseArgument(Func: PFunction;Arg: TParameter;out Slug: TExprSlug): TQuicheError;
begin
  Slug.Initialise;

  //Parse argument - and validate type compatibility
  if Arg.VarType = vtTypeDef then
  begin
    //Special handling for TypeDefs - Passing vtTypeDef to ParseExpressionToSlug
    //will give errors if the arg doesn't result in a type name...
    Result := ParseExprToSlug(Slug);
    if Result <> qeNone then
      EXIT;

    //...and if it does we need to convert that result here
    if Slug.ResultVarType <> vtTypeDef then
    //If we have an intrinsic, leave this bit for the intrinsic argument massager
    if Func.CallingConvention <> ccIntrinsic then
      //If caller wants a TypeDef can we get TypeDef of value?
      //Can't do this for expressions (at least, not yet! - TODO)
      if Slug.ILItem = nil then
      begin
        Slug.Operand.Imm.CreateTypeDef(Slug.ResultType);
        Slug.Operand.Kind := pkImmediate;
        Slug.ResultType := GetSystemType(vtTypeDef);
      end;
  end
  else
  begin
    Result := ParseExprToSlugWithTypeCheck(Slug, Arg.UserType);
    if Result <> qeNone then
      EXIT;
  end;
end;

//Validate an argument and process as necessary, depending on the calling
//convention (e.g. writing it to a hidden variable or pushing it onto the stack.
function ProcessArgument(Func: PFunction;CallingConvention: TCallingConvention;
  ArgIndex: Integer;var Slug: TExprSlug): TQuicheError;
var Arg: PParameter;
  ILItem: PILItem;
  V: PVariable;
begin
  Arg := @Func.Params[ArgIndex];

  //We have a pointered literal - it needs adding to a ConstList...
  if (Slug.ILItem = nil) and (Slug.Operand.Kind = pkImmediate) then
    if IsPointeredType(Slug.ResultType) then
        Consts.Add('<Unnamed>', Slug.ResultType, Slug.Operand.Imm);

  //Validate argument:
  case Arg.Access of
    paVal, paConst, paResult: ;  //Any
    paVar, paOut:
    begin //Argument needs to be a variable reference.
      if (Slug.ILItem <> nil) or (Slug.Operand.Kind <> pkVarSource) then
        EXIT(ErrFuncCallSub(qeArgMustBeVariable, Arg.Name, Func));
      //Variable mustn't be a Const
      Assert(Slug.Operand.Kind = pkVarSource);
      V := Slug.Operand.Variable;
      if V.IsConst then
        EXIT(ErrFuncCallSub(qeCantPassCONSTasVARorOUT, Arg.Name, Func));
      if not ((Arg.Access = paVar) and (Func.CallingConvention = ccIntrinsic)) then
      begin
        //TODO: Proper type checking of user types
        if V.VarType <> Arg.VarType then
          EXIT(ErrFuncCallSub3(qeReturnedArgTypeMismatch, Arg.Name,
            Slug.Operand.Variable.UserType.Description, Arg.UserType.Description,
            Func));
      end;
    end;
    else
    Assert(False, 'Unknown access specifier');
  end;

  //Do we need to push this argument on the stack?
  case CallingConvention of
    ccRegister, ccCall, ccRST, ccExtern, ccIntrinsic: ; //Registers will be loaded later
    ccStack: //Put parameters on the stack
    begin
      //The expression has returned a pointered value. We need to create a temporary
      //variable to store the returned data, and set it's value into the ResultByRefParam.
      if Slug.ResultByRefParam <> nil then
      begin
        Assert(Arg.IsByRef);  //Ensure we take the correct path in the next IF statement
        Slug.AssignToHiddenVar;
        ILItem := ILAppend(OpMove);
        ILItem.ResultType := GetPointerToType(Slug.ResultType);
        ILItem.Param1.SetVarRef(Slug.Operand.Variable);
      end
      else
      begin
        ILItem := Slug.ToILItemNoDest;
        if ILItem.Op = OpUnknown then
          ILItem.Op := OpMove;
      end;

      if Arg.IsByRef then
      begin
        //TODO: Push /address/ of the variable
        //If we have an expression we need to store the result to a variable then
        //pass the address of that variable
        Assert(ILItem.Param1.Kind in [pkVarSource, pkVarRef]);
        if ILItem.Param1.Kind = pkVarSource then
          ILItem.Param1.Kind := pkVarRef;
        ILItem.Dest.Kind := pkPush;
        ILItem.Dest.PushType := GetPointerToType(Slug.ResultType);
      end
      else
      begin
        //Slug to ILItem
        case GetTypeRegSize(Arg.UserType) of
          1: ILItem.Dest.Kind := pkPushByte;
          2: ILItem.Dest.Kind := pkPush;
        else
          Assert(False);
        end;
        ILItem.Dest.PushType := Arg.UserType;
      end;
    end;
  else
    raise ECallingConvention.Create;
  end;
  Result := qeNone;
end;

function ParseArgList(Func: PFunction;out Slugs: TSlugArray): TQuicheError;
var
  I: Integer;
  Brace: Boolean; //True if arg list uses brackets
  Ch: Char;
  ArgIndex: Integer;  //Index into Func's argument list
begin
  for I := 0 to high(Slugs) do
    Slugs[I].Initialise;

  Brace := Parser.TestChar = '(';
  if Brace then
    Parser.SkipChar;
  Parser.SkipWhite;

  ArgIndex := 0;

  Ch := Parser.TestChar;
  //If there is a Brace we definitely have a parameter list. If not we could
  //a parameter list without parenthesis. To test for this we need to examine
  //next character. If it's the start of an identifier or literal then it's a
  //parameter (or an error). If not then it must be a symbol or a marker which
  //indicates the end of an expression such as end-of-line (#0) or  ; , )
  //end of line, end of parameter, end of expression
  //Test for empty list
  if ((not Brace) and (TestIdentFirst or TestLiteralFirst) and (Func.ParamCount > 0)) or
//  (CharInSet(Ch, IdentFirst + LiteralFirst))) or
  //not CharInSet(Ch, [#0,';',',',')'])) or  //No Brace and #0 (EOLN) -> no arguments
    (Brace and (Ch <> ')')) then      //Brace and ')' -> no arguments
  repeat
    if ArgIndex >= Func.ParamCount then
      EXIT(ErrFuncCall(qeTooManyArgs, Func));

    Result := ParseArgument(Func, Func.Params[ArgIndex], Slugs[ArgIndex]);
    if Result <> qeNone then
      EXIT;
    Result := ProcessArgument(Func, Func.CallingConvention, ArgIndex, Slugs[ArgIndex]);
    if Result <> qeNone then
      EXIT;

    //Manual validation of some intrinsic properties
    case Func.Op of
      Def.Operators.OpInc, Def.Operators.OpDec:
        if ArgIndex = 1 then
          //Second arg must be an integer constant
          if (Slugs[ArgIndex].ILItem <> nil) or (Slugs[ArgIndex].Operand.Kind <> pkImmediate) then
            EXIT(errFuncCallSub(qeIntegerConstantArgExpected, Func.Params[1].Name, Func));
      OpHi, OpLo, OpSwap:
        //Arg must be > 8 bits wide
        if (Slugs[ArgIndex].ILItem <> nil) or (Slugs[ArgIndex].Operand.Kind <> pkImmediate) then
          if GetTypeRegSize(Slugs[ArgIndex].ResultType) = 1 then
            EXIT(errFuncCall(qeBytePassedToHiLoSwap, Func));
      OpPoke:
        //Second arg must be a valid byte value (if Range Check is on)
        if ArgIndex = 1 then
          if (Slugs[ArgIndex].ILItem = nil) and (Slugs[ArgIndex].Operand.Kind = pkImmediate) then
            if IsIntegerVarType(Slugs[ArgIndex].Operand.Imm.VarType) then
            begin
              if TypeCheckFromSlug(GetSystemType(vtByte), Slugs[ArgIndex]) <> qeNone then
                EXIT(errFuncCall(qeConstantOutOfRange, Func));
              if Result <> qeNone then
                EXIT;
            end;
      OpSetLength:
        if ArgIndex = 0 then
          //First arg must be a List type variable
          if (Slugs[ArgIndex].ILItem <> nil) or
            (Slugs[ArgIndex].Operand.Kind <> pkVarSource) or
            (Slugs[ArgIndex].ResultVarType <> vtArrayType) or
            (Slugs[ArgIndex].ResultType.ArrayDef.ArrayType <> atList) then
            EXIT(errFuncCallSub(qeListVariableArgExpected, Func.Params[1].Name, Func));
    end;

    //More parameters
    Parser.SkipWhite;
    Ch := Parser.TestChar;
    if Ch = ',' then
      Parser.SkipChar;
    inc(ArgIndex);
  until Ch <> ',';

  if Brace then
    if Ch = ')' then
      Parser.SkipChar
    else
      EXIT(Err(qeCommaOrCloseParensExpected));

  //Set any default parameter values
  while (ArgIndex < Func.ParamCount) and Func.Params[ArgIndex].HasDefaultValue do
  begin
    Slugs[ArgIndex].Initialise;
    Slugs[ArgIndex].Operand.Kind := pkImmediate;
    if Func.Params[ArgIndex].UserType <> nil then
      Slugs[ArgIndex].Operand.Imm := Func.Params[ArgIndex].DefaultValue
    else  //SuperTypes - we have an Intrinsic!
      if Func.Params[ArgIndex].SuperType in [stAnyInteger, stOrdinal] then
        //TODO: This should give a better analysis of the type
        Slugs[ArgIndex].Operand.Imm := Func.Params[ArgIndex].DefaultValue
//
//        Slugs[ArgIndex].Operand.Imm := .CreateInteger(vtInteger, TVarType(Func.Params[ArgIndex].DefaultValue.IntValue))
      else
        Assert(False, 'Type or Supertype required');

    Slugs[ArgIndex].ResultType := Slugs[ArgIndex].Operand.Imm.UserType;
    Slugs[ArgIndex].ImplicitType := Slugs[ArgIndex].Operand.Imm.UserType;
    Result := ProcessArgument(Func, Func.CallingConvention, ArgIndex, Slugs[ArgIndex]);
    if Result <> qeNone then
      EXIT;
    inc(ArgIndex);
  end;

  //Validate number of arguments
  if ArgIndex <> Func.ParamCount then
    EXIT(ErrFuncCallSub2(qeNotEnoughParameters, ArgIndex.ToString, Func.ParamCount.ToString,
      Func));

  Result := qeNone;
end;

//===================================== INTRINSICS

//Generate the IL code for an intrinsic.
//Func is the function template for the intrinsic
//If OpOverride is opUnknown the Operator from Func will be used, otherwise
//  the Op from OpOverride will be used. (Used by writeln and readln to do funky stuff)
//ParamCount is the number of paramaters supplied to IntrinsicGenerateIL (Left, Right).
//Valid values are 0, 1, and 2.
//This may be different to the value in Func if the intrinsic is doing funky things
//(ie. write(ln), read(ln) etc).
//Left, Right are the parameters (see ParamCount).
//Slug is the slug to be returned.
//  NOTE: Slug MUST have been initialised. Slug ResultType and ImplicitType should
//    be set /before/ the call to this function
procedure IntrinsicGenerateIL(Func: PFunction;OpOverride: TOperator;ParamCount: Integer;
  const Left, Right: TExprSlug;var Slug: TExprSlug);
var V: PVariable;
begin
  //Can we convert an existing ILItem to the intrinsic operation? (and save an IL step)
  if (ParamCount = 1) and (Left.ILItem <> nil) and (Left.ILItem.Op = opUnknown) then
  begin //We can just 'patch' the Op into the ILItem
    Slug := Left;
    if OpOverride <> opUnknown then
      Slug.ILItem.Op := OpOverride
    else
      Slug.ILItem.Op := Func.Op;
  end
  else
  begin //Create the ILItem for the operation
    if OpOverride <> opUnknown then
      Slug.ILItem := ILAppend(OpOverride)
    else
      Slug.ILItem := ILAppend(Func.Op);

    if ParamCount >= 1 then
    begin //First parameter
      if Left.ILItem <> nil then
      begin //We have an ILItem. Assign it to a temp var and use that
        Left.AssignToHiddenVar;
        if Func.Params[0].IsByRef then
          Slug.ILItem.Param1.SetVarRef(Left.ILItem.Dest.Variable)
        else
          Slug.ILItem.Param1.SetVarSource(Left.ILItem.Dest.Variable);
      end
      else  //Otherwise it's either a constant or variable we can assign directly
      begin
        Slug.ILItem.Param1 := Left.Operand;
        if Func.Params[0].IsByRef then
          if Slug.ILItem.Param1.Kind = pkVarSource then
            Slug.ILItem.Param1.Kind := pkVarRef;
      end;
    end;

    if ParamCount >= 2 then
    begin //Same for second parameter
      if Right.ILItem <> nil then
      begin
        Right.AssignToHiddenVar;
        Slug.ILItem.Param2.SetVarSource(Right.ILItem.Dest.Variable);
      end
      else
        Slug.ILItem.Param2 := Right.Operand;
    end;
  end;

  if (Func.Params[0].Access = paVar) and not (Func.Params[0].IsByRef) then
  begin //Var parameter - result is written back to Param1
    V := Slug.ILItem.Param1.Variable;
    V.IncVersion;
    Slug.ILItem.Dest.SetVarDestAndVersion(V, V.Version);
    Slug.ILItem.ResultType := V.UserType;
  end
  else
    Slug.ILItem.ResultType := Slug.ResultType;
end;

//Slug must have been Initialised and a preliminary result type set.
//No other data should have been set
function TryEvalIntrinsic(Func: PFunction;const Arg0, Arg1: TExprSlug;
  var Slug: TExprSlug;out Evalled: Boolean): TQuicheError;
begin
  Evalled := False;
  Result := qeNone;

  //Evaluate at compile time (if possible)
  if not Assigned(Arg0.ILItem) and (Arg0.Operand.Kind = pkImmediate) then
    if Func.ParamCount = 1 then
    begin
      Result := EvalIntrinsicUnary(Func.Op, Arg0.Operand,
        Slug.Operand.Imm, Evalled);
      if Result <> qeNone then
        EXIT;
      if Evalled then
      begin
        if Result <> qeNone then
          EXIT;
        Slug.ResultType := Slug.Operand.Imm.UserType;
        Slug.ImplicitType := Slug.ResultType;
        Slug.Operand.Kind := pkImmediate;
      //TODO: We currently use the ResultType from the available Primitives.
      //For constants we probably want to base the ResultType on the value, but
      //two lines below are too blunt and need refining
//      Slug.ResultType := Slug.Operand.ImmType;
//      Slug.ImplicitType := Slug.ResultType;
        EXIT;
      end
    end
    else //Two parameters
      if not Assigned(Arg1.ILItem) and (Arg1.Operand.Kind = pkImmediate) then
      begin
        Result := EvalIntrinsicBi(Func.Op, Arg0.Operand, Arg1.Operand,
          Slug.Operand.Imm, Evalled);
        if Result <> qeNone then
          EXIT;
        if Evalled then
        begin
          if Result <> qeNone then
            EXIT;
          Slug.ResultType := Slug.Operand.Imm.UserType;
          Slug.ImplicitType := Slug.ResultType;
          Slug.Operand.Kind := pkImmediate;
        //TODO: As previous TODO
//        Slug.ResultType := Slug.Operand.ImmType;
//        Slug.ImplicitType := Slug.ResultType;
          EXIT;
        end;
      end;
end;

//Sizeof for an unbounded array needs to use a primitive to convert the length/capacity
//to the bytesize. If the length/capacity is <> 1 the primitive also needs to
//multiply by the element size. So, if length/capacity is <> 1 we will supply a second
//parameter containing the element size.
procedure MassageUnboundedSizeOf(var Slugs: TSlugArray);
var ElementSize: Integer;
begin
  Assert(Slugs[0].ResultVarType = vtArrayType);
  Assert(Slugs[0].ResultType.ArrayDef.ArrayType in [atVector, atList]);

  ElementSize := Slugs[0].ResultType.ArrayDef.ElementSize;
  if ElementSize <> 1 then
  begin
    Assert(Slugs[1].Operand.Kind = pkNone);
    Slugs[1].Operand.Kind := pkImmediate;
    Slugs[1].Operand.Imm.CreateTyped(Slugs[0].ResultType.ArrayDef.MetaType, ElementSize);
    Slugs[1].ResultType := GetSystemType(vtWord);
    Slugs[1].ImplicitType := Slugs[1].ResultType;
  end;
end;

//Applies any special processing and validation required when an array is being passed
//as an argument.
//Param is the relevant parameter
//SlugIndex is the index of the parameter which is being passed an array
function MassageArrayArgument(Op: TOperator;Param: PParameter;var Slugs: TSlugArray;
  SlugIndex: Integer): TQuicheError;
var Slug: PExprSlug;
  IsTypeDef: Boolean; //We have a TypeDef - Ie the argument was a type (immediate) not a variable
                        //  - we can use the type provide it is bounded.
                        //  - If it's unbounded we have a compile-time error
  IsImmediate: Boolean; //If true we have an immediate value, other than a type (eg a typed constant)
                        //  - we can use the type of the constant at compile time
                        //If False we have a variable
                        //  - We may need to do runtime processing to establish the result
                        //  - Ie if the variable is a list, or is an unbounded array.
  IsUnbounded: Boolean; //We have an unbounded array
  ArrayUT: PUserType;
  ArrayType: TArrayType;
begin
  //TODO: Should this should raise a TypeMismatch error?
  Assert(Param.VarType = vtTypeDef);

  Slug := @Slugs[SlugIndex];

  IsTypeDef := Slug.ResultVarType = vtTypeDef;
  IsImmediate := Slug.IsImmediate and not IsTypeDef;

  //Establish the type of the array argument
  if IsTypeDef then
  begin
    Assert(Slug.IsImmediate);
    ArrayUT := Slug.Operand.Imm.TypeDefValue;
    Assert(ArrayUT.VarType = vtArrayType);
  end
  else
    ArrayUT := Slug.ResultType;

  ArrayType := ArrayUT.ArrayDef.ArrayType;
  IsUnbounded := ArrayUT.ArrayDef.IsUnbounded;

  case Op of
    opHigh: ;
    opLow:
      //Vectors and list always have a Low of zero
      if ArrayType in [atVector, atList] then
      begin
        SlugToTypeDef(Slugs[SlugIndex]);
        EXIT(qeNone);
      end;
    opSizeof: //Special handling if runtime calculations are needed
      //Leave array literals for compile time evaluation
      if IsImmediate then
        EXIT(qeNone)
      else if IsUnbounded then
      begin
        Assert(SlugIndex = 0);
        //Unbounded arrays can be processed at run-time but need an extra argument
        MassageUnboundedSizeOf(Slugs)
      end
      else if ArrayType = atList then
      begin //We can determine sizeof all bounded lists at compile time so
        //skip error checking and other processing
        if not IsTypeDef then
          SlugToTypeDef(Slugs[SlugIndex]);
        EXIT(qeNone);
      end;
    opCapacity, opSetLength: //We require an atList
      if (ArrayType <> atList) or IsImmediate then
        EXIT(ErrSub2(qeTypeMismatch, ArrayUT.Description, Param.UserType.Description));
    opLength: ; //No further processing
  else
  end;

  if IsTypeDef and (IsUnbounded or (ArrayType = atList)) then
    //A TypeDef is a compile time constant. Operations on unbounded arrays and list
    //are runtime only.
    //TODO: Func Param doesn't specify arrays!!
    EXIT(ErrSub2(qeTypeMismatch, ArrayUT.Description, Param.UserType.Description));

  //Leave array literals for evaluation
  if IsImmediate then
    EXIT(qeNone);

  if IsUnbounded or (ArrayType = atList) then
  begin //Operations on unbounded arrays and list require a VarRef at runtime
    Assert(Slugs[0].ILItem = nil);  //TODO: Syntax error if we have an expression??
    Assert(Slugs[0].Operand.Kind = pkVarSource);
    Slugs[0].Operand.Kind := pkVarRef;
  end
  else
    //If size can be determined at compile time then convert the argument to a
    //TypeDef with value equal to the argument's type
    if not IsTypeDef and not IsUnbounded and (ArrayType <> atList) then
      SlugToTypeDef(Slugs[SlugIndex]);

  Result := qeNone;
end;

//Apply MassageArrayArgument to any parameter which is having an array passed to it
function MassageArrayArguments(Func: PFunction;var Slugs: TSlugArray): TQuicheError;
var I: Integer;
begin
  for I := 0 to Func.ParamCount-1 do
    if (Slugs[I].ResultVarType = vtArrayType) or
      (Slugs[I].ResultVarType = vtTypeDef) and (Slugs[I].Operand.Imm.TypeDefValue.VarType = vtArrayType) then
    begin
        Result := MassageArrayArgument(Func.Op, @Func.Params[I], Slugs, I);
        if Result <> qeNone then
          EXIT;
    end;

  Result := qeNone;
end;

//For any parameters which require a vtTypeDef, ensures the argument is a vtTypeDef
//**Unless** the parameter is a vtArrayType
//(ie if it is not then convert it to one);
procedure SolidifyTypeDefs(Func: PFunction;var Slugs: TSlugArray);
var I: Integer;
begin
  for I := 0 to Func.ParamCount-1 do
    if Func.Params[I].VarType = vtTypeDef then
      //TODO: Not if Slug is an array???
      if not (Slugs[I].ResultVarType in [vtTypeDef, vtArrayType]) then
        SlugToTypeDef(Slugs[I]);
end;

//Solidify a parameterized result type:
//If ResultType is specified as Parameterized it's type is given via a parameter
//of type TypeDef. We need to find that parameter and 'solidify' the type of both
//that parameter and the result to the compile time value of that TypeDef parameter.
//Also handles parameters tagged with ifToType. These need to be converted to a
//Immediate of type vtVarType which is the same as the parameterised result.
function SolidifyParameterisedResultType(Func: PFunction;var Slugs: TSlugArray;var ResultType: PUserType): TQuicheError;
var Param: PParameter;
  Slug: PExprSlug;
  I: Integer;
begin
//  ResultType := nil;
  //We we have a parameterised result?
  if Func.ResultCount = 0 then
    EXIT(qeNone);
  if (Func.Params[Func.ParamCount].UserType <> nil) then
    EXIT(qeNone);
  if Func.Params[Func.ParamCount].SuperType <> stParameterized then
    EXIT(qeNone);

  //Find the parameter we will use to solidify the result
  Param := nil;
  Slug := nil;
  for I := 0 to Func.ParamCount-1 do
    if Func.Params[I].VarType = vtTypeDef then
    begin
      Assert(Param = nil);  //Multiple parameterised params is an error
      Param := @Func.Params[I];
      Slug := @Slugs[I];
    end;
  if Param = nil then
    EXIT(qeNone);

  if ifToType in Param.IntrinsicFlags then
  begin //Convert the TypeDef to an immediate of the given type...
    Assert(Slug.IsImmediate);
    ResultType := Slug.Operand.Imm.TypeDefValue;

    Slug.ResultType := ResultType;
    //...the value we'll use will be the extreme value of that type to force
    //the prim selector to return a routine of that type
    if IsSignedType(Slug.ResultType) then
      Result := GetTypeLowValue(Slug.ResultType, Slug.Operand.Imm)
    else
      Result := GetTypeHighValue(Slug.ResultType, Slug.Operand.Imm);
    if Result <> qeNone then
      EXIT;
  end
  else
    Result := qeNone;
end;

function DispatchIntrinsic(Func: PFunction;var Slugs: TSlugArray;out Slug: TExprSlug): TQuicheError;
var
  ResultType: PUserType;
  ResultTypeDebug: PUserType; //Only used for error messaging
  PrimResultType: PUserType;
  Found: Boolean;
  LType: PUserType;
  RType: PUserType;
  Msg: String;
  Evalled: Boolean;
  ParamCount: Integer;
begin
  //Special handling of array arguments
  Result := MassageArrayArguments(Func, Slugs);
  if Result <> qeNone then
    EXIT;

  //If the parameter wants a TypeDef then ensure we supply one.
  SolidifyTypeDefs(Func, Slugs);

  //Returns de-parameterised ResultType, or nil
  ResultType := nil;
  Result := SolidifyParameterisedResultType(Func, Slugs, ResultType);
  if Result <> qeNone then
    EXIT;

  ResultTypeDebug := ResultType;

  PrimResultType := ResultType;
  Found := False;
  //Find a suitable Primitive - to validate these parameter types are suitable, and
  //establish the result type
  if Slugs[0].ResultType = nil then
  begin //No parameters - just return the functions return type
    ParamCount := 0;
    LType := nil;
    RType := nil;
    Found := True;
    PrimResultType := Func.Params[0].UserType;
  end
  else if Slugs[1].ResultType = nil then
  begin
    ParamCount := 1;
    LType := Slugs[0].ResultType;
    RType := nil;
    Found := PrimFindParseUnary(Func.Op, Slugs[0], LType, PrimResultType);
  end
  else if Slugs[2].ResultType = nil then
  begin
    ParamCount := 2;
    LType := Slugs[0].ResultType;
    RType := Slugs[1].ResultType;
    Found := PrimFindParse(Func.Op, Slugs[0], Slugs[1], LType, RType, PrimResultType);
  end
  else
    Assert(False);

  //No primitive found :(
  if not Found then
  begin
    Msg := Slugs[0].ResultType.Description;
    if RType <> nil then
      Msg := Msg + ', ' + Slugs[1].ResultType.Description;
    Msg := '(' + Msg + ')';
    if Assigned(ResultTypeDebug) then
      Msg := Msg + ': ' + ResultTypeDebug.Name;

    EXIT(errFuncCallSub(qeFuncPrimitivenotFound, Func.Name + Msg, Func));
  end;

  Slug.Initialise;
  //Default value - might be modified by these functions below
  Slug.ResultType := PrimResultType;
  Slug.ImplicitType := PrimResultType;

  //Can we eval this at run time (and return an immediate value)?
  Result := TryEvalIntrinsic(Func, Slugs[0], Slugs[1], Slug, Evalled);
  if Result <> qeNone then
    EXIT;

  if not Evalled then
    //Generate the IL code :)
    IntrinsicGenerateIL(Func, opUnknown, ParamCount, Slugs[0], Slugs[1], Slug);
end;

//===================================== WRITE(LN)

function WriteOrdinalGenIL(Func: PFunction;const Slug: TExprSlug): TQuicheError;
var DummySlug: TExprSlug;
begin
  //Verify the argument is a type we can handle
  if not (Slug.ResultVarType in [vtInt8, vtInteger, vtByte, vtWord, vtPointer,
    vtBoolean, vtChar]) then
    EXIT(ErrTODO('Unhandled parameter type for Write/ln Ordinal: ' + Slug.ResultType.Description));

  //Generate the code.
  //NOTE: We need to pass in two slugs. Second will be ignored because of ParamCount value of 1
  DummySlug.Initialise;
  DummySlug.ResultType := nil;
  DummySlug.ImplicitType := nil;
  IntrinsicGenerateIL(Func, opWrite, 1, Slug, Slug, DummySlug);

  Result := qeNone;
end;


//Write and Writeln are special cases!
function DispatchWrite(Func: PFunction;NewLine: Boolean): TQuicheError;
var Ch: Char;
  Brace: Boolean; //Is parameter list wrapped in braces?
  Slug: TExprSlug;
  DummySlug: TExprSlug;
begin
  //!!Don't skip whitespace: Brace indicating parameter list must come immediately after function
  Ch := Parser.TestChar;
  //Parameters?
  if not Parser.EOS then
  begin
    Brace := Ch = '(';
    if Brace then
      Parser.SkipChar;

    //Empty parameter list?
    Ch := Parser.TestChar;
    if (not Brace) or (Brace and (Ch <> ')')) then
    repeat
      //Read the argument
      Result := ParseArgument(Func, Func.Params[0], Slug);
      if Result <> qeNone then
        EXIT;

      //Validate the argument
      Result := ProcessArgument(Func, Func.CallingConvention, 0, Slug);
      if Result <> qeNone then
        EXIT;

      //Generate the code.
      if IsOrdinalType(Slug.ResultType) then
        Result := WriteOrdinalGenIL(Func, Slug)
      else
        EXIT(ErrTODO('Unhandled parameter type for Write/ln: ' + Slug.ResultType.Description));
      if Result <> qeNone then
        EXIT;

      Parser.SkipWhite;
      Ch := Parser.TestChar;
      if Ch = ',' then
        Parser.SkipChar;
    until Ch <> ',';

    if Brace then
      if Ch = ')' then
        Parser.SkipChar
      else
        EXIT(Err(qeCommaOrCloseParensExpected));
  end;

  if NewLine then
    ILAppend(OpWriteln);
  Result := qeNone;
end;

//===================================== DISPATCH (EXCEPT INSTRINSICS)

//IL code for Register calling convention
function DispatchRegister(Func: PFunction;var Slugs: TSlugArray;var Slug: TExprSlug): PILItem;
var
  ArgIndex: Integer;
  Arg: PParameter;
  ILItem: PILItem;
  Param: PILParam;
begin
  //NOTE: The following assumes values being passed are appropriate for the functions arguments

//1. Assign any expressions to variables

  //For any arguments which are being passed an expression:
  //assign the result of the expression to a hidden variable.
  for ArgIndex := 0 to Func.ParamCount-1 do
    if Slugs[ArgIndex].ILItem <> nil then
      Slugs[ArgIndex].AssignToHiddenVar;

//2. Generate IL code to load arguments into registers

  //We haven't created any IL data yet
  ILItem := nil;

  //Load any input parameters
  for ArgIndex := 0 to Func.ParamCount + Func.ResultCount - 1 do
    if Func.Params[ArgIndex].PassDataIn then
    begin
      //Allocate a Param
      Param := ILAppendFuncData(ILItem);

      if Func.Params[ArgIndex].Access = paResult then
      begin
        //Result will not have a Slug in Slugs. We need to set Slug.ResultByRefParam
        //so caller can update the Param to load the VarRef
        Param.Initialise;
        Param.Reg := Func.Params[ArgIndex].Reg;
        Slug.ResultByRefParam := Param;
      end
      else
      begin
        Param^ := Slugs[ArgIndex].Operand;
        Param.Reg := Func.Params[ArgIndex].Reg;
        if Func.Params[ArgIndex].IsByRef then
        begin
          Assert(Param.Kind in [pkVarSource, pkVarDest]);
          Param.Kind := pkVarRef;
        end;
      end;
    end;

//3. Generate IL code to write returned values into variables.

  //Store any output parameters
  for ArgIndex := 0 to Func.ParamCount - 1 do
    if Func.Params[ArgIndex].ReturnsData then
    begin
      //Allocate a Param
      Param := ILAppendFuncData(ILItem);

      //We need to write the returned value to a variable, so Param must be
      //pkVarDest + variable + VarVersion
      Param^ := Slugs[ArgIndex].Operand; //???
      Assert(Param.Kind in [pkVarSource, pkVarDest]);
      if Param.Kind = pkVarSource then
        Param.Kind := pkVarDest;
      Param.Reg := Func.Params[ArgIndex].Reg;
      Param.Variable.IncVersion;
      Param.VarVersion := Param.Variable.Version;
    end;

//4. Generate IL code for the function call itself

  //Add the function call
  ILAppendFuncCall(ILItem, Func);

//5, Generate IL code to write a function Result to a variable

  //Return value has to be the very last one we do - it will be assigned by caller
  if Func.ResultCount > 0 then
  begin
    Arg := Func.FindResult;
    if Arg.ReturnsData then
    begin
      Param := ILAppendFuncResult(ILItem);
      ILItem.ResultType := Arg.UserType;
      Param.Reg := Arg.Reg;
    end;
  end;
  Result := ILItem;
end;

function DispatchStack(Func: PFunction;var Slug: TExprSlug): PILItem;
var Arg: PParameter;
  ILItem: PILItem;
begin
  //If we have a Result and it's Pass-By-Ref (i.e. a Pointered Type) then we need to
  //also push the data address on the stack
  if Func.ResultCount > 0 then
  begin
    Arg := Func.FindResult;
    if Arg.PassDataIn then
    begin
      ILItem := ILAppend(opMove);
      ILItem.Param1.Kind := pkNone;  //Variable data to be set later
      Slug.ResultByRefParam := @ILItem.Param1;
      ILItem.ResultType := GetPointerToType(Arg.UserType);
      ILItem.Dest.Kind := pkPush;
      ILItem.Dest.PushType := ILItem.ResultType;
    end;
  end;

  Result := nil;
  ILAppendFuncCall(Result, Func);

  //Process return value(s)
  if (Func.ResultCount > 0) and Func.FindResult.ReturnsData then
  begin
    ILAppendFuncResult(Result);
    Arg := Func.FindResult;
    Result.ResultType := Arg.UserType;

    case GetTypeRegSize(Arg.UserType) of
      1: Result.Dest.Reg := rA;   //Byte params returned in A
      2: Result.Dest.Reg := rHL;  //2 byte params returned in HL
    else
      Assert(False, 'Uncoded result type');
    end;
  end;
end;



//IL code for Stack calling convention
//1. Generate code for parameter. Address of var if needed - var, out, Result
//2. Generate code to push each parameter in turn to the stack
//3. Generate code to dispatch the call (via stack frame code)
//4. Generate code to store return value (var, out, Result)
//     into vars/temp vars (temp var only for result)
//5. Generate code to cleanup the stack if needed.

function DoParseProcedureCall(Func: PFunction): TQuicheError;
var Slugs: TSlugArray;  //Data and IL code for each argument
  DummySlug: TExprSlug;  //Dummy
begin
  //Special handling required for these puppies...
  if Func.CallingConvention = ccintrinsic then
    case Func.Op of
      opWrite: EXIT(DispatchWrite(Func, False));
      opWriteln: EXIT(DispatchWrite(Func, True));
    end;

  //Parse and validate arguments
  Result := ParseArgList(Func, Slugs);
  if Result <> qeNone then
    EXIT;

  //Generate IL code for the parameters & call
  case Func.CallingConvention of
    ccRegister, ccCall, ccRST, ccExtern: DispatchRegister(Func, Slugs, DummySlug);
    ccIntrinsic:
        Result := DispatchIntrinsic(Func, Slugs, DummySlug);
    ccStack: DispatchStack(Func, DummySlug);
  else
    raise ECallingConvention.Create;
  end;
  if Result <> qeNone then
    EXIT;

  //Generate IL code for after call/stack cleanup etc.
end;

function DoParseFunctionCall(Func: PFunction;var Slug: TExprSlug): TQuicheError;
var Slugs: TSlugArray;
  Param: PParameter;
begin
  if Func.ResultCount = 0 then
    EXIT(ErrFuncCall(qeCantAssignProcedure, Func));

  //Parse and validate arguments
  Result := ParseArgList(Func, Slugs);
  if Result <> qeNone then
    EXIT;

  //Generate IL code for the parameters & call
  case Func.CallingConvention of
    ccRegister, ccCall, ccRST, ccExtern: Slug.ILItem := DispatchRegister(Func, Slugs, Slug);
    ccIntrinsic: Result := DispatchIntrinsic(Func, Slugs, Slug);
    ccStack: Slug.ILItem := DispatchStack(Func, Slug);
  else
    raise ECallingConvention.Create;
  end;
  if Result <> qeNone then
    EXIT;

  if Func.CallingConvention <> ccIntrinsic then
  //Process return value(s)
  begin
    Param := Func.FindResult;
    begin
      Slug.ResultType := Param.UserType;
      Slug.ImplicitType := Param.UserType;
    end;
  end;
end;

end.
