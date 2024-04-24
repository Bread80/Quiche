unit ParseFuncCall;

interface
uses QTypes, ParseErrors, Functions, ParseExpr;

//Parse a procedure call, or a function call with the result being ignored.
function DoParseProcedureCall(Func: PFunction): TQuicheError;

//Parse a function call (which returns a value)
function DoParseFunctionCall(Func: PFunction;var Slug: TExprSlug): TQuicheError;


implementation
uses SysUtils, ILData, Operators, PrimitivesEx, ParserBase, Variables, Eval,
  Z80.CPU;

const ParamRegToAllocLoc: array[low(TParamReg)..high(TParamReg)] of TCPUReg =
  (rNone, rA, rB, rC, rD, rE, rH, rL, rBC, rDE, rHL, rCF, rZF);


type TSlugArray= array[0..MaxFunctionParams] of TExprSlug;

//Read an argument from the source code, and return it in a Slug
function ParseArgument(Arg: TParameter;var Slug: TExprSlug): TQuicheError;
var VType: TVarType;
  ILItem: PILItem;
begin
  Slug.Initialise;

  //Parse argument - and validate type compatibility
  VType := Arg.VarType;
  Result := ParseExpressionToSlug(Slug, VType);
end;

//Validate an argument and process as necessary, depending on the calling
//convention (e.g. writing it to a hidden variable or pushing it onto the stack.
function ProcessArgument(Func: PFunction;CallingConvention: TCallingConvention;
  Arg: TParameter;var Slug: TExprSlug): TQuicheError;
var ILItem: PILItem;
begin
  //Validate argument:
  case Arg.Access of
    vaVal, vaConst, vaResult: ;  //Any
    vaVar, vaOut:
    begin //Argument needs to be a variable reference.
      if (Slug.ILItem <> nil) or (Slug.Operand.Kind <> pkVarSource) then
        EXIT(ErrFuncCall('Argument ''' + Arg.Name +
          ''' must be a variable', Func));
      end;
    else
      Assert(False, 'Unknown access specifier');
    end;

  //Do we need to push this argument on the stack?
  case CallingConvention of
    ccRegister, ccCall, ccRST, ccIntrinsic: ; //Registers will be loaded later
    ccStack: //Put parameters on the stack
    begin
      ILItem := Slug.ToILItemNoDest;
      if ILItem.Op = OpUnknown then
        ILItem.Op := OpMove;
      ILItem.ResultType := Arg.VarType;
      //Slug to ILItem
      case GetTypeSize(Arg.VarType) of
        1: ILItem.Dest.Kind := pkPushByte;//ILItem to PUSHBYTE
        2: ILItem.Dest.Kind := pkPush;//ILItem to PUSH
      else
        Assert(False, 'Item too large for stack - needs to be passed by reference');
      end;
    end;
  else
    Assert(False, 'Invalid calling convention');
  end;
  Result := qeNone;
end;

function ParseArgList(Func: PFunction;var Slugs: TSlugArray): TQuicheError;
var
  Brace: Boolean; //True if arg list uses brackets
  Ch: Char;
  ArgIndex: Integer;  //Index into Func's argument list
begin
  Brace := Parser.TestChar = '(';
  if Brace then
    Parser.SkipChar;
  Parser.SkipWhiteSpace;

  ArgIndex := 0;

  Ch := Parser.TestChar;
  //Test for empty list
  if ((not Brace) and not CharInSet(Ch, [#0,';'])) or  //No Brace and #0 (EOLN) -> no arguments
    (Brace and (Ch <> ')')) then      //Bace and ')' -> no arguments
  repeat
    if ArgIndex >= Func.ParamCount then
      EXIT(ErrFuncCall('Too many parameters', Func));

    Result := ParseArgument(Func.Params[ArgIndex], Slugs[ArgIndex]);
    if Result <> qeNone then
      EXIT;
    Result := ProcessArgument(Func, Func.CallingConvention, Func.Params[ArgIndex], Slugs[ArgIndex]);
    if Result <> qeNone then
      EXIT;

    //Manual validation of some intrinsic properties
    if Func.Op in [opInc, opDec] then
      if (ArgIndex = 1) then
        //First arg must be an integer constant
        if (Slugs[ArgIndex].ILItem <> nil) or (Slugs[ArgIndex].Operand.Kind <> pkImmediate) then
          EXIT(errFuncCall('Argument ''' + Func.Params[1].Name + ''' must be an integer constant expression', Func));
    if Func.Op in [opHi, opLo, opSwap] then
      //Arg must be > 8 bits wide
      if (Slugs[ArgIndex].ILItem <> nil) or (Slugs[ArgIndex].Operand.Kind <> pkImmediate) then
        if GetTypeSize(Slugs[ArgIndex].ResultType) = 1 then
          EXIT(errFuncCall('Argument must be larger than one byte', Func));


    //More parameters
    Parser.SkipWhiteSpace;
    Ch := Parser.TestChar;
    if Ch = ',' then
      Parser.SkipChar;
    inc(ArgIndex);
  until Ch <> ',';

  if Brace then
    if Ch = ')' then
      Parser.SkipChar
    else
      EXIT(ErrMsg(qeSyntax, ermCommaOrCloseParensExpected));

  //Set any default parameter values
  while (ArgIndex < Func.ParamCount) and Func.Params[ArgIndex].HasDefaultValue do
  begin
    Slugs[ArgIndex].Initialise;
    Slugs[ArgIndex].Operand.Kind := pkImmediate;
    if Func.Params[ArgIndex].VarType <> vtUnknown then
      Slugs[ArgIndex].Operand.Imm.VarType := Func.Params[ArgIndex].VarType
    else  //SuperTypes - we have an Intrinsic!
      if Func.Params[ArgIndex].SuperType in [stAnyInteger, stOrdinal] then
        //TODO: This should give a better analysis of the type
        Slugs[ArgIndex].Operand.Imm.VarType := vtInteger;//ValueToVarType(Func.Params[ArgIndex].DefaultValueInt);

    Slugs[ArgIndex].Operand.Imm := Func.Params[ArgIndex].DefaultValue;
    Slugs[ArgIndex].ResultType := Slugs[ArgIndex].Operand.Imm.VarType;
    Slugs[ArgIndex].ImplicitType := Slugs[ArgIndex].Operand.Imm.VarType;
    Result := ProcessArgument(Func, Func.CallingConvention, Func.Params[ArgIndex], Slugs[ArgIndex]);
    if Result <> qeNone then
      EXIT;
    inc(ArgIndex);
  end;

  //Validate number of arguments
  if ArgIndex <> Func.ParamCount then
    EXIT(ErrFuncCall('Not enough parameters (found '+ ArgIndex.ToString +
      ', wanted '+ Func.ParamCount.ToString +')', Func));

  Result := qeNone;
end;

//Generate the IL code for an intrinsic.
//Func is the function template for teh intrinsic
//ParamCount is the number of paramaters supplied to IntrinsicGenerateIL (Left, Right).
//Valid values are 0, 1, and 2.
//This may be different to the value in Func if the intrinsic is doing funky things
//(ie. write(ln), read(ln) etc).
//Left, Right are the parameters (see ParamCount).
//Slug is the slug to be returned.
procedure IntrinsicGenerateIL(Func: PFunction;ParamCount: Integer;const Left, Right: TExprSlug;out Slug: TExprSlug);
var V: PVariable;
begin
  //Can we convert an existing ILItem to the intrinsic operation? (and save an IL step)
  if (ParamCount = 1) and (Left.ILItem <> nil) and (Left.ILItem.Op = opUnknown) then
  begin //We can just 'patch' the Op into the ILItem
    Slug := Left;
    Slug.ILItem.Op := Func.Op;
  end
  else
  begin //Create the ILItem for the operation
    Slug.ILItem := ILAppend(Func.Op);

    if ParamCount >= 1 then
    begin //First parameter
      if Left.ILItem <> nil then
      begin //We have an ILItem. Assign it to a temp var and use that
        Left.AssignToHiddenVar;
        Slug.ILItem.Param1.SetVarSource(Left.ILItem.Dest.Variable);
      end
      else  //Otherwise it's either a constant or variable we can assign directly
        Slug.ILItem.Param1 := Left.Operand;
    end;

    if Func.ParamCount >= 2 then
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

  if Func.Params[0].Access = vaVar then
  begin //Var parameter - result is written back to Param1
    V := Slug.ILItem.Param1.Variable;
    V.IncWriteCount;
    Slug.ILItem.Dest.SetVarDestAndVersion(V, V.WriteCount);
    Slug.ILItem.ResultType := V.VarType;
  end
  else
    Slug.ILItem.ResultType := Slug.ResultType;
end;

function DispatchIntrinsic(Func: PFunction;var Slugs: TSlugArray;out Slug: TExprSlug): TQuicheError;
var
  I: Integer;
  ResultType: TVarType;
  ResultTypeDebug: TVarType; //Only used for error messaging
  Found: Boolean;
  LType: TVarType;
  RType: TVarType;
  Msg: String;
begin
  Slug.Initialise;

  ResultType := vtUnknown;
  //Solidify a paramaterized intrinsic:
  //If ResultType is specified as Parameterized it's type is given via a parameter
  //of type TypeDef. We need to find that parameter and 'solidify' the type of both
  //that parameter and the result to the compile time value of that TypeDef parameter.
  if Func.ResultCount > 0 then
  begin
    if (Func.Params[Func.ParamCount].VarType = vtUnknown) and
      (Func.Params[Func.ParamCount].SuperType = stParameterized) then
      for I := 0 to Func.ParamCount-1 do
        if Func.Params[I].VarType = vtTypeDef then
        begin
          Assert((Slugs[I].ILItem = nil) and (Slugs[I].Operand.Kind = pkImmediate));
          //Update the Result Type of the slug
          ResultType := Slugs[I].Operand.Imm.TypeValue;
          Slugs[I].ResultType := ResultType;
          //Convert the TypeDef value to be the type. The actual value is ignored and irrelevent
          Slugs[I].Operand.Imm.VarType := ResultType;
          //Prim search plays havoc with constants. Here we force the Range value
          if IsSignedType(ResultType) then
            SetMinValue(Slugs[I].Operand.Imm)
          else
            SetMaxValue(Slugs[I].Operand.Imm);
        end;
  end;
  ResultTypeDebug := ResultType;

  //Find a suitable Primitive - to validate these parameter types are suitable, and
  //establish the result type
  if Func.ParamCount = 1 then
    Found := PrimFindParseUnary(Func.Op, Slugs[0], LType, ResultType)
  else //2 params
    Found := PrimFindParse(Func.Op, Slugs[0], Slugs[1], LType, RType, ResultType);
  Slug.ResultType := ResultType;
  Slug.ImplicitType := Slug.ResultType;

  //No primitive found :(
  if not Found then
  begin
    Msg := VarTypeToName(Slugs[0].ResultType);
    if RType <> vtUnknown then
      Msg := Msg + ', ' + VarTypeToName(Slugs[1].ResultType);
    Msg := '(' + Msg + ')';
    if ResultTypeDebug <> vtUnknown then
      Msg := Msg + ': ' + VarTypeToName(ResultTypeDebug);

    EXIT(errFuncCall('Couldn''t find a primitive with matching argument types: ''' + Func.Name + Msg + '''', Func));
  end;

  //Evaluate at compile time (if possible)
  if not Assigned(Slugs[0].ILItem) and (Slugs[0].Operand.Kind = pkImmediate) then
    if Func.ParamCount = 1 then
    begin
      Result := EvalIntrinsicUnary(Func.Op, Slugs[0].Operand,
        Slug.Operand.Imm);
      if Result = qeIntrinsicCantBeEvaluatedAtCompileTime then
        //Continue with IL code generatio
        Result := qeNone
      else
      begin
        if Result <> qeNone then
          EXIT;
        Slug.ResultType := Slug.Operand.Imm.VarType;
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
      if not Assigned(Slugs[1].ILItem) and (Slugs[1].Operand.Kind = pkImmediate) then
      begin
        Result := EvalIntrinsicBi(Func.Op, Slugs[0].Operand, Slugs[1].Operand,
          Slug.Operand.Imm);
        if Result = qeIntrinsicCantBeEvaluatedAtCompileTime then
          Result := qeNone
        else
        begin
          if Result <> qeNone then
            EXIT;
          Slug.Operand.Kind := pkImmediate;
        //TODO: As previous TODO
//        Slug.ResultType := Slug.Operand.ImmType;
//        Slug.ImplicitType := Slug.ResultType;
          EXIT;
        end;
      end;

  //Generate the IL code :)
  IntrinsicGenerateIL(Func, Func.ParamCount, Slugs[0], Slugs[1], Slug);
end;


//Write and Writeln are special cases!
function DispatchWrite(Func: PFunction;NewLine: Boolean): TQuicheError;
var Ch: Char;
  Brace: Boolean; //Is parameter list wrapped in braces?
  Slug: TExprSlug;
  ILItem: PILItem;
  VType: TVarType;
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
      Result := ParseArgument(Func.Params[0], Slug);
      if Result <> qeNone then
        EXIT;

      //Validate the argument
      Result := ProcessArgument(Func, Func.CallingConvention, Func.Params[0], Slug);
      if Result <> qeNone then
        EXIT;

      //Verify the argument is a type we can handle
      if Slug.ResultType in [vtInt8, vtInteger, vtByte, vtWord, vtPointer,
        vtBoolean, vtChar] then
        ILItem.Op := OpWrite
      else
        EXIT(ErrMsg(qeTodo, 'Unhandled parameter type for Write/ln: ' + VarTypeToName(VType)));

      //Generate the code.
      //NOTE: We need to pass in two slugs. Second will be ignored because of ParamCount value of 1
      IntrinsicGenerateIL(Func, 1, Slug, Slug, DummySlug);

      Parser.SkipWhiteSpace;
      Ch := Parser.TestChar;
      if Ch = ',' then
        Parser.SkipChar;
    until Ch <> ',';

    if Brace then
      if Ch = ')' then
        Parser.SkipChar
      else
        EXIT(ErrMsg(qeSyntax, ermCommaOrCloseParensExpected));
  end;

  if NewLine then
    ILItem := ILAppend(OpWriteln);
  Result := qeNone;
end;

//IL code for Register calling convention
function DispatchRegister(Func: PFunction;var Slugs: TSlugArray): PILItem;
var
  InParamCount: Integer;  //Number of parameters being passed *into* the function
                          //I.e. excluding Out and Result paramaters being returned
  ArgIndex: Integer;
  Arg: TParameter;
  ILParamIndex: Integer;  //Each ILItem can store 3 parameters (0: Param1, 1: Param2, 2: Dest)
                          //This is an index as to which of those we are storing the
                          //current argument into
  ILItem: PILItem;
  InParamsDone: Integer;  //Number of params processed thus far
begin
  //NOTE: The following assumes values being passed are appropriate for the functions arguments

  InParamCount := 0;
  for ArgIndex := 0 to Func.ParamCount-1 do
  begin
    //For parameters which are being passed an expression:
    //assign the result of the expression to a hidden variable.
    if Slugs[ArgIndex].ILItem <> nil then
      Slugs[ArgIndex].AssignToHiddenVar;

    //Count number of Input parameters
    if not (Func.Params[ArgIndex].Access in [vaOut, vaResult]) then
      inc(InParamCount);
  end;

//2. Generate IL code to load parameters into registers
//   (For Each SlugList/Func.Param)
//     ILType of 'load register'?
//       * From temp vars (created in step 1)
//       * Variables (and other temp vars)
//       * Addresses of vars (TODO)
//       * Immediate data
  if Func.ParamCount = 0 then
  begin
    ILItem := ILAppend(OpFuncCall);
    ILItem.Func := Func;
  end
  else
  begin
    ILParamIndex := 0;
    ArgIndex := 0;
    InParamsDone := 0;
    while ArgIndex < Func.ParamCount do
    begin
      while (ArgIndex < Func.ParamCount) and (Func.Params[ArgIndex].Access in [vaOut, vaResult]) do
        inc (ArgIndex);

      if ArgIndex < Func.ParamCount then
      begin
        //Each ILItem stores up to three parameters.
        //Create one for the first of every three arguments
        if ILParamIndex = 0 then
        begin
          if (InParamCount - InParamsDone) < 3 then
          begin
            ILItem := ILAppend(OpFuncCall);
            Result := ILItem; //Pass CALL back in case we're in a function call
            ILItem.Func := Func;
          end
          else
//            raise Exception.Create('Register function with more than three params.');
            ILItem := ILAppend(OpDataLoad);
        end;

        //Set parameter data into ILItem
        //TODO: If Func.Params[ArgCount] is Result then ...
        //  Result always goes into Dest
        //else
        case Func.Params[ArgIndex].Access of
          vaVal:
            case ILParamIndex of
              0:
              begin
                ILItem.Param1 := Slugs[ArgIndex].Operand;
                ILItem.Param1.Reg := ParamRegToAllocLoc[Func.Params[ArgIndex].Reg];
              end;
              1:
              begin
                ILItem.Param2 := Slugs[ArgIndex].Operand;
                ILItem.Param2.Reg := ParamRegToAllocLoc[Func.Params[ArgIndex].Reg];
              end;
              2:
              begin
                ILItem.Param3 := Slugs[ArgIndex].Operand;
                ILItem.Param3.Reg := ParamRegToAllocLoc[Func.Params[ArgIndex].Reg];
              end;
            end;
{
          paVar: ;
          paConst: ;
//          paIn: ;
          paOut: ;
          paResult: ;
}       else
          raise Exception.Create('Unknown access specfier');
        end;

        Inc(ILParamIndex);
        if ILParamIndex > 2 then
          ILParamIndex := 0;
      end;

      inc(ArgIndex);
    end;
  end;

//3. Generate IL code to dispatch the call
//     (Poss combine with step 2?)
//4. Generate code to store return values (var, out, Result)
//     into vars/temp vars (temp var only for Result)
end;

function DispatchStack(Func: PFunction;var Slugs: TSlugArray): PILItem;
var Param: PParameter;
begin
  Result := ILAppend(OpFuncCall);
  Result.Func := Func;

  //Process return value(s)
  if Func.ResultCount > 0 then
  begin
    //NOTE: Setting Dest data is now the task of the Register allocator
    //THIS (COMMENTED) CODE TO BE REMOVED
{    Result.SetDestType(dtData);
    case GetTypeSize(Param.VarType) of
      1: Result.Dest.Reg := rA;   //Byte params returned in A
      2: Result.Dest.Reg := rHL;  //2 byte params returned in HL
    else
      Assert(False, 'Uncoded result type');
    end;
}
    Param := Func.FindResult;
    Result.ResultType := Param.VarType;
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
    ccRegister, ccCall, ccRST: DispatchRegister(Func, Slugs);
    ccIntrinsic:
        Result := DispatchIntrinsic(Func, Slugs, DummySlug);
    ccStack: DispatchStack(Func, Slugs);
  else
    raise Exception.Create('Unknown calling convention in function despatch :(');
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
    EXIT(ErrFuncCall(ermCantAssignProcedure, Func));

  //Parse and validate arguments
  Result := ParseArgList(Func, Slugs);
  if Result <> qeNone then
    EXIT;

  //Generate IL code for the parameters & call
  case Func.CallingConvention of
    ccRegister, ccCall, ccRST: Slug.ILItem := DispatchRegister(Func, Slugs);
    ccIntrinsic: Result := DispatchIntrinsic(Func, Slugs, Slug);
    ccStack: Slug.ILItem := DispatchStack(Func, Slugs);
  else
    raise Exception.Create('Unknown calling convention in function despatch :(');
  end;
  if Result <> qeNone then
    EXIT;

  //Generate IL code for after call/saving result
  if Func.CallingConvention <> ccIntrinsic then
  begin
    Param := Func.FindResult;
    Slug.ResultType := Param.VarType;
    Slug.ImplicitType := Param.VarType;
  end;
end;

end.
