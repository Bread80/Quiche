unit ParseFuncCall;

interface
uses QTypes, ParseErrors, Functions, ParseExpr;

//Parse a procedure call, or a function call with the result being ignored.
function DoParseProcedureCall(Func: PFunction): TQuicheError;

//Parse a function call (which returns a value)
function DoParseFunctionCall(Func: PFunction;var Slug: TExprSlug): TQuicheError;


implementation
uses SysUtils, ILData, Operators, PrimitivesEx, ParserBase, Variables, Eval;

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
      if (Slug.ILItem <> nil) or (Slug.Operand.Kind <> pkVar) then
        EXIT(ErrFuncCall('Argument ''' + Arg.Name +
          ''' must be a variable', Func));
      end;
    else
      Assert(False, 'Unknown access specifier');
    end;

  //Do we need to push this argument on the stack?
  case CallingConvention of
    ccRegister, ccIntrinsic: ; //Registers will be loaded later
    ccStackLocal: //Put parameters on the stack
    begin
      ILItem := Slug.ToILItemNoDest(dtData);
      if ILItem.Op = OpUnknown then
        ILItem.Op := OpMove;
      ILItem.ResultType := VarTypeToOpType(Arg.VarType);
      //Slug to ILItem
      case GetTypeSize(Arg.VarType) of
        1: ILItem.Dest.Kind := pkStackByte;//ILItem to PUSHBYTE
        2: ILItem.Dest.Kind := pkStack;//ILItem to PUSH
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
  if Brace or (Brace and (Ch <> ')')) then
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
    if Func.Op in [opHi, opLo] then
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
      Slugs[ArgIndex].Operand.ImmType := Func.Params[ArgIndex].VarType
    else  //SuperTypes - we have an Intrinsic!
      if Func.Params[ArgIndex].SuperType in [stAnyInteger, stEnumerable] then
        //TODO: This should give a better analysis of the type
        Slugs[ArgIndex].Operand.ImmType := vtInteger;//ValueToVarType(Func.Params[ArgIndex].DefaultValueInt);

    Slugs[ArgIndex].Operand.ImmValueInt := Func.Params[ArgIndex].DefaultValueInt;
    Slugs[ArgIndex].ResultType := Slugs[ArgIndex].Operand.ImmType;
    Slugs[ArgIndex].ImplicitType := Slugs[ArgIndex].Operand.ImmType;
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

function DispatchIntrinsic(Func: PFunction;var Slugs: TSlugArray;out Slug: TExprSlug): TQuicheError;
var LType: TVarType;
  LRange: TNumberRange;
  LIsRange: Boolean;
  RRange: TNumberRange;
  RIsRange: Boolean;
  RType: TVarType;
  Found: Boolean;
  Msg: String;
  Value: Integer;
  V: PVariable;
begin
  RType := vtUnknown;
  LType := vtUnknown;
  LIsRange := False;
  RIsRange := False;
  if Func.ParamCount >= 1 then

    if IsNumericType(Slugs[0].ResultType) and
      not Assigned(Slugs[0].ILItem) and (Slugs[0].Operand.Kind = pkImmediate) then
    begin
      LRange := IntToNumberRange(Slugs[0].Operand.ImmValueInt);
      LIsRange := True;
    end
    else
      LType := Slugs[0].ResultType;
  if Func.ParamCount = 2 then
    if IsNumericType(Slugs[0].ResultType) and
      not Assigned(Slugs[1].ILItem) and (Slugs[1].Operand.Kind = pkImmediate) then
    begin
      RRange := IntToNumberRange(Slugs[1].Operand.ImmValueInt);
      RIsRange := True;
    end
    else
      RType := Slugs[1].ResultType;

  Slug.Initialise;
  if LIsRange then
    if RIsRange then
      Found := PrimFindBestMatchRangeRange(Func.Op, LType, RType, LRange, RRange, Slug.ResultType)
    else
      Found := PrimFindBestMatchRangeVar(Func.Op, LType, RType, LRange, Slug.ResultType)
  else //Not LIsRange
    if RIsRange then
      Found := PrimFindBestMatchVarRange(Func.Op, LType, RType, RRange, Slug.ResultType)
    else
      Found := PrimFindBestMatchVarVar(Func.Op, LType, RType, Slug.ResultType);
  if not Found then
  begin
    Msg := VarTypeToName(Slugs[0].ResultType);
    if RType <> vtUnknown then
      Msg := Msg + ', ' + VarTypeToName(Slugs[1].ResultType);
    EXIT(errFuncCall('Invalid argument types for function ''' + Func.Name + ''': (' + Msg + ')', Func));
  end;
  Slug.ImplicitType := Slug.ResultType;

  //Check for, and evaluate, contant expressions
  if not Assigned(Slugs[0].ILItem) and (Slugs[0].Operand.Kind = pkImmediate) then
    if Func.ParamCount = 1 then
    begin
      Result := EvalIntrinsicUnary(Func.Op, Slugs[0].Operand,
        Slug.Operand.ImmValueInt, Slug.Operand.ImmType);
      if Result = qeIntrinsicCantBeEvaluatedAtCompileTime then
        //Continue with IL code generatio
        Result := qeNone
      else
      begin
        if Result <> qeNone then
          EXIT;
        Slug.ResultType := Slug.Operand.ImmType;
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
          Slug.Operand.ImmValueInt, Slug.Operand.ImmType);
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


  if (Func.ParamCount = 1) and (Slugs[0].ILItem <> nil) and (Slugs[0].ILItem.Op = opUnknown) then
  begin //We can just 'patch' the Op into the ILItem
    Slug := Slugs[0];
    Slug.ILItem.Op := Func.Op;
    Slug.ILItem.OpType := VarTypeToOpType(Slug.ResultType);
  end
  else
  begin //Create the ILItem for the operation
    Slug.ILItem := ILAppend(dtNone, Func.Op);
    Slug.ILItem.OpType := VarTypeToOpType(Slug.ResultType);

    if Func.ParamCount >= 1 then
    begin //First parameter
      if Slugs[0].ILItem <> nil then
      begin //We have an ILItem. Assign it to a temp var and use that
        Slugs[0].AssignToHiddenVar;
        Slug.ILItem.Param1.SetVariable(Slugs[0].ILItem.Dest.Variable);
      end
      else  //Otherwise it's either a constant or variable we can assign directly
        Slug.ILItem.Param1 := Slugs[0].Operand;
    end;

    if Func.ParamCount >= 2 then
    begin //Same for second parameter
      if Slugs[1].ILItem <> nil then
      begin
        Slugs[1].AssignToHiddenVar;
        Slug.ILItem.Param2.SetVariable(Slugs[1].ILItem.Dest.Variable);
      end
      else
        Slug.ILItem.Param2 := Slugs[1].Operand;
    end;
  end;

  if Func.Params[0].Access = vaVar then
  begin //Var parameter - result is written back to Param1
    Slug.ILItem.DestType := dtData;
    V := Slug.ILItem.Param1.Variable;
    V.IncWriteCount;
    Slug.ILItem.Dest.SetVarAndSub(V, V.WriteCount);
    Slug.ILItem.ResultType := VarTypeToOpType(V.VarType);
    Slug.ILItem.OpType := Slug.ILItem.ResultType;
  end
  else
    Slug.ILItem.ResultType := Slug.ILItem.OpType;
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
    ILItem := ILAppend(dtNone, OpFuncCall);
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
            if Func.ResultCount > 0 then
              ILItem := ILAppend(dtData, OpFuncCall)
            else
              ILItem := ILAppend(dtNone, OpFuncCall);
            Result := ILItem; //Pass CALL back in case we're in a function call
            ILItem.Func := Func;
          end
          else
//            raise Exception.Create('Register function with more than three params.');
            ILItem := ILAppend(dtDataLoad, OpDataLoad);
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
                ILItem.Dest.Reg := ParamRegToAllocLoc[Func.Params[ArgIndex].Reg];
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
  if Func.ResultCount > 0 then
    Result := ILAppend(dtData, OpFuncCall)
  else
    Result := ILAppend(dtNone, OpFuncCall);
  Result.Func := Func;

  //Process return value(s)
  if Func.ResultCount > 0 then
  begin
    Param := Func.FindResult;
    case GetTypeSize(Param.VarType) of
      1: Result.Dest.Reg := rA;   //Byte params returned in A
      2: Result.Dest.Reg := rHL;  //2 byte params returned in HL
    else
      Assert(False, 'Uncoded result type');
    end;
    Result.ResultType := VarTypeToOpType(Param.VarType);
    Result.OpType := Result.ResultType;
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
  //Parse and validate arguments
  Result := ParseArgList(Func, Slugs);
  if Result <> qeNone then
    EXIT;

  //Generate IL code for the parameters & call
  case Func.CallingConvention of
    ccRegister: DispatchRegister(Func, Slugs);
    ccIntrinsic: Result := DispatchIntrinsic(Func, Slugs, DummySlug);
    ccStackLocal: DispatchStack(Func, Slugs);
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
  //Parse and validate arguments
  Result := ParseArgList(Func, Slugs);
  if Result <> qeNone then
    EXIT;

  //Generate IL code for the parameters & call
  case Func.CallingConvention of
    ccRegister: Slug.ILItem := DispatchRegister(Func, Slugs);
    ccIntrinsic: Result := DispatchIntrinsic(Func, Slugs, Slug);
    ccStackLocal: Slug.ILItem := DispatchStack(Func, Slugs);
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
