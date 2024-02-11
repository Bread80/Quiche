unit ParseFuncCall;

interface
uses QTypes, ParseErrors, Functions, ParseExpr;

//Parse a procedure call, or a function call with the result being ignored.
function DoParseProcedureCall(Func: PFunction): TQuicheError;

//Parse a function call (which returns a value)
function DoParseFunctionCall(Func: PFunction;var Slug: TExprSlug): TQuicheError;


implementation
uses SysUtils, ILData, Operators, ParserBase, Variables;

const ParamRegToAllocLoc: array[low(TParamReg)..high(TParamReg)] of TCPUReg =
  (rNone, rA, rB, rC, rD, rE, rH, rL, rBC, rDE, rHL, rCF, rZF);


type TSlugArray= array[0..MaxFunctionParams] of TExprSlug;

function ParseArgument(Func: PFunction;CallingConvention: TCallingConvention;
  Arg: TParameter;var Slug: TExprSlug): TQuicheError;
var VType: TVarType;
  ILItem: PILItem;
begin
  Slug.Initialise;

  //Parse argument - and validate type compatibility
  VType := Arg.VarType;
  Result := ParseExpressionToSlug(Slug, VType);
  if Result <> qeNone then
    EXIT;

  //Validate argument:
  case Arg.Access of
    vaVal, vaConst, vaResult: ;  //Any
    vaVar, vaOut:
    begin //Argument needs to be a variable reference.
      if (Slug.ILItem <> nil) or (Slug.Operand.Kind <> pkVar) then
        EXIT(ErrFuncCall('Parameter   ' + Arg.Name +
          ''' must be a simple variable reference (VAR and OUT parameter).', Func));
      end;
//      paIn: ;
    else
      raise Exception.Create('Unknown access specifier');
    end;

  //Do we need to push this argument on the stack?
  case CallingConvention of
    ccRegister: ; //Registers will be loaded later
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
        Assert(False, 'Item to large for stack - needs to be passed as pointer');
      end;
    end;
  else
    Assert(False, 'Invalid calling convention');
  end;
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

    Result := ParseArgument(Func, Func.CallingConvention, Func.Params[ArgIndex], Slugs[ArgIndex]);
    if Result <> qeNone then
      EXIT;

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

  //Validate number of arguments
  if ArgIndex <> Func.ParamCount then
    EXIT(ErrFuncCall('Not enough parameters (found '+ ArgIndex.ToString +
      ', wanted '+ Func.ParamCount.ToString +')', Func));

  Result := qeNone;
end;

//IL code for Register calling convention
function DoRegister(Func: PFunction;var Slugs: TSlugArray): PILItem;
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
  //NOTEL: The following assumes values being passed are appropriate for the functions arguments

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

function DoStack(Func: PFunction;var Slugs: TSlugArray): PILItem;
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
begin
  //Parse and validate arguments
  Result := ParseArgList(Func, Slugs);
  if Result <> qeNone then
    EXIT;

  //Generate IL code for the parameters & call
  case Func.CallingConvention of
    ccRegister: DoRegister(Func, Slugs);
    ccStackLocal: DoStack(Func, Slugs);
//    ccIntrinsic: Result := ErrMsg(qeBUG, 'Instrinic dispatch shouldn''t end up here');  //Special cases :)
  else
    raise Exception.Create('Unknown calling convention in function despatch :(');
  end;

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
    ccRegister: Slug.ILItem := DoRegister(Func, Slugs);
//    ccIntrinsic: Result := ErrMsg(qeBUG, 'Instrinic dispatch shouldn''t end up here');  //Special cases :)
    ccStackLocal: Slug.ILItem := DoStack(Func, Slugs);
  else
    raise Exception.Create('Unknown calling convention in function despatch :(');
  end;

  //Generate IL code for after call/saving result
  Param := Func.FindResult;
//  Slug.Op := OpFuncCall;
  Slug.ResultType := Param.VarType;
  Slug.ImplicitType := Param.VarType;
//  Slug.OpType := Slug.ILItem.ResultType;
end;

end.
