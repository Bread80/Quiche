unit Functions;

interface
uses QTypes, Classes, Generics.Collections;

//Maximum number of parameters which can be specified for a routine, including results
const MaxParams = 10;

type
  //To specify corrupted/preserved registers
  //Ideally wants to be 8 or less items so it fits into a byte
  TUsedReg = (urA, urFlags, urB, urC, urD, urE, urH, urL, urIX, urIY);
  TUsedRegSet = set of TUsedReg;

  //To specify registers etc used to pass parameters
  //CF = carry flag, ZF = zero flag
  TParamReg = (prNone, prA, prB, prC, prD, prE, prH, prL, prBC, prDE, prHL, prCF, prZF);
const ParamRegToVarType: array[low(TParamReg)..high(TParamReg)] of TVarType =
    (vtUnknown, vtByte, vtByte, vtByte, vtByte, vtByte, vtByte, vtByte,
    vtWord, vtWord, vtWord, vtBoolean, vtBoolean);

type
  TParamSpecifier = (
    psVal,    //Passed as a value
    psVar,    //Passed as a reference (pointer)
    psConst,  //Value cannot be edited (allows larger data structures to be passed by reference)
    psIn,     //Input only
    psOut,    //Output only
    psResult);  //Is a result

  TFuncFlag = (
    ffForward,    //This is a forward declaration
    ffOverloaded  //Overloaded if multiple functions with the same name, but different parameters
                  //(Currently no plans to support overloading)

    );
  TFuncFlagSet = set of TFuncFlag;

  TCallingConvention = (
    ccUnknown,    //I.e. not yet assigned/parser error
    ccStackFrame, //Parameters allocated to an IX based stack frame
    ccRegister,   //Parameters allocated to registers (if possible), otherwise
                  //spilled to IX based stack frame
    ccIntrinsic,  //No call needed! Generated by code generator
                  //Not available for user defined functions
    ccExtern      //A call to external code (at fixed address). Parameters allocated to fixed addresses
    );

//This probably ought to be a compiler option. TODO
const DefaultCallingConvention = ccStackFrame;

type
  PParameter = ^TParameter;
  TParameter = record
    Specifier: TParamSpecifier; //Ignored for Intrinsics
    Name: String;

    Reg: TParamReg;       //If the parameter is passed via a register, otherwise prNone
                          //Ignored for Intrinsics
    VarType: TVarType;    //Parameter type
//    VarTypes: TTypeEnumSet;   //The type of this this parameter.
                          //NOTE: Only intrinsics can handle multiple types

//    DefaultValue: Word;   //If the parameter is optional
                          //Not used by Intrinsics
  end;

  TParamArray = array[0..MaxParams] of TParameter;

  //For functions or procedures
  PFunction = ^TFunction;
  TFunction = record
    NameSpace: String;      //I.e. current file
    Name: String;
    Flags: TFuncFlagSet;    //Meta data about the function

    Corrupts: TUsedRegSet;  //Registers which are corrupted (not including OutRegs)
//    InRegs: TUsedRegSet;    //Registers used for inputs (parameters)
//    OutRegs: TUsedRegSet;   //Registers used for outputs (parameters and return values)

    Overloaded: Boolean;    //Overloaded if multiple functions with the same name, but different parameters
    CallingConvention: TCallingConvention;
    ParamCount: Integer;
    ResultCount: Integer;   //Result count of zero = Procedure

    CodeAddress: Word;      //If implemented in machine code at fixed address

    Params: TParamArray;    //Parameter data
  end;

type TFuncList = TList<PFunction>;

function CreateFuncList: TFuncList;

procedure ClearFunclist(FuncList: TFuncList);

procedure SetCurrentFuncList(AFuncList: TFuncList);

function IdentToParamReg(Ident: String): TParamReg;

//Find the given function
function FuncFindAllScopes(Name: String): PFunction;

function FuncFindInScope(Name: String): PFunction;

//Create a new function and return it
function FuncCreate(NameSpace, Name: String): PFunction;

//Search the parameters of Func, from parameter 0 to Parameter LastIndex for
//one matching the given ParamName
function FuncFindParam(Func: PFunction; ParamName: String; LastIndex: Integer): PParameter;


function FunctionsToStrings(S: TStrings): String;


implementation
uses SysUtils, Scopes;

const ParamRegStrings: array[low(TParamReg)..high(TParamReg)] of String = (
  '',  //Placeholder for Unknown value
  'A', 'B', 'C', 'D', 'E', 'H', 'L', 'BC', 'DE', 'HL', 'CF', 'ZF');

function IdentToParamReg(Ident: String): TParamReg;
begin
  for Result := low(TParamReg) to high(TParamReg) do
    if CompareText(ParamRegStrings[Result], Ident) = 0 then
      EXIT;

  Result := prNone;
end;

var FuncList: TFuncList;

function CreateFuncList: TFuncList;
begin
  Result := TFuncList.Create;
end;

procedure ClearFunclist(FuncList: TFuncList);
var F: PFunction;
begin
  for F in FuncList do
    Dispose(F);

  FuncList.Clear;
end;

procedure SetCurrentFuncList(AFuncList: TFuncList);
begin
  FuncList := AFuncList;
end;

function FuncFindAllScopes(Name: String): PFunction;
var IdentType: TIdentType;
  Scope: PScope;
  Item: Pointer;
  Index: Integer; //Dummy
begin
  if SearchScopes(Name,IdentType,Scope,Item,Index) then
    if IdentType = itFunction then
      EXIT(PFunction(Item));

  Result := nil;
end;

function FuncFindInScope(Name: String): PFunction;
begin
  for Result in FuncList do
    if CompareText(Result.Name, Name) = 0 then
      EXIT;

  Result := nil;
end;

function FuncCreate(NameSpace, Name: STring): PFunction;
var I: Integer;
begin
  New(Result);
  FuncList.Add(Result);
  Result.NameSpace := NameSpace;
  Result.Name := Name;
  Result.Flags := [];
  Result.Corrupts := [];
//  Result.InRegs := [];
//  Result.OutRegs := [];
  Result.CallingConvention := ccUnknown;
  Result.ParamCount := 0;
  Result.ResultCount := 0;
  Result.CodeAddress := 0;

  for I := 0 to MaxParams do
  begin
    Result.Params[I].Name := '';
    Result.Params[I].Reg := prNone;
    Result.Params[I].VarType := vtUnknown;
//    Result.Params[I].VarTypes := [];
    Result.Params[I].Specifier := psVal;
//    Result.Params[I].DefaultValue := 0;
  end;
end;

function FuncFindParam(Func: PFunction; ParamName: String; LastIndex: Integer): PParameter;
var I: Integer;
begin
  for I := 0 to LastIndex do
    if I > MaxParams then
      EXIT(nil)
    else if CompareText(Func.Params[I].Name, ParamName) = 0 then
      EXIT(@Func.Params[I]);

  Result := nil;
end;

function ParamToString(Param: TParameter): String;
//var VT: TTypeEnum;
begin
  case Param.Specifier of
    psVal, psResult: Result := '';
    psVar: Result := 'var ';
    psConst: Result := 'const ';
    psOut: Result := 'out ';
  else
    raise Exception.Create('Unknown param specifier in param');
  end;

  if Param.Specifier <> psResult then
    Result := Result + Param.Name;

  Result := Result + ': ';

  if Param.Reg <> prNone then
  begin
    Result := Result + ParamRegStrings[Param.Reg];
    Result := Result + ' as';
  end;

  Result := VarTypeToName(Param.VarType);
//  for VT in Param.VarTypes do
//    Result := Result + ' ' + TypeEnumNames[VT];
end;

function FuncToString(Func: PFunction): String;
var Param: Integer;
  Flag: TFuncFlag;
begin
  if Func.ResultCount = 0 then
    Result := 'procedure'
  else
    Result := 'function';
  Result := Result + ' ' + Func.Name;
  if Func.ParamCount > 0 then
  begin
    Result := Result + '(';
    for Param := 0 to Func.ParamCount-1 do
    begin
      if Param <> 0 then
        Result := Result + '; ';
      Result := Result + ParamToString(Func.Params[Param]);
    end;
    Result := Result + ')';
  end;
  if Func.ResultCount <> 0 then
  begin
    for Param := Func.ParamCount to Func.ParamCount+Func.ResultCount-1 do
    begin
      if Param > Func.ParamCount then
        Result := Result + ';';
      Result := Result + ParamToString(Func.Params[Param]);
    end;
  end;

  Result := Result + ';';
  case Func.CallingConvention of
    ccUnknown: Result := Result + 'UNKNOWN!!';
    ccIntrinsic: Result := Result + '(Intrinsic)';
    ccStackFrame: Result := Result + 'StackFrame';
    ccExtern: Result := Result + 'Extern';
  else
    raise Exception.Create('Unknown calling convention');
  end;

  for Flag in Func.Flags do
  case Flag of
    ffForward: Result := Result + ' Forward (Unresolved!)';
    ffOverloaded: Result := Result + ' Overloaded';
  else
    raise Exception.Create('Unknown function flag');
  end;

  Result := Result + ' $' + IntToHex(Func.CodeAddress,4);
end;

function FunctionsToStrings(S: TStrings): String;
var Func: PFunction;
  NameSpace: String;
begin
  NameSpace := '';
  S.Clear;
  for Func in FuncList do
  begin
    if Func.NameSpace <> NameSpace then
    begin
      NameSpace := Func.NameSpace;
      S.Add('NameSpace: ' + NameSpace);
    end;
    S.Add(FuncToString(Func));
  end;
end;

initialization
  FuncList := nil;
finalization
end.
