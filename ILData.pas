unit ILData;

interface
uses Generics.Collections, Classes, QTypes, Variables, Functions, Operators,
  Z80.CPU;

//Locations where an operation will find and store it's data
type TILParamKind = (
    pkNone,         //No parameter
    pkImmediate,    //Immediate data (constant)
    pkPhiVarSource, //Parameter for a phi function for a variable
    pkPhiVarDest,   // "
    pkVarSource,    //Read from variable
    pkVarDest,      //Write to variable
    pkPop,          //The stack
    pkPopByte,      //Single byte on the stack
    pkPush,
    pkPushByte,
    pkBranch,        //Unconditional branch. Only valid as Param1
    pkCondBranch     //Conditional branch. Only valid as Param3
    );

type
  PILParam = ^TILParam;
  TILParam = record
    //If the parameter is Sourcing data (ie. before an operation) this is the
    //register the data will be loaded or move into
    //If the parameter is a Deestination/Result this is the register in which the
    //data will found after the operation. The code generator will store the value
    //back into the variable (or other location) specified by the other values
    Reg: TCPUReg;

    procedure Initialise;

    //These routines set the Kind and (usually) the payload. They also perform
    //validation where possible
    procedure SetImmediate(AImmValue: Integer;AImmType: TVarType);
    procedure SetPhiVarSource(ABlockID: Integer; AVarVersion: Integer);
    procedure SetPhiVarDest(AVar: PVariable;AVersion: Integer);
    procedure SetVarSourceAndVersion(AVariable: PVariable; AVersion: Integer);
    //Obtains Version from AVariable
    procedure SetVarSource(AVariable: PVariable);
    procedure SetVarDestAndVersion(AVariable: PVariable; AVersion: Integer);
    procedure SetCondBranch;

    //If a Param has an immediate value, convert that value to a 'modern'
    //(cross-compiler native) Integer value
    function ImmToInteger: Integer;
    function ToVariable: PVariable;

    function GetVarType: TVarType;
//    function GetRawType: TOpType;

    //For error messages
    function ImmValueToString: String;

    function ToString: String;

    //The payload data
    case Kind: TILParamKind of
      pkNone: ();
      pkImmediate: (
        ImmValueInt: Integer;       //Immediate (constant) data
        ImmType: TVarType; ); //Type of the above value
      pkPhiVarSource: (            //Phi variable (specified in the Dest data)
        PhiBlockID: Integer;  //If we come from this block...
        PhiSourceVersion: Integer; );   //...use this version of the variable
      pkPhiVarDest: (
        PhiVar: PVariable;
        PhiDestVersion: Integer; );
      pkVarSource, pkVarDest: (
        Variable: PVariable;  //The variable
        VarVersion: Integer; );   //Current version of the variable
      pkPop: ();           //Currently invalid for input params
      pkPopByte: ();       //Currently invalid for input params
      pkPush: ();
      pkPushByte: ();
      pkBranch: (           //For unconditional branches.
        BranchBlockID: Integer; );  //Block number to branch to
      pkCondBranch: (        //For conditional branches
        BranchInvert: Boolean;    //Value has been NOTted
        TrueBlockID: Integer;     //Block number to branch to if condition is true
        FalseBlockID: Integer;    //Block number to branch to if condition is false
      );
    end; {TILParam}

type TCodeGenFlags = (cgOverFlowCheck);
  TCodeGenFlagSet = set of TCodeGenFlags;

//Record for an atomic IL item
//The operation combines paramer1 and parameter2 and writes the result (if it has one) to
//the temp location specified in Dest
type
  PILItem = ^TILItem;
  TILItem = record
    BlockID: Integer;       //Contains +ve value on first line of a block, otherwise -1
    SourceLineNo: Integer;  //For commented code generation
    Comments: String;       //For debugging <g>. Current only used if BlockID <> -1

    Op: TOperator;          //The operation
    ResultType: TVarType;   //CURRENT: The type which will be output by the Operation
                            //OLD: If overflow checking is on, specifies the output type
                            //May also be used by typecasts to change/reduce value size
    Func: PFunction;        //If OpIndex is OpFuncCall this contains details of the function
                            //to call
    CodeGenFlags: TCodeGenFlagSet;

    //We can have up to three parameters. Each parameter can be Source or Destination
    //far depending on it's Kind. Dest is a pseudonym for Param3 for times when
    //that is useful. Actual Param usage depends on the operation
    Param1: TILParam;       //Data for the first parameter
    Param2: TILParam;       //Data for the second parameter

    //Where Op = dtBranch
    function GetBranchBlockID: Integer;
    procedure SetBranchBlockID(BlockID: Integer);

    //Creates a hidden variable of the given type, sets it as the Dest,
    //and sets DestType to dtData using SetDestType
    function AssignToHiddenVar(VarType: TVarType): PVariable;

    procedure SwapParams;   //For Operations. Swap order of Param1 and Param2
    function ToString: String;  //For debugging

    case Integer of       //Third param (usually result or destination)
    1: (Param3: TILParam);
    2: (Dest: TILParam);
  end;

procedure InitialiseILData;

type TILList = TList<PILItem>;

function CreateILList: TILList;
procedure ClearILList(List: TILList);
procedure SetCurrentILList(List: TILList);

//These two functions are used by SkipMode to enable
//unwanted code to be removed

//Sets a marker at the current IL position
procedure ILMark;
//Removes any IL code after the marker set by ILMark
procedure ILRollback;

//If set True the next ILItem to be created will be a new block
var NewBlock: Boolean;
  //Sets the comments field of the next block created (see NewBlock)
  //Used by control structure parsing code to add debugging comments
  NewBlockComment: String;
function GetCurrBlockID: Integer;
function GetNextBlockID: Integer;

//Allocates a new IL item and appends it to the list
function ILAppend(AnOp: TOperator): PILItem;
//Allocates a new IL item and inserts it into the IL list at the given Index
function ILInsert(Index: Integer;AnOp: TOperator): PILItem;

//Append an ILItem for an unconditional branch
function ILAppendBranch(BranchID: Integer): PILItem;

//Returns the current number of items in the IL list
function ILGetCount: Integer;

//Returns the Index'th item in the IL list
function ILIndexToData(Index: Integer): PILItem;



//Utilities for the UI displays
procedure ILToStrings(S: TStrings);

var NewSourceLine: Boolean;

implementation
uses SysUtils, SourceReader, ParserBase, Globals;

const CPURegStrings: array[low(TCPUReg)..high(TCPUReg)] of String = (
    'None','Immediate','Indirect','Offset',
    'Param1',
    'A','B','C','D','E','H','L',
    'AF',
    'HL','DE','BC','IX','IY',
    'ZF','ZFandA','NZF','NZFandA','CF','NCF','CPLofA','Flags');

var
  ILList: TILList;
  CurrBlockID: Integer;
  CurrSourceLineNo: Integer;
  ILMarkPosition: Integer;

function GetCurrBlockID: Integer;
begin
  Result := CurrBlockID;
end;

function GetNextBlockID: Integer;
begin
  inc(CurrBlockID);
  Result := CurrBlockID;
end;

procedure InitialiseILData;
begin
  ILList := nil;
  CurrBlockID := 0;
  ILMarkPosition := -1;
end;

procedure ILMark;
begin
  Assert(ILMarkPosition = -1);
  ILMarkPosition := ILList.Count;
end;

procedure ILRollback;
begin
  Assert(ILMarkPosition <> -1);
  while ILList.Count > ILMarkPosition do
  begin
    Dispose(ILList[ILList.Count-1]);
    ILList.Delete(ILList.Count-1);
  end;
  ILMarkPosition := -1;
end;

function CreateILList: TILList;
begin
  Result := TILList.Create;
  NewBlock := True;
  NewBlockComment := '';
  NewSourceLine := True;

  //We can't SkipMode over a change of scope
  ILMarkPosition := -1;
end;

procedure ClearILList(List: TILList);
var Item: PILItem;
begin
  for Item in List do
    Dispose(Item);
  List.Clear;

  //We can't SkipMode over a change of scope
  ILMarkPosition := -1;
end;

procedure SetCurrentILList(List: TILList);
begin
  ILList := List;
end;

function ILCreate: PILItem;
begin
  New(Result);
  if NewBlock then
  begin
    Result.BlockID := GetNextBlockID;
    NewBlock := False;
  end
  else
    Result.BlockID := -1;
  Result.Comments := NewBlockComment;
  NewBlockComment := '';

  if Parser.LineNo <> CurrSourceLineNo then
  begin
    Result.SourceLineNo := Parser.LineNo;
    CurrSourceLineNo := Parser.LineNo;
  end
  else
    Result.SourceLineNo := -1;
  Result.Op := opUnknown;
  if optOverflowChecks then
    Result.CodeGenFlags := [cgOverflowCheck]
  else
    Result.CodeGenFlags := [];

  Result.ResultType := vtUnknown;
  Result.Func := nil;

  Result.Param1.Initialise;
  Result.Param2.Initialise;
  Result.Param3.Initialise;
end;

function ILAppend(AnOp: TOperator): PILItem;
begin
  Result := ILCreate;
  Result.Op := AnOp;
  ILList.Add(Result);
end;

function ILInsert(Index: Integer;AnOp: TOperator): PILItem;
begin
  Result := ILCreate;
  Result.Op := AnOp;
  ILList.Insert(Index, Result);
end;

function ILAppendBranch(BranchID: Integer): PILItem;
begin
  Result := ILAppend(opBranch);
  Result.SetBranchBlockID(BranchID);
end;

function ILGetCount: Integer;
begin
  Result := ILList.Count;
end;

function ILIndexToData(Index: Integer): PILItem;
begin
  Result := ILList[Index];
end;

{ TILParam }

procedure TILParam.SetCondBranch;
begin
  Assert(Kind in [pkNone, pkCondBranch]);
  Kind := pkCondBranch;
  TrueBlockID := -1;
  FalseBlockID := -1;
  BranchInvert := False;
end;

procedure TILParam.SetImmediate(AImmValue: Integer;AImmType: TVarType);
begin
  //Assert(Kind in [pkNone, pkImmediate]);
  Kind := pkImmediate;
  ImmValueInt := AImmValue;
  ImmType := AImmType;
end;

procedure TILParam.SetPhiVarDest(AVar: PVariable; AVersion: Integer);
begin
  Assert(Kind = pkNone);
  Kind := pkPhiVarDest;
  PhiVar := AVar;
  PhiDestVersion := AVersion;
end;

procedure TILParam.SetPhiVarSource(ABlockID, AVarVersion: Integer);
begin
  Assert(Kind = pkNone);
  Kind := pkPhiVarSource;
  PhiBlockID := ABlockID;
  PhiSourceVersion := AVarVersion;
end;

procedure TILParam.SetVarSourceAndVersion(AVariable: PVariable;AVersion: Integer);
begin
//  Assert(Kind = pkNone);
  Kind := pkVarSource;
  Variable := AVariable;
  VarVersion := AVersion;
end;

procedure TILParam.SetVarDestAndVersion(AVariable: PVariable;
  AVersion: Integer);
begin
  Assert(Kind in [pkNone, pkVarDest]);
  Kind := pkVarDest;
  Variable := AVariable;
  VarVersion := AVersion;
end;

procedure TILParam.SetVarSource(AVariable: PVariable);
begin
   SetVarSourceAndVersion(AVariable, AVariable.WriteCount);
end;

function TILParam.ImmToInteger: Integer;
begin
  Assert(Kind = pkImmediate);

  Result := ImmValueInt;
//  if IsSignedType(ImmType) and (Result >= $8000) then
//    Result := Result or (-1 xor $ffff);
end;


function TILParam.GetVarType: TVarType;
begin
  case Kind of
    pkNone, pkPhiVarSource, pkPhiVarDest: EXIT(vtUnknown);
    pkImmediate:
      Result := ImmType;
    pkVarSource, pkVarDest:
      Result := Variable.VarType;
  else
    raise Exception.Create('Unknown parameter kind');
  end;
end;

(*function TILParam.GetRawType: TOpType;
begin
  Result := VarTypeToOpType(GetVarType);
end;
*)
function TILParam.ImmValueToString: String;
begin
  Assert(Kind = pkImmediate);
  case ImmType of
    vtByte: Result := '$' + IntToHex(ImmValueInt, 2);
    vtWord, vtPointer: Result := '$' + IntToHex(ImmValueInt, 4);
    vtInt8, vtInteger: Result := ImmValueInt.ToString;
    vtBoolean:
      if ImmValueInt = 0 then
        Result := 'False'
      else
        Result := 'True';
    vtChar:
      if ImmValueInt in [32..127] then
        Result := ''''+chr(ImmValueInt)+''''
      else
        Result := '#' + ImmValueInt.ToString;
    vtTypeDef: Result := VarTypeToName(TVarType(ImmValueInt));
  else
    Assert(False);
  end;
end;

procedure TILParam.Initialise;
begin
  Reg := rNone;
  Kind := pkNone;
end;

function TILParam.ToString: String;

  function VarToString: String;
  begin
    Result := '%' + Variable.Name + '_' + IntToStr(VarVersion) +
      ':' + VarTypeToName(Variable.VarType) + '/' + CPURegStrings[Reg];
  end;

begin
  case Kind of
    pkNone: Result := '_';
    pkImmediate:
    begin
      Result := ImmValueToString + ':' +
        VarTypeToName(ImmType) + '/' + CPURegStrings[Reg];
    end;
    pkPhiVarSource:
      Result := '[%_' + IntToStr(PhiSourceVersion) + ' {' + IntToStr(PhiBlockID) + '}] ';
    pkPhiVarDest:
    begin
      Assert(Assigned(PhiVar));
      Result := '%' + PhiVar.Name + '_' + IntToStr(PhiDestVersion);
    end;
    pkVarSource, pkVarDest:
    begin
      Assert(Assigned(Variable));
      Result := VarToString;
    end;
    pkPush: Result := Result + 'PUSH /' + CPURegStrings[Reg];
    pkPushByte: Result := Result + 'PUSHBYTE /' + CPURegStrings[Reg];
    pkBranch:
    begin
//      Assert(Param = @ILItem.Param1, 'pkBranch must be Param1');
      Result := 'Branch ' + '{' + IntToStr(BranchBlockID) + '} ';
    end;
    pkCondBranch:
    begin
//      Assert(Param = @ILItem.Param3, 'pkCondBranch must be Param3');
      Result := 'CondBranch ';
      if FalseBlockID = -1 then
        Result := Result + '{' + IntToStr(TrueBlockID) + '} '
      else
        Result := Result + '{' + IntToStr(TrueBlockID) + ',' + IntToStr(FalseBlockID) + '} ';
      Result := Result + '/' + CPURegStrings[Reg];
    end;
  else
    Assert(False);
  end;
end;

function TILParam.ToVariable: PVariable;
begin
  Assert(Kind in [pkVarSource, pkVarDest]);
  Result := Variable;
end;


{ TILItem }

function TILItem.AssignToHiddenVar(VarType: TVarType): PVariable;
begin
  Result := VarCreateHidden(VarType);
  Dest.SetVarDestAndVersion(Result, Result.WriteCount);
end;

function TILItem.GetBranchBlockID: Integer;
begin
  Assert(Op = opBranch);
  Assert(Dest.Kind = pkBranch);
  Result := Dest.BranchBlockID;
end;

procedure TILItem.SetBranchBlockID(BlockID: Integer);
begin
  Assert(Op = opBranch);
  Dest.Kind := pkBranch;
  Dest.BranchBlockID := BlockID;
end;

procedure TILItem.SwapParams;
var Temp: TILParam;
begin
  Temp := Param1;
  Param1 := Param2;
  Param2 := Temp;
end;

function TILItem.ToString: String;
begin
  Result := '';

  case Op of
    //Special case operation types
    opDataLoad:
    begin
      Assert(Param1.Kind in [pkImmediate, pkVarSource]);
      Assert(Param2.Kind in [pkNone, pkImmediate, pkVarSource]);
      Assert(Param3.Kind in [pkNone, pkImmediate, pkVarSource]);
      Result := Result + 'DATALOAD: ';
      if Param1.Kind <> pkNone then
        Result := Result + CPURegStrings[Param1.Reg] + ':=' + Param1.ToString;
      if Param2.Kind <> pkNone then
        Result := Result + CPURegStrings[Param2.Reg] + ':=' + Param2.ToString;
      if Param3.Kind <> pkNone then
        Result := Result + CPURegStrings[Param3.Reg] + ':=' + Param3.ToString;
    end;
  else //General operation types
    Assert(Param1.Kind in [pkNone, pkImmediate, pkVarSource, pkPhiVarSource]);
    Assert(Param2.Kind in [pkNone, pkImmediate, pkVarSource, pkPhiVarSource]);
    Assert(Dest.Kind in [pkNone, pkCondBranch, pkBranch, pkVarDest, pkPhiVarDest, pkPush, pkPushByte]);
    Result := Result + Param3.ToString;

    if Dest.Kind in [pkVarDest, pkPhiVarDest, pkCondbranch, pkPush, pkPushByte] then
      Result := Result + ' = ';
    if Op <> opUnknown then
    begin
      Result := Result + Operations[Op].Name;
      if Dest.Kind <> pkPhiVarDest then
        Result := Result + ':' + VarTypeToName(ResultType) + ' ';
    end;
    Result := Result + Param1.ToString + ', ';
    Result := Result + Param2.ToString;

    if Func <> nil then
      Result := Result + #13'    ' + Func.ToString;
  end;
end;

procedure ILToStrings(S: TStrings);
var Item: PILItem;
  I: Integer;
begin
  S.Clear;
  for I := 0 to ILGetCount-1 do
  begin
    Item := ILIndexToData(I);
    if Item.BlockID <> -1 then
      S.Add('   ' + IntToStr(Item.BlockID)+':  ' + Item.Comments);
    S.Add(IntToStr(I)+'- ' + Item.ToString);
  end;
end;

initialization
  ILList := nil;
finalization
end.
