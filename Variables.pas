unit Variables;
{
var A:=10

procedure P;
var PX: Integer
begin
  PX := A
end

procedure Q;
var QX: Byte
begin
  QX := 100;
end

var X: Word
begin
  P;
  X := 10000;
end.
}
interface
uses Classes, ILData, QTypes, Generics.Collections;

type
  TVarStorage = (
    vsAbsolute,   //Absolute, permanent memory location
    vsRelative);  //Relaive, offset from a base address (i.e in stack frame)

  PVariable = ^TVariable;
  TVariable = record
    Name: String;
    VarType: TVarType;
    InScope: Boolean; //If False the variable hsa gone out of scope and should be ignored
    Depth: Integer;   //The block depth within the current scope.
                      //Every time we encounted a BEGIN the scopes depth is increased.
                      //Every time a variable is declared the scope depth is recorded here.
                      //Every time we encounter an END the scope's depth is decreased...
                      //...and any variable which are too deep are marked as out of scope.
    Storage: TVarStorage; //Fixed or offset location?
    Offset: Integer;  //If the Storage is:
                      //vsOffset, this is the offset from the stack base address
                      //vsFixed: the offset from the start of the Data segment

    //Compile and execution time data
    Sub: Integer;

    //Compile time only data
    Touched: Boolean; //Temporary data used when generating phi functions
    AdjustSubFrom: Integer;  //Temp data used while doing branch fixups.
                            //If a variable read has the given sub (varsion) index...
    AdjustSubTo: Integer;   //...we need to change that read to reference this sub version.

    //Execution time only data
    ValueInt: Integer;
  end;

type TVarList = TList<PVariable>;

procedure InitialiseVars;

//These two functions are used by SkipMode to enable
//unwanted code to be removed

//Sets a marker at the current var list position
procedure VarMark;
//Removes any vars generated after the marker set by VarMark
procedure VarRollback;

//-----------Creating

//Creates a new, uniquely named variable. If a variable with that name already exists
//returns nil
function VarCreate(AName: String;VarType: TVarType;Storage: TVarStorage;
  out Index: Integer): PVariable;
//function FindOrAddVar(AName: String;VarType: TVarType;out Index: Integer): PVariable;
function VarCreateHidden(VarType: TVarType;Storage: TVarStorage;out Index: Integer): PVariable;
function VarCreateTemp(TempIndex: Integer;VarType: TVarType;Storage: TVarStorage): PVariable;

//The Name can *only* be set if it is blank. *Only* to be used where the
//variable has to be created where the name is unknown, and the name is
//assigned immediately after.
procedure VarSetName(Variable: PVariable;AName: String);
//Set the variables type. Only allowed in the variale is the last on the list.
//*Only* to be used where the variable needs to be created where the eventual type
//is unknown, and the type is assigned immediately after.
procedure VarSetType(Variable: PVariable;VType: TVarType);

function VarGetCount: Integer;

//Returns the name of the variable used in Assemby code
function VarGetAsmName(Variable: PVariable): String;

procedure VarClearTempList;
//Gets the next temp value index, and increments it (CurrTempIndex) for the next call
function GetNextTempIndex: Integer;
function GetTempCount: Integer;

//--------------Finding/accessing
//Find a variable by name across all current scopes.
function VarFindByNameAllScopes(AName: String;out Index: Integer): PVariable;

//Find a varible by name only searching the current Scope (ie list).
function VarFindByNameInScope(AName: String;out Index: Integer): PVariable;


function VarIndexToData(Index: Integer): PVariable;
function VarTempToData(TempIndex: Integer): PVariable;

//Incremenents the Sub Count of a variable and returns the value
//Does not increment if SkipMode is enabled
function VarIncWriteCount(Variable: PVariable): Integer;
function VarIndexIncWriteCount(VarIndex: Integer): Integer;

function ILParamToVariable(ILParam: PILParam): PVariable;
function ILDestToVariable(const ILDest: TILDest): PVariable;



//------------CodeGen

procedure VarUpdateLocalOffsets;

function VarsStackFrameSize: Integer;


//------------Scope related

//The maximum number of vars which can be created per Scope/VarList
const ScopeVarCount = 1000;

//These routines are called by the Scopes routines to create, clear and set Scopes
function CreateVarList: TVarList;
procedure ClearVarList(List: TVarList);
procedure SetCurrentVarList(List: TVarList;FirstIndex: Integer);
//Scope depth has been DECrememented. Any in scope variable with higher scope depth
//need to go out of scope
procedure ScopeDepthDecced(NewDepth: Integer);


//-----------UI related

//Prepare all variables for execution
procedure VarsExecClear;

//Returns contents of output buffer
function LoadVarsFromMemoryDump(Filename: String;Base: Integer;
  out RunTimeError: Byte;out RunTimeErrorAddress: Word): String;

function VarIndexToName(Index: Integer): String;
//If TypeSummary is true, only lists name and type,
//otherwise also lists location/offset and value
function VarToString(V: PVariable;TypeSummary: Boolean): String;
procedure VarsToStrings(S: TStrings;TypeSummary: Boolean);


//-----------------------------Phis and fixups
//Clear the Touched flag for every variable
procedure VarClearTouches;
procedure VarClearAdjust;

//Mark a variable as 'touched'
procedure VarTouch(Variable: PVariable);




implementation
uses SysUtils, ILExec, IOUtils, Scopes, ParserBase, Globals;

var Vars: TVarList;
  VarsFirstIndex: Integer;
  VarMarkPosition: Integer;

procedure InitialiseVars;
begin
  Vars := nil;
  VarsFirstIndex := 0;
  VarMarkPosition := -1;
end;

function CreateVarList: TVarList;
begin
  Result := TVarList.Create;
  VarMarkPosition := -1;
end;

procedure ClearVarList(List: TVarList);
var V: PVariable;
begin
  for V in List do
    Dispose(V);
  List.Clear;
  VarMarkPosition := -1;
end;

function VarGetCount: Integer;
begin
  Result := Vars.Count;
end;

procedure ScopeDepthDecced(NewDepth: Integer);
var I: Integer;
begin
  I := Vars.Count-1;
  while (I >= 0) and (Vars[I].Depth > NewDepth) do
  begin
    Vars[I].InScope := False;
    dec(I);
  end;
end;

procedure VarMark;
begin
  Assert(VarMarkPosition = -1);
  VarMarkPosition := Vars.Count;
end;

procedure VarRollback;
begin
  Assert(VarMarkPosition <> -1);
  while Vars.Count > VarMarkPosition do
  begin
    Dispose(Vars[Vars.Count-1]);
    Vars.Delete(Vars.Count-1);
  end;

  VarMarkPosition := -1;
end;

procedure SetCurrentVarList(List: TVarList;FirstIndex: Integer);
begin
  Vars := List;
  VarsFirstIndex := FirstIndex;
end;

function VarsStackFrameSize: Integer;
begin
  if Vars.Count = 0 then
    Result := 0
  else  //Stack frame! We only need current vars
    Result := Vars[Vars.Count-1].Offset + GetTypeSize(Vars[Vars.Count-1].VarType);
end;

//Doesn't check whether a variable with that name already exists!
function VarCreateInt(AName: String;VType: TVarType;Storage: TVarStorage;out Index: Integer): PVariable;
begin
  New(Result);
  Result.Name := AName;
  Result.VarType := VType;
  Result.Depth := GetCurrentScope.Depth;
  Result.InScope := True;
  Result.Storage := Storage;
  Result.Sub := 0;
  Result.Touched := False;
  if Vars.Count = 0 then
    Result.Offset := 0
  else  //Adding to current Scope
    Result.Offset := Vars[Vars.Count-1].Offset + GetTypeSize(Vars[Vars.Count-1].VarType);

  Index := VarsFirstIndex + Vars.Add(Result);
end;

function VarFindByNameAllScopes(AName: String;out Index: Integer): PVariable;
var IdentType: TIdentType;
  Scope: PScope;
  Item: Pointer;
begin
  if SearchScopes(AName,IdentType,Scope,Item,Index) then
    if IdentType = itVar then
      EXIT(PVariable(Item));

  Result := nil;
end;

function VarFindByNameInScope(AName: String;out Index: Integer): PVariable;
var I: Integer;
begin
  for I := 0 to Vars.Count-1 do
    if (CompareText(Vars[I].Name, AName) = 0) and Vars[I].InScope then
    begin
      Result := Vars[I];
      Index := VarsFirstIndex + I;
      EXIT;
    end;

  Index := -1;
  Result := nil;
end;

{function FindOrAddVar(AName: String;VarType: TVarType;Storage: TVarStorage;out Index: Integer): PVariable;
begin
  Result := VarFindByNameAllScopes(AName, Index);

  if Result = nil then
    Result := VarCreateInt(AName, VarType, Storage, Index);
end;
}
function VarCreateHidden(VarType: TVarType;Storage: TVarStorage;out Index: Integer): PVariable;
begin
  Result := VarCreateInt('',VarType, Storage, Index);
end;

function VarCreateTemp(TempIndex: Integer;VarType: TVarType;Storage: TVarStorage): PVariable;
var Index: Integer;
begin
  Result := VarCreateInt('=temp'+IntToStr(TempIndex), VarType, Storage, Index);
end;

function VarCreate(AName: String;VarType: TVarType;Storage: TVarStorage;out Index: Integer): PVariable;
begin
  if VarFindByNameInScope(AName, Index) <> nil then
  begin
    Index := -1;
    Result := nil;
  end
  else
    Result := VarCreateInt(AName, VarType, Storage, Index);
end;

procedure VarSetName(Variable: PVariable;AName: String);
begin
  if Variable.Name <> '' then
    raise Exception.Create('VarSetName must only be called when Name is blank');
  Variable.Name := AName;
end;

procedure VarSetType(Variable: PVariable;VType: TVarType);
begin
  //Last item in current list??
  if Variable <> Vars[Vars.Count-1] then
    raise Exception.Create('VarSetType must only be called when it is the last in the VarList');
  Variable.VarType := VType;
end;

function VarGetAsmName(Variable: PVariable): String;
begin
  Result := 'v_' + GetCurrentScope.Name + '_' + Variable.Name;
end;

var CurrTempIndex: Integer;

procedure VarClearTempList;
begin
  CurrTempIndex := 0;
end;

function GetNextTempIndex: Integer;
begin
  Result := CurrTempIndex;
  inc(CurrTempIndex);
end;

function GetTempCount: Integer;
begin
  Result := CurrTempIndex;
end;


function VarIncWriteCount(Variable: PVariable): Integer;
begin
  Result := Variable.Sub + 1;
  if not SkipMode then
    Variable.Sub := Result;
end;

function VarIndexIncWriteCount(VarIndex: Integer): Integer;
begin
  Result := VarIncWriteCount(VarIndexToData(VarIndex));
end;

function VarIndexToData(Index: Integer): PVariable;
var Scope: PScope;
begin
  Scope := GetCurrentScope;

  //Recurse up parent Scopes until we find it
  repeat
    if Index >= VarsFirstIndex then
    begin
      Result := Vars[Index - VarsFirstIndex];
      SetCurrentScope(Scope);
      EXIT;
    end;
  until not SetParentScope;

  Result := nil;
  SetCurrentScope(Scope);
end;

function VarTempToData(TempIndex: Integer): PVariable;
var Index: Integer;
begin
  Result := VarFindByNameInScope('=temp'+IntToStr(TempIndex), Index);
end;

function VarIndexToName(Index: Integer): String;
begin
  Result := VarIndexToData(Index).Name;
end;

procedure VarTouch(Variable: PVariable);
begin
  Variable.Touched := True;
end;

procedure VarClearTouches;
var V: PVariable;
begin
  for V in Vars do
    V.Touched := False;
end;

procedure VarClearAdjust;
var V: PVariable;
begin
  for V in Vars do
  begin
    V.AdjustSubFrom := -1;
    V.AdjustSubTo := -1;
  end;
end;


function ILParamToVariable(ILParam: PILParam): PVariable;
begin
  case ILParam.Loc of
    locVar: Result := VarIndexToData(ILParam.VarIndex);
    locTemp: Result := VarTempToData(ILParam.TempIndex);
  else
    raise Exception.Create('Illegal Param.Loc');
  end;
end;

function ILDestToVariable(const ILDest: TILDest): PVariable;
begin
  case ILDest.Loc of
    locVar: Result := VarIndexToData(ILDest.VarIndex);
    locTemp: Result := VarTempToData(ILDest.TempIndex);
  else
    raise Exception.Create('Invalid Dest.Loc');
  end;
end;

procedure VarUpdateLocalOffsets;
var Offset: Integer;
  V: PVariable;
begin
  Offset := 0;
  for V in Vars do
  begin
    Offset := Offset - GetTypeSize(V.VarType);
    //If local var and requires storage
    V.Offset := Offset;
//    Offset := Offset + TypeSize[V.VarType];
  end;
end;

function LoadVarsFromMemoryDump(Filename: String;Base: Integer;
  out RunTimeError: Byte;out RunTimeErrorAddress: Word): String;
var Mem: TBytes;
  V: PVariable;
  BufHead: Byte;
  BufPtr: Word;
begin
  Assert(sizeof(TQType) = 1,'LoadVarsFromMemoryDump: sizeof vtType needs updating');
  Mem := TFile.ReadAllBytes(Filename);

  RuntimeError := Mem[$800b];
  RunTimeErrorAddress := Mem[$800c] + (Mem[$800d] shl 8);

  for V in Vars do
    //if <suitable var>
    begin
      case V.VarType of
        vtInteger: V.ValueInt := Int16(Mem[Base+V.Offset] + (Mem[Base+V.Offset+1] shl 8));
        vtInt8: V.ValueInt := Int8(Mem[Base+V.Offset]);
        vtWord, vtPointer: V.ValueInt := Mem[Base+V.Offset] + (Mem[Base+V.Offset+1] shl 8);
        vtByte, vtBoolean, vtChar, vtType:  V.ValueInt := Mem[Base+V.Offset];
      else
        raise Exception.Create('Invalid VarType in LoadVarsFromMemoryDump');
      end;
    end;

  BufHead := Mem[$800e];
  Result := '';
  BufPtr := $800f;
  while BufHead > 0 do
  begin
    Result := Result + chr(Mem[BufPtr]);
    inc(BufPtr);
    dec(BufHead);
  end;
end;








function VarToString(V: PVariable;TypeSummary: Boolean): String;
begin
  if TypeSummary then
    Result := ''
  else if V.Storage = vsAbsolute then
    Result := '@'+IntToHex(V.Offset, 4).Tolower
  //vsOffset
  else if V.Offset < 0 then
    Result := '-' + IntToHex(0-V.Offset, 2) + ' '
  else
    Result := '+' + IntToHex(V.Offset, 2) + ' ';
  Result := Result + V.Name + ': ' + VarTypeToName(V.VarType);
  if not TypeSummary then
  begin
    Result := Result + ' = ';
    case V.VarType of
      vtUnknown: ;
      vtInteger, vtInt8: Result := Result + IntToStr(V.ValueInt);
      vtWord, vtByte, vtPointer: Result := Result + IntToStr(Word(V.ValueInt));
      vtBoolean:
        case V.ValueInt of
          valueFalse: Result := Result + 'False';
          valueTrue and $ff: Result := Result + 'True';
        else
          Result := Result + 'ILLEGAL BOOLEAN: ' + IntToStr(V.ValueInt);
        end;
      vtChar: Result := Result + '''' + chr(V.ValueInt and $ff) + '''';
      vtType: Result := Result + 'type ' + VarTypeToName(V.ValueInt);
//      vtString: ;
//      vtReal: ;
    else
      Result := Result + '*** Unknown variable type ***';
    end;
  end;
end;

procedure VarsToStrings(S: TStrings;TypeSummary: Boolean);
var I: Integer;
begin
  S.Clear;
  if TypeSummary then
    S.Add('Variables summary:')
  else
    S.Add('Variables dump:');
  for I := 0 to Vars.Count-1 do
    S.Add(IntToStr(I) + '- ' + VarToString(Vars[I], TypeSummary));
end;


procedure VarsExecClear;
var V: PVariable;
begin
  for V in Vars do
  begin
    V.ValueInt := 0;
    V.VarType := vtUnknown;
    V.Sub := 0;
  end;
end;

end.
