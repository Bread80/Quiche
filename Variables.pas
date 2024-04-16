unit Variables;

interface
uses Classes, QTypes, Generics.Collections;

type
  TVarStorage = (
    vsStatic,   //Absolute, permanent memory location
    vsStack);   //Relative, offset from a base address (i.e in stack frame)

type  //Controls accessibility of variables and function parameters
  TVarAccess = (
    vaLocal,  //Local variable
//    vaGlobal, //Globally stored variable (but only accessible within Scope)

    //Function parameters:
    vaVal,    //Passed as a value
    vaVar,    //Passed as a reference (pointer)
    vaConst,  //Value cannot be edited (allows larger data structures to be passed by reference)
//    vaIn,     //Input only
    vaOut,    //Output only
    vaResult);  //Is a result

  PVariable = ^TVariable;
  TVariable = record
    Name: String;
    VarType: TVarType;
    InScope: Boolean; //If False the variable has gone out of scope and should be ignored
    Depth: Integer;   //The block depth within the current scope.
                      //Every time we encounted a BEGIN the scopes depth is increased.
                      //Every time a variable is declared the scope depth is recorded here.
                      //Every time we encounter an END the scope's depth is decreased...
                      //...and any variable which are too deep are marked as out of scope.
    Storage: TVarStorage; //Fixed or offset location?
    Access: TVarAccess; //Access type for parameters (paValue for locals)
    Offset: Integer;  //If the Storage is:
                      //vsOffset, this is the offset from the stack base address
                      //vsFixed: the offset from the start of the Data segment

    //Parse and execution time data
    WriteCount: Integer;

    //Compile time only data
    Touched: Boolean; //Temporary data used when generating phi functions
    AdjustVersionFrom: Integer;  //Temp data used while doing branch fixups.
                            //If a variable read has the given version index...
    AdjustVersionTo: Integer;   //...we need to change that read to reference this version.

    //Execution time only data
    ValueInt: Integer;

    //The Name can *only* be set if it is blank. *Only* to be used where the
    //variable has to be created where the name is unknown, and the name is
    //assigned immediately after.
    procedure SetName(AName: String);

    //Set the variables type. Only allowed in the variale is the last on the list.
    //*Only* to be used where the variable needs to be created where the eventual type
    //is unknown, and the type is assigned immediately after.
    procedure SetType(VType: TVarType);

    //Incremenents the Sub Count of a variable and returns the value
    //Does not increment if SkipMode is enabled
    function IncWriteCount: Integer;

    //Returns the name of the variable used in Assemby code
    function GetAsmName: String;

    //If TypeSummary is true, only lists name and type,
    //otherwise also lists location/offset and value
    function ToString(TypeSummary: Boolean): String;
  end;

type TVarList = TList<PVariable>;

procedure InitialiseVars;

//---------------SkipMode
//These two functions are used by SkipMode to enable
//unwanted code to be removed

//Sets a marker at the current var list position
procedure VarMark;
//Removes any vars generated after the marker set by VarMark
procedure VarRollback;

//-----------Creating

//Creates a new, uniquely named variable. If a variable with that name already exists
//returns nil. Uses storage type for current Scope (and/or Func)
function VarCreate(AName: String;VarType: TVarType): PVariable;
//Creates a variable with no name. The name *must* be assigned as soon as they
//are known.
function VarCreateUnknown(VarType: TVarType): PVariable;
function VarCreateHidden(VarType: TVarType): PVariable;
//Creates a parameter for a function. Sets the access specifier and also
//initialises the stack offset (if Storage is vsRelative)
function VarCreateParameter(AName: String;VarType: TVarType;Storage: TVarStorage;
  Access: TVarAccess): PVariable;


//Returns the number of variables in the current list/scope
function VarGetCount: Integer;

//Returns the number of bytes required for local variables in the current scope
//NOTE: Result is considered a local variable, not a parameter
function VarGetLocalsByteSize: Integer;
//Returns number of bytes required for stack parameters by this scope.
//Result is not a parameter.
function VarGetParamsByteSize: Integer;

//--------------Finding/accessing
//Find a variable by name across all current scopes.
function VarFindByNameAllScopes(AName: String): PVariable;

//Finds the Result variable in the currrent Scope.
//If there is no result variable, returns nil
function VarFindResult: PVariable;

//------------CodeGen

//Set the Offset values for local, relative, variables.
procedure VarSetOffsets;

//Find a varible by name only searching the current Scope (ie list).
function VarFindByNameInScope(AName: String): PVariable;

function VarIndexToData(Index: Integer): PVariable;

//------------Scope related

//The maximum number of vars which can be created per Scope/VarList
const ScopeVarCount = 1000;

//These routines are called by the Scopes routines to create, clear and set Scopes
function CreateVarList: TVarList;
procedure ClearVarList(List: TVarList);
procedure SetCurrentVarList(List: TVarList);
//Scope depth has been DECrememented. Any in scope variable with higher scope depth
//need to go out of scope
procedure ScopeDepthDecced(NewDepth: Integer);


//-----------UI related

//Prepare all variables for execution
procedure VarsExecClear;

//Returns contents of output buffer
function LoadVarsFromMemoryDump(Filename: String;Base: Integer;
  out RunTimeError: Byte;out RunTimeErrorAddress: Word): String;

procedure VarsToStrings(S: TStrings;TypeSummary: Boolean);


//-----------------------------Phis and fixups
//Clear the Touched flag for every variable
procedure VarClearTouches;
procedure VarClearAdjust;

//Mark a variable as 'touched'
procedure VarTouch(Variable: PVariable);




implementation
uses SysUtils, ILExec, IOUtils, Scopes, ParserBase, Globals;

const
  //Offset from IX to the first parameter
  //IX+4... is parameters
  //IX+2 is return address
  //IX+0 is previous IX
  //< IX is local variables
  iStackOffsetForFirstParam = +4;

var Vars: TVarList;
  VarMarkPosition: Integer;

procedure InitialiseVars;
begin
  Vars := nil;
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

procedure SetCurrentVarList(List: TVarList);
begin
  Vars := List;
end;

function VarFindByNameAllScopes(AName: String): PVariable;
var IdentType: TIdentType;
  Scope: PScope;
  Item: Pointer;
begin
  Item := SearchScopes(AName,IdentType,Scope);
  if Assigned(Item) then
    if IdentType = itVar then
      EXIT(PVariable(Item));

  Result := nil;
end;

function VarFindByNameInScope(AName: String): PVariable;
var I: Integer;
begin
  //Result pseudo variable
  if CompareText(AName, 'Result') = 0 then
  begin
    for I := 0 to Vars.Count-1 do
      if Vars[I].Access = vaResult then
        EXIT(Vars[I]);
    EXIT(nil);
  end;

  for I := 0 to Vars.Count-1 do
    if (CompareText(Vars[I].Name, AName) = 0) and Vars[I].InScope then
      EXIT(Vars[I]);

  Result := nil;
end;

function VarFindResult: PVariable;
begin
  for Result in Vars do
    if Result.Access = vaResult then
      EXIT;

  Result := nil;
end;

//Doesn't check whether a variable with that name already exists!
function VarCreateInt(AName: String;VType: TVarType;Storage: TVarStorage): PVariable;
begin
  New(Result);
  Result.Name := AName;
  Result.VarType := VType;
  Result.Depth := GetCurrentScope.Depth;
  Result.InScope := True;
  Result.Storage := Storage;
  Result.WriteCount := 0;
  Result.Touched := False;
  Result.Offset := -1;
  Result.Access := vaLocal;
  Vars.Add(Result);
end;

function VarCreateHidden(VarType: TVarType): PVariable;
begin
  Result := VarCreateInt('',VarType, GetCurrentScope.GetLocalStorage);
  Result.Name := '_temp' + Vars.IndexOf(Result).ToString;
//  Result.SetName('%'+IntToStr(Index));
end;

function VarCreate(AName: String;VarType: TVarType): PVariable;
begin
  Assert(AName <> '');
  if VarFindByNameInScope(AName) <> nil then
    Result := nil
  else
    Result := VarCreateInt(AName, VarType, GetCurrentScope.GetLocalStorage);
end;

function VarCreateUnknown(VarType: TVarType): PVariable;
begin
  Result := VarCreateInt('', VarType, GetCurrentScope.GetLocalStorage);
end;

//Adds a variable for a function parameter. To be called at the beginning of a function
//declaration.
//If Storage is vsStack space will be allocated on the stack and the Offset property
//of the variable initialised (unless Access is vsResult)
//If Access is vsResult: Results are parsed as local variable to the function. The
//Offset property for them will be initiated to -1.
function VarCreateParameter(AName: String;VarType: TVarType;Storage: TVarStorage;
  Access: TVarAccess): PVariable;
var Offset: Integer;  //Offset to the /previous/ stack variable (parameter)
  PrevIndex: Integer; //Indesx of last Parameter added to the variable list
begin
  //If variable is being stored on the stack we need to ascertain the offset of
  //the variable from SP. Otherwise we set offset to -1 to signify no offset has
  //yet been calculated.
  if (Storage = vsStatic) or (Access = vaResult) then
    Offset := -1
  else  //vsRelative
  begin
    //Get index of the last parameter which has already been added as a variable.
    //We need to ignore any Result parameters, so step backwards through Vars
    PrevIndex := Vars.Count-1;
    while (PrevIndex >= 0) and (Vars[PrevIndex].Access = vaResult) do
      dec(PrevIndex);

    if PrevIndex < 0 then
      //This is the first parameter to be added
      Offset := iStackOffsetForFirstParam
    else
    begin
      Offset := Vars[PrevIndex].Offset;
      //Offsets for 'regular' variables are assigned later. Offsets for parameters
      //are assigned here. If previous variable does not have an offset assigned we have a bug.
      Assert(Offset <> -1,'Adding a parameter variable, but previous variable is NOT a parameter variable');
      Offset := Offset + GetTypeSize(Vars[PrevIndex].VarType);
    end;
  end;

  Result := VarCreateInt(AName, VarType, Storage);
  Result.Offset := Offset;
  Result.Access := Access;
end;

procedure TVariable.SetName(AName: String);
begin
  Assert(Name = '', 'Variable.SetName must only be called when Name is blank');
  Name := AName;
end;

procedure TVariable.SetType(VType: TVarType);
begin
  //Last item in current list??
  Assert(@Self = Vars[Vars.Count-1], 'VarSetType must only be called when it is the last in the VarList');
  VarType := VType;
end;

function VarInList(AVar: PVariable): Boolean;
var V: PVariable;
begin
  for V in Vars do
    if V = AVar then
      EXIT(True);
  Result := False;
end;

function VarToScope(V: PVariable): PScope;
var OldScope: PScope;
  Done: Boolean;
begin
  OldScope := GetCurrentScope;

  Result := nil;
  Done := False;
  repeat
    if VarInList(V) then
    begin
      Result := GetCurrentScope;
      Done := True;
    end
    else
      Done := not SetParentScope;
  until Done;

  SetCurrentScope(OldScope);
end;

function TVariable.GetAsmName: String;
var LName: String;
  Scope: PScope;
begin
  //Find the Scope which 'owns' the variable
  Scope := VarToScope(@Self);
  Assert(Scope <> nil, 'Scope not found (for variable)');

  if Name = '' then
    LName := '_temp' + Vars.IndexOf(@Self).ToString
  else
    LName := Name;
  Result := 'v_' + Scope.Name + '_' + LName;
end;

function TVariable.IncWriteCount: Integer;
begin
  Result := WriteCount + 1;
  if not SkipMode then
    WriteCount := Result;
end;

function VarIndexToData(Index: Integer): PVariable;
//var Scope: PScope;
begin
  Result := Vars[Index];
{  Scope := GetCurrentScope;

  //Recurse up parent Scopes until we find it
  repeat
    if Index >= GetVarCount then
    begin
      Result := Vars[Index - VarsFirstIndex];
      SetCurrentScope(Scope);
      EXIT;
    end;
  until not SetParentScope;

  Result := nil;
  SetCurrentScope(Scope);
}end;

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
    V.AdjustVersionFrom := -1;
    V.AdjustVersionTo := -1;
  end;
end;

procedure VarSetOffsets;
var Offset: Integer;
  V: PVariable;
begin
  Offset := 0;
  for V in Vars do
  begin
    //Ignore parameters (which already have offsets assigned)
    //Ignore variable with global/absoluate storage addresses
    if (V.Offset = -1) and (V.Storage = vsStack) then
    begin //TODO: Ignore if optimised away
      Offset := Offset - GetTypeSize(V.VarType);
      //If local var and requires storage
      V.Offset := Offset;
    end;
  end;
end;

function VarGetLocalsByteSize: Integer;
var V: PVariable;
begin
  Result := 0;
  for V in Vars do
    if V.Access in [vaLocal, vaResult] then
      Result := Result + GetTypeSize(V.VarType);
end;

function VarGetParamsByteSize: Integer;
var V: PVariable;
begin
  Result := 0;
  for V in Vars do
    if not (V.Access in [vaLocal, vaResult]) then
      Result := Result + GetTypeSize(V.VarType);
end;

function LoadVarsFromMemoryDump(Filename: String;Base: Integer;
  out RunTimeError: Byte;out RunTimeErrorAddress: Word): String;
var Mem: TBytes;
  V: PVariable;
  BufHead: Byte;
  BufPtr: Word;
begin
  Assert(sizeof(TVarType) = 1,'LoadVarsFromMemoryDump: sizeof vtType needs updating');
  Mem := TFile.ReadAllBytes(Filename);

  RuntimeError := Mem[$800b];
  RunTimeErrorAddress := Mem[$800c] + (Mem[$800d] shl 8);

  for V in Vars do
    //if <suitable var>
    begin
      case V.Storage of
        vsStack:
          case V.VarType of
            vtInteger: V.ValueInt := Int16(Mem[Base+V.Offset] + (Mem[Base+V.Offset+1] shl 8));
            vtInt8: V.ValueInt := Int8(Mem[Base+V.Offset]);
            vtWord, vtPointer: V.ValueInt := Mem[Base+V.Offset] + (Mem[Base+V.Offset+1] shl 8);
            vtByte, vtBoolean, vtChar, vtTypeDef:  V.ValueInt := Mem[Base+V.Offset];
          else
            raise Exception.Create('Invalid VarType in LoadVarsFromMemoryDump');
          end;
        vsStatic:
          //TODO
          V.ValueInt := -1;
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








function TVariable.ToString(TypeSummary: Boolean): String;
begin
  if TypeSummary then
    Result := ''
  else if Storage = vsStatic then
    Result := '@'+IntToHex(Offset, 4).Tolower
  //vsOffset
  else if Offset < 0 then
    Result := '-' + IntToHex(0-Offset, 2) + ' '
  else
    Result := '+' + IntToHex(Offset, 2) + ' ';
  Result := Result + GetAsmName + ': ' + VarTypeToName(VarType);
  if not TypeSummary then
  begin
    Result := Result + ' = ';
    case VarType of
      vtUnknown: ;
      vtInteger, vtInt8: Result := Result + IntToStr(ValueInt);
      vtWord, vtByte, vtPointer: Result := Result + IntToStr(Word(ValueInt));
      vtBoolean:
        case ValueInt of
          valueFalse: Result := Result + 'False';
          valueTrue and $ff: Result := Result + 'True';
        else
          Result := Result + 'ILLEGAL BOOLEAN: ' + IntToStr(ValueInt);
        end;
      vtChar: Result := Result + '''' + chr(ValueInt and $ff) + '''';
      vtTypeDef: Result := Result + 'type ' + VarTypeToName(TVarType(ValueInt));
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
    S.Add(IntToStr(I) + '- ' + Vars[I].ToString(TypeSummary));
end;


procedure VarsExecClear;
var V: PVariable;
begin
  for V in Vars do
  begin
    V.ValueInt := 0;
    V.VarType := vtUnknown;
    V.WriteCount := 0;
  end;
end;

end.
