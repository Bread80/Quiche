unit Def.Variables;

interface
uses Classes, Generics.Collections, SysUtils,
  Def.VarTypes, Def.UserTypes, Def.Consts;

type
  TAddrMode = ( //Addressing mode for variable storage
    amStatic,   //Absolute, permanent memory location
                //VarAddr := <label>
                //VarValue := (<label>)
    amStack,    //Relative, offset from a base address (i.e in stack frame)
                //VarAddr := IX+<offset>
                //VarValue := (IX+<offset>)
    amStaticRef,  //The address of the variable data is stored at an absolute (static)
                //location. The variable *must* be of a Pointered type
                //VarAddr := (<label>)
                //VarValue := <addr> := (<label>). <value> := (<addr>)
    amStackRef  //The address of the variable data is stored on the stack at a location
                //relative to the stack pointer (IX)
                //VarAddr := (IX+<offset>)
                //VarValue := <addr> := (IX+<offset>). <value> := (<addr>)
    );
    EAddrMode = class(Exception)
    constructor Create;reintroduce;
  end;

type  //Controls accessibility of variables and function parameters
  TVarAccess = (
    vaNone,   //Parameter not assigned or other error
    vaLocal);  //Local variable
//    vaGlobal, //Globally stored variable (but only accessible within Scope)

type
  PVariable = ^TVariable;
  TVariable = record
    Name: String;
    UserType: PUserType;  //Replaces VarType
    InScope: Boolean; //If False the variable has gone out of scope and should be ignored
    Depth: Integer;   //The block depth within the current scope.
                      //Every time we encounted a BEGIN the scopes depth is increased.
                      //Every time a variable is declared the scope depth is recorded here.
                      //Every time we encounter an END the scope's depth is decreased...
                      //...and any variable which are too deep are marked as out of scope.
    AddrMode: TAddrMode; //Addressing mode - where is the data stored and how to access it
    Access: TVarAccess; //Access type for parameters (paValue for locals)
    IsParam: Boolean;   //True if the variable is a function parameter (and NOT Result)
    IsResult: Boolean;  //True is the variable is the Result of a function
    IsConst: Boolean;   //True if the variable is a CONST argument. If so it must not
                        //be written to and must not be passed as an argument to any
                        //function which may modify it's value (ie. VAR or OUT access)
    Offset: Integer;  //If the Storage is:
                      //vsOffset, this is the offset from the stack base address
                      //vsFixed: the offset from the start of the Data segment
    FuncParamIndex: Integer;  //>= 0 if this is a function parameter, otherwise -1
    //Parse and execution time data
    Version: Integer; //Version is incremented every time the variable is written to
                      //This (along with phi vars) enable the optimiser and code generator
                      //to track what is stored in the variable at any point and
                      //optimise appropriately

    //Compile time only data
    Touched: Boolean; //Temporary data used when generating phi functions
    AdjustVersionFrom: Integer;  //Temp data used while doing branch fixups.
                            //If a variable read has the given version index...
    AdjustVersionTo: Integer;   //...we need to change that read to reference this version.

    //Execution time only data
    Value: TImmValue; //Value (read from emulator)

    //The Name can *only* be set if it is blank. *Only* to be used where the
    //variable has to be created where the name is unknown, and the name is
    //assigned immediately after.
    procedure SetName(AName: String);

    //You probably want to read UserType instead
    function VarType: TVarType;

    //Set the variables type. Only allowed in the variale is the last on the list.
    //*Only* to be used where the variable needs to be created where the eventual type
    //is unknown, and the type is assigned immediately after.
    procedure SetType(AUserType: PUserType);

    //Incremenents the Version of a variable and returns the new value
    //Does not increment if SkipMode is enabled
    function IncVersion: Integer;



    //Mark a variable as 'touched'
    procedure Touch;

    //Returns true if the variable requires storage, false if not.
    //Varaibles don't require storage include:
    // - those which have been optimised away
    // - ShadowOf variables
    function RequiresStorage: Boolean;

    //Returns the name of the variable used in Assemby code
    function GetAsmName: String;

    //If TypeSummary is true, only lists name and type,
    //otherwise also lists location/offset and value
    function ToString(TypeSummary: Boolean): String;
  end;

type
  PVarList = ^TVarList;
  TVarList = record
    Items: TList<PVariable>;

    //---------------SkipMode
    //These two functions are used by SkipMode to enable
    //unwanted code to be removed
    //TODO: SkipMode is to be replace by a parser which can skip code. (This will be
    //required for conditional compilation ($idef etc)
    MarkPosition: Integer;  //For clunky undos of code (to be replaced TODO)

    //Sets a marker at the current var list position
    procedure Mark;
    //Removes any vars generated after the marker set by VarMark
    procedure Rollback;

    procedure Initialise;
    //Disposes of an owned data.
    procedure Clear;
    //Returns the number of variables in the current list/scope
    function GetCount: Integer;

    //-----------Creating
    //PRIVATE!!
    function AddInt(AName: String;UserType: PUserType;AddrMode: TAddrMode): PVariable;

    //Creates a new, uniquely named variable. If a variable with that name already exists
    //returns nil. Uses storage type for current Scope (and/or Func)
    function Add(AName: String;UserType: PUserType): PVariable;
    //Creates a variable with no name. The name *must* be assigned as soon as they
    //are known.
    function AddUnnamed(UserType: PUserType): PVariable;
    function AddHidden(UserType: PUserType): PVariable;
    //Adds a variable for a function parameter. To be called at the beginning of a function
    //declaration.
    //If Access is vsResult: Results are parsed as local variable to the function. The
    //Offset property for them will be initiated to -1, unless PassDataIn is True in
    //which case the address of tha data will be passed in the parameters and no data
    //will be returned in registers
    function AddParameter(const AName: String;UserType: PUserType;AddrMode: TAddrMode;
      AIsResult, PassDataIn: Boolean): PVariable;

    //Returns the number of bytes required for local variables in the current scope
    //NOTE: Result is considered a local variable, not a parameter
    function GetLocalsByteSize: Integer;
    //Returns number of bytes required for stack parameters by this scope.
    //Result is not a parameter.
    function GetParamsByteSize: Integer;

    //--------------Finding/accessing
    //Find a variable by name across all current scopes.
    function FindByNameAllScopes(AName: String): PVariable;

    //Find in current scope only
    function FindByFuncParamIndex(Index: Integer): PVariable;

    //Finds the Result variable in the current Scope.
    //If there is no result variable, returns nil
    function FindResult: PVariable;

    //Find the variable for the function argument with the given index.
    //Returns nil if not found
    function FindVarForArg(Index: Integer): PVariable;

    //If we own V, returns it's index, otherwise returns -1
    function IndexOf(V: PVariable): Integer;

    function IndexToData(Index: Integer): PVariable;

    //------------CodeGen

    //Set the Offset values for local, relative, variables.
    procedure SetOffsets;

    //Find a varible by name only searching the current Scope (ie list).
    function FindByNameInScope(AName: String): PVariable;

    //---Scopes

    //Scope depth has been DECrememented. Any in scope variable with higher scope depth
    //need to go out of scope
    procedure ScopeDepthDecced(NewDepth: Integer);

    //-----------UI related

    //Prepare all variables for execution
    procedure ExecClear;

    procedure ToStrings(S: TStrings;TypeSummary: Boolean);


    //-----------------------------Phis and fixups
    //Clear the Touched flag for every variable
    procedure ClearTouches;
    procedure ClearAdjust;
  end;

procedure InitialiseVars;

//ONLY used by Def.Functions.
//Returns the variables asm name (ie label) given the names of the scope and
//variable.
//If VarName is empty then index will be appended (used for temp/hidden vars)
//function VarGetAsmName(const ScopeName, VarName: String;Index: Integer): String;

//------------Scope related

//These routines are called by the Scopes routines to create, clear and set Scopes
function CreateVarList: PVarList;
procedure SetCurrentVarList(List: PVarList);

//The currently active variable list
var Vars: PVarList;

implementation
uses {IOUtils,}
  Def.Globals, Def.Scopes,
  Parse.Base;

{ EAddrMode }

constructor EAddrMode.Create;
begin
  inherited Create('Invalid addressing mode');
end;

const
  //Offset from IX to the first parameter
  //IX+4... is parameters
  //IX+2 is return address
  //IX+0 is previous IX
  //< IX is local variables
  iStackOffsetForFirstParam = +4;

procedure InitialiseVars;
begin
  Vars := nil;
end;

function CreateVarList: PVarList;
begin
  New(Result);
  Result.Initialise;
  Result.MarkPosition := -1;
end;

procedure SetCurrentVarList(List: PVarList);
begin
  Vars := List;
end;

{ TVariable }

procedure TVariable.SetName(AName: String);
begin
  Assert(Name = '', 'Variable.SetName must only be called when Name is blank');
  Name := AName;
end;

procedure TVariable.SetType(AUserType: PUserType);
begin
  //Last item in current list??
  Assert(@Self = Vars.Items[Vars.Items.Count-1], 'VarSetType must only be called when it is the last in the VarList');
  UserType := AUserType;
end;

function VarInList(AVar: PVariable): Boolean;
var V: PVariable;
begin
  for V in Vars.Items do
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
var Scope: PScope;
  Index: Integer;
  LName: String;
begin
  //Find the Scope which 'owns' the variable (we need to scope name)
  Scope := FindScopeForVar(@Self, Index);
  Assert(Scope <> nil, 'Scope not found (for variable)');

  if Name = '' then
    LName := '_temp'
  else
    LName := Name;
  Result := '_v_' + Scope.Name + '_' + LName;

  //Index allows us to uniquify the label name within the scope - important due
  //to the ability to declare variables inline, which means we might end up with
  //multiple variables with the same name.
  //We mustn't do this for function argument variables because they are (very occasionally)
  //used from outside out scope (ie by BlockCopy operations for Pointered types)
  if FuncParamIndex = -1 then
    Result := Result + Index.ToString;
end;

function TVariable.IncVersion: Integer;
begin
  Assert(not IsConst);
  Result := Version + 1;
  if not SkipMode then
    Version := Result;
end;

function TVariable.RequiresStorage: Boolean;
begin
  Result := True;
end;

function TVariable.ToString(TypeSummary: Boolean): String;
begin
  if TypeSummary then
    Result := ''
  else if AddrMode = amStatic then
    Result := '@'+IntToHex(Offset, 4).Tolower
  //vsOffset
  else if Offset < 0 then
    Result := '-' + IntToHex(0-Offset, 2) + ' '
  else
    Result := '+' + IntToHex(Offset, 2) + ' ';
  Result := Result + GetAsmName + ': ' + UserType.Description;
  if not TypeSummary then
    Result := Result + ' = ' + Value.ToString;
end;

procedure TVariable.Touch;
begin
  Touched := True;
end;

function TVariable.VarType: TVarType;
begin
  Assert(Assigned(UserType));
  Result := UserType.VarType;
end;



{ TVarList }

//Doesn't check whether a variable with that name already exists!
function TVarList.AddInt(AName: String;UserType: PUserType;AddrMode: TAddrMode): PVariable;
begin
  New(Result);
  Result.Name := AName;
  Result.UserType := UserType;
  Result.Depth := GetCurrentScope.Depth;
  Result.InScope := True;
  Result.AddrMode := AddrMode;
  Result.FuncParamIndex := -1;
  Result.Version := 0;
  Result.Touched := False;
  Result.Offset := -1;
  Result.Access := vaLocal;
  Result.IsConst := False;
  Result.IsParam := False;
  Result.IsResult := False;
  Vars.Items.Add(Result);

  Result.Value.CreateTyped(UserType, 0);
end;


function TVarList.Add(AName: String; UserType: PUserType): PVariable;
begin
  Assert(AName <> '');
  if GetCurrentScope.Search(AName).IdentType <> itUnknown then
    Result := nil
  else
    Result := AddInt(AName, UserType, GetCurrentScope.GetLocalAddrMode);
end;

function TVarList.AddHidden(UserType: PUserType): PVariable;
begin
  Result := AddInt('',UserType, GetCurrentScope.GetLocalAddrMode);
  Result.Name := '_temp' + Items.IndexOf(Result).ToString;
//  Result.SetName('%'+IntToStr(Index));
end;

function TVarList.AddParameter(const AName: String; UserType: PUserType;
  AddrMode: TAddrMode; AIsResult, PassDataIn: Boolean): PVariable;
var Offset: Integer;  //Offset to the /previous/ stack variable (parameter)
  PrevIndex: Integer; //Indesx of last Parameter added to the variable list
begin
  //If variable is being stored on the stack we need to ascertain the offset of
  //the variable from SP. Otherwise we set offset to -1 to signify no offset has
  //yet been calculated.
  if (AddrMode = amStatic) or (AIsResult and not PassDataIn) then
    Offset := -1
  else  //vsRelative
  begin
    //Get index of the last parameter which has already been added as a variable.
    //We need to ignore any Result parameters, so step backwards through Vars
    PrevIndex := Items.Count-1;
    while (PrevIndex >= 0) and (Items[PrevIndex].IsResult) do
      dec(PrevIndex);

    if PrevIndex < 0 then
      //This is the first parameter to be added
      Offset := iStackOffsetForFirstParam
    else
    begin
      Offset := Items[PrevIndex].Offset;
      //Offsets for 'regular' variables are assigned later. Offsets for parameters
      //are assigned here. If previous variable does not have an offset assigned we have a bug.
      Assert(Offset <> -1,'Adding a parameter variable, but previous variable is NOT a parameter variable');
      Offset := Offset + GetTypeSize(Items[PrevIndex].UserType);
    end;
  end;

  Result := AddInt(AName, UserType, AddrMode);
  Result.Offset := Offset;
  Result.IsResult := AIsResult;
  Result.IsParam := PassDataIn or not AIsResult;
end;

function TVarList.AddUnnamed(UserType: PUserType): PVariable;
begin
  Result := AddInt('', UserType, GetCurrentScope.GetLocalAddrMode);
end;

procedure TVarList.Clear;
var V: PVariable;
begin
  for V in Items do
    Dispose(V);
  MarkPosition := -1;
  Items.Free;
  Items := nil;
end;

procedure TVarList.ClearAdjust;
var V: PVariable;
begin
  for V in Items do
  begin
    V.AdjustVersionFrom := -1;
    V.AdjustVersionTo := -1;
  end;
end;

procedure TVarList.ClearTouches;
var V: PVariable;
begin
  for V in Items do
    V.Touched := False;
end;

procedure TVarList.ExecClear;
var V: PVariable;
begin
  for V in Items do
  begin
    V.Value := TImmValue.CreateInteger(0);
    V.UserType := nil;
    V.Version := 0;
  end;
end;

function TVarList.FindByFuncParamIndex(Index: Integer): PVariable;
var I: Integer;
begin
  for I := 0 to Items.Count-1 do
    if Items[I].FuncParamIndex = Index then
      EXIT(Items[I]);

  Result := nil;
end;

function TVarList.FindByNameAllScopes(AName: String): PVariable;
var IdentData: TIdentData;
begin
  IdentData := GetCurrentScope.SearchAllInScope(AName);
  if IdentData.IdentType = itVar then
    EXIT(IdentData.V);

  Result := nil;
end;

function TVarList.FindByNameInScope(AName: String): PVariable;
var I: Integer;
begin
  //Result pseudo variable
  if CompareText(AName, 'Result') = 0 then
  begin
    for I := 0 to Items.Count-1 do
      if Items[I].IsResult then
        EXIT(Items[I]);
    EXIT(nil);
  end;

  for I := 0 to Items.Count-1 do
    if (CompareText(Items[I].Name, AName) = 0) and Items[I].InScope then
      EXIT(Items[I]);

  Result := nil;
end;

function TVarList.FindResult: PVariable;
begin
  for Result in Items do
    if Result.IsResult then
      EXIT;

  Result := nil;
end;

function TVarList.FindVarForArg(Index: Integer): PVariable;
begin
  for Result in Items do
    if Result.FuncParamIndex = Index then
      EXIT;

  Result := nil;
end;

function TVarList.GetCount: Integer;
begin
  Result := Items.Count;
end;

function TVarList.GetLocalsByteSize: Integer;
var V: PVariable;
begin
  Result := 0;
  for V in Items do
    if (V.Access in [vaLocal]) or V.IsResult then
      if V.RequiresStorage then
        case V.AddrMode of
          amStatic, amStaticRef: ;  //Ignore
          amStack: Result := Result + GetTypeSize(V.UserType);
          amStackRef: Result := Result + GetVarTypeSize(vtPointer);
        else
          raise EAddrMode.Create;
        end;
end;

function TVarList.GetParamsByteSize: Integer;
var V: PVariable;
begin
  Result := 0;
  for V in Items do
    if V.IsParam then
      if V.RequiresStorage then
        case V.AddrMode of
          amStatic, amStaticRef: ;  //Ignore
          amStack: Result := Result + GetTypeSize(V.UserType);
          amStackRef: Result := Result + GetVarTypeSize(vtPointer);
        else
          raise EAddrMode.Create;
        end;
end;

function TVarList.IndexOf(V: PVariable): Integer;
begin
  Result := Items.IndexOf(V);
end;

function TVarList.IndexToData(Index: Integer): PVariable;
begin
  Result := Items[Index];
end;

procedure TVarList.Initialise;
begin
  Items := TList<PVariable>.Create;
  MarkPosition := -1;
end;

procedure TVarList.Mark;
begin
  Assert(MarkPosition = -1);
  MarkPosition := Items.Count;
end;

procedure TVarList.Rollback;
begin
  Assert(MarkPosition <> -1);
  while Items.Count > MarkPosition do
  begin
    Dispose(Items[Items.Count-1]);
    Items.Delete(Items.Count-1);
  end;

  MarkPosition := -1;
end;

procedure TVarList.ScopeDepthDecced(NewDepth: Integer);
var I: Integer;
begin
  I := Items.Count-1;
  while (I >= 0) and (Items[I].Depth > NewDepth) do
  begin
    Items[I].InScope := False;
    dec(I);
  end;
end;

procedure TVarList.SetOffsets;
var Offset: Integer;
  V: PVariable;
begin
  Offset := 0;
  for V in Items do
  begin
    //Ignore parameters (which already have offsets assigned)
    //Ignore variable with global/absolute storage addresses
    if (V.Offset = -1) and (V.AddrMode in [amStack, amStackRef]) then
    begin
      //TODO: Ignore if optimised away
      if V.AddrMode = amStack then
        Offset := Offset - GetTypeRegSize(V.UserType)
      else if V.AddrMode = amStackRef then
        //If StackPtr we need storage for a pointer
        Offset := Offset - GetVarTypeSize(vtPointer);

      //If local var and requires storage
      V.Offset := Offset;
    end;
  end;
end;

procedure TVarList.ToStrings(S: TStrings; TypeSummary: Boolean);
var I: Integer;
begin
  S.Clear;
  if TypeSummary then
    S.Add('Variables summary:')
  else
    S.Add('Variables dump:');
  for I := 0 to Items.Count-1 do
    S.Add(IntToStr(I) + '- ' + Items[I].ToString(TypeSummary));
end;

end.
