(*
A scope holds data about variables, types, constants, functions etc that have been
created by the current code 'segment'. Segment here usually refers to a single function
or the global scope.

A scope also has a pointer to the 'parent' scope. The parent scope is the next higher
level function or, ultimately, the global scope.

The parser will create a new scope at the start of every new function. The parser
will end that scope once it reaches the end of the function.

Scopes are mostly transparent to the rest of the compiler. Other than the above use
case at the start and end of parsing a function, scopes are also use by Variables,
Function, Types and Consts when searching and, occasionally, when adding, items.
In such uses the CurrentScope must be obtained and restored after setting and searching
parent scopes.

The parser also stores ILData and Assembly output for each scope for debigging etc
purposes.
*)

unit Def.Scopes;

interface
uses Classes,
  Def.Functions, Def.IL, Def.Variables, Def.Consts, Def.UserTypes,
  CleverPuppy;

type
  PScope= ^TScope;
  TScope = record
    Parent: PScope;   //The next higher Scope, or nil in none
    Name: String;     //For the scope. For reference only
    Depth: Integer;   //The block depth within the current scope. Used to determine
                      //which variables are in scope. Ie. within a BEGIN...END block
                      //Depth is increased for every BEGIN and decreased for every END
    Func: PFunction;  //The function which owns this scope. Nil for main/global code
    ConstList: PConstList;  //Constants declared at this scope level
    TypeList: PTypeList;       //Ditto for Types
    VarList: TVarList;      //Ditto for Variables
    FuncList: TFuncList;    //Functions declared in this scope

    AsmCode: TStringList;   //Assembly code for Code segment for this scope (from CodeGen)
    AsmData: TStringList;   //Assembly code for Data segment (from CodeGen)
    ILList: TILList;
    CleverPuppy: TCleverPuppy;  //Clever puppy codegen data (if any)

    //Returns the storage type for local variables
    function GetLocalAddrMode: TAddrMode;
  end;

//Only to be used with caution!!
//(Used by vars, consts, types to search through parent scopes)
procedure SetCurrentScope(Scope: PScope);

//Sets the CurrentScope to the parent of the Current Scope
//(Used by vars, consts, types to search through parent scopes)
//Returns True if there was a parent scope, False if not
function SetParentScope: Boolean;

//Get te current scope. Used by vars, consts, types, functions to cache the current
//value before searching parent scopes
function GetCurrentScope: PScope;

//Creates a new scope and sets it as the current scope
//If no main scope is assigned, also sets this as the main scope
//If the Scope is for a function, Func is that function, otherwise Func should be nil.
//Name is the name of the scope. If Name is '' the name will be retrived from Func.
function CreateCurrentScope(Func: PFunction;const Name: String): PScope;

//Creates a new scope to be used in a record deefinition.
//Scope will be created a selected as the current scope such that variable and
//function definitions will be created in the appropriate lists.
//NOTE:
//Current scope *must* be preserved before calling this (GetCurrentScope)...
//...and reselected afterwards (SetCurrentScope)
//TODO: Parent reference may need to be removed(??)
function CreateRecordScope(const Name: String): PScope;

//Set the current scope to the parent of the curent scope
procedure EndCurrentScope;

//Initialise parser data. Specifically, creates the MainScope and sets it as the
//current scope
procedure InitialiseScopes;


//Increase the Depth of the current Scope (called for each BEGIN)
procedure ScopeIncDepth;

//ecrease the Depth of the current Scope (called for each END)
procedure ScopeDecDepth;

//Get the depth of the current Scope
function ScopeGetDepth: Integer;

//========== SEARCH FOR IDENTIFIERS
type
  TIdentType = (itUnknown, itVar, itFunction, itConst, itType, itEnumItem);

  TIdentData = record
    case IdentType: TIdentType of
      itUnknown: ();
      itVar: (
        V: PVariable;
        );
      itFunction: (
        F: PFunction;
        );
      itConst: (
        C: PConst;
        );
      itType, itEnumItem: (
        T: PUserType;
        Index: Integer; //Not used for itType
        );
    end;

//Search the current scope for an item with the given identifier (name)
//Returns a pointer to the item if an item was found, otherwise returns nil.
//The return value can be cast to the appropriate type: PVariable, PFunction, etc.
//Return values:
//IdentType identifies if the found item is a variable, function, const or type etc.
//IgnoreFuncs is temporarily required so that searches for Type names return the type instead
function SearchCurrentScope(const Ident: String; IgnoreFuncs: Boolean = False): TIdentData;
//Search the current scope and it's parents for an item with the given identifier (name)
//Scope returns the scope in which the item was found.
function SearchScopes(const Ident: String;out Scope: PScope;
  IgnoreFuncs: Boolean = False): TIdentData;

//Searches all current scopes for a typed pointer to the given UserType
function SearchScopesForAnonTypedPointer(UserType: PUserType): PUserType;

//-----GUI utlilities
procedure ScopesToStrings(S: TStrings);

//Find a scope and set it as the CurrentScope.
//NOT to be used whilst compiling!
function ScopeSelectByName(Name: String): Boolean;

//For 'built-in' types, consts and functions (intrinsics)
var SystemScope: TScope;

//A couple of functions to hack our way around circular unit references.
//(NEVER directly typecast one to the other, just in case this changes)
function ScopeToScopeHandle(Scope: PScope): TScopeHandle;
function ScopeHandleToScope(Handle: TScopeHandle): PScope;

implementation
uses Generics.Collections, SysUtils,
  Def.Globals, Def.VarTypes;

var
  MainScope: PScope;    //Scope for the program itself/highest level
  CurrentScope: PScope; //Currently active scope

var ScopeList: TList<PScope>;

function GetCurrentScope: PScope;
begin
  Result := CurrentScope;
end;

//Free any objects owned by a scope and clear it's list
procedure ClearScopeList;
var Scope: PScope;
begin
  if ScopeList = nil then
    EXIT;
  for Scope in ScopeList do
  begin
    //Free items owned by the scope??
    Scope.ConstList.Clear;
    Dispose(Scope.ConstList);
    Scope.TypeList.Clear;
    Dispose(Scope.TypeList);
    ClearVarList(Scope.VarList);
    Scope.VarList.Free;
    ClearFuncList(Scope.FuncList);
    Scope.FuncList.Free;
    Scope.AsmCode.Free;
    Scope.AsmData.Free;
    ClearILList(Scope.ILList);
    Scope.ILList.Free;
    Scope.CleverPuppy.Free;
    Dispose(Scope);
  end;

  ScopeList.Clear;
  NewBlock := True;
end;

procedure SetCurrentScope(Scope: PScope);
begin
  CurrentScope := Scope;
  SetCurrentConstList(Scope.ConstList);
  SetCurrentTypeList(Scope.TypeList);
  SetCurrentVarList(Scope.VarList);
  SetCurrentFuncList(Scope.FuncList);
  //AsmCode - nowt to do
  SetCurrentILList(Scope.ILList);
end;

function SetParentScope: Boolean;
begin
  Result := Assigned(CurrentScope.Parent);
  if Result then
    SetCurrentScope(CurrentScope.Parent);
end;

procedure EndCurrentScope;
begin
  if not Assigned(CurrentScope.Parent) then
    raise Exception.Create('Ending scope but parent is not assigned');
  SetCurrentScope(CurrentScope.Parent);

  NewBlock := True;
end;

procedure InitScope(AScope: PScope);
begin
  AScope.Parent := CurrentScope;
  AScope.Depth := 0;
  AScope.Func := nil;
  AScope.ConstList := CreateConstList(AScope.Name);
  AScope.TypeList := CreateTypeList;
  AScope.VarList := CreateVarList;
  AScope.FuncList := CreateFuncList;
  AScope.AsmCode := TStringlist.Create;
  AScope.AsmData := TStringList.Create;
  AScope.ILList:= CreateILList;
  AScope.CleverPuppy := nil;
end;

function CreateCurrentScope(Func: PFunction;const Name: String): PScope;
begin
  Assert((Func <> nil) or (Name <> ''), 'Scope requires a Func or a Name (or both)');
  New(Result);
  ScopeList.Add(Result);
  if Name <> '' then
    Result.Name := Name
  else
    Result.Name := Func.Name;

  Initscope(Result);
  Result.Func := Func;

  if MainScope = nil then
    MainScope := Result;
  SetCurrentScope(Result);
end;

function CreateRecordScope(const Name: String): PScope;
begin
  Result := CreateCurrentScope(nil, Name);
  Result.Parent := nil;
end;

procedure InitialiseScopes;
begin
  ClearScopeList;
  InitialiseILData;
  if ScopeList = nil then
    ScopeList := TList<PScope>.Create;
  CurrentScope := nil;
  MainScope := nil;
  CreateCurrentScope(nil, '_Global');
end;

function SearchCurrentScope(const Ident: String;IgnoreFuncs: Boolean = False): TIdentData;
var
  Scope: PScope;
begin
  Scope := GetCurrentScope;

  //Consts
  if Assigned(Scope.ConstList) then
  begin
    Result.C := Consts.FindByNameInScope(Ident);
    if Result.C <> nil then
    begin
      Result.IdentType := itConst;
      EXIT;
    end;
  end;

  //Types and Enumerations
  if Assigned(Scope.TypeList) then
  begin
    Result.T := Types.FindByNameInScope(Ident);
    if Result.T <> nil then
    begin
      Result.IdentType := itType;
      EXIT;
    end;
    Result.T := Types.FindByEnumNameInScope(Ident, Result.Index);
    if Result.T <> nil then
    begin
      Result.IdentType := itEnumItem;
      EXIT;
    end;
  end;

  //Variables
  if Assigned(Scope.VarList) then
  begin
    Result.V := VarFindByNameInScope(Ident);
    if Result.V <> nil then
    begin
      Result.IdentType := itVar;
      EXIT;
    end;
  end;

  if not IgnoreFuncs then //TEMP
    if Assigned(Scope.FuncList) then
    begin
      Result.F := FuncFindInScope(Ident);
      if Result.F <> nil then
      begin
        Result.IdentType := itFunction;
        EXIT;
      end;
    end;

  Result.IdentType := itUnknown;;
end;

function SearchScopes(const Ident: String;out Scope: PScope;IgnoreFuncs: Boolean = False): TIdentData;
begin
  Scope := GetCurrentScope;

  repeat
    //Search scope
    Result := SearchCurrentScope(Ident, IgnoreFuncs);
    if Result.IdentType <> itUnknown then
    begin
      //Restore original scope
      SetCurrentScope(Scope);
      EXIT;
    end;
  until not SetParentScope;

  //Search System scope
  SetCurrentScope(@SystemScope);
  Result := SearchCurrentScope(Ident, IgnoreFuncs);

  //Restore original scope
  SetCurrentScope(Scope);
end;

function SearchScopesForAnonTypedPointer(UserType: PUserType): PUserType;
var Scope: PScope;
begin
  Scope := GetCurrentScope;

  repeat
    //Search scope
    Result := Types.FindAnonPointerForType(UserType);
    if Assigned(Result) then
    begin
      //Restore original scope
      SetCurrentScope(Scope);
      EXIT;
    end;
  until not SetParentScope;

  //Search System scope
  SetCurrentScope(@SystemScope);
  Result := Types.FindAnonPointerForType(UserType);

  //Restore original scope
  SetCurrentScope(Scope);
end;

procedure ScopeIncDepth;
begin
  CurrentScope.Depth := CurrentScope.Depth + 1;
end;

//ecrease the Depth of the current Scope (called for each END)
procedure ScopeDecDepth;
begin
  Assert(CurrentScope.Depth > 0);
  CurrentScope.Depth := CurrentScope.Depth - 1;
  //TODO Tell variables about the new depth
  Def.Variables.ScopeDepthDecced(CurrentScope.Depth);
  Consts.ScopeDepthDecced(CurrentScope.Depth);
end;

//Get the depth of the current Scope
function ScopeGetDepth: Integer;
begin
  Result := CurrentScope.Depth;
end;



//---------------GUI
procedure ScopesToStrings(S: TStrings);
var Scope: PScope;
begin
  S.Clear;
  for Scope in ScopeList do
    S.Add(Scope.Name);
end;

function ScopeSelectByName(Name: String): Boolean;
var Scope: PScope;
begin
  for Scope in ScopeList do
    if CompareText(Scope.Name, Name) = 0 then
    begin
      SetCurrentScope(Scope);
      EXIT(True);
    end;

  Result := False;
end;

{ TScope }

function TScope.GetLocalAddrMode: TAddrMode;
begin
  if Func = nil then
    Result := optDefaultAddrMode
  else
    Result := Func.GetLocalAddrMode;
end;

procedure InitSystemScope;
begin
  InitScope(@SystemScope);
  SystemScope.Name := 'System';
  try
    //Fake the current scope
    SetCurrentScope(@SystemScope);
    SetSystemTypes;
    SetSystemConsts;
  finally
//    SetCurrentScope(nil);
  end;
end;

function ScopeToScopeHandle(Scope: PScope): TScopeHandle;
begin
  Result := TScopeHandle(Scope);
end;

function ScopeHandleToScope(Handle: TScopeHandle): PScope;
begin
  Result := PScope(Handle);
end;

initialization
  ScopeList := nil;
  InitSystemScope;
end.
