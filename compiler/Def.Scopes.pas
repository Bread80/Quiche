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
  Def.Functions, Def.IL, Def.Variables, Def.Consts,
  CleverPuppy;

type
  TIdentType = (itVar, itFunction, itConst, itType);

  PScope= ^TScope;
  TScope = record
    Parent: PScope;   //The next higher Scope, or nil in none
    Name: String;     //For the scope. For reference only
    Depth: Integer;   //The block depth within the current scope. Used to determine
                      //which variables are in scope. Ie. within a BEGIN...END block
                      //Depth is increased for every BEGIN and decreased for every END
    Func: PFunction;  //The function which owns this scope. Nil for main/global code
    ConstList: PConstList;  //Constants declared at this scope level
//    Types: ;          //Ditto for Types
    VarList: TVarList;  //Ditto for Variables

    FuncList: TFuncList;  //Functions declared in this scope
    AsmCode: TStringList;  //Assembly code for Code segment for this scope (from CodeGen)
    AsmData: TStringList;   //Assembly code for Data segment (from CodeGen)
    ILList: TILList;
    CleverPuppy: TCleverPuppy;  //Clever puppy codegen data (if any)

    //Returns the storage type for local variables
    function GetLocalStorage: TVarStorage;
  end;

//Only to be used with caution!!
//(Used by vars, consts, types to search through parent scopes)
procedure SetCurrentScope(Scope: PScope);

//Sets the CurrentScope to the parent of the Current Scope
//(Used by vars, consts, types to search through parent scopes)
//Returns True if there was a parent scope, False if not
function SetParentScope: Boolean;

//Get te current scope. Used by varsm consts, types, functions to cache the current
//value before searching parent scopes
function GetCurrentScope: PScope;

//Creates a new scope and sets it as the current scope
//If no main scope is assigned, also sets this as the main scope
//If the Scope is for a function, Func is that function, otherwise Func should be nil.
//Name is the name of the scope. If Name is '' the name will be retrived from Func.
function CreateCurrentScope(Func: PFunction;const Name: String): PScope;

//Set the current scope to the parent of the curent scope
procedure EndCurrentScope;

//Initialise parser data. Specifically, creates the MainScope and sets it as the
//current scope
procedure InitialiseScopes;

//Search the current scope for an item with the given identifier (name)
//Returns a pointer to the item if an item was found, otherwise returns nil.
//The return value can be cast to the appropriate type: PVariable, PFunction, etc.
//Return values:
//IdentType identifies if the found item is a variable, function, const or type etc.
function SearchCurrentScope(const Ident: String;out IdentType: TIdentType): Pointer;
//Search the current scope and it's parents for an item with the given identifier (name)
//Scope returns the scope in which the item was found.
function SearchScopes(const Ident: String;out IdentType: TIdentType;out Scope: PScope): Pointer;

//Increase the Depth of the current Scope (called for each BEGIN)
procedure ScopeIncDepth;

//ecrease the Depth of the current Scope (called for each END)
procedure ScopeDecDepth;

//Get the depth of the current Scope
function ScopeGetDepth: Integer;


//-----GUI utlilities
procedure ScopesToStrings(S: TStrings);

//Find a scope and set it as the CurrentScope.
//NOT to be used whilst compiling!
function ScopeSelectByName(Name: String): Boolean;

var SystemScope: TScope;

implementation
uses Generics.Collections, SysUtils,
  Def.Globals, Def.QTypes;

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
//    Types: ;
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
  //Types
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
//    Types: ;
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

function SearchCurrentScope(const Ident: String;out IdentType: TIdentType): Pointer;
var
  Scope: PScope;
  C: PConst;
  V: PVariable;
  Func: PFunction;
begin
  Scope := GetCurrentScope;

  //Consts
  if Assigned(Scope.ConstList) then
  begin
    C := Consts.FindByNameInScope(Ident);
    if C <> nil then
    begin
      IdentType := itConst;
      EXIT(C);
    end;
  end;

  //Variables
  if Assigned(Scope.VarList) then
  begin
    V := VarFindByNameInScope(Ident);
    if V <> nil then
    begin
      IdentType := itVar;
      EXIT(V);
    end;
  end;

  if Assigned(Scope.FuncList) then
  begin
    Func := FuncFindInScope(Ident);
    if Func <> nil then
    begin
      IdentType := itFunction;
      EXIT(Func);
    end;
  end;

  Result := nil;
end;

function SearchScopes(const Ident: String;out IdentType: TIdentType;
  out Scope: PScope): Pointer;
begin
  Scope := GetCurrentScope;

  repeat
    //Search scope
    Result := SearchCurrentScope(Ident, IdentType);
    if Assigned(Result) then
    begin
      //Restore original scope
      SetCurrentScope(Scope);
      EXIT;
    end;
  until not SetParentScope;

  //Search System scope
  SetCurrentScope(@SystemScope);
  Result := SearchCurrentScope(Ident, IdentType);
  if Assigned(Result) then
  begin
    //Restore original scope
    SetCurrentScope(Scope);
    EXIT;
  end;

  //Restore original scope
  SetCurrentScope(Scope);
  Result := nil;
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

function TScope.GetLocalStorage: TVarStorage;
begin
  if Func = nil then
    Result := optDefaultVarStorage
  else
    Result := Func.GetLocalStorage;
end;

procedure InitSystemScope;
begin
  InitScope(@SystemScope);
  SystemScope.Name := 'System';
  try
    //Fake the current scope
    SetCurrentScope(@SystemScope);
    SystemScope.ConstList.Add('False', vtBoolean, TImmValue.CreateBoolean(False));
    SystemScope.ConstList.Add('True', vtBoolean, TImmValue.CreateBoolean(True));
    SystemScope.ConstList.Add('Maxint', vtInteger, TImmValue.CreateTyped(vtInteger, GetMaxValue(vtInteger)));
    SystemScope.ConstList.Add('Minint', vtInteger, TImmValue.CreateTyped(vtInteger, GetMinValue(vtInteger)));
    SystemScope.ConstList.Add('nil', vtPointer, TImmValue.CreateTyped(vtPointer, $0000));
  finally
//    SetCurrentScope(nil);
  end;
end;

initialization
  ScopeList := nil;
  InitSystemScope;
end.
