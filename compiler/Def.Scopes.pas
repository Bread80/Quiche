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

The parser also stores ILData and Assembly output for each scope for debugging etc
purposes.
*)

unit Def.Scopes;

interface
uses Classes,
  Def.Functions, Def.IL, Def.Variables, Def.Consts, Def.UserTypes, Def.ScopesEX,
  CleverPuppy;

type
//========== SEARCH FOR IDENTIFIERS
  TIdentData = record
    Value: TDeclaredItem;

    function AsEnumItem: TEnumItem;
    function AsType: TUserType;

    function GetUserType: TUserType;
    function GetAddrMode: TAddrMode;

    case IdentType: TIdentType of
      itUnknown: ();
      itFunction: (
        F: PFunction;
        );
    end;

  PScope= ^TScopeOLD;
  TScopeOLD = record
    FunctionScope: TCodeBlock;  //The current function (or program, unit scope)
    BlockScope: TCodeBlock; //The current code block. Initially the same as FunctionScope
                      //NOTE: WE WILL LEAK THESE UNTIL MIGRATION IS COMPLETE
    Parent: PScope;   //The next higher Scope, or nil in none
    Name: String;     //For the scope. For reference only
    Func: PFunction;  //The function which owns this scope. Nil for main/global code
    FuncList: PFuncList;    //Functions declared in this scope

    AsmCode: TStringList;   //Assembly code for Code segment for this scope (from CodeGen)
    AsmData: TStringList;   //Assembly code for Data segment (from CodeGen)
    ILList: TILList;
    CleverPuppy: TCleverPuppy;  //Clever puppy codegen data (if any)

    procedure Initialise(const AName: String;AParent: PScope);

    //Returns the storage type for local variables
    function GetLocalAddrMode: TAddrMode;

    //Search the current scope for an item with the given identifier (name)
    //Returns a pointer to the item if an item was found, otherwise returns nil.
    //The return value can be cast to the appropriate type: PVariable, PFunction, etc.
    //Return values:
    //IdentType identifies if the found item is a variable, function, const or type etc.
    //IgnoreFuncs is temporarily required so that searches for Type names return the type instead
    function SearchUpLocal(const Ident: String; IgnoreFuncs: Boolean = False): TIdentData;

    //Searches all scopes which are in scope (ie. it recursed up parent scopes and
    //also searches any library scopes
    function SearchUpAll(const Ident: String;IgnoreFuncs: Boolean = False): TIdentData;
  end;

//Only to be used with caution!!
//(Used by vars, consts, types to search through parent scopes)
procedure SetCurrentScope(Scope: PScope);

//Sets the CurrentScope to the parent of the Current Scope
//(Used by vars, consts, types to search through parent scopes)
//Returns True if there was a parent scope, False if not
function SetParentScope: Boolean;

//Get the current scope. Used by vars, consts, types, functions to cache the current
//value before searching parent scopes
function GetCurrentScope: PScope;

//Creates a new scope and sets it as the current scope
//If no main scope is assigned, also sets this as the main scope
//If the Scope is for a function, Func is that function, otherwise Func should be nil.
//Name is the name of the scope. If Name is '' the name will be retrieved from Func.
function CreateCurrentScope(Func: PFunction;const Name: String): PScope;

//Creates a new scope to be used in a record definition.
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


//Scope blocks relate to BEGIN..END[1] structures in the code. Do not confuse with
//the IL concept of blocks.
//[1] Which can be implicit for a single statement after, eg, an IF or FOR, or
//    in a REPEAT..UNTIL or CASE clause.
//
//Starts a new nested code block and returns it.
//The result must be passed to ScopeEndBlock to properly close the block
function ScopeBeginBlock: TCodeBlock;
//Ends a code block. CodeBlock is the block to close. An error will be raised if
//if is not the current code block
function ScopeEndBlock(CodeBlock: TCodeBlock): TCodeBlock;
//Deletes the block and any data created within it.
//Used to remove null code, such as IF False, WHILE False etc
//TODO: REPLACE WITH A ROUTINE TO SKIP A CODEBLOCK
procedure ScopeRollbackBlock(CodeBlock: TCodeBlock);



//Searched /all/ scopes for the given function
function FindScopeForFunc(AFunc: PFunction): PScope;

//-----GUI utlilities
procedure ScopesToStrings(S: TStrings);

//Find a scope and set it as the CurrentScope.
//NOT to be used whilst compiling!
function ScopeSelectByName(Name: String): Boolean;

//For 'built-in' types, consts and functions (intrinsics)
var SystemScope: TScopeOLD;

//A couple of functions to hack our way around circular unit references.
//(NEVER directly typecast one to the other, just in case this changes)
function ScopeToScopeHandle(Scope: PScope): TScopeHandle;
function ScopeHandleToScope(Handle: TScopeHandle): PScope;

implementation
uses Generics.Collections, SysUtils,
  Def.Globals;

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
    Scope.FuncList.Clear;
    Dispose(Scope.FuncList);
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

function CreateCurrentScope(Func: PFunction;const Name: String): PScope;
var LName: String;
begin
  Assert((Func <> nil) or (Name <> ''), 'Scope requires a Func or a Name (or both)');

  New(Result);
  ScopeList.Add(Result);
  if Name <> '' then
    LName := Name
  else
    LName := Func.Name;

  Result.Initialise(LName, GetCurrentScope);
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

procedure InitSystemScope;
var SCope: PScope;
begin
  SystemScope.Initialise('System', nil);
  Scope := GetCUrrentScope;
  SetCurrentScope(@SystemScope);
  CreateSystemTypes(SystemScope.FunctionScope);
  CreateSystemConsts(SystemScope.FunctionScope);
  SetCurrentScope(Scope);
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
  InitSystemScope;
end;
(*
function FindScopeForVar(AVar: TVariable;out Index: Integer): PScope;
begin
  for Result in ScopeList do
  begin
    Index := Result.VarList.IndexOf(AVar);
    if Index >= 0 then
      EXIT;
  end;

  Index := -1;
  Result := nil;
end;
*)
function FindScopeForFunc(AFunc: PFunction): PScope;
var Scope: PScope;
begin
  for Scope in ScopeList do
    if AFunc = Scope.Func then
      EXIT(Scope);

  Result := nil;
end;

//TODO:
//1. Store ILIndex with TCodeBlock - will allow us to undo it DONE
//2. Add IsEnded flag to TCodeBlock - will allow us to verify block is ended before we delete it DOEN
//3. ScopeBeginBlock returns the newly created TCodeBlock. DONE
//4. After the call to ScopeEndBlock (Also returns block(?))...
//    we can rollback the IL code, and delete the CodeBlock (delete via the parents Items list).
//    a. Verify IsEnded.
//    b. Verify is same block we created(?)
//    c. Verify is last item on parents Items list
function ScopeBeginBlock: TCodeBlock;
var ILIndex: Integer; //Current IL Position (ie first item in new scope)
begin
  ILIndex := GetCurrentScope.ILList.Count;
  Result := GetCurrentScope.BlockScope.BeginCodeBlock(ILIndex);
  GetCurrentScope.BlockScope := Result;
end;

//ecrease the Depth of the current Scope (called for each END)
function ScopeEndBlock(CodeBlock: TCodeBlock): TCodeBlock;
var ILIndex: Integer; //Current IL Position (ie first item in new scope)
begin
  Assert(CodeBlock = GetCurrentScope.BlockScope);
  Assert(not CodeBlock.IsEnded);
  ILIndex := GetCurrentScope.ILList.Count-1;
  if CodeBlock.Parent is TCodeBlock then
    GetCurrentScope.BlockScope := TCodeBlock(CodeBlock.Parent)
  else
    GetCurrentScope.BlockScope := GetCurrentScope.FunctionScope;
  Result := CodeBlock.EndCodeBlock(ILIndex);
(*  GetCurrentScope.BlockScope := GetCurrentScope.BlockScope.EndCodeBlock(ILIndex);
  Assert(Assigned(GetCurrentScope.BlockScope));
*)end;

procedure ScopeRollbackBlock(CodeBlock: TCodeBlock);
var ILIndex: Integer; //Current IL Position (ie first item in new scope)
begin
  Assert(CodeBlock = GetCurrentScope.BlockScope);
  Assert(CodeBlock.IsEnded);
  ILRollback(CodeBlock.ILIndexBegin);
  Assert(Assigned(CodeBlock.Parent));
(*  CodeBlock.Parent.DeleteChild(CodeBlock);*)

  Assert(False, 'TODO');
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

function TScopeOLD.GetLocalAddrMode: TAddrMode;
begin
  if Func = nil then
    Result := optDefaultAddrMode
  else
    Result := Func.GetLocalAddrMode;
end;

function ScopeToScopeHandle(Scope: PScope): TScopeHandle;
begin
  Result := TScopeHandle(Scope);
end;

function ScopeHandleToScope(Handle: TScopeHandle): PScope;
begin
  Result := PScope(Handle);
end;

procedure TScopeOLD.Initialise(const AName: String;AParent: PScope);
begin
  Parent := AParent;
  Name := AName;
  Func := nil;
  FuncList := CreateFuncList;
  AsmCode := TStringlist.Create;
  AsmData := TStringList.Create;
  ILList:= CreateILList;
  CleverPuppy := nil;

  if AParent <> nil then
    BlockScope := TCodeBlock.Create(Name, AParent.BlockScope, 0, 0)
  else
    BlockScope := TCodeBlock.Create(Name, nil, 0, 0);
  FunctionScope := BlockScope;

end;

function TScopeOLD.SearchUpLocal(const Ident: String; IgnoreFuncs: Boolean): TIdentData;
begin
  Result.Value := BlockScope.FindIdentifierUpLocal(Ident, IgnoreFuncs);
  if Result.Value <> nil then
  begin
    Result.IdentType := Result.Value.IdentType;
    EXIT;
  end;

  if not IgnoreFuncs then //TEMP
    if Assigned(FuncList) then
    begin
      Result.F := FuncList.FindByNameInScope(Ident);
      if Result.F <> nil then
      begin
        Result.IdentType := itFunction;
        EXIT;
      end;
    end;

  Result.IdentType := itUnknown;;
end;

function TScopeOLD.SearchUpAll(const Ident: String; IgnoreFuncs: Boolean): TIdentData;
var Scope: PScope;
begin
  //Search current scope
  Result.Value := BlockScope.FindIdentifierUpAll(Ident, IgnoreFuncs);
  if Result.Value <> nil then
  begin
    Result.IdentType := Result.Value.IdentType;
    EXIT;
  end;

  //Search SystemScope
  Result.Value := SystemScope.BlockScope.FindIdentifierUpAll(Ident, IgnoreFuncs);
  if Result.Value <> nil then
  begin
    Result.IdentType := Result.Value.IdentType;
    EXIT;
  end;

  //----OLD
  Scope := @Self;

  repeat
    //Search scope
    Result := Scope.SearchUpLocal(Ident, IgnoreFuncs);
    if Result.IdentType <> itUnknown then
      EXIT;
    Scope := Scope.Parent;
  until Scope = nil;

  //Search libraries (more TODO)
  Scope := @SystemScope;
  Result := Scope.SearchUpLocal(Ident, IgnoreFuncs);
end;

{ TIdentData }

function TIdentData.AsEnumItem: TEnumItem;
begin
  Assert(IdentType = itEnumItem);
  Assert(Value is TEnumItem);
  Result := Value as TEnumItem;
end;

function TIdentData.AsType: TUserType;
begin
  Assert(IdentType = itType);
  Assert(Value is TUserType);
  Result := Value as TUserType;
end;

function TIdentData.GetAddrMode: TAddrMode;
begin
  case IdentType of
    itVariable: Result := (Value as TVariable).AddrMode;
    itConst: Result := amStatic;
  else
    raise Exception.Create('Invalid IdentVar.UserType');
  end;
end;

function TIdentData.GetUserType: TUserType;
begin
  case IdentType of
    itVariable, itConst:
      Result := (Value as TTypedIdentifier).UserType
  else
    raise Exception.Create('Invalid IdentVar.UserType');
  end;
end;

initialization
  ScopeList := nil;
//  InitSystemScope;
end.
