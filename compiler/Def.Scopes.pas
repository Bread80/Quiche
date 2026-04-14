//Update to the compiler to be object based
unit Def.Scopes;

interface
uses Generics.Collections, SysUtils;

type
  //Using IdentType in addition to class hierarchy allows us to use case statements
  //to distinguish between search results
  TIdentType = (itUnknown, itScope, itVariable, itFunction, itIntrinsic, itConst, itType, itEnumItem);

  TScope = class;

  TScopedItem = class
  private
    FName: String;
    FOwner: TScope;
  //Scoped Items can Use other Scoped Items. This can form the base for only linking
  //library items which are referenced by the final code

  //Non-identifier items might be assembly chunks (for re-assembling into output code)
  public
    constructor Create(const AName: String;AOwner: TScope);
    //Will raise an error if the item already has a name.
    //During definitions if can be useful to declare items unnamed can help to stop them being found in searches
    procedure AssignName(const AName: String);
    function IdentType: TIdentType;virtual;abstract;

    property Name: String read FName;
    property Owner: TScope read FOwner;
  end;

  //An item which is specific to the Quiche language, as opposed to generic libraries etc.
  //(Primarily for future use)
  TQuicheItem = class(TScopedItem)
  end;

  //Variables, Consts, Types, EnumItems, Functions, Record Fields, Units etc all
  //derive from here.
  //Also TCompiler is a scope containing libraries, units, program, etc

  //List of TScopedItem (or TIdentifier??)
  //Allows us to search for identifiers which are currently in scope
  TScope = class(TQuicheItem)
  private
    FIsRootBlock: Boolean;
    FUID: Integer;
    FNextBlockID: Integer;
    FILIndexEnd: Integer;
    FILIndexBegin: Integer;  //Used by GetUniqueBlockID

    FItems: TObjectList<TScopedItem>;
    FIsEnded: Boolean;
  protected
    //AILIndexBegin is the index of the first ILItem within this block
    constructor CreateCodeBlock(const AName: String;AOwner: TScope;AUID, AILIndexBegin: Integer);

    function GetUniqueBlockID: Integer;
  public
    constructor Create(const AName: String;AOwner: TScope);
    destructor Destroy;override;
    function IdentType: TIdentType;override;

    procedure Add(AItem: TScopedItem);
    //Removes a child from Items. The child *must* be the last item on the list.
    //If not an exception will be raised.
    procedure RemoveChild(AChild: TScope);

    function IsRootBlock: Boolean;

    //Creates and returns a new code block. Used to store declarations local to
    //a code block. Use for BEGIN..END, and other code structures such as REPEAT..UNTIL,
    //CASE clauses and even single statement blocks after IF, FOR etc.
    //AILIndex is the index of the first ILItem in the block
    function BeginCodeBlock(AILIndex: Integer): TScope;
    //End a code block initiated by BeginCodeBlock. Raises an exception if the number
    //of calls to each is not balanced. If Self.Items is empty, Self will be removed
    //from it's parents Items and freed
    function EndCodeBlock(AILIndex: Integer): TScope;

    //Enumerates all child items, all their child items etc.
    procedure EachDown<T: Class>(Proc: TProc<T>);
    //As above but only recurses into code block. Ie not into declaration blocks
    procedure EachDownCode<T: Class>(Proc: TProc<T>);
    //Searches local scope for any items of the given class, then calls Proc for
    //each of them. Local scope includes all identifiers declared within the current
    //scope (Ie starting at the IsRootBlock above us).
    procedure EachAllLevel<T: Class>(Proc: TProc<T>);

    //Enumerates all child items, all their child items etc.
    function SearchDown<T: Class>(Func: TFunc<T, Boolean>): T;
    //Searches local scope for any items of the given class, then calls Proc for
    //each of them. Local scope includes all identifiers declared within the current
    //scope (Ie starting at the IsRootBlock above us).
    function SearchAllLevel<T: Class>(Func: TFunc<T, Boolean>): T;

    //Searches child items at current scope. Does not recurse up or down the scope.
    function SearchItems<T: Class>(Func: TFunc<T, Boolean>): T;
    //Search all items at local scope the current level, then recurses upwards through
    //the block scopes. Ie searches items declared in the current scope
    function SearchUpLocal<T: Class>(Func: TFunc<T, Boolean>): T;
    //Searches upwards to the top or all scopes, functions, declarations etc.
    function SearchUpAll<T: Class>(Func: TFunc<T, Boolean>): T;

    //Search the current scope for an item with the given identifier (name)
    //Primarily used too determine whether it is okay to declare a new identifier
    //with the given name.
    //Includes special handling for the Result pseudo function
    //Returns a the item if an item was found, otherwise returns nil.
    //The return value can be cast to the appropriate type: TVariable, TFunction, etc.
    //IgnoreFuncs is temporarily required so that searches for Type names return the type instead(??)
    function FindIdentifierUpLocal(const Ident: String; IgnoreFuncs: Boolean = False): TQuicheItem;
    function FindIdentifierUpAll(const Ident: String; IgnoreFuncs: Boolean = False): TQuicheItem;

    function ToString: String;override;

    property Items: TObjectList<TScopedItem> read FItems;
    property IsEnded: Boolean read FIsEnded;

     //Index of the first ILItem in the block
    property ILIndexBegin: Integer read FILIndexBegin;
    //Index of the last ILItem in the block
    property ILIndexEnd: Integer read FILIndexEnd;
end;

type TScopeHandle = Pointer;

//A couple of functions to hack our way around circular unit references.
//(NEVER directly typecast one to the other, just in case this changes)
function ScopeToScopeHandle(Scope: TScope): TScopeHandle;
function ScopeHandleToScope(Handle: TScopeHandle): TScope;

implementation
uses Def.Compiler, Def.Functions, Def.Variables;

{ TScopedItem }

procedure TScopedItem.AssignName(const AName: String);
begin
  Assert(FName = '');
  FName := AName;
end;

constructor TScopedItem.Create(const AName: String; AOwner: TScope);
begin
  inherited Create;
  FName := AName;
  FOwner := AOwner;
end;

{ TScope }

procedure TScope.Add(AItem: TScopedItem);
begin
  if AItem.Name <> '' then
    Assert(FindIdentifierUpLocal(AItem.Name) = nil);
  FItems.Add(AItem);
end;

function TScope.BeginCodeBlock(AILIndex: Integer): TScope;
var NewID: Integer;
begin
  NewID := GetUniqueBlockID;
  Result := TScope.CreateCodeBlock(Name, Self, NewID, AILIndex);
  Items.Add(Result);
end;

constructor TScope.Create(const AName: String; AOwner: TScope);
begin
  inherited Create(AName, AOwner);
  FItems := TObjectList<TScopedItem>.Create(True);
  FIsEnded := True;
  FIsRootBlock := True;
  FUID := 0;
  FILIndexBegin := -1;
  FILIndexEnd := -1;
  FNextBlockID := 1;
end;

constructor TScope.CreateCodeBlock(const AName: String; AOwner: TScope;
  AUID, AILIndexBegin: Integer);
begin
  inherited Create(AName, AOwner);
  FItems := TObjectList<TScopedItem>.Create(True);
  FISEnded := False;
  FIsRootBlock := False;
  FUID := AUID;
  FILIndexBegin := AILIndexBegin;
  FILIndexEnd := -1;
  FNextBlockID := 1;
end;

destructor TScope.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TScope.EachDown<T>(Proc: TProc<T>);
var Item: TScopedItem;
begin
  for Item in Items do
    if Item is T then
      Proc(T(Item))
    else if Item is TScope then
      TScope(Item).EachDown<T>(Proc);
end;

procedure TScope.EachDownCode<T>(Proc: TProc<T>);
var Item: TScopedItem;
begin
  for Item in Items do
    if Item is T then
      Proc(T(Item))
    else if (Item is TScope) and not (Item is TILScope) then
      TScope(Item).EachDownCode<T>(Proc);
end;

procedure TScope.EachAllLevel<T>(Proc: TProc<T>);
begin
  if Self is TILScope then
    EachDown<T>(Proc)
  else
  begin
    Assert(Assigned(Owner));
    Owner.EachAllLevel<T>(Proc);
  end;
end;

function TScope.EndCodeBlock(AILIndex: Integer): TScope;
begin
  if Assigned(Owner) and (Items.Count = 0) then
  begin //Trim block with no declarations
    Result := nil;
    Owner.RemoveChild(Self);
//    Self.Free;  //Free by RemoveChild call
  end
  else
  begin
    Result := Owner;
    FILIndexEnd := AILIndex;
    FIsEnded := True;
  end;
end;

function TScope.FindIdentifierUpAll(const Ident: String;
  IgnoreFuncs: Boolean): TQuicheItem;
begin
  if CompareText(Ident, 'Result') = 0 then
  begin
    Result := TFuncs.FindResult;
    if Assigned(Result) then
      EXIT;
  end;

  Result := SearchUpAll<TQuicheItem>(
    function(Item: TQuicheItem): Boolean
    begin
      Result := CompareText(Item.Name, Ident) = 0;
    end);
end;

function TScope.FindIdentifierUpLocal(const Ident: String;
  IgnoreFuncs: Boolean): TQuicheItem;
begin
  if CompareText(Ident, 'Result') = 0 then
  begin
    Result := TFuncs.FindResult;
    if Assigned(Result) then
      EXIT;
  end;

  Result := SearchUpLocal<TQuicheItem>(
    function(Item: TQuicheItem): Boolean
    begin
      Result := CompareText(Item.Name, Ident) = 0;
    end);
end;

function TScope.GetUniqueBlockID: Integer;
begin
  if IsRootBlock then
  begin
    Result := FNextBlockID;
    inc(FNextBlockID);
  end
  else
    Result := Owner.GetUniqueBlockID;
end;

function TScope.IdentType: TIdentType;
begin
  Result := itScope;
end;

function TScope.IsRootBlock: Boolean;
begin
  Result := FUID = 0;
end;

procedure TScope.RemoveChild(AChild: TScope);
begin
  Assert(Items.Count > 0);
  Assert(Items[Items.Count-1] = AChild);
  Items.Delete(Items.Count-1);
end;

function TScope.SearchAllLevel<T>(Func: TFunc<T, Boolean>): T;
begin
  Result := nil;
  if Self is TILScope then
    Result := SearchDown<T>(Func)
  else
  begin
    Assert(Assigned(Owner));
    Result := Owner.SearchAllLevel<T>(Func);
  end;
end;

function TScope.SearchDown<T>(Func: TFunc<T, Boolean>): T;
var Item: TScopedItem;
begin
  Result := nil;
  for Item in Items do
  begin
    if Item is T then
      if Func(T(Item)) then
        EXIT(T(Item))
    else if Item is TScope then
    begin
      Result := (TScope(Item).SearchDown<T>(Func));
      if Assigned(Result) then
        EXIT;
    end;
  end;

  if (Result = nil) and (Self is TProgram) then
    Result := TProgram(Self).SearchDownUnits<T>(Func);
end;

function TScope.SearchItems<T>(Func: TFunc<T, Boolean>): T;
var Item: TScopedItem;
begin
  Result := nil;
  for Item in Items do
    if Item is T then
      if Func(T(Item)) then
        EXIT(T(Item));
end;

function TScope.SearchUpAll<T>(Func: TFunc<T, Boolean>): T;
var Item: TScopedItem;
begin
  Result := SearchItems<T>(Func);
  if Assigned(Result) then
    EXIT;

  if Assigned(Owner) then
    Result := Owner.SearchUpAll<T>(Func);

  if (Result = nil) and (Self is TProgram) then
    Result := TProgram(Self).SearchItemsUnits<T>(Func);
end;

function TScope.SearchUpLocal<T>(Func: TFunc<T, Boolean>): T;
var Item: TScopedItem;
begin
  Result := SearchItems<T>(Func);
  if Assigned(Result) then
    EXIT;

  if Assigned(Owner) then
    if not (Self as TScope).IsRootBlock then
      Result := Owner.SearchUpLocal<T>(Func);
end;

function TScope.ToString: String;
var Item: TScopedItem;
  S: String;
begin
  if Items.Count > 0 then
  begin
    Result := #13'Scope: ' + Name;
    if ILIndexBegin >= 0 then
      Result := inherited + ' IL: ' + ILIndexBegin.ToString + '..' + ILIndexEnd.ToString;
    Result := Result + #13;
    for Item in FItems do
    begin
      S := Item.ToString;
      if S <> '' then
        Result := Result + S + #13;
    end;
  end;
end;

{ Utilities }

function ScopeToScopeHandle(Scope: TScope): TScopeHandle;
begin
  Result := TScopeHandle(Scope);
end;

function ScopeHandleToScope(Handle: TScopeHandle): TScope;
begin
  Result := TScope(Handle);
end;

end.
