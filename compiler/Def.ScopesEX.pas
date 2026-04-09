//Update to the compiler to be object based
unit Def.ScopesEX;

interface
uses Generics.Collections, SysUtils;

type
  //Using IdentType in addition to class hierarchy allows us to use case statements
  //to distinguish between search results
  TIdentType = (itUnknown, itScope, itVariable, itFunction, itConst, itType, itEnumItem);

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

  TDeclaredItem = class(TScopedItem)
  end;

  //Variables, Consts, Types, EnumItems, Functions, Record Fields, Units etc all
  //derive from here.
  //Also TCompiler is a scope containing libraries, units, program, etc

  //List of TScopedItem (or TIdentifier??)
  //Allows us to search for identifiers which are currently in scope
  TScope = class(TScopedItem)
  private
    FParent: TScope;
    FItems: TObjectList<TScopedItem>;
    FIsEnded: Boolean;
  public
    constructor Create(const AName: String;AParent: TScope);
    destructor Destroy;override;
    function IdentType: TIdentType;override;

    procedure Add(AItem: TScopedItem);

    //Search all items at local scope the current level, then recurses upwards through
    //the block scopes. Ie searches items declared in the current scope
    function SearchUpLocal<T: Class>(Func: TFunc<T, T>): T;
    //Searches upwards to the top or all scopes, functions, declarations etc.
    function SearchUpAll<T: Class>(Func: TFunc<T, T>): T;


    //Search the current scope for an item with the given identifier (name)
    //Primarily used too determine whether it is okay to declare a new identifier
    //with the given name.
    //Includes special handling for the Result pseudo function
    //Returns a the item if an item was found, otherwise returns nil.
    //The return value can be cast to the appropriate type: TVariable, TFunction, etc.
    //IgnoreFuncs is temporarily required so that searches for Type names return the type instead(??)
    function FindIdentifierUpLocal(const Ident: String; IgnoreFuncs: Boolean = False): TDeclaredItem;
    function FindIdentifierUpAll(const Ident: String; IgnoreFuncs: Boolean = False): TDeclaredItem;

    //Recurse up scopes, and into libraries, until a match is found
//    function FindIdentUp(const Ident: String; IgnoreFuncs: Boolean = False): TDeclaredItem;

    function ToString: String;override;

    property Parent: TScope read FParent;
    property Items: TObjectList<TScopedItem> read FItems;

    property IsEnded: Boolean read FIsEnded;
(*
    AsmCode: TStringList;   //Assembly code for Code segment for this scope (from CodeGen)
    AsmData: TStringList;   //Assembly code for Data segment (from CodeGen)
    ILList: TILList;
    CleverPuppy: TCleverPuppy;  //Clever puppy codegen data (if any)


    //Returns the storage type for local variables
    function GetLocalAddrMode: TAddrMode;
  *)end;


  TCodeBlock = class(TScope)
  private
    FUID: Integer;
    FNextBlockID: Integer;
    FILIndexEnd: Integer;
    FILIndexBegin: Integer;  //Used by GetUniqueBlockID
    function GetUniqueBlockID: Integer;
    //Removes a child from Items. The child *must* be the last item on the list.
    //If not an exception will be raised.
    procedure RemoveChild(AChild: TCodeBlock);
  public
    //AILIndexBegin is the index of the first ILItem within this block
    constructor Create(const AName: String;AParent: TScope;AUID, AILIndexBegin: Integer);

    //Creates and returns a new code block. Used to store declarations local to
    //a code block. Use for BEGIN..END, and other code structures such as REPEAT..UNTIL,
    //CASE clauses and even single statement blocks after IF, FOR etc.
    //AILIndex is the index of the first ILItem in the block
    function BeginCodeBlock(AILIndex: Integer): TCodeBlock;
    //End a code block initiated by BeginCodeBlock. Raises an exception if the number
    //of calls to each is not balanced. If Self.Items is empty, Self will be removed
    //from it's parents Items and freed
    function EndCodeBlock(AILIndex: Integer): TCodeBlock;

    function IsRootBlock: Boolean;
    //Used to identify this block within the current scope
//    property UID: Integer read FUID;

    //Enumerates all child items, all their child items etc.
    procedure EachDown<T: Class>(Proc: TProc<T>);
    //Searches local scope for any items of the given class, then calls Proc for
    //each of them. Local scope includes all identifiers declared within the current
    //scope (Ie starting at the IsRootBlock above us).
    procedure EachAllLevel<T: Class>(Proc: TProc<T>);

    //Enumerates all child items, all their child items etc.
    function SearchDown<T: Class>(Func: TFunc<T, T>): T;
    //Searches local scope for any items of the given class, then calls Proc for
    //each of them. Local scope includes all identifiers declared within the current
    //scope (Ie starting at the IsRootBlock above us).
    function SearchAllLevel<T: Class>(Func: TFunc<T, T>): T;

    function ToString: String;override;
    //Index of the first ILItem in the block
    property ILIndexBegin: Integer read FILIndexBegin;
    //Index of the last ILItem in the block
    property ILIndexEnd: Integer read FILIndexEnd;
  end;

implementation
uses Def.Functions, Def.Variables;

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

constructor TScope.Create(const AName: String; AParent: TScope);
begin
  inherited Create(AName, AParent);
  FParent := AParent;
  FItems := TObjectList<TScopedItem>.Create(True);
  FIsEnded := False;
end;

destructor TScope.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TScope.FindIdentifierUpAll(const Ident: String;
  IgnoreFuncs: Boolean): TDeclaredItem;
begin
  if CompareText(Ident, 'Result') = 0 then
  begin
    Result := TFuncs.FindResult;
    if Assigned(Result) then
      EXIT;
  end;

  Result := SearchUpAll<TDeclaredItem>(
    function(Item: TDeclaredItem): TDeclaredItem
    begin
      if CompareText(Item.Name, Ident) = 0 then
        Result := Item
      else
        Result := nil;
    end);
end;

function TScope.FindIdentifierUpLocal(const Ident: String;
  IgnoreFuncs: Boolean): TDeclaredItem;
begin
  if CompareText(Ident, 'Result') = 0 then
  begin
    Result := TFuncs.FindResult;
    if Assigned(Result) then
      EXIT;
  end;

  Result := SearchUpLocal<TDeclaredItem>(
    function(Item: TDeclaredItem): TDeclaredItem
    begin
      if CompareText(Item.Name, Ident) = 0 then
        Result := Item
      else
        Result := nil;
    end);
end;

function TScope.IdentType: TIdentType;
begin
  Result := itScope;
end;

function TScope.SearchUpAll<T>(Func: TFunc<T, T>): T;
var Item: TScopedItem;
begin
  Result := nil;
  for Item in Items do
  begin
    if Item is T then
      Result := Func(T(Item));

    if Assigned(Result) then
      EXIT;
  end;

  if Assigned(Parent) then
    Result := Parent.SearchUpAll<T>(Func);
end;

function TScope.SearchUpLocal<T>(Func: TFunc<T, T>): T;
var Item: TScopedItem;
begin
  Result := nil;
  for Item in Items do
  begin
    if Item is T then
      Result := Func(T(Item));

    if Assigned(Result) then
      EXIT;
  end;

  if Assigned(Parent) and (Parent is TCodeBlock) then
    if not (Self as TCodeBlock).IsRootBlock then
      Result := Parent.SearchUpLocal<T>(Func);
end;

function TScope.ToString: String;
var Item: TScopedItem;
  S: String;
begin
  if Items.Count > 0 then
  begin
    Result := #13'Scope: ' + Name + #13;
    for Item in FItems do
    begin
      S := Item.ToString;
      if S <> '' then
        Result := Result + S + #13;
    end;
  end;
end;

{ TCodeBlock }

function TCodeBlock.BeginCodeBlock(AILIndex: Integer): TCodeBlock;
var NewID: Integer;
begin
  NewID := GetUniqueBlockID;
  Result := TCodeBlock.Create(Name, Self, NewID, AILIndex);
  Items.Add(Result);
end;

constructor TCodeBlock.Create(const AName: String; AParent: TScope;
  AUID, AILIndexBegin: Integer);
begin
  if AUID > 0 then
    inherited Create(AName + '.' + AUID.ToString, AParent)
  else
    inherited Create(AName, AParent);
  FUID := AUID;
  FILIndexBegin := AILIndexBegin;
  FILIndexEnd := -1;
  FNextBlockID := 1;
end;

procedure TCodeBlock.EachDown<T>(Proc: TProc<T>);
var Item: TScopedItem;
begin
  for Item in Items do
    if Item is T then
      Proc(T(Item))
    else if Item is TCodeBlock then
      TCodeBlock(Item).EachDown<T>(Proc);
end;

procedure TCodeBlock.EachAllLevel<T>(Proc: TProc<T>);
begin
  if not IsRootBlock then
  begin
    Assert(Assigned(Parent));
    (Parent as TCodeBlock).EachAllLevel<T>(Proc);
  end
  else
    EachDown<T>(Proc);
end;

function TCodeBlock.EndCodeBlock(AILIndex: Integer): TCodeBlock;
begin
  Assert(Parent is TCodeBlock);
  if Items.Count = 0 then
  begin
    Result := nil;
    TCodeBlock(Parent).RemoveChild(Self);
//    Self.Free;  //Free by RemoveChild call
  end
  else
  begin
  Result := TCodeBlock(Parent);
    FILIndexEnd := AILIndex;
    FIsEnded := True;
  end;
end;

function TCodeBlock.GetUniqueBlockID: Integer;
begin
  if IsRootBlock then
  begin
    Result := FNextBlockID;
    inc(FNextBlockID);
  end
  else
  begin
    Assert(Parent is TCodeBlock);
    Result := TCodeBlock(Parent).GetUniqueBlockID;
  end
end;

function TCodeBlock.IsRootBlock: Boolean;
begin
  Result := FUID = 0;
end;

procedure TCodeBlock.RemoveChild(AChild: TCodeBlock);
begin
  Assert(Items.Count > 0);
  Assert(Items[Items.Count-1] = AChild);
  Items.Delete(Items.Count-1);
end;

function TCodeBlock.SearchDown<T>(Func: TFunc<T, T>): T;
var Item: TScopedItem;
begin
  Result := nil;
  for Item in Items do
  begin
    if Item is T then
      Result := Func(T(Item))
    else if Item is TCodeBlock then
      Result := (TCodeBlock(Item).SearchDown<T>(Func));

    if Assigned(Result) then
      EXIT;
  end;
end;

function TCodeBlock.ToString: String;
begin
  Result := inherited + ' IL: ' + ILIndexBegin.ToString + '..' + ILIndexEnd.ToString;
end;

function TCodeBlock.SearchAllLevel<T>(Func: TFunc<T, T>): T;
begin
  Result := nil;
  if not IsRootBlock then
  begin
    Assert(Assigned(Parent));
    Result := (Parent as TCodeBlock).SearchAllLevel<T>(Func);
  end
  else
    Result := SearchDown<T>(Func);
end;

end.
