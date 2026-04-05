//Update to the compiler to be object based
unit Def.ScopesEX;

interface
uses Generics.Collections;

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

  TIdentifier = class(TScopedItem)
  end;

  //Variables, Consts, Types, EnumItems, Functions, Record Fields, Units etc all
  //derive from here.
  //Also TCompiler is a scope containing libraries, units, program, etc

  //List of TScopedItem (or TIdentifier??)
  //Allows us to search for identifiers which are currently in scope
  TScope = class(TIdentifier)
  private
    FParent: TScope;
    FItems: TObjectList<TScopedItem>;
    FDepth: Integer;
  public
    constructor Create(const AName: String;AParent: TScope);
    destructor Destroy;override;
    function IdentType: TIdentType;override;

    procedure Add(AItem: TScopedItem);

    //Search the current scope for an item with the given identifier (name)
    //Returns a the item if an item was found, otherwise returns nil.
    //The return value can be cast to the appropriate type: TVariable, TFunction, etc.
    //IgnoreFuncs is temporarily required so that searches for Type names return the type instead
    function SearchLocal(const Ident: String; IgnoreFuncs: Boolean = False): TIdentifier;

    //Recurse up scopes, and into libraries, until a match is found
    function SearchUpScopes(const Ident: String; IgnoreFuncs: Boolean = False): TIdentifier;

    function ToString: String;

    property Parent: TScope read FParent;
    //TODO: Remove!
    property Depth: Integer read FDepth write FDepth;
    property Items: TObjectList<TScopedItem> read FItems;
(*

    Depth: Integer;   //The block depth within the current scope. Used to determine
                      //which variables are in scope. Ie. within a BEGIN...END block
                      //Depth is increased for every BEGIN and decreased for every END
    Func: PFunction;  //The function which owns this scope. Nil for main/global code
    VarList: PVarList;      //Ditto for Variables
    FuncList: PFuncList;    //Functions declared in this scope

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
    FNextBlockID: Integer;  //Used by GetUniqueBlockID
    function GetUniqueBlockID: Integer;
    //Removes a child from Items. The child *must* be the last item on the list.
    //If not an exception will be raised.
    procedure RemoveChild(AChild: TCodeBlock);
  public
    constructor Create(const AName: String;AParent: TScope;AUID: Integer);

    //Creates and returns a new code block. Used to store declarations local to
    //a code block. Use for BEGIN..END, and other code structures such as REPEAT..UNTIL,
    //CASE clauses and even single statement blocks after IF, FOR etc.
    function BeginCodeBlock: TCodeBlock;
    //End a code block initiated by BeginCodeBlock. Raises an exception if the number
    //of calls to each is not balanced. If Self.Items is empty, Self will be removed
    //from it's parents Items and freed
    function EndCodeBlock: TCodeBlock;

    //Used to identify this block within the current scope
    property UID: Integer read FUID;
  end;

implementation
uses SysUtils;

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
    Assert(SearchLocal(AItem.Name) = nil);
  FItems.Add(AItem);
end;

constructor TScope.Create(const AName: String; AParent: TScope);
begin
  inherited Create(AName, AParent);
  FParent := AParent;
  FItems := TObjectList<TScopedItem>.Create(True);
  FDepth := 0;
end;

destructor TScope.Destroy;
begin
  FItems.Free;
  inherited;
end;


function TScope.IdentType: TIdentType;
begin
  Result := itScope;
end;

function TScope.SearchLocal(const Ident: String; IgnoreFuncs: Boolean): TIdentifier;
var Item: TScopedItem;
begin
  for Item in FItems do
    if Item is TIdentifier then
      if CompareText(Item.Name, Ident) = 0 then
        if (Item.IdentType <> itFunction) or not IgnoreFuncs then
          EXIT(Item as TIdentifier);

  Result := nil;
end;

function TScope.SearchUpScopes(const Ident: String;
  IgnoreFuncs: Boolean): TIdentifier;
begin
  Result := SearchLocal(Ident, IgnoreFuncs);

  if (Result = nil) and (Parent <> nil) then
    Result := Parent.SearchUpScopes(Ident, IgnoreFuncs);
end;

function TScope.ToString: String;
var Item: TScopedItem;
begin
  Result := 'Scope: ' + Name + #13#13;
  for Item in FItems do
    Result := Result + Item.ToString + #13;
end;

{ TCodeBlock }

function TCodeBlock.BeginCodeBlock: TCodeBlock;
var NewID: Integer;
begin
  NewID := GetUniqueBlockID;
  Result := TCodeBlock.Create(Name, Self, NewID);
  Items.Add(Result);
end;

constructor TCodeBlock.Create(const AName: String; AParent: TScope;
  AUID: Integer);
begin
  if AUID > 0 then
    inherited Create(AName + '.' + AUID.ToString, AParent)
  else
    inherited Create(AName, AParent);
  FUID := AUID;
  FNextBlockID := 1;
end;

function TCodeBlock.EndCodeBlock: TCodeBlock;
begin
  Assert(Parent is TCodeBlock);
  Result := Parent as TCodeBlock;
  if Items.Count = 0 then
  begin
    (Parent as TCodeBlock).RemoveChild(Self);
//    Self.Free;
  end;
end;

function TCodeBlock.GetUniqueBlockID: Integer;
begin
  if UID <> 0 then
  begin
    Assert(Parent is TCodeBlock);
    Result := (Parent as TCodeBlock).GetUniqueBlockID;
  end
  else
  begin
    Result := FNextBlockID;
    inc(FNextBlockID);
  end;
end;

procedure TCodeBlock.RemoveChild(AChild: TCodeBlock);
begin
  Assert(Items.Count > 0);
  Assert(Items[Items.Count-1] = AChild);
  Items.Delete(Items.Count-1);
end;

end.
