unit Def.UserTypes;

interface
uses Generics.Collections, Classes,
  Def.ScopesEX, Def.VarTypes;

type
  TScopeHandle = Pointer;
  TFunctionHandle = Pointer;

  TUserType = class(TDeclaredItem)
  private
    FSynonymOf: TUserType;
    FVarType: TVarType;
  public
    constructor Create(const AName: String;AOwner: TScope);overload;
    constructor Create(const AName: String;AOwner: TScope;AVarType: TVarType);overload;
    //Clones From *except* Name
    constructor CreateClone(const AName: String;AOwner: TScope;From: TUserType);virtual;
    function IdentType: TIdentType;override;

    function GetItemCount: Integer;virtual;abstract;

    property VarType: TVarType read FVarType;
    //Used for type synomyms. Ie. when this type is an exact duplicate of another
    //type. Otherwise nil.
    property SynonymOf: TUserType read FSynonymOf;


    //Returns True if the vtArrayType of vtChar
    function IsStringType: Boolean;virtual;
    //Returns True if the type is a vtChar or a vtArrayType of vtChar
    function IsStringableType: Boolean;
    //Returns the bytesize of the stored data for the type
    function DataSize: Integer;virtual;abstract;
    //Returns the size of register required to refer to the type.
    //For registered types this will equal DataSize. For pointered types this
    //will be the pointer size
    function RegSize: Integer;virtual;abstract;

    //Returns the name, if the type had one. Otherwise returns the DefinitionString
    function Description: String;
    function DefinitionString: String;virtual;
    function ToString: String;override;  //Returns the declaration
  end;

  //Types where the value can be stored in register(s)
  TRegisteredType = class(TUserType);

  TOrdinalType = class(TRegisteredType)
  private
    FSubrangeOf: TOrdinalType;
    FLow: Integer;
    FHigh: Integer;
    function GetIsSubRange: Boolean;
  public
    constructor Create(const AName: String; AOwner: TScope; AVarType: TVarType;
      ALow, AHigh: Integer);
    constructor CreateSubRange(const AName: String; AOwner: TScope; ASubRangeOf: TOrdinalType;
      ALow, AHigh: Integer);
    constructor CreateClone(const AName: String;AOwner: TScope;From: TUserType);override;
    function GetItemCount: Integer;override;
    function DataSize: Integer;override;
    function RegSize: Integer;override;
    function DefinitionString: String;override;

    //If we are a subrange, returns the type we are a SubRangeOf, otherwise
    //returns Self
    function BaseType: TOrdinalType;
    //If we are a subrange. Especially useful for enumerated types - for others
    //we can infer the base type from the VarType
    property SubRangeOf: TOrdinalType read FSubrangeOf;
    //True if this type is a SubRange of another enumerated type.
    //If so, if it's a subrange of a user created type
    //(ie. an enumeration) then OfType will point to the base type,
    //otherwise VarType will contain the base type
    property IsSubRange: Boolean read GetIsSubRange;
    //Specified for any ordinal type. High must be > Low.
    property Low: Integer read FLow;
    //Ditto
    property High: Integer read FHigh;

  end;

  TIntegerType = class(TOrdinalType)
  public
    constructor CreateSubRange(const AName: String; AOwner: TScope; AOfType: TIntegerType;
      ALow, AHigh: Integer);
  end;

  TEnumItem = class(TDeclaredItem)
  private
    FIndex: Integer;
    FUserType: TUserType;
  public
    constructor Create(const AName: String;AOwner: TScope;AUserType: TUserType;AnIndex: Integer);
    function IdentType: TIdentType;override;
    function ToString: String;override;
    property UserType: TUserType read FUserType;
    property Index: Integer read FIndex;
  end;

  //User defined enumeration (and SubRanges thereeof)
  TEnumeration = class(TOrdinalType)
  private
    FEnumItems: TArray<TEnumItem>;
    procedure SetEnumItems(Items: TArray<String>);
  public
    constructor Create(const AName: String;AOwner: TScope;Items: TArray<String>);
    constructor CreateSubRange(const AName: String; AOwner: TScope; ASubRangeOf: TEnumeration;
      ALow, AHigh: Integer);
    constructor CreateClone(const AName: String;AOwner: TScope;From: TUserType);override;

    function EnumItemToString(Index: Integer): String;
    function StringToEnumIndex(const Ident: String): Integer;
    function DefinitionString: String;override;

    property EnumItems: TArray<TEnumItem> read FEnumItems;
  end;

  TBooleanType = class(TEnumeration)
  public
    constructor Create(const AName: String;AOwner: TScope;VarType: TVarType);
  end;

  TTypedPointer = class(TRegisteredType)
  private
    FOfType: TUserType;
  public
    constructor Create(const AName: String;AOwner: TScope;AOfType: TUserType);
    function DefinitionString: String;override;
    constructor CreateClone(const AName: String;AOwner: TScope;From: TUserType);override;
    function DataSize: Integer;override;
    function RegSize: Integer;override;
    property OfType: TUserType read FOfType;
  end;



  TPointeredType = class(TUserType)
  public
    function RegSize: Integer;override;
  end;

  TRealType = class(TPointeredType)
    function DataSize: Integer;override;
  end;

  TSetMemType = class(TPointeredType)
  private
    FOfType: TOrdinalType;
  public
    constructor Create(const AName: String;AOwner: TScope;AOfType: TOrdinalType);
    function DataSize: Integer;override;
    function DefinitionString: String;override;
    constructor CreateClone(const AName: String;AOwner: TScope;From: TUserType);override;
    property OfType: TOrdinalType read FOfType;
  end;

  TRecordType = class(TPointeredType)
  private
    FScope: TScopeHandle;
  public
    constructor Create(const AName: String;AOwner: TScope;AScopeHandle: TScopeHandle);
    constructor CreateClone(const AName: String;AOwner: TScope;From: TUserType);override;
    function DataSize: Integer;override;
    function DefinitionString: String;override;
    //We can't directly reference a PScope without creating a circular unit
    //reference (both to Scope, Variables and Functions) so we'll use a
    //generic pointer and hack this with typecasts(!)
    property Scope: TScopeHandle read FScope;
  end;

  TFunctionType = class(TPointeredType)
  private
    FFunc: TFunctionHandle;
  public
    constructor Create(const AName: String;AOwner: TScope;AFuncHandle: TFunctionHandle);
    constructor CreateClone(const AName: String;AOwner: TScope;From: TUserType);override;
    function DataSize: Integer;override;
    function DefinitionString: String;override;
    property Func: TFunctionHandle read FFunc;  //Function Template
  end;

(*
  TFileType(TPointeredType)
*)

  TTypeDef = class(TPointeredType)
  public
    constructor Create(const AName: String;AOwner: TScope);
    function DataSize: Integer;override;
  end;

  TArrayType = class(TPointeredType)
  private
    FArraySize: TArraySize;
    FIsUnbounded: Boolean;
    FOfType: TUserType;
    FElementSize: Integer;
  public
    constructor Create(const AName: String;AOwner: TScope;AArraySize: TArraySize;AOfType: TUserType);
    constructor CreateClone(const AName: String;AOwner: TScope;From: TUserType);override;

    function ArrayKind: TArrayKind;virtual;abstract;
    function IndexMetaType: TOrdinalType;virtual;
    property ArraySize: TArraySize read FArraySize;
    //Number of bytes of meta data
    function MetaSize: Integer;
    //Returns the type to use for indexing the arrau
    function IsStringType: Boolean;override;

    //The element type
    property OfType: TUserType read FOfType;
    property IsUnbounded: Boolean read FIsUnbounded;
    property ElementSize: Integer read FElementSize;
  end;

  TPascalArrayType = class(TArrayType)
  private
    FArraySize: TArraySize;
    FBoundsType: TOrdinalType;
  public
    constructor Create(const AName: String;AOwner: TScope;AArraySize: TArraySize;ABoundsType: TOrdinalType;
      AOfType: TUserType);
    constructor CreateUnbounded(const AName: String;AOwner: TScope;AArraySize: TArraySize;
      AOfType: TUserType);
    constructor CreateClone(const AName: String;AOwner: TScope;From: TUserType);override;
    function DataSize: Integer;override;

    function ArrayKind: TArrayKind;override;
    function IndexMetaType: TOrdinalType;override;
    function GetItemCount: Integer;override;
    function DefinitionString: String;override;
    property BoundsType: TOrdinalType read FBoundsType;
  end;

  TVectorType = class(TArrayType)
  private
    FLength: Integer;
  public
    constructor Create(const AName: String;AOwner: TScope;AArraySize: TArraySize;ALength: Integer;AOfType: TUserType);
    constructor CreateUnbounded(const AName: String;AOwner: TScope;AArraySize: TArraySize;
      AOfType: TUserType);
    constructor CreateClone(const AName: String;AOwner: TScope;From: TUserType);override;
    function DataSize: Integer;override;
    function ArrayKind: TArrayKind;override;
    function GetItemCount: Integer;override;
    function DefinitionString: String;override;
    property Length: Integer read FLength;
  end;

  TListType = class(TArrayType)
  private
    FCapacity: Integer;
  public
    constructor Create(const AName: String;AOwner: TScope;AArraySize: TArraySize;ACapacity: Integer;AOfType: TUserType);
    constructor CreateUnbounded(const AName: String;AOwner: TScope;AArraySize: TArraySize;
      AOfType: TUserType);
    constructor CreateClone(const AName: String;AOwner: TScope;From: TUserType);override;
    function DataSize: Integer;override;
    function ArrayKind: TArrayKind;override;
    function DefinitionString: String;override;
    function GetItemCount: Integer;override;
    property Capacity: Integer read FCapacity;
  end;



  TTypes = class
  private
    class function SearchScopeForAnonTypedPointer(Scope: TScope;UserType: TUserType): TTypedPointer;
  public
    class function CreateIntegerType(const AName: String;AVarType: TVarType;ALow, AHigh: Integer): TIntegerType;overload;
    class function CreateIntegerType(AVarType: TVarType;ALow, AHigh: Integer): TIntegerType;overload;
    class function CreateOrdinalType(const AName: String;AVarType: TVarType;ALow, AHigh: Integer): TOrdinalType;
    class function CreateEnumeration(EnumItems: TArray<String>): TEnumeration;
    class function CreateBooleanType(const AName: String;AVarType: TVarType): TBooleanType;overload;
    class function CreateSubRange(CommonType: TOrdinalType; ALow, AHigh: Integer): TOrdinalType;
    class function CreateTypedPointer(AOfType: TUserType): TTypedPointer;
    class function CreateRealType(const AName: String;AVarType: TVarType): TRealType;
    class function CreateSetType(AVarType: TVarType;AOfType: TOrdinalType): TSetMemType;
    class function CreateRecordType(AScope: TScopeHandle): TRecordType;
    class function CreateFunctionType(AScope: TScopeHandle): TFunctionType;
    class function CreateTypeDef(const AName: String): TTypeDef;
    class function CreateArrayType(Kind: TArrayKind; Size: TArraySize;
      BoundsType: TOrdinalType; LengthOrCapacity: Integer; OfType: TUserType): TArrayType;
    class function CreateUnboundedArrayType(Kind: TArrayKind; Size: TArraySize;OfType: TUserType): TArrayType;overload;
    class function CreateUnboundedArrayType(const AName: String;Kind: TArrayKind; Size: TArraySize;OfType: TUserType): TArrayType;overload;

    class function CreateSynonym(const AName: String;FromType: TUserType): TUserType;

    //Returns an anonymous type which is a typed pointer to the UserType.
    //The anonymous type will be created if it does not exist.
    class function SearchScopesForAnonTypedPointer(UserType: TUserType): TTypedPointer;
  end;



  TTypedIdentifier = class(TDeclaredItem)
  private
    FUserType: TUserType;
    function GetVarType: TVarType;
  protected
    //ONLY to be called by TConst!!
    procedure UpdateUserType(NewType: TUserType);
  public
    constructor Create(const AName: String;AScope: TScope;AUserType: TUserType);
    property UserType: TUserType read FUserType;
    property VarType: TVarType read GetVarType;
  end;

function UTToVT(UserType: TUserType): TVarType;

//A Register type is one where the value of the variable is stored in registers.
//A Pointered type is one where the register stores a pointer to the actual data.
//Register types are usually those whose value fits into a register and Pointered
//types are those whose values (data) are often too large to fit into a register.
//Every type is either a Pointered Type or a Register Type. nothing is both.
function IsPointeredType(UserType: TUserType): Boolean;
function IsRegisterType(UserType: TUserType): Boolean;

//Any numeric type. Not typed pointers - these can't be used in expressions
function IsNumericType(UserType: TUserType): Boolean;

//Any integer numeric type, or subrange of one
function IsIntegerType(UserType: TUserType): Boolean;

function IsSignedType(UserType: TUserType): Boolean;

//Any type which only occupies a single byte
function IsByteType(UserType: TUserType): Boolean;

//Any numeric type which only occupies two bytes
function IsWordType(UserType: TUserType): Boolean;

//Boolean, Flag etc
function IsBooleanType(UserType: TUserType): Boolean;

//An ordinal type is one with an ordered set of values with a defined first and
//last value. These are the integer numeric types, chars, booleans, enumerations
//and subranges.
function IsOrdinalType(UserType: TUserType): Boolean;

//An enumerable type is one with an ordered set of values which can be 'enumerated'
//over is sequence. This includes Ordinal types as well as array types.
function IsEnumerableType(UserType: TUserType): Boolean;

//Any arrayed type. Array, vector, list, string etc.
function IsArrayType(UserType: TUserType): Boolean;

//An unbounded array is a Vector with VecLength of iUnboundedArray or a List
//with capacity of iUnboundedArray
function IsUnboundedArray(UserType: TUserType): Boolean;

//Returns the type required to represent a TArraySize. Returns nil if the size is unknown!
function ArraySizeToType(Size: TArraySize): TOrdinalType;

//Are the array types the same?
//Returns False if either array is Unbounded, or if any other fields in the ArrayDef
//differ.
function CompareArrayDefs(DataType, AsType: TArrayType): Boolean;

//If the UserType is a subtype - either a SubRange or Synonym, returns the
//BaseType. Will recurse up BaseTypes if necessary
//For all other cases returns UserType
function GetBaseType(UserType: TUserType): TUSerType;

//Where ValueLow and ValueHigh are numbers, finds the optimal common integer type
//based on their type and value
function TryFindCommonIntegerType(ValueLow, ValueHigh: Integer;out CommonType: TUserType): Boolean;

//Parses a type definition string. These strings are used in library data files.
//If the definition string is for an array type (vtArrayDef) returns extra array
//data in ArrayDef. (In all other cases ArrayDef is invalid)
function StringToType(TypeString: String;out ArrayDef: TArrayDef): TVarType;

//Create global/system types. CurrentScope must be SystemScope
procedure CreateSystemTypes(Scope: TScope);

//Returns the UserType for a VarType. If VarType is vtArrayType then ArrayDef will be
//used to find a matching type. If the relevant type does not exist it will be created.
function GetSystemType(VarType: TVarType;ArrayDef: PArrayDef = nil): TUserType;
function GetSystemOrdinalType(VarType: TVarType): TOrdinalType;

function GetSystemStringType: TUserType;
function GetSystemStringLiteralType: TUserType;

function IdentToType(const Ident: String): TUserType;

implementation
uses SysUtils, RTTI,
  Def.Functions, Def.Scopes, Def.Variables;

{ TUserType }

constructor TUserType.CreateClone(const AName: String;AOwner: TScope;From: TUserType);
begin
  inherited Create(AName, AOwner);
  FVarType := From.VarType;
  FSynonymOf := From.SynonymOf;
end;

constructor TUserType.Create(const AName: String; AOwner: TScope);
begin
  inherited Create(AName, AOwner);

  FSynonymOf := nil;
  FVarType := vtUnknown;
end;

constructor TUserType.Create(const AName: String; AOwner: TScope;
  AVarType: TVarType);
begin
  inherited Create(AName, AOwner);

  FSynonymOf := nil;
  FVarType := AVarType;
end;

function TUserType.DefinitionString: String;
var S: String;
  EI: TEnumItem;
begin
  if SynonymOf <> nil then
    //We're a synonym of the base type
    Result := SynonymOf.Name
  else if VarType = vtUnknown then
    Result := '<Unknown>'
  else
    Result := '';
end;

function TUserType.Description: String;
begin
  if Name <> '' then
    Result := Name
  else
    Result := DefinitionString;
end;

function TUserType.IsStringableType: Boolean;
begin
  Result := (VarType = vtChar) or IsStringType;
end;

function TUserType.IsStringType: Boolean;
begin
  Result := False;
end;

function TUserType.IdentType: TIdentType;
begin
  Result := itType;
end;

function TUserType.ToString: String;
begin
  Result := 'type ';
  if Name <> '' then
    Result := Result + Name
  else
    Result := Result + '<anon>';
  Result := Result + ' = ' + DefinitionString;
end;

{ TOrdinalType }

function TOrdinalType.BaseType: TOrdinalType;
begin
  Result := Self;
  if IsSubRange then
  begin
    Result := SubRangeOf;
    Assert(Result <> nil);
  end;
end;

constructor TOrdinalType.CreateClone(const AName: String;AOwner: TScope;From: TUserType);
begin
  inherited;
  FSubRangeOf := (From as TOrdinalType).SubRangeOf;
  FLow := (From as TOrdinalType).Low;
  FHigh := (From as TOrdinalType).High;
end;

constructor TOrdinalType.Create(const AName: String; AOwner: TScope;
  AVarType: TVarType; ALow, AHigh: Integer);
begin
  inherited Create(AName, AOwner, AVarType);
  FLow := ALow;
  FHigh := AHigh;
  FSubRangeOf := nil;
end;

constructor TOrdinalType.CreateSubRange(const AName: String; AOwner: TScope;
  ASubRangeOf: TOrdinalType; ALow, AHigh: Integer);
begin
  inherited Create(AName, AOwner, ASubRangeOf.VarType);
  FSubRangeOf := ASubRangeOf;
  FLow := ALow;
  FHigh := AHigh;
end;

function TOrdinalType.DataSize: Integer;
begin
  Result := GetVarTypeSize(VarType);
end;

function TOrdinalType.DefinitionString: String;
begin
  if IsSubRange then
    Result := Low.ToString + '..' + High.ToString
  else
    //If ParentType is nil we're the base type,
    //(otherwise we'd be a synonym for the ParentType)
    Result := Name;
end;

function TOrdinalType.GetIsSubRange: Boolean;
begin
  Result := SubRangeOf <> nil;
end;

function TOrdinalType.GetItemCount: Integer;
begin
  Result := High-Low+1;
end;

function TOrdinalType.RegSize: Integer;
begin
  Result := DataSize;
end;

{ TIntegerType }

constructor TIntegerType.CreateSubRange(const AName: String; AOwner: TScope;
  AOfType: TIntegerType; ALow, AHigh: Integer);
begin
  inherited CreateSubRange(AName, AOwner, AOfType, ALow, AHigh);
end;

{ TEnumItem }

constructor TEnumItem.Create(const AName: String; AOwner: TScope;
  AUserType: TUserType; AnIndex: Integer);
begin
  inherited Create(AName, AOwner);
  FUserType := AUserType;
  FIndex := AnIndex;
end;

function TEnumItem.IdentType: TIdentType;
begin
  Result := itEnumItem;
end;

function TEnumItem.ToString: String;
begin
  Result := Name + ' (EnumItem ' + Index.ToString + ' of ' + UserType.Name + ')';
end;

{ TEnumeration }

constructor TEnumeration.CreateClone(const AName: String;AOwner: TScope;From: TUserType);
begin
  inherited;
  FEnumItems := (From as TEnumeration).EnumItems;
end;

constructor TEnumeration.Create(const AName: String; AOwner: TScope;
  Items: TArray<String>);
begin
  inherited Create(AName, AOwner, vtEnumeration, 0, Length(Items)-1);
  SetEnumItems(Items);
end;

constructor TEnumeration.CreateSubRange(const AName: String; AOwner: TScope;
  ASubRangeOf: TEnumeration; ALow, AHigh: Integer);
begin
  inherited CreateSubRange(AName, AOwner, ASubRangeOf, ALow, AHigh);
  FEnumItems := ASubRangeOf.EnumItems;
end;

function TEnumeration.DefinitionString: String;
var Item: TEnumItem;
begin
  if IsSubRange then
  begin
    Assert(Assigned(SubRangeOf));
    Assert(SubRangeOf is TEnumeration);
    Result := (SubRangeOf as TEnumeration).EnumItems[Low].Name + '(' + Low.ToString + ')..' +
        (SubRangeOf as TEnumeration).EnumItems[High].Name + '(' + High.ToString + ')';
  end
  else
  begin
    Result := '';
    for Item in EnumItems do
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + Item.Name;
    end;
    Result := '(' + Result + ')';
  end;
end;

function TEnumeration.EnumItemToString(Index: Integer): String;
begin
  if IsSubRange then
    Result := (SubRangeOf as TEnumeration).EnumItemToString(Index)
  else if (Index >= 0) and (Index < length(EnumItems)) then
    Result := EnumItems[Index].Name
  else
    Result := '<Enumeration value out of range>';
end;

procedure TEnumeration.SetEnumItems(Items: TArray<String>);
var I: Integer;
  Scope: PScope;
begin
  SetLength(FEnumItems, Length(Items));
  Scope := GetCurrentScope;
  for I := 0 to Length(Items)-1 do
  begin
    EnumItems[I] := TEnumItem.Create(Items[I], Scope.BlockScope, Self, I);
    Scope.BlockScope.Add(EnumItems[I]);
  end;
end;

function TEnumeration.StringToEnumIndex(const Ident: String): Integer;
var Item: TEnumItem;
begin
  for Item in EnumItems do
    if CompareText(Ident, Item.Name) = 0 then
      EXIT(Item.Index);

  Result := -1;
end;

{ TBooleanType }

constructor TBooleanType.Create(const AName: String; AOwner: TScope;
  VarType: TVarType);
begin
  if VarType = vtBoolean then
    inherited Create(AName, AOwner, ['False', 'True'])
  else
    inherited Create(AName, AOwner, ['<False>', '<True>']);
  FVarType := VarType;
end;

{ TTypedPointer }

constructor TTypedPointer.CreateClone(const AName: String;AOwner: TScope;From: TUserType);
begin
  inherited;
  FOfType := (From as TTypedPointer).OfType;
end;

constructor TTypedPointer.Create(const AName: String; AOwner: TScope;
  AOfType: TUserType);
begin
  inherited Create(AName, AOwner, vtTypedPointer);
  FOfType := AOfType;
end;

function TTypedPointer.DataSize: Integer;
begin
  Result := 2;
end;

function TTypedPointer.DefinitionString: String;
begin
  Assert(Assigned(OfType));
  Result := Result + '^' + OfType.Description;
end;

function TTypedPointer.RegSize: Integer;
begin
  Result := 2;
end;

{ TPointeredType }

function TPointeredType.RegSize: Integer;
begin
  Result := 2;
end;

{ TRealType }

function TRealType.DataSize: Integer;
begin
  Result := iRealByteSize;
end;

{ TSetMemType }

constructor TSetMemType.CreateClone(const AName: String;AOwner: TScope;From: TUserType);
begin
  inherited;
  FOfType := (From as TSetMemType).OfType;
end;

constructor TSetMemType.Create(const AName: String; AOwner: TScope;
  AOfType: TOrdinalType);
begin
  inherited Create(AName, AOwner, vtSetMem{TODO});
  FOfType := AOfType;
end;

function TSetMemType.DataSize: Integer;
var Size: Integer;
begin
  Size := OfType.GetItemCount - 1;
  Result := (Size shr 3) + 1;
end;

function TSetMemType.DefinitionString: String;
begin
  Result := 'set of ' + OfType.DefinitionString;
end;

{ TRecordType }

constructor TRecordType.CreateClone(const AName: String;AOwner: TScope;From: TUserType);
begin
  inherited;
  FScope := (From as TRecordType).Scope;
end;

constructor TRecordType.Create(const AName: String; AOwner: TScope;
  AScopeHandle: TScopeHandle);
begin
  inherited Create(AName, AOwner, vtRecord);
  FScope := AScopeHandle;
end;

function TRecordType.DataSize: Integer;
var LScope: PScope;
  Return: Integer;
  NewSize: Integer;
begin
  Return := 0;
  LScope := ScopeHandleToScope(Scope);
  LScope.BlockScope.EachDown<TVariable>(
    procedure(V: TVariable)
    begin
      //V.Offset is set when we parse the VarDef. When we add variant records some
      //fields will overlap (ie each offset will not be contiguous)
      NewSize := V.Offset + V.UserType.DataSize;
      if NewSize > Return then
        Return := NewSize;
    end);

  Result := Return;
end;

function TRecordType.DefinitionString: String;
var S: String;
  PrevScope: PScope;
begin
  Assert(VarType = vtRecord);
  Result := 'record'#13;

  PrevScope := GetCurrentScope;
  SetCurrentScope(ScopeHandleToScope(Scope));

  S := '';
  GetCurrentScope.BlockScope.EachAllLevel<TVariable>(
    procedure(V: TVariable)
    begin
      Assert(Assigned(V.UserType));
      S := S + '  +' + V.Offset.ToString + ' ' +
        V.Name + ': ' + V.UserType.DefinitionString + #13;
    end);

  SetCurrentScope(PrevScope);

  Result := S + 'end';
end;

{ TFunctionType }

constructor TFunctionType.CreateClone(const AName: String;AOwner: TScope;From: TUserType);
begin
  inherited;
  FFunc := (From as TFunctionType).Func;
end;

constructor TFunctionType.Create(const AName: String; AOwner: TScope;
  AFuncHandle: TFunctionHandle);
begin
  inherited Create(AName, AOwner, vtFunction);
  FFunc := AFuncHAndle;
end;

function TFunctionType.DataSize: Integer;
begin
  Result := 2;   //Code as data(??)
end;

function TFunctionType.DefinitionString: String;
begin
  Result := FunctionHandleToFunction(Func).ToString;
end;

{ TTypeDef }

constructor TTypeDef.Create(const AName: String; AOwner: TScope);
begin
  inherited Create(AName, AOwner, vtTypeDef);
end;

function TTypeDef.DataSize: Integer;
begin
  Result := 2;  //??
end;

{ TArrayType }

constructor TArrayType.CreateClone(const AName: String;AOwner: TScope;From: TUserType);
begin
  inherited;
  FArraySize := (From as TArrayType).ArraySize;
  FOfType := (From as TArrayType).OfType;
  FIsUnbounded := (From as TArrayType).IsUnbounded;
  FElementSize := (From as TArrayType).ElementSize;
end;

constructor TArrayType.Create(const AName: String; AOwner: TScope;AArraySize: TArraySize;
  AOfType: TUserType);
begin
  inherited Create(AName, AOwner, vtArrayType);
  FArraySize := AArraySize;
  FOfType := AOfType;
  FElementSize := OfType.DataSize;
end;

function TArrayType.IndexMetaType: TOrdinalType;
begin
  Result := ArraySizeToType(ArraySize);
end;

function TArrayType.IsStringType: Boolean;
begin
  Result := OfType.VarType = vtChar;
end;

function TArrayType.MetaSize: Integer;
const MetaUnitSize: array[low(TArraySize)..high(TArraySize)] of Integer =
  (0,1,2);
const MetaUnitCount: array[low(TArrayKind)..high(TArrayKind)] of Integer =
  (0,0,1,2);
begin
  Result := MetaUnitSize[ArraySize] * MetaUnitCount[ArrayKind];
end;

{ TPascalArrayType }

function TPascalArrayType.ArrayKind: TArrayKind;
begin
  Result := atArray;
end;

constructor TPascalArrayType.CreateClone(const AName: String;AOwner: TScope;From: TUserType);
begin
  inherited;
  FArraySize := (From as TPascalArrayType).ArraySize;
  FBoundsType := (From as TPascalArrayType).BoundsType;
end;

constructor TPascalArrayType.Create(const AName: String; AOwner: TScope;
  AArraySize: TArraySize;ABoundsType: TOrdinalType; AOfType: TUserType);
begin
  inherited Create(AName, AOwner, AArraySize, AOfType);
  FBoundsType := ABoundsType;
  FIsUnbounded := False;
end;

constructor TPascalArrayType.CreateUnbounded(const AName: String;
  AOwner: TScope;AArraySize: TArraySize; AOfType: TUserType);
begin
  inherited Create(AName, AOwner, AArraySize, AOfType);
  FBoundsType := nil;
  FIsUnbounded := True;
end;

function TPascalArrayType.DataSize: Integer;
begin
  Assert(not FIsUnbounded);
  //Element count * element size
  Assert(Assigned(BoundsType));
  Result := BoundsType.GetItemCount * OfType.DataSize;
end;

function TPascalArrayType.DefinitionString: String;
begin
  case ArraySize of
    asUnknown: Result := '';
    asShort: Result := Result + 'short ';
    asLong: Result := Result + 'long ';
  else
    raise EVarType.Create;
  end;
  Result := Result + 'array';

  if not IsUnbounded then
  begin
    Assert(Assigned(BoundsType));
    Assert(BoundsType.SynonymOf = nil);
    Result := Result + '[' + BoundsType.Description + ']';
  end;

  Result := Result + ' of ' + OfType.Description;
end;

function TPascalArrayType.GetItemCount: Integer;
begin
  Assert(not IsUnbounded);
  Result := BoundsType.GetItemCount;
end;

function TPascalArrayType.IndexMetaType: TOrdinalType;
begin
  if IsUnbounded then
    Result := inherited
  else
    Result := BoundsType;
end;

{ TVectorType }

function TVectorType.ArrayKind: TArrayKind;
begin
  Result := atVector;
end;

constructor TVectorType.CreateClone(const AName: String;AOwner: TScope;From: TUserType);
begin
  inherited;
  FLength := (From as TVectorType).Length;
end;

constructor TVectorType.Create(const AName: String; AOwner: TScope;AArraySize: TArraySize;
  ALength: Integer; AOfType: TUserType);
begin
  inherited Create(AName, AOwner, AArraySize, AOfType);
  FLength := ALength;
  FIsUnbounded := False;
end;

constructor TVectorType.CreateUnbounded(const AName: String; AOwner: TScope;AArraySize: TArraySize;
  AOfType: TUserType);
begin
  inherited Create(AName, AOwner, AArraySize, AOfType);
  FLength := -1;
  FIsUnbounded := True;
end;

function TVectorType.DataSize: Integer;
begin
  Assert(not IsUnbounded);
  //Meta size + Length * element size
  Assert(Length <> -1); //String literal with unhardened type?
  Result := MetaSize + Length * ElementSize;
end;

function TVectorType.DefinitionString: String;
begin
  case ArraySize of
    asUnknown: Result := '';
    asShort: Result := Result + 'short ';
    asLong: Result := Result + 'long ';
  else
    raise EVarType.Create;
  end;
  Result := Result + 'vector';

  if not IsUnbounded then
    Result := Result + '[' + Length.ToString + ']';

  Result := Result + ' of ' + OfType.Description;
end;

function TVectorType.GetItemCount: Integer;
begin
  Assert(not IsUnbounded);
  Result := Length;
end;

{ TListType }

function TListType.ArrayKind: TArrayKind;
begin
  Result := atList;
end;

constructor TListType.CreateClone(const AName: String;AOwner: TScope;From: TUserType);
begin
  inherited;
  FCapacity := (From as TListType).Capacity;
end;

constructor TListType.Create(const AName: String; AOwner: TScope;AArraySize: TArraySize;
  ACapacity: Integer; AOfType: TUserType);
begin
  inherited Create(AName, AOwner, AArraySize, AOfType);
  FCapacity := ACapacity;
  FIsUnbounded := False;
end;

constructor TListType.CreateUnbounded(const AName: String; AOwner: TScope;AArraySize: TArraySize;
  AOfType: TUserType);
begin
  inherited Create(AName, AOwner, AArraySize, AOfType);
  FCapacity := -1;
  FIsUnbounded := True;
end;

function TListType.DataSize: Integer;
begin
  Assert(not IsUnbounded);
  //Meta size + Capacity * element size
  Result := MetaSize + Capacity * ElementSize;
end;

function TListType.DefinitionString: String;
begin
  case ArraySize of
    asUnknown: Result := '';  //Nothing
    asShort: Result := Result + 'short ';
    asLong: Result := Result + 'long ';
  else
    raise EVarType.Create;
  end;
  Result := Result + 'list';

  if not IsUnbounded then
    Result := Result + '[' + Capacity.ToString + ']';

  Result := Result + ' of ' + OfType.Description;
end;

function TListType.GetItemCount: Integer;
begin
  Assert(not IsUnbounded);
  Result := Capacity;
end;

{-----}

function UTToVT(UserType: TUserType): TVarType;
begin
  if Assigned(UserType) then
    Result := UserType.VarType
  else
    Result := vtUnknown;
end;

function IsPointeredType(UserType: TUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsPointeredVarType(UserType.VarType);
end;

function IsRegisterType(UserType: TUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsRegisterVarType(UserType.VarType);
end;

function IsNumericType(UserType: TUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsNumericVarType(UserType.VarType);
end;

function IsIntegerType(UserType: TUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsIntegerVarType(UserType.VarType);
end;

function IsSignedType(UserType: TUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsSignedVarType(UserType.VarType);
end;

function IsByteType(UserType: TUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsByteVarType(UserType.VarType);
end;

function IsWordType(UserType: TUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsWordVarType(UserType.VarType);
end;

function IsBooleanType(UserType: TUserType): Boolean;
begin
  Result := IsBooleanVarType(UserType.VarType);
end;

function IsOrdinalType(UserType: TUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsOrdinalVarType(UserType.VarType);
end;

function IsEnumerableType(UserType: TUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsEnumerableVarType(UserType.VarType);
end;

function IsArrayType(UserType: TUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsArrayVarType(UserType.VarType);
end;

function IsUnboundedArray(UserType: TUserType): Boolean;
begin
  Result := (UserType is TArrayType) and (UserType as TArrayType).IsUnbounded;
end;

function ArraySizeToType(Size: TArraySize): TOrdinalType;
begin
  case Size of
    asShort: Result := GetSystemOrdinalType(vtByte);
    asLong: Result := GetSystemOrdinalType(vtWord);
  else
    Result := nil;
  end;
end;

function CompareArrayDefs(DataType, AsType: TArrayType): Boolean;
begin
  Assert(IsArrayType(DataType));
  Assert(IsArrayType(AsType));

  //Unbounded arrays are inherently different
  if DataType.IsUnbounded then
    EXIT(False);
  if AsType.IsUnbounded then
    EXIT(False);

  Result := (DataType.ArrayKind = AsType.ArrayKind) and
  (DataType.ArraySize = AsType.ArraySize) and
  (DataType.ElementSize = AsType.ElementSize);
end;

function GetBaseType(UserType: TUserType): TUserType;
begin
  Result := UserType;
  if Assigned(Result) then
    while Assigned(Result.SynonymOf) do
      Result := Result.SynonymOf
end;

//Where ValueLow and ValueHigh are numbers, finds the optimal common integer type
//based on their type and value
function TryFindCommonIntegerType(ValueLow, ValueHigh: Integer;out CommonType: TUserType): Boolean;
var CommonVT: TVarType;
begin
  //If we can't fit all Word values into an Integer type...
  //(This code is future proofed against wider Integer types)
  if GetMaxValue(vtInteger) < GetMaxValue(vtWord) then
    //...and the range of values is too wide to fit into any available Integer type
    if (ValueLow < 0) and (ValueHigh > 32767) then
      EXIT(False);

  //Find most suitable common numeric type (if any)
  if ValueLow < -128 then
    CommonVT := vtInteger
  else if ValueHigh > 32767 then
    CommonVT := vtWord  //TODO: We want to preserve any Pointer typing so use parsed types
  else if ValueLow < 0 then
    if ValueHigh > 127 then
      CommonVT := vtInteger
    else
      CommonVT := vtInt8
  else  //ValueLow >= 0
    if ValueHigh > 255 then
      CommonVT := vtWord
    else
      CommonVT := vtByte;

  CommonType := GetSystemType(CommonVT);
  Result := True;
end;

//Where UserType is an array based type, returns the bytecount of the types
//meta data. Meta data is the Length and Capacity fields (if used). The returned
//value takes into account the byte-size of the fields (ie. byte, word etc).
function GetArrayMetaSize(UserType: TArrayType): Integer;
begin
  Assert(Assigned(UserType));
  Result := UserType.MetaSize;
end;

var SystemTypes: array[low(TVarType)..high(TVarType)] of TUserType;
  StringType: TUserType;        //To use for string variables
  StringLiteralType: TUserType; //To use for string literals


function StringToType(TypeString: String;out ArrayDef: TArrayDef): TVarType;
var Fields: TArray<String>;
  ArrayType: TArrayType;
begin
  Result := StringToVarType(TypeString);
  if Result <> vtUnknown then
    EXIT;

  if CompareText(TypeString, 'String') = 0 then
  begin
    ArrayType := GetSystemStringType as TArrayType;
    ArrayDef.ArrayType := ArrayType.ArrayKind;
    ArrayDef.ArraySize := ArrayType.ArraySize;
    ArrayDef.IsUnbounded := ArrayType.IsUnbounded;
    ArrayDef.ElementSize := ArrayType.ElementSize;
    EXIT(vtArrayType);
  end;

  ArrayDef.ArrayType := atUnknown;
  ArrayDef.ArraySize := asUnknown;
  ArrayDef.IsUnbounded := False;
  ArrayDef.ElementSize := 0;

  Fields := TypeString.Split([':']);
  if Length(Fields) < 1 then
    EXIT(vtUnknown);

  Result := vtArrayType;
  if CompareText(Fields[0], 'ArrayType') = 0 then
    ArrayDef.ArrayType := atUnknown //Matches any array type
  else if CompareText(Fields[0], 'Array') = 0 then
    ArrayDef.ArrayType := atArray
  else if CompareText(Fields[0], 'Vector') = 0 then
    ArrayDef.ArrayType := atVector
  else if CompareText(Fields[0], 'List') = 0 then
    ArrayDef.ArrayType := atUnknown
  else
    EXIT(vtUnknown);

  if Length(Fields) = 1 then
    EXIT;

  if CompareText(Fields[1], 'Short') = 0 then
    ArrayDef.ArraySize := asShort
  else if CompareText(Fields[1], 'Long') = 0 then
    ArrayDef.ArraySize := asLong
  else if Fields[1] = '' then
    ArrayDef.ArraySize := asUnknown
  else if Fields[1] = '*' then
    ArrayDef.ArraySize := asUnknown
  else
    EXIT(vtUnknown);

  if Length(Fields) = 2 then
    EXIT;

  Result := vtUnknown;
end;

function CreateIntegerSystemType(const Name: String;VarType: TVarType;Low, High: Integer): TIntegerType;
begin
  Result := TTypes.CreateIntegerType(Name, VarType, Low, High);
  SystemTypes[VarType] := Result;
end;

function CreateOrdinalSystemType(const Name: String;VarType: TVarType;Low, High: Integer): TOrdinalType;
begin
  Result := TTypes.CreateOrdinalType(Name, VarType, Low, High);
  SystemTypes[VarType] := Result;
end;

function CreateBooleanSystemType(const Name: String;VarType: TVarType): TOrdinalType;
begin
  Result := TTypes.CreateBooleanType(Name, VarType);
  SystemTypes[VarType] := Result;
end;

function CreateRealSystemType(const Name: String;VarType: TVarType): TRealType;
begin
  Result := TTypes.CreateRealType(Name, VarType);
  SystemTypes[VarType] := Result;
end;

function CreateTypeDefSystemType(const Name: String): TTypeDef;
begin
  Result := TTypes.CreateTypeDef(Name);
  SystemTypes[vtTypeDef] := Result;
end;

function GetSystemType(VarType: TVarType;ArrayDef: PArrayDef = nil): TUserType;
begin
  if VarType <> vtArrayType then
    EXIT(SystemTypes[VarType]);

  if ArrayDef = nil then
    EXIT(nil);

  Assert(False);
end;

function GetSystemOrdinalType(VarType: TVarType): TOrdinalType;
begin
  if VarType <> vtArrayType then
    EXIT(SystemTypes[VarType] as TOrdinalType);
end;

function GetSystemStringType: TUserType;
begin
  Result := StringType;
end;

function GetSystemStringLiteralType: TUserType;
begin
  Result := StringLiteralType;
end;

procedure CreateSystemTypes(Scope: TScope);
var VT: TVarType;
  CharType: TUserType;
begin
  for VT := low(TVarType) to high(TVarType) do
    SystemTypes[VT] := nil;


  CreateIntegerSystemType('Int8', vtInt8, -128, 127);
  CreateIntegerSystemType('Integer', vtInteger, -32768, 32767);
  CreateIntegerSystemType('Byte', vtByte, 0, 255);
  CreateIntegerSystemType('Word', vtWord, 0, 65535);
  CreateIntegerSystemType('Pointer', vtPointer, 0, 65535);
  CreateRealSystemType('Real', vtReal);
  CreateBooleanSystemType('Boolean', vtBoolean);
  CreateBooleanSystemType('<Flag>', vtFlag);
  CharType := CreateOrdinalSystemType('Char', vtChar, 0, 255);
  CreateTypeDefSystemType('<TypeDef>');
  StringType := TTypes.CreateUnboundedArrayType('String', atList,
    asShort {TODO: Use COnfig}, CharType);
  StringLiteralType := TTypes.CreateUnboundedArrayType('<StringLiteral>', atVector,
    asShort {TODO: Use config}, CharType);
end;


function IdentToType(const Ident: String): TUserType;
var
  IdentData: TIdentData;
begin
  IdentData := GetCurrentScope.SearchUpAll(Ident, False);
  if IdentData.IdentType = itUnknown then
    EXIT(nil);
  if IdentData.IdentType <> itType then
    EXIT(nil);
  Assert(IdentData.Value <> nil);
  Result := IdentData.AsType;
end;

{ TTypes }

class function TTypes.CreateArrayType(Kind: TArrayKind; Size: TArraySize;
   BoundsType: TOrdinalType; LengthOrCapacity: Integer; OfType: TUserType): TArrayType;
begin
  case Kind of
    atArray: Result := TPascalArrayType.Create('', GetCurrentScope.BlockScope, Size, BoundsType, OfType);
    atVector: Result := TVectorType.Create('', GetCurrentScope.BlockScope, Size, LengthOrCapacity, OfType);
    atList: Result := TListType.Create('', GetCurrentScope.BlockScope, Size, LengthOrCapacity, OfType);
  else
    raise EVarType.Create;
  end;
  GetCurrentScope.BlockScope.Add(Result);
end;

class function TTypes.CreateBooleanType(const AName: String;
  AVarType: TVarType): TBooleanType;
begin
  Result := TBooleanType.Create(AName, GetCurrentScope.BlockScope, AVarType);
  GetCurrentScope.BlockScope.Add(Result);
end;

class function TTypes.CreateEnumeration(EnumItems: TArray<String>): TEnumeration;
begin
  Result := TEnumeration.Create('', GetCurrentScope.BlockScope, EnumItems);
  GetCurrentScope.BlockScope.Add(Result);
end;

class function TTypes.CreateFunctionType(AScope: TScopeHandle): TFunctionType;
begin
  Result := TFunctionType.Create('', GetCurrentScope.BlockScope, AScope);
  GetCurrentScope.BlockScope.Add(Result);
end;

class function TTypes.CreateIntegerType(const AName: String;
  AVarType: TVarType; ALow, AHigh: Integer): TIntegerType;
begin
  Result := TIntegerType.Create(AName, GetCurrentScope.BlockScope, AVarType, ALow, AHigh);
  GetCurrentScope.BlockScope.Add(Result);
end;

class function TTypes.CreateIntegerType(AVarType: TVarType; ALow,
  AHigh: Integer): TIntegerType;
begin
  Result := CreateIntegerType('', AVarType, ALow, AHigh);
end;

class function TTypes.CreateOrdinalType(const AName: String;
  AVarType: TVarType; ALow, AHigh: Integer): TOrdinalType;
begin
  Result := TOrdinalType.Create(AName, GetCurrentScope.BlockScope, AVarType, ALow, AHigh);
  GetCurrentScope.BlockScope.Add(Result);
end;

class function TTypes.CreateRealType(const AName: String;
  AVarType: TVarType): TRealType;
begin
  Result := TRealType.Create(AName, GetCurrentScope.BlockScope, AVarType);
  GetCurrentScope.BlockScope.Add(Result);
end;

class function TTypes.CreateRecordType(AScope: TScopeHandle): TRecordType;
begin
  Result := TRecordType.Create('', GetCurrentScope.BlockScope, AScope);
  GetCurrentScope.BlockScope.Add(Result);
end;

class function TTypes.CreateSetType(AVarType: TVarType;AOfType: TOrdinalType): TSetMemType;
begin
  case AVarType of
    vtSetMem:  Result := TSetMemType.Create('', GetCurrentScope.BlockScope, AOfType);
  else
    raise EVarType.Create;
  end;
  GetCurrentScope.BlockScope.Add(Result);
end;

class function TTypes.CreateSubRange(CommonType: TOrdinalType; ALow,
  AHigh: Integer): TOrdinalType;
begin
  if CommonType is TEnumeration then
    Result := TEnumeration.CreateSubRange('', GetCurrentScope.BlockScope, CommonType as TEnumeration, ALow, AHigh)
  else if CommonType is TIntegerType then
    Result := TIntegerType.CreateSubRange('', GetCurrentScope.BlockScope, TIntegerType(CommonType){!!!}, ALow, AHigh)
  else if CommonType is TOrdinalType then
    Result := TOrdinalType.CreateSubRange('', GetCurrentScope.BlockScope, TOrdinalType(CommonType){!!!}, ALow, AHigh)
  else
    raise Exception.Create('Unable to subrange type ' + CommonType.ClassName);
  GetCurrentScope.BlockScope.Add(Result);
end;

class function TTypes.CreateSynonym(const AName: String;
  FromType: TUserType): TUserType;
var  C: TRTTIContext;
  T: TRTTIType;
  V: TValue;
begin
  C := TRTTIContext.Create;
  T := C.GetType(FromType.ClassType);
  V := T.GetMethod('CreateClone').Invoke(T.AsInstance.MetaClassType, [AName, GetCurrentScope.BlockScope, FromType]);
  Result := TUserType(V.AsObject);
  GetCurrentScope.BlockScope.Add(Result);
end;

class function TTypes.CreateTypeDef(const AName: String): TTypeDef;
begin
  Result := TTypeDef.Create(AName, GetCurrentScope.BlockScope);
  GetCurrentScope.BlockScope.Add(Result);
end;

class function TTypes.CreateTypedPointer(AOfType: TUserType): TTypedPointer;
begin
  Result := TTypedPointer.Create('', GetCurrentScope.BlockScope, AOfType);
  GetCurrentScope.BlockScope.Add(Result);
end;

class function TTypes.CreateUnboundedArrayType(const AName: String;
  Kind: TArrayKind; Size: TArraySize; OfType: TUserType): TArrayType;
begin
  case Kind of
    atArray: Result := TPascalArrayType.CreateUnbounded(AName, GetCurrentScope.BlockScope, Size, OfType);
    atVector: Result := TVectorType.CreateUnbounded(AName, GetCurrentScope.BlockScope, Size, OfType);
    atList: Result := TListType.CreateUnbounded(AName, GetCurrentScope.BlockScope, Size, OfType);
  else
    raise EVarType.Create;
  end;
  GetCurrentScope.BlockScope.Add(Result);
end;

class function TTypes.CreateUnboundedArrayType(Kind: TArrayKind;
  Size: TArraySize; OfType: TUserType): TArrayType;
begin
  Result := CreateUnboundedArrayType('', Kind, Size, OfType);
end;

class function TTypes.SearchScopeForAnonTypedPointer(Scope: TScope;
  UserType: TUserType): TTypedPointer;
var Item: TScopedItem;
begin
  for Item in Scope.Items do
    if Item is TTypedPointer then
    begin
      Result := Item as TTypedPointer;
      if (Result.OfType = UserType) and (Result.Name = '') then
        EXIT;
    end;

  Result := nil;
  if not Assigned(Result) then
    Result := CreateTypedPointer(UserType);
end;

class function TTypes.SearchScopesForAnonTypedPointer(
  UserType: TUserType): TTypedPointer;
var Scope: TScope;
begin
  Scope := GetCurrentScope.BlockScope;
  while Assigned(Scope) do
  begin
    Result := SearchScopeForAnonTypedPointer(Scope, UserType);
    if Assigned(Result) then
      EXIT;
    Scope := Scope.Parent;
  end;

  Result := SearchScopeForAnonTypedPointer(SystemScope.BlockScope, UserType);
end;

{ TTypedIdentifier }

constructor TTypedIdentifier.Create(const AName: String; AScope: TScope;
  AUserType: TUserType);
begin
  inherited Create(AName, AScope);
  FUSerType := AUserType;
end;

function TTypedIdentifier.GetVarType: TVarType;
begin
  Result := UserType.VarType;
end;

procedure TTypedIdentifier.UpdateUserType(NewType: TUserType);
begin
  Assert(ClassName = 'TConst');
  FUserType := NewType;
end;

end.
