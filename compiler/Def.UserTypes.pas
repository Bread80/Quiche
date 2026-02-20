unit Def.UserTypes;

interface
uses Generics.Collections, Classes,
  Def.VarTypes;

type
  TScopeHandle = Pointer;
  TFunctionHandle = Pointer;

  PUserType = ^TUserType;
  TUserType = record
    Name: String;           //User declared type name
    ParentType: PUserType;  //Used for type synomyms. Ie. when this type is an exact
                            //duplicate of another type. Otherwhise nil.
    OfType: PUserType;      //Used for:
                            //SubRanges: The base type of the emuneration
                            //Sets: The element type
                            //Arrays (etc): The element type
                            //Typed pointers: the type of the pointed to data
    IsSubRange: Boolean;    //True if this type is a SubRange of another enumerated type.
                            //If so, if it's a subrange of a user created type
                            //(ie. an enumeration) then OfType will point to the base type,
                            //otherwise VarType will contain the base type
    Low: Integer;           //Specified for any ordinal type. High must be > Low.
    High: Integer;          //Ditto

    EnumItems: TArray<String>;  //For enumeration *only*

    procedure Initialise;

    //Clones From *except* Name
    procedure CloneTypeData(From: PUserType);

    //Where VarType is vtEnumeration
    function EnumItemToString(Index: Integer): String;

    //Returns True if the vtArrayType of vtChar
    function IsStringType: Boolean;
    //Returns True if the type is a vtChar or a vtArrayType of vtChar
    function IsStringableType: Boolean;

    //Returns the name, if the type had one. Otherwise returns the DefinitionString
    function Description: String;
    function RecordDefToString: String;
    function DefinitionString: String;
    function ToString: String;  //Returns the declaration

    case VarType: TVarType of
      vtArrayType: (
        ArrayDef: TArrayDef;
        BoundsType: PUserType;  //If ArrayType = atArray
        VectorLength: Integer;  //If ArrayType = atVector
        ListCapacity: Integer;  //If ArrayType = atList
      );
      vtRecord: (
        //We can't directly reference a PScope without creating a circular unit
        //reference (both to Scope, Variables and Functions) so we'll use a
        //generic pointer and hack this with typecasts(!)
        Scope: TScopeHandle;
      );
      vtFunction: (
        Func: TFunctionHandle;  //Function Template
      );
    end;

  PTypeList = ^TTypeList;
  TTypeList = record
  private
    Items: TList<PUserType>;
  public
    procedure Initialise;
    procedure Clear;

    function Add(AVarType: TVarType): PUserType;
    function AddOfType(AVarType: TVarType;AOfType: PUserType): PUserType;
    //Creates a new type as a copy of FromType with name AName.
    //The new types BaseType will be FromType
    function AddSynonym(const AName: String;AParentType: PUserType): PUserType;

    function FindByNameInScope(const AName: String): PUserType;
    //Searches enumeration types. If AName is a member of an enumeration
    //returns the type containing it
    function FindByEnumNameInScope(const AName: String;out Index: Integer): PUserType;

    //If an anonymous TypedPointer type for UserType exists return it,
    //otherwise return nil
    function FindAnonPointerForType(UserType: PUserType): PUserType;

    function ToString: String;
  end;

function UTToVT(UserType: PUserType): TVarType;

//A Register type is one where the value of the variable is stored in registers.
//A Pointered type is one where the register stores a pointer to the actual data.
//Register types are usually those whose value fits into a register and Pointered
//types are those whose values (data) are often too large to fit into a register.
//Every type is either a Pointered Type or a Register Type. nothing is both.
function IsPointeredType(UserType: PUserType): Boolean;
function IsRegisterType(UserType: PUserType): Boolean;

//Any numeric type. Not typed pointers - these can't be used in expressions
function IsNumericType(UserType: PUserType): Boolean;

//Any integer numeric type, or subrange of one
function IsIntegerType(UserType: PUserType): Boolean;

function IsSignedType(UserType: PUserType): Boolean;

//Any type which only occupies a single byte
function IsByteType(UserType: PUserType): Boolean;

//Any numeric type which only occupies two bytes
function IsWordType(UserType: PUserType): Boolean;

//Boolean, Flag etc
function IsBooleanType(UserType: PUserType): Boolean;

//An ordinal type is one with an ordered set of values with a defined first and
//last value. These are the integer numeric types, chars, booleans, enumerations
//and subranges.
function IsOrdinalType(UserType: PUserType): Boolean;

//An enumerable type is one with an ordered set of values which can be 'enumerated'
//over is sequence. This includes Ordinal types as well as array types.
function IsEnumerableType(UserType: PUserType): Boolean;

//Any arrayed type. Array, vector, list, string etc.
function IsArrayType(UserType: PUserType): Boolean;

//An unbounded array is a Vector with VecLength of iUnboundedArray or a List
//with capacity of iUnboundedArray
function IsUnboundedArray(UserType: PUserType): Boolean;

//If the UserType is a subtype - either a SubRange or Synonym, returns the
//BaseType. Will recurse up BaseTypes if necessary
//For all other cases returns UserType
function GetBaseType(UserType: PUserType): PUSerType;

//If UserType has an OfType returns it, otherwise returns UserType
//Used by array types to get the base type of the bounds type
function GetOfType(UserType: PUserType): PUserType;

//If UserType is a SubRange returns it's 'Of' type,
//otherwise returns UserType
//Intended to use with SubRanges to find the basic type
function RemoveSubRange(UserType: PUserType): PUserType;

//Returns an anonymous type which is a typed pointer to the UserType
//The anonymous type will be created if it does not exist
function GetPointerToType(UserType: PUserType): PUserType;

//For any enumerable type, returns the total number of discrete values.
//Ie. An Int8 has an ItemCount of 256 (-128..127).
function GetTypeItemCount(UserType: PUserType): Integer;

//Returns the number of bytes required for storage of the given type.
//Will return -1 if size is unknown, for example for an unbounded array
//UserType should *not* be nil. If it is results will be unpredictable.
function GetTypeDataSize(UserType: PUserType): Integer;
function GetTypeRegSize(UserType: PUserType): Integer;

//Where AType is an enumeration, if Ident is a member of the enumeration returns
//it's index within the enumeration, otherwise returns -1.
function IdentToEnumIndex(AType: PUserType;const Ident: String): Integer;

function CreateTypeList: PTypeList;
procedure SetCurrentTypeList(List: PTypeList);

//Parses a type definition string. These strings are used in library data files.
//If the definition string is for an array type (vtArrayDef) returns extra array
//data in ArrayDef. (In all other cases ArrayDef is invalid)
function StringToType(TypeString: String;out ArrayDef: TArrayDef): TVarType;

procedure InitialiseTypes;

//Create global/system types. CurrentScope must be SystemScope
procedure CreateSystemTypes(List: PTypeList);

//Returns the UserType for a VarType. If VarType is vtArrayType then ArrayDef will be
//used to find a matching type. If the relevant type does not exist it will be created.
function GetSystemType(VarType: TVarType;ArrayDef: PArrayDef = nil): PUserType;

function GetSystemStringType: PUserType;
function GetSystemStringLiteralType: PUserType;

function IdentToType(const Ident: String): PUserType;


var Types: PTypeList;

procedure TypesToStrings(S: TStrings);

implementation
uses SysUtils,
  Def.Functions, Def.Scopes, Def.Variables;
{ TUserType }

procedure TUserType.CloneTypeData(From: PUserType);
begin
  VarType := From.VarType;
  ParentType := From.ParentType;
  OfType := From.OfType;
  IsSubRange := From.IsSubRange;
  Low := From.Low;
  High := From.High;
  case VarType of
    vtReal,
    vtBoolean, vtFlag,
    vtTypeDef,
    vtInt8, vtInteger, vtByte, vtWord, vtPointer,
    vtChar,
    vtSetByte, vtSetWord, vtSetMem,
    vtUnknown: ; //Nothing to do
    vtEnumeration: EnumItems := From.EnumItems;
    vtArrayType:
    begin
      ArrayDef := From.ArrayDef;
      BoundsType := From.BoundsType;
      VectorLength := From.VectorLength;
      ListCapacity := From.ListCapacity;
    end;
    vtRecord: Assert(False);  //TODO
    vtFunction: Assert(False);  //TODO
  else
    Assert(False);
  end;
end;

function TUserType.DefinitionString: String;
var S: String;
begin
  if ParentType <> nil then
    //We're a synonym of the base type
    EXIT(ParentType.Description);

  //OfType and BoundsType must both be base types
  if Assigned(OfType) then
    Assert(OfType.ParentType = nil);

  if IsSubRange then
  begin
    if Assigned(OfType) and (OfType.VarType = vtEnumeration) then
      Result := Result + OfType.EnumItems[Low] + '(' + Low.ToString + ')..' +
        OfType.EnumItems[High] + '(' + High.ToString + ')'
    else  //TODO: Non integer types??
      Result := Result + Low.ToString + '..' + High.ToString;
  end
  else
  case VarType of
    vtInt8, vtInteger, vtByte, vtWord, vtPointer,
    vtReal,
    vtBoolean, vtFlag,
    vtTypeDef,
    vtChar,
    vtUnknown:
      //If ParentType is nil we're the base type,
      //(otherwise we'd be a synonym for the ParentType)
      Result := Name;
    vtEnumeration:
    begin
      Result := '';
      for S in EnumItems do
      begin
        if Result <> '' then
          Result := Result + ',';
        Result := Result + S;
      end;
      Result := '(' + Result + ')';
    end;
    vtSetByte, vtSetWord, vtSetMem:
    begin
      Assert(Assigned(OfType));
      Result := Result + 'set of ' + OfType.Description;
    end;
    vtArrayType:
    begin
      Assert(Assigned(OfType));

      case ArrayDef.ArraySize of
        asUnknown: ;  //Nothing
        asShort: Result := Result + 'short ';
        asLong: Result := Result + 'long ';
      else
        raise EVarType.Create;
      end;
      case ArrayDef.ArrayType of
        atArray: Result := Result + 'array';
        atVector: Result := Result + 'vector';
        atList: Result := Result + 'list';
      else
        raise EVarType.Create;
      end;

      if not ArrayDef.IsUnbounded then
        case ArrayDef.ArrayType of
          atArray:
          begin
            Assert(Assigned(BoundsType));
            Assert(BoundsType.ParentType = nil);
            Result := Result + '[' + BoundsType.Description + ']';
          end;
          atVector:
            Result := Result + '[' + VectorLength.ToString + ']';
          atList:
            Result := Result + '[' + ListCapacity.ToString + ']';
        else
          raise EVarType.Create;
        end;

      Result := Result + ' of ' + OfType.Description;
    end;
    vtRecord: Result := RecordDefToString;
    vtFunction: Result := FunctionHandleToFunction(Func).ToString;
    vtTypedPointer:
    begin
      Assert(Assigned(OfType));
      Result := Result + '^' + OfType.Description;
    end;
  else
    Assert(False);
  end;
end;

function TUserType.Description: String;
begin
  if Name <> '' then
    Result := Name
  else
    Result := DefinitionString;
end;

function TUserType.EnumItemToString(Index: Integer): String;
begin
  Assert(VarType = vtEnumeration);
  if IsSubRange then
    Result := OfType.EnumItemToString(Index)
  else if (Index >= 0) and (Index < length(EnumItems)) then
    Result := EnumItems[Index]
  else
    Result := '<Enumeration value out of range>';
end;

procedure TUserType.Initialise;
begin
  Name := '';
  ParentType := nil;
  OfType := nil;
  VarType := vtUnknown;
  BoundsType := nil;
  IsSubRange := False;
  Low := -1;
  High := -2;
end;

function TUserType.IsStringableType: Boolean;
begin
  Result := (VarType = vtChar) or IsStringType;
end;

function TUserType.IsStringType: Boolean;
begin
  Result := (VarType = vtArrayType) and (OfType.VarType = vtChar);
end;

function TUserType.RecordDefToString: String;
var
  PrevScope: PScope;
  I: Integer;
  V: PVariable;
begin
  Assert(VarType = vtRecord);
  Result := 'record'#13;

  PrevScope := GetCurrentScope;
  SetCurrentScope(ScopeHandleToScope(Scope));

  for I := 0 to Vars.GetCount-1 do
  begin
    V := Vars.IndexToData(I);
    Assert(Assigned(V.UserType));
    Result := Result + '  +' + V.Offset.ToString + ' ' +
      V.Name + ': ' + V.UserType.DefinitionString + #13;
  end;

  SetCurrentScope(PrevScope);

  Result := Result + 'end';
end;

function TUserType.ToString: String;
begin
  if Name <> '' then
    Result := Name
  else
    Result := '<anon>';
  Result := Result + ' = ' + DefinitionString;
end;

function IdentToEnumIndex(AType: PUserType;const Ident: String): Integer;
begin
  Assert(AType.VarType = vtEnumeration);
  for Result := 0 to Length(AType.EnumItems)-1 do
    if CompareText(Ident, AType.EnumItems[Result]) = 0 then
      EXIT;
  Result := -1;
end;

function UTToVT(UserType: PUserType): TVarType;
begin
  if Assigned(UserType) then
    Result := UserType.VarType
  else
    Result := vtUnknown;
end;

function IsPointeredType(UserType: PUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsPointeredVarType(UserType.VarType);
end;

function IsRegisterType(UserType: PUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsRegisterVarType(UserType.VarType);
end;

function IsNumericType(UserType: PUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsNumericVarType(UserType.VarType);
end;

function IsIntegerType(UserType: PUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsIntegerVarType(UserType.VarType);
end;

function IsSignedType(UserType: PUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsSignedVarType(UserType.VarType);
end;

function IsByteType(UserType: PUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsByteVarType(UserType.VarType);
end;

function IsWordType(UserType: PUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsWordVarType(UserType.VarType);
end;

function IsBooleanType(UserType: PUserType): Boolean;
begin
  Result := IsBooleanVarType(UserType.VarType);
end;

function IsOrdinalType(UserType: PUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsOrdinalVarType(UserType.VarType);
end;

function IsEnumerableType(UserType: PUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsEnumerableVarType(UserType.VarType);
end;

function IsArrayType(UserType: PUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsArrayVarType(UserType.VarType);
end;

function IsUnboundedArray(UserType: PUserType): Boolean;
begin
  Result := (UTToVT(UserType) = vtArrayType) and UserType.ArrayDef.IsUnbounded;
end;

function GetBaseType(UserType: PUserType): PUserType;
begin
  Result := UserType;
  if Assigned(Result) then
    while Assigned(Result.ParentType) do
      Result := Result.ParentType
end;

function GetOfType(UserType: PUserType): PUserType;
begin
  Result := UserType;
  if Assigned(Result) then
    if Result.OfType <> nil then
      Result := Result.OfType;
end;

function RemoveSubRange(UserType: PUserType): PUserType;
begin
  Result := UserType;
  if UserType = nil then
    EXIT;

  if UserType.IsSubRange then
  begin
    Result := UserType.OfType;
    Assert(Result <> nil);
  end;
end;

function GetPointerToType(UserType: PUserType): PUserType;
begin
  Result := SearchScopesForAnonTypedPointer(UserType);
  if not Assigned(Result) then
  begin
    Result := Types.AddOfType(vtTypedPointer, UserType);
  end;
end;

function GetTypeItemCount(UserType: PUserType): Integer;
begin
  Assert(UserType <> nil);

  if IsOrdinalType(UserType) then
    EXIT(UserType.High-UserType.Low+1);

  case UserType.VarType of
    //========== Types which require a full declaration to instantiate
    //User types
//  vtSet,      //TODO

    //Array types
    vtArrayType:
      if not UserType.ArrayDef.IsUnbounded then
        case UserType.arrayDef.ArrayType of
          atArray: EXIT(GetTypeItemCount(UserType.BoundsType));
          atVector: EXIT(UserType.VectorLength);
        end;

  //Other complex types
//  vtRecord    //Not enumerable
//  vtFunction  //Not enumerable

  //Error/undefined
//  vtUnknown   //Not enumerable
  end;

  raise Exception.Create('Enumerable type expected');
end;

//Where UserType is an array based type, returns the bytecount of the types
//meta data. Meta data is the Length and Capacity fields (if used). The returned
//value takes into account the byte-size of the fields (ie. byte, word etc).
function GetArrayMetaSize(UserType: PUserType): Integer;
begin
  Assert(Assigned(UserType));
  Result := UserType.ArrayDef.MetaSize;
end;

function GetRecordTypeSize(UserType: PUserType): Integer;
var Scope: PScope;
  I: Integer;
  V: PVariable;
  NewSize: Integer;
begin
  Assert(UserType.VarType = vtRecord);
  Result := 0;
  Scope := ScopeHandleToScope(UserType.Scope);
  for I := 0 to Scope.VarList.GetCount-1 do
  begin
    V := Scope.VarList.IndexToData(I);
    //V.Offset is set when we parse the VarDef. When we add variant records some
    //fields will overlap (ie each offset will not be contiguous)
    NewSize := V.Offset + GetTypeDataSize(V.UserType);
    if NewSize > Result then
      Result := NewSize;
  end;
end;

function GetTypeDataSize(UserType: PUserType): Integer;
var Size: Integer;
begin
  Assert(UserType <> nil);

  case UserType.VarType of
    vtInt8, vtByte, vtChar, vtBoolean, vtEnumeration:
      Result := 1;
    vtInteger, vtWord, vtPointer, vtTypedPointer:
      Result := 2;
    vtReal: Result := iRealSize;

    //User types
    vtSetByte, vtSetWord, vtSetMem:
    begin
      Size := GetTypeItemCount(UserType.OfType) - 1;
      Result := (Size shr 3) + 1;
    end;

    //Array types
    vtArrayType:
    begin
      Assert(not UserType.ArrayDef.IsUnbounded);
      case UserType.ArrayDef.ArrayType of
        atArray:  //Element count * element size
        begin
          Assert(Assigned(UserType.BoundsType));
          Result := GetTypeItemCount(UserType.BoundsType) * GetTypeDataSize(UserType.OfType);
        end;
        atVector: //Meta size + Length * element size
          Result := GetArrayMetaSize(UserType) +
            UserType.VectorLength * UserType.ArrayDef.ElementSize;
        atList:   //Meta size + Capacity * element size
          Result := GetArrayMetaSize(UserType) +
            UserType.ListCapacity * UserType.ArrayDef.ElementSize;
      else
        raise EVarType.Create;
      end;
    end;

    vtRecord:
      Result := GetRecordTypeSize(UserType);

    vtFunction: Result := 2;   //Code as data.
  else
    Assert(False);  //Unknown type
  end;
end;

function GetTypeRegSize(UserType: PUserType): Integer;
begin
  if IsPointeredType(UserType) then
    Result := 2
  else
    Result := GetTypeDataSize(UserType);
end;

procedure SetCurrentTypeList(List: PTypeList);
begin
  Types := List;
end;

function CreateTypeList: PTypeList;
begin
  New(Result);
  Result.Initialise;
end;

var SystemTypes: array[low(TVarType)..high(TVarType)] of PUserType;
  StringType: PUserType;        //To use for string variables
  StringLiteralType: PUserType; //To use for string literals


function StringToType(TypeString: String;out ArrayDef: TArrayDef): TVarType;
var Fields: TArray<String>;
begin
  Result := StringToVarType(TypeString);
  if Result <> vtUnknown then
    EXIT;

  if CompareText(TypeString, 'String') = 0 then
  begin
    ArrayDef := GetSystemStringType.ArrayDef;
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

function CreateSystemType(List: PTypeList;const Name: String;VarType: TVarType): PUserType;
begin
  Result := List.Add(VarType);
  Result.Name := Name;
  SystemTypes[VarType] := Result;
end;

function CreateEnumSystemType(List: PTypeList;const Name: String;VarType: TVarType;Low, High: Integer): PUserType;
begin
  Result := CreateSystemType(List, Name, VarType);
  Result.Low := Low;
  Result.High := High;
end;

function GetSystemType(VarType: TVarType;ArrayDef: PArrayDef = nil): PUserType;
begin
  if VarType <> vtArrayType then
    EXIT(SystemTypes[VarType]);

  if ArrayDef = nil then
    EXIT(nil);

  Assert(False);
end;

function GetSystemStringType: PUserType;
begin
  Result := StringType;
end;

function GetSystemStringLiteralType: PUserType;
begin
  Result := StringLiteralType;
end;

procedure CreateSystemTypes(List: PTypeList);
var VT: TVarType;
  CharType: PUserType;
begin
  for VT := low(TVarType) to high(TVarType) do
    SystemTypes[VT] := nil;

  CreateEnumSystemType(List, 'Int8', vtInt8, -128, 127);
  CreateEnumSystemType(List, 'Integer', vtInteger, -32768, 32767);
  CreateEnumSystemType(List, 'Byte', vtByte, 0, 255);
  CreateEnumSystemType(List, 'Word', vtWord, 0, 65535);
  CreateEnumSystemType(List, 'Pointer', vtPointer, 0, 65535);
  CreateSystemType(List, 'Real', vtReal);
  CreateSystemType(List, 'Boolean', vtBoolean);
  CreateSystemType(List, '<Flag>', vtFlag);
  CharType := CreateEnumSystemType(List, 'Char', vtChar, 0, 255);
  CreateSystemType(List, '<TypeDef>', vtTypeDef);
  StringType := CreateSystemType(List, 'String', vtArrayType);
  StringType.OfType := CharType;
  StringType.ArrayDef.ArrayType := atList;
  StringType.ArrayDef.ArraySize := asShort;   //TODO - use config
  StringType.ArrayDef.IsUnbounded := True;
  StringType.ArrayDef.ElementSize := 1;
  StringLiteralType := CreateSystemType(List, '<StringLiteral>', vtArrayType);
  StringLiteralType.OfType := CharType;
  StringLiteralType.ArrayDef.ArrayType := atVector; //Can't modify string literals!
  StringLiteralType.ArrayDef.ArraySize := asShort;  //TODO - use config
  StringLiteralType.ArrayDef.IsUnbounded := False;
  StringLiteralType.ArrayDef.ElementSize := 1;
end;

{ TTypeList }

function TTypeList.Add(AVarType: TVarType): PUserType;
begin
  Result := New(PUserType);
  Items.Add(Result);
  Result.Initialise;
  Result.VarType := AVarType;
end;

function TTypeList.AddOfType(AVarType: TVarType;
  AOfType: PUserType): PUserType;
begin
  Result := New(PUserType);
  Items.Add(Result);
  Result.Initialise;
  Result.VarType := AVarType;
  Result.OfType := AOfType;
end;

function TTypeList.AddSynonym(const AName: String;
  AParentType: PUserType): PUserType;
begin
  Result := New(PUserType);
  Items.Add(Result);
  Result.Initialise;
  Result.CloneTypeData(AParentType);

  Result.Name := AName;
  Result.VarType := AParentType.VarType;
  Result.ParentType := AParentType;
end;

procedure TTypeList.Clear;
var UT: PUserType;
begin
  for UT in Items do
    Dispose(UT);
end;

function TTypeList.FindByNameInScope(const AName: String): PUserType;
var UT: PUserType;
begin
  for UT in Items do
    if CompareText(UT.Name, AName) = 0 then
      EXIT(UT);

  Result := nil;
end;

function TTypeList.FindAnonPointerForType(UserType: PUserType): PUserType;
var UT: PUserType;
begin
  for UT in Items do
    if (UT.VarType = vtTypedPointer) and (UT.OfType = UserType) and (UT.Name = '') then
      EXIT(UT);

  Result := nil;
end;

function TTypeList.FindByEnumNameInScope(const AName: String;out Index: Integer): PUserType;
var I: Integer;
begin
  for I := 0 to Items.Count-1 do
  begin
    Result := Items[I];
    if Result.VarType = vtEnumeration then
    begin
      Index := IdentToEnumIndex(Result, AName);
      if Index <> -1 then
        EXIT;
    end;
  end;

  Result := nil;
  Index := -1;
end;

procedure TTypeList.Initialise;
begin
  Items := TList<PUserType>.Create;
end;

function TTypeList.ToString: String;
var Item: PUserType;
begin
  Result := '';
  for Item in Items do
    Result := Result + Item.ToString + #13;
end;

procedure InitialiseTypes;
begin
  Types := nil;
end;

function IdentToType(const Ident: String): PUserType;
var
  IdentData: TIdentData;
begin
  IdentData := GetCurrentScope.SearchAllInScope(Ident, False);
  if IdentData.IdentType = itUnknown then
    EXIT(nil);
  if IdentData.IdentType <> itType then
    EXIT(nil);
  Result := IdentData.T;
end;

procedure TypesToStrings(S: TStrings);
begin
  S.Clear;
  S.Text := Types.ToString;
end;

end.
