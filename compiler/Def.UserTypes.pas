unit Def.UserTypes;

interface
uses Generics.Collections, Classes,
  Def.VarTypes;

const iUnboundedArray = 0;  //For Vector.Length and List.Capacity

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
    IsSubRange: Boolean;    //True if this type is a SubRange of another enumerard type.
                            //If so, if it's a subrange of a user created type
                            //(ie. an enumartion) then OfType will point to the base type,
                            //otherwise VarType will contain the base type
    Low: Integer;           //Specified for any ordinal type. High must be > Low.
    High: Integer;          //Ditto

    EnumItems: TArray<String>;  //For enumeration *only*

    procedure Initialise;

    //Clones From *except* Name
    procedure CloneTypeData(From: PUserType);

    //Where VarType is vtEnumeration
    function EnumItemToString(Index: Integer): String;

    //Returns the name, if the type had one. Otherwise returns the DefinitionString
    function Description: String;
    function RecordDefToString: String;
    function DefinitionString: String;
    function ToString: String;

    case VarType: TVarType of
(*      vtSparseSet: (
        //BaseType
        RangeItems: PSparseRangeItems; //(Or TImmValue?)
      );
      vtRange: (
        //BaseType
      );
*)    vtArray: (
        BoundsType: PUserType;  //nil if unbounded
      );
      vtVector: (
        VecLength: Integer;  //iUnboundedArray if unbounded
      );
      vtList: (
        Capacity: Integer;  //iUnboundedArray if unbounded
      );
      vtRecord: (
        //We can't directly reference a PScope without creating a circular unit
        //reference (both to Scope, Variables and Functions) so we'll use a
        //generic pointer and hack this with typecasts(!)
        Scope: TScopeHandle;
      );
(*      vtStream: (
        //??
      );
*)      vtFunction: (
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

//Any integer numeric type, or subrange of one
function IsIntegerType(UserType: PUserType): Boolean;

function IsSignedType(UserType: PUserType): Boolean;

//If the UserType is a subtype - either a SubRange or Synonym, returns the
//BaseType. Will recurse up BaseTypes if necessary
//For all other cases returns UserType
function GetBaseType(UserType: PUserType): PUSerType;

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
//***REDACTED***
function GetTypeSize(UserType: PUserType): Integer;
function GetTypeDataSize(UserType: PUserType): Integer;
function GetTypeRegSize(UserType: PUserType): Integer;

//Where AType is an enumeration, if Ident is a member of the enumeration returns
//it's index within the enumeration, otherwise returns -1.
function IdentToEnumIndex(AType: PUserType;const Ident: String): Integer;

function CreateTypeList: PTypeList;
procedure SetCurrentTypeList(List: PTypeList);

procedure InitialiseTypes;

//Create global/system types. CurrentScope must be SystemScope
procedure CreateSystemTypes(List: PTypeList);

//Returns the UserType for a VarType
function GetSystemType(VarType: TVarType): PUserType;

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
    vtString,
    vtInt8, vtInteger, vtByte, vtWord, vtPointer,
    vtChar,
    vtSetByte, vtSetWord, vtSetMem,
    vtUnknown: ; //Nothing to do
    vtEnumeration: EnumItems := From.EnumItems;
(*    vtSparseRange: ;*)
(*    vtRange: ;*)
    vtArray:  BoundsType := From.BoundsType;
    vtVector: VecLength := From.VecLength;
    vtList:   Capacity := From.Capacity;
    vtRecord: Assert(False);  //TODO
(*    vtStream: ;*)
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
    vtChar, vtString,
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
    vtArray:
    begin
      Assert(Assigned(BoundsType));
      Assert(BoundsType.ParentType = nil);
      Assert(Assigned(OfType));
      Result := Result + 'array[' + BoundsType.Description + '] of ' + OfType.Description;
    end;
    vtUnboundArray:
    begin
      Assert(Assigned(OfType));
      Result := Result + 'array of ' + OfType.Description;
    end;
    vtVector:
    begin
      Assert(Assigned(OfType));
      Result := VarTypeToName(VarType);
      if VecLength > 0 then
        Result := Result + '[' + VecLength.ToString + ']';
      Result := Result + ' of ' + OfType.Description;
    end;
    vtList:
    begin
      Assert(Assigned(OfType));
      Result := VarTypeToName(VarType);
      if Capacity > 0 then
        Result := Result + '[' + Capacity.ToString + ']';
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

function IsIntegerType(UserType: PUserType): Boolean;
begin
  if not Assigned(UserType) then
    EXIT(False);
  Result := IsIntegerVarType(UserType.VarType);
end;

function IsSignedType(UserType: PUserType): Boolean;
begin
  Result := IsSignedVarType(UserType.VarType);
end;

function GetBaseType(UserType: PUserType): PUserType;
begin
  Result := UserType;
  if Assigned(Result) then
    while Assigned(Result.ParentType) do
      Result := Result.ParentType
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

  if IsOrdinalType(UserType.VarType) then
    Result := UserType.High-UserType.Low+1
  else
  case UserType.VarType of
(*    vtInt8, vtByte, vtChar: Result := 128;
    vtInteger, vtPointer, vtWord, vtTypedPointer: Result := 65536;
      //Or: Result := GetMaxValue(UserType.VarType) - GetMinValue(UserType.VarType) + 1;
//    vtReal  //Not enumerable
    vtBoolean: Result := 2;
//    vtFlag  //Not a true enumerable
//    vtTypeDef //Not enumerable

    //String types
//    vtString: //As vector or List (depending on base type)
//  vtWideString:   //Ditto

    //========== Types which require a full declaration to instantiate
    //User types
    vtEnumeration:  Result := Length(UserType.EnumItems);
    vtSubRange: Result := UserType.High - UserType.Low + 1;
*)//  vtSparseSet //TODO
//  vtRange     //TODO
//  vtSet,      //TODO

    //Array types
    vtArray:  Result := GetTypeItemCount(UserType.BoundsType);
//  vtUnboundArray  //No length specified in the type, so invalid query
//  vtVector: //As Array plus Length field size
//  vtList:   //As Array plus Length field size and Capacity field size
//  vtWideDynArray  //Ditto
//  vtWideList,     //Ditto

  //Other complex types
//  vtRecord    //Not enumerable
//  vtStream    //Not enumerable
//  vtFunction  //Not enumerable

  //Error/undefined
//  vtUnknown   //Not enumerable
  else
    raise Exception.Create('Enumerable type expected');
  end;
end;

//Where UserType is an array based type, returns the bytecount of the types
//meta data. Meta data is the Length and Capacity fields (if used). The returned
//value takes into account the byte-size of the fields (ie. byte, word etc).
function GetArrayMetaSize(UserType: PUserType): Integer;
begin
  Assert(Assigned(UserType));
  Assert(IsArrayType(UserType.VarType));

  case UserType.VarType of
    vtArray: Result := 0; //No meta data
    vtVector:
    begin
      Assert(UserType.VecLength <> iUnboundedArray);
      if UserType.VecLength < 256 then
        Result := 1   //One byte
      else
        Result := 2;  //One word
    end;
    vtList:
    begin
      Assert(UserType.Capacity <> iUnboundedArray);
      if UserType.Capacity < 256 then
        Result := 1*2   //Two bytes
      else
        Result := 2*2;  //Two words
    end;
  else
    Assert(False);  //Not an array type
  end;
end;

function GetRecordTypeSize(UserType: PUserType): Integer;
var Scope: PScope;
  I: Integer;
  V: PVariable;
  NewSize: Integer;
begin
  Assert(UserType.VarType = vtRecord);
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

function GetTypeSize(UserType: PUserType): Integer;
var Size: Integer;
begin
  Assert(UserType <> nil);

  case UserType.VarType of
    vtInt8, vtByte, vtChar, vtBoolean, vtEnumeration:
      Result := 1;
    vtInteger, vtWord, vtPointer, vtTypedPointer:
      Result := 2;
    vtReal: Result := iRealSize;

    //String types
    vtString,       //aka List of Char

    //User types
(*  vtSparseSet,  //Parse time only  - contains values and ranges
  vtRange,        //Run time
*)
    vtSetByte, vtSetWord, vtSetMem:
    begin
      Size := Length(UserType.OfType.EnumItems) - 1;
      Result := (Size - (Size mod 8)) div 8;
    end;

    //Array types
    vtArray:  //Element count * element size
    begin
      Assert(Assigned(UserType.BoundsType));
      Result := GetTypeItemCount(UserType.BoundsType) * GetTypeSize(UserType.OfType);
    end;
//    vtUnboundArray: Assert(False);  //Not available
    vtVector: //Meta size + Length * element size
    begin
      Assert(UserType.VecLength <> iUnboundedArray);
      Result := GetArrayMetaSize(UserType) +
        UserType.VecLength * GetTypeSize(UserType.OfType);
    end;
    vtList:   //Meta size + Capacity * element size
    begin
      Assert(UserType.Capacity <> iUnboundedArray);
      Result := GetArrayMetaSize(UserType) +
        UserType.Capacity * GetTypeSize(UserType.OfType);
    end;
//  vtWideDynArray, //Max 65535 elements
//  vtWideList,     //Max 65535 elements
     vtRecord:
      Result := GetRecordTypeSize(UserType);
//  vtStream,       //Readble or writeable sequence of bytes or chars
  vtFunction: Result := 2;   //Code as data.
  else
    Assert(False);  //Unknown type
//    Result := GetVarTypeSize(UserType.VarType);
  end;
end;

function GetTypeDataSize(UserType: PUserType): Integer;
begin
  Result := GetTypeSize(UserType);
end;

function GetTypeRegSize(UserType: PUserType): Integer;
begin
  if IsPointeredType(UTToVT(UserType)) then
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

function GetSystemType(VarType: TVarType): PUserType;
begin
  Result := SystemTypes[VarType];
end;

procedure CreateSystemTypes(List: PTypeList);
var UT: TVarType;
begin
  for UT := low(TVarType) to high(TVarType) do
    SystemTypes[UT] := nil;

  CreateEnumSystemType(List, 'Int8', vtInt8, -128, 127);
  CreateEnumSystemType(List, 'Integer', vtInteger, -32768, 32767);
  CreateEnumSystemType(List, 'Byte', vtByte, 0, 255);
  CreateEnumSystemType(List, 'Word', vtWord, 0, 65535);
  CreateEnumSystemType(List, 'Pointer', vtPointer, 0, 65535);
  CreateSystemType(List, 'Real', vtReal);
  CreateSystemType(List, 'Boolean', vtBoolean);
  CreateSystemType(List, '<Flag>', vtFlag);
  CreateEnumSystemType(List, 'Char', vtChar, 0, 255);
  CreateSystemType(List, '<TypeDef>', vtTypeDef);
  CreateSystemType(List, 'String', vtString);
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
