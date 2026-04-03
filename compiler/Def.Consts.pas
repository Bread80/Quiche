(*
  Compile time constants

  Currently repurposes the code and data structures for Variables
*)
unit Def.Consts;

interface
uses
  Generics.Collections, Classes,
  Def.VarTypes, Def.UserTypes;

type PConstList = ^TConstList;
  PConst = ^TConst;

  //Used to store binary data (for complex types)
  TBlob = TArray<Byte>;

//===============TImmValue

//Record to store a typed constant value. Used within ILParams and as default
//parameters within function definitions
  PImmValue = ^TImmValue;
  TImmValue = record
  public
    //AValue is converted to the appropriate type, if possible,
    //For complex types, the only acceptable value for AValue is 0, which will
    //result in a default initialisation
    constructor CreateTyped(AType: TVarType;AValue: Integer);overload;
    constructor CreateTyped(AType: TUserType;AValue: Integer);overload;

    //For generic integer values
    constructor CreateInteger(AValue: Integer);

    constructor CreateChar(AValue: Char);
    constructor CreateBoolean(AValue: Boolean);
    constructor CreateTypeDef(AValue: TUserType);

    //Pointered types
    constructor CreateReal(AValue: Double);overload;
    constructor CreateReal(AValue: TRealBinary);overload;
    constructor CreateString(AString: String);
    constructor CreateBlob(AType: TUserType;ABlob: TBlob);

    //Can *only* be user for integer types (both new and current)
    procedure UpdateUserType(NewType: TUserType);

    function UserType: TUserType;
    function VarType: TVarType;

    //Type must be integer numeric. Use ToInteger for other ordinal types
    function IntValue: Integer;
    //Type must be numeric. Integers will be auto-converted
    function RealValue: Double;
    //Type must be numeric. Integers will be auto-converted
    function RealBinaryValue: TBlob;
    function BoolValue: Boolean;
    function CharValue: Char;
    function TypeDefValue: TUserType;
    //If VarType is vtChar or a vtArrayType of vtChar
    function StringValue: String;
    //If VarType is vtList ... More TODO
    function BlobValue: TBlob;
    //If VarType is vtArrayType
    function ArrayLength: Integer;
    //Valuable for Pointered types, especially arrays
    function DataByteSize: Integer;

    //For (mostly) code generation
    //Only applicable to ordinal types
    function ToInteger: Integer;

    //Returns a string suitable for passing to the assembler.
    //Value returned must be a single byte. Value will be masked (with $ff) if necessary
    //This routine can return chars as string literals and makae code easier to read
    //than using numeric literals
    function ToStringByte: String;
    //Returns a 16-bit value masked with $ffff
    function ToStringWord: String;

    //Where data the stored as a pointer (ie Strings etc), returns a label for the
    //constant data
    function ToLabel: String;

    //For debugging. Sometimes for code generation
    function ToString: String;
  private
    FUserType: TUserType;
    FConst: PConst; //For pointered types data is stored in the Const (due to
                    //Delphi requring such types to be finalized).
                    //NOTE: For null strings and empty arrays FConst will be nil!

    case FVarType: TVarType of
      vtInt8, vtInteger, vtByte, vtWord, vtPointer, vtEnumeration:
        (FIntValue: Integer);
      vtReal: (FRealValue: Double);
      vtBoolean, vtFlag: (FBoolValue: Boolean);
      vtChar: (FCharValue: Char);
      vtTypeDef: (FTypeDefValue: TUserType);
      vtArrayType: ( //All data is stored in FConst. String data is stored in strings. Other data is in Blobs
        );
  end;

//==================CONST

  TConst = record
    ConstList: PConstList;
    Name: String;
    UserType: TUserType;
    InScope: Boolean;
    Depth: Integer;

    //Only for registered types
    Value: TImmValue;
    //For arraytypes, distinguish between string literals (stored as strings) and
    //other array literals (stored as blobs)
    IsString: Boolean;
    StringValue: String;
    BlobValue: TBlob;

    //Where data the stored as a pointer (ie Strings etc), returns a label for the
    //constant data
    function ToLabel: String;
    function VarType: TVarType;
  end;

  TConstList = record
  private
    //A slight hack to be able to generate labels for string literal data wihout
    //requiring a Scope pointer (which woud create circular references)
    ScopeName: String;

    FItems: TList<PConst>;
    MarkPosition: Integer;
//    Strings: TStringList; //String literals defined within this scope
    Blobs: TList<TBlob>;
    function GetItems(Index: Integer): PConst;
    function GetCount: Integer;  //Binary literals defined within this scope
                          //(for any complex type other than strings)
  public
    procedure Initialise;
    procedure Clear;
    //Scope depth has been DECrememented. Any in scope constants with higher scope depth
    //need to go out of scope
    procedure ScopeDepthDecced(NewDepth: Integer);

    function Add(const AName: String;UType: TUserType;const AValue: TImmValue): PConst;
    function AddString(const AName: String;UType: TUserType;const AValue: TImmValue;
      const AStringValue: String): PConst;
    function AddBlob(const AName: String;UType: TUserType;const AValue: TImmValue;
      const ABlobValue: TBlob): PConst;

    //ONLY for Pointered Types. Finds a pre-existing definition of the same string data.
    //Searches the current ConstList as well as AValue's Const List.
    //Returns nil if nothing found.
    function FindDupValueInScope(const AValue: TImmValue): PConst;
    function FindByNameInScope(const AName: String): PConst;

    property Items[Index: Integer]: PConst read GetItems;default;
    property Count: Integer read GetCount;
  end;

//Currently scoped list of constants
var Consts: PConstList;

procedure InitialiseConsts;

//------------Scope related

//These routines are called by the Scopes routines to create, clear and set Scopes
function CreateConstList(const ScopeName: String): PConstList;
procedure SetCurrentConstList(List: PConstList);

//Add global/system consts to the /current/ const list (Ie current scope should be global scope)
procedure CreateSystemConsts(List: PConstList);

implementation
uses
  SysUtils,
  Def.Globals, Def.Scopes,
  CG.Data;

//----------------------TImmValue

function TImmValue.ArrayLength: Integer;
begin
  Assert(FVarType = vtArrayType);
  Assert(FConst <> nil);
  if FConst.IsString then
    Result := Length(FConst.StringValue)
  else
    Result := (GetTypeDataSize(FUserType) - FUserType.ArrayDef.MetaSize) div FUserType.ArrayDef.ElementSize;
end;

function TImmValue.BlobValue: TBlob;
begin
  case FVarType of
    vtArrayType, vtRecord:
    begin
      Assert(Assigned(FConst));
      Assert(not FConst.IsString);
      Result := FConst.BlobValue;
    end;
  else
    Assert(False);
  end;
end;

function TImmValue.BoolValue: Boolean;
begin
  Assert(FVarType = vtBoolean);
  Result := FBoolValue;
end;

function TImmValue.CharValue: Char;
begin
  Assert(FVarType = vtChar);
  Result := FCharValue;
end;

constructor TImmValue.CreateBlob(AType: TUserType; ABlob: TBlob);
var ConstList: PConstList;
begin
  FVarType := UTToVT(AType);
  FUserType := AType;
  FConst := nil;
  ConstList := GetCurrentScope.ConstList;
  Assert(Assigned(ConstList));
  //We need to set first because the Const will get a copy of us,
  //and us will fall out of scope
  FConst := nil;
  FConst := ConstList.AddBlob('', AType, Self, ABlob);
end;

constructor TImmValue.CreateBoolean(AValue: Boolean);
begin
  FVarType := vtBoolean;
  FUserType := GetSystemType(FVarType);
  FBoolValue := AValue;
  FConst := nil;
end;

constructor TImmValue.CreateChar(AValue: Char);
begin
  FVarType := vtChar;
  FUserType := GetSystemType(FVarType);
  FCharValue := AValue;
  FConst := nil;
end;

constructor TImmValue.CreateInteger(AValue: Integer);
begin
  FVarType := vtInteger;
  FUserType := GetSystemType(FVarType);
  FIntValue := AValue;
  FConst := nil;
end;

constructor TImmValue.CreateReal(AValue: Double);
var ConstList: PConstList;
begin
  FVarType := vtReal;
  FUserType := GetSystemType(FVarType);
  FConst := nil;
  FRealValue := AValue;
  ConstList := GetCurrentScope.ConstList;
  Assert(Assigned(ConstList));
  //We need to set first because the Const will get a copy of us,
  //and us will fall out of scope
  FConst := ConstList.AddBlob('', FUserType, Self, DoubleToRealBinary(FRealValue));
end;

constructor TImmValue.CreateReal(AValue: TRealBinary);
var RB: TRealBinary;
begin
  FVarType := vtReal;
  FUserType := GetSystemType(FVarType);
  FConst := nil;
  FRealValue := RealBinaryToDouble(AValue);
end;

constructor TImmValue.CreateString(AString: String);
var ConstList: PConstList;
begin
  FUserType := GetSystemStringLiteralType;
  FVarType := FUserType.VarType;
  ConstList := GetCurrentScope.ConstList;
  Assert(Assigned(ConstList));
  //We need to set first because the Const will get a copy of us,
  //and us will fall out of scope
  FConst := nil;
  FConst := ConstList.AddString('', GetSystemStringType, Self, AString);
end;

constructor TImmValue.CreateTyped(AType: TVarType; AValue: Integer);
begin
  FUserType := GetSystemType(AType);
  FVarType := AType;
  FConst := nil;
  case AType of
    vtInt8, vtInteger, vtByte, vtWord, vtPointer, vtTypedPointer, vtEnumeration:
      FIntValue := AValue;
    vtReal:
      CreateReal(0);
    vtBoolean, vtFlag:
      FBoolValue:= AValue <> 0;
    vtChar:
      FCharValue := chr(AValue);
    vtTypeDef:
      FTypeDefValue := TUserType(AValue);
    vtArrayType: //Only empty arrays can be created here (useful for empty strings)
      Assert(AValue = 0);
  else
    Assert(False);  //NOTE: We can't do complex types here!!
  end;
end;

constructor TImmValue.CreateTyped(AType: TUserType; AValue: Integer);
begin
  Assert(Assigned(AType));
  FUserType := AType;
  FVarType := AType.VarType;
  FConst := nil;
  case FVarType of
    vtInt8, vtInteger, vtByte, vtWord, vtPointer,
      vtEnumeration, vtSetByte, vtSetWord,
      vtTypedPointer, vtFunction:
      FIntValue := AValue;
    vtBoolean, vtFlag: FBoolValue:= AValue <> 0;
    vtChar: FCharValue := chr(AValue);
    vtTypeDef: FTypeDefValue := TUserType(AValue);

    //Pointered types. AValue must be 0
    vtReal, vtArrayType, vtSetMem, vtRecord:
    begin
      Assert(AValue = 0);
      FRealValue := 0;
    end;
  else
    Assert(False);
  end;
end;

constructor TImmValue.CreateTypeDef(AValue: TUserType);
begin
  FVarType := vtTypeDef;
  FUserType := GetSystemType(FVarType);
  FTypeDefValue := AValue;
  FConst := nil;
end;

function TImmValue.DataByteSize: Integer;
begin
  Assert(FConst <> nil);

  if FConst.IsString then
    Result := FConst.UserType.ArrayDef.MetaSize + Length(FConst.StringValue)
  else
    Result := Length(FConst.BlobValue);
end;

function TImmValue.IntValue: Integer;
begin
  Assert(IsIntegerVarType(FVarType) or IsIntegerType(FUserType));
  Result := FIntValue;
end;

function TImmValue.RealBinaryValue: TBlob;
begin
  Assert(VarType = vtReal);
  Assert(FConst <> nil);
  Result := FConst.BlobValue;
end;

function TImmValue.RealValue: Double;
begin
  Assert(IsNumericVarType(FVarType));
  if VarType = vtReal then
    Result := FRealValue
  else
    Result := FIntValue;
end;

function TImmValue.StringValue: String;
begin
  case FVarType of
    vtChar: Result := CharValue;
    vtEnumeration:
    begin
      Assert(Assigned(FUserType));
      Result := UserType.EnumItemToString(FIntValue);
    end;
    vtArrayType:
    begin
      if FConst = nil then
        Result := ''
      else
      begin
        Assert(FConst.IsString);
        Result := FConst.StringValue;
      end;
    end;
    else
    Assert(False);
  end;
end;

function TImmValue.ToInteger: Integer;
begin
  case FVarType of
    vtInt8, vtInteger, vtByte, vtWord, vtPointer, vtTypedPointer,
      vtEnumeration:
        Result := FIntValue;
    vtBoolean:
      if BoolValue then
        Result := valueTrue
      else
        Result := valueFalse;
    vtChar: Result := ord(CharValue);
    vtTypeDef: Result := Integer(TypeDefValue);
  else
    Assert(False);
    Result := 0;
  end;
end;

function TImmValue.ToLabel: String;
begin
  Assert(FConst <> nil);
  Result := FConst.ToLabel;
end;

function IntValueToString(AUserType: TUserType;Value: Integer): String;
begin
  case UTToVT(AUserType) of
    vtByte: Result := '$' + IntToHex(Value, 2);
    vtWord, vtPointer, vtTypedPointer:
      Result := '$' + IntToHex(Value, 4);
    vtInt8, vtInteger: Result := Value.ToString;
    vtBoolean:
      if Value = ValueFalse then
        Result := 'False'
      else
        Result := 'True';
    vtChar:
      if Value in [32..126] then
        if Value = ord('''') then
          Result := ''''''
        else
          Result := ''''+Chr(Value)+''''
      else
        Result := '#' + Value.ToString;
    vtEnumeration:
    begin
      Assert(Assigned(AUserType));
      Result := AUserType.EnumItemToString(Value) + '(' + Value.ToString + ')';
    end;
  else
    Assert(False);
  end;
end;

function BlobArrayToString(const Blob: TBlob;AUserType: TUserType;Offset: Integer): String;
var ElementSize: Integer;
  Count: Integer;
  I: Integer;
  Value: Integer;
begin
  Assert(AUserType.VarType = vtArrayType);
  ElementSize := GetTypeDataSize(AUserType.OfType);
  case AUserType.ArrayDef.ArrayType of
    atArray:
    begin
      Count := AUserType.BoundsType.High-AUserType.BoundsType.Low+1;
      Offset := 0;
    end;
    atVector:
      case AUserType.ArrayDef.ArraySize of
        asShort:
        begin //Length byte
          Count := Blob[0];
          Offset := 1;
        end;
        asLong:
        begin //Length word
          Count := Blob[0] + Blob[1] shl 8;
          Offset := 2;
        end;
      else
        raise EVarType.Create;
      end;
    atList:
      case AUserType.ArrayDef.ArraySize of
        asShort:
        begin //Capacity and length bytes
          Count := Blob[1];
          Offset := 2;
        end;
        asLong:
        begin //Capacity and length words
          Count := Blob[2] + Blob[3] shl 8;
          Offset := 4;
        end;
      else
        raise EVarType.Create;
      end;
  else
    raise EVarType.Create;
  end;

  Result := '[';
  for I := 0 to Count-1 do
  begin
    case ElementSize of
      1:
      begin
        Value := Blob[Offset];
        if IsSignedType(AUserType.OfType) then
          //SignExtend
          if Value and $80 <> 0 then
            Value := Value or (-1 xor $ff);
      end;
      2:
      begin
        Value := Blob[Offset] + (Blob[Offset+1] shl 8);
        if IsSignedType(AUserType.OfType) then
          //SignExtend
          if Value and $8000 <> 0 then
            Value := Value or (-1 xor $ffff);
      end;
    else
      Assert(False);
    end;
    Result := Result + IntValueToString(AUserType.OfType, Value);
    if I < Count-1 then
      Result := Result + ',';

    inc(Offset, ElementSize);
  end;
  Result := Result + ']';
end;

function TImmValue.ToString: String;

  function ToUserTypedString(AUserType: TUserType): String;
  begin
    case UTToVT(AUserType) of
      vtReal:
        Result := FloatToStr(FRealValue);
      vtTypeDef:
      if TypeDefValue <> nil then
        Result := TypeDefValue.Name
      else
        Result := 'nil';
      vtArrayType:
        if FConst = nil then
          Result := '<UNASSIGNED>'
        else
          if FConst.IsString then
            Result := FConst.StringValue
          else
            Result := BlobArrayToString(FConst.BlobValue, AUserType, 0);
    else
      Result := IntValueToString(AUserType, ToInteger);
    end;
  end;

begin
  if Assigned(FUserType) then
    Result := ToUserTypedString(FUserType)
  else
    Assert(False);
//    Result := ToVarTypedString(FVarType);
end;

function TImmValue.ToStringByte: String;
begin
  case FVarType of
    vtBoolean: Result := ByteToStr(ToInteger);
    vtChar:
      if CharInSet(CharValue, [#32..#127]) then
        Result := '''' + CharValue + ''''
      else
        Result := ByteToStr(ToInteger);
  else
    Result := ByteToStr(ToInteger);
  end;
end;

function TImmValue.ToStringWord: String;
begin
  Result := WordToStr(ToInteger);
end;

function TImmValue.TypeDefValue: TUserType;
begin
  Assert(FVarType = vtTypeDef);
  Result := FTypeDefValue;
end;

procedure TImmValue.UpdateUserType(NewType: TUserType);
begin
  Assert(IsIntegerType(NewType) = IsIntegerType(FUserType));
  if FUserType = NewType then
    EXIT;

  FVarType := UTToVT(NewType);
  FUserType := NewType;
  if Assigned(FConst) then
  begin
    FConst.UserType := NewType;
    FConst.Value.UpdateUserType(NewType);
  end;
end;

function TImmValue.UserType: TUserType;
begin
  Result := FUserType;
end;

function TImmValue.VarType: TVarType;
begin
(*  if UserType = nil then
    Result := vtUnknown
  else
*)    Result := FVarType;
end;

//================SCOPE RELATED
(*
var
  Consts: TConstList;
  ConstMarkPosition: Integer;
*)
procedure InitialiseConsts;
begin
  Consts := nil;
//  ConstMarkPosition := -1;
end;

function CreateConstList(const ScopeName: String): PConstList;
begin
  New(Result);
  Result.Initialise;
  Result.ScopeName := ScopeName;
//  ConstMarkPosition := -1;
end;

procedure SetCurrentConstList(List: PConstList);
begin
  Consts := List;
end;


{ TConst }

function TConst.ToLabel: String;
var
  Prefix: String;
  NamePrefix: String;

  ScopeName: String;
  LName: String;
begin
  case VarType of
    vtReal:
      Prefix := 'real';
    vtArrayType:
      if IsString then
        Prefix := 'string'
      else
        Prefix := 'array';
  else
    Assert(False);
  end;

  Result := '__' + Prefix.Chars[0] + 'l_' + ConstList.ScopeName.ToLower + '_';
  if Name <> '' then
    Result := Result + Name
  else
    Result := Result + Prefix + Integer(@Self).ToString;
end;

function TConst.VarType: TVarType;
begin
  Result := UTToVT(UserType);
end;

{ TConstList }

function TConstList.Add(const AName: String; UType: TUserType;const AValue: TImmValue): PConst;
var CL: PConstList;
begin
  //Expression parser will return a TImmValue with an unnamed TConst. Handle the
  //case where that value is being assigned to a CONST
  if AValue.FConst <> nil then
  begin
    if AValue.FConst.Name = '' then
    begin
      AValue.FConst.Name := AName;
      AValue.UpdateUserType(UType);
      EXIT(AValue.FConst);
    end;
    if (AName = '') and (UType = AValue.FConst.UserType) then
      EXIT(AValue.FConst);

  end;


(*  TODO: REWRITE
  if IsPointeredType(UType) and (UType.VarType <> vtReal) then
  begin
    //Do we already have this Value?
    Result := FindDupValueInScope(AValue);
    if Result <> nil then
      EXIT;

    //Is this value already in the Values ConstList?
    CL := AValue.GetConstList;
    if (CL <> nil) and (CL <> @Self) then
      Result := CL.Add(AName, UType, AValue);
    if Result <> nil then
      EXIT;
  end;
*)

  //If not add it
  New(Result);
  FItems.Add(Result);
  Result.ConstList := @Self;
  Result.Name := AName;
  Result.UserType := UType;
  Result.InScope := True;
  if GetCurrentScope <> nil then
    Result.Depth := GetCurrentScope.Depth
  else
    Result.Depth := 0;
  Result.Value := AValue;
  Result.Value.FConst := Result;
  Result.IsString := False;
end;

function TConstList.AddBlob(const AName: String; UType: TUserType;
  const AValue: TImmValue; const ABlobValue: TBlob): PConst;
begin
  Result := Add(AName, UType, AValue);
  Result.IsString := False;
  Result.BlobValue := ABlobValue;
end;

function TConstList.AddString(const AName: String; UType: TUserType;
  const AValue: TImmValue;const AStringValue: String): PConst;
begin
  Result := Add(AName, UType, AValue);
  Result.IsString := True;
  Result.StringValue := AStringValue;
end;

procedure TConstList.Clear;
var V: PConst;
begin
  for V in FItems do
    Dispose(V);
  FItems.Clear;
  MarkPosition := -1;
  Blobs.Clear;
end;

function TConstList.FindByNameInScope(const AName: String): PConst;
var I: Integer;
begin
  for I := 0 to FItems.Count-1 do
    if (CompareText(Items[I].Name, AName) = 0) and Items[I].InScope then
      EXIT(Items[I]);

  Result := nil;
end;

function TConstList.FindDupValueInScope(const AValue: TImmValue): PConst;
var I: Integer;
begin
  Assert(IsPointeredType(AValue.UserType));
(*  //TODO: Rewrite
  for I := 0 to FItems.Count-1 do
    case AValue.VarType of
      vtArrayType:
        if (AValue.FIsString) and (AValue.FConst <> nil) then
        begin
          if AValue.FStringIndex = FItems[I].Value.FStringIndex then
            EXIT(FItems[I]);
        end
        else
        if (AValue.FBlobConstList <> nil) and (AValue.FBlobConstList = FItems[I].Value.FBlobConstList) then
        begin
          if AValue.FBlobIndex = FItems[I].Value.FBlobIndex then
            EXIT(FItems[I]);
        end;
    else
      raise Exception.Create('Unknown type');
    end;
*)
  Result := nil;
end;

function TConstList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TConstList.GetItems(Index: Integer): PConst;
begin
  Result := FItems[Index];
end;

procedure TConstList.Initialise;
begin
  FItems := TList<PConst>.Create;
  MarkPosition := -1;
  Blobs := TList<TBlob>.Create;
end;

procedure TConstList.ScopeDepthDecced(NewDepth: Integer);
var I: Integer;
begin
  I := FItems.Count-1;
  while (I >= 0) and (Items[I].Depth > NewDepth) do
  begin
    Items[I].InScope := False;
    dec(I);
  end;
end;



procedure CreateSystemConsts(List: PConstList);
var SystemFalse: TImmValue;
  SystemTrue: TImmValue;
  SystemMaxint: TImmValue;
  SystemMinint: TImmValue;
  SystemNil: TImmValue;
begin
  SystemFalse.CreateBoolean(False);
  SystemTrue.CreateBoolean(True);
  SystemMaxint.CreateTyped(vtInteger, GetMaxValue(vtInteger));
  SystemMinint.CreateTyped(vtInteger, GetMinValue(vtInteger));
  SystemNil.CreateTyped(vtPointer, $0000);

  List.Add('False', GetSystemType(vtBoolean), SystemFalse);
  List.Add('True', GetSystemType(vtBoolean), SystemTrue);
  List.Add('Maxint', GetSystemType(vtInteger), SystemMaxint);
  List.Add('Minint', GetSystemType(vtInteger), SystemMinint);
  List.Add('nil', GetSystemType(vtPointer), SystemNil);
end;

initialization
  Consts := nil;
end.

