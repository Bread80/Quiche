(*
  Compile time constants

  Currently repurposes the code and data structures for Variables
*)
unit Def.Consts;

interface
uses
  Generics.Collections, Classes,
  Def.Scopes, Def.VarTypes, Def.UserTypes;

type
  //Used to store binary data (for complex types)
  TBlob = TArray<Byte>;

  TConst = class;

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

    //Only where existing Const is nil(??)
    procedure SetConst(AConst: TConst);

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
    FConst: TConst; //For pointered types data is stored in the Const (due to
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

  TConst = class(TTypedIdentifier)
  private
    FValue: TImmValue;

    FIsString: Boolean;
    //For arraytypes, distinguish between string literals (stored as strings) and
    //other array literals (stored as blobs)
    FStringValue: String;
    FBlobValue: TBlob;
    //Can ONLY be called from a TImmValue (ie our TImmValue!).
    //Used when the expession parser tweaks integer/numeric types or hardens RangeLists
    procedure UpdateUserType(NewType: TUserType);
  public
    constructor Create(const AName: String; AOwner: TScope; AUserType: TUserType;
      const AValue: TImmValue);
    constructor CreateBlob(const AName: String;AOwner: TScope;AUserType: TUserType;
      const AValue: TImmValue;const ABlobValue: TBlob);
    constructor CreateString(const AName: String;AOwner: TScope;AUserType: TUserType;
      const AValue: TImmValue;const AStringValue: String);

    function IdentType: TIdentType;override;

    //Where data the stored as a pointer (ie Strings etc), returns a label for the
    //constant data
    function ToLabel: String;

    function Description: String;
    function ToString: String;override;  //Returns the declaration

    //Only used where the type is a registered types
    property Value: TImmValue read FValue;
    property BlobValue: TBlob read FBlobValue;
    property IsString: Boolean read FIsString;
    property StringValue: String read FStringValue;
 end;

  TConsts = class
  public
    class function Add(const AName: String;UType: TUserType;const AValue: TImmValue): TConst;
    class function AddBlob(const AName: String; UType: TUserType;const AValue: TImmValue;
      const ABlobValue: TBlob): TConst;
    class function AddString(const AName: String;UType: TUserType;const AValue: TImmValue;
      const AStringValue: String): TConst;
  end;

//Add global/system consts to the /current/ const list (Ie current scope should be global scope)
procedure CreateSystemConsts(SysUnit: TScope);

implementation
uses
  SysUtils,
  Parse.Base,
  Def.Compiler, Def.Globals,
  CG.Data;

//----------------------TImmValue

function TImmValue.ArrayLength: Integer;
begin
  Assert(FVarType = vtArrayType);
  Assert(FConst <> nil);
  if FConst.IsString then
    Result := Length(FConst.StringValue)
  else
    Result := (FUserType.DataSize - (FUserType as TArrayType).MetaSize) div (FUserType as TArrayType).ElementSize;
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
begin
  FVarType := UTToVT(AType);
  FUserType := AType;
  //We need to set first because the Const will get a copy of us,
  //and us will fall out of scope
  FConst := nil;
  FConst := TConsts.AddBlob('', AType, Self, ABlob);
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
begin
  FVarType := vtReal;
  FUserType := GetSystemType(FVarType);
  FConst := nil;
  FRealValue := AValue;
  //We need to set first because the Const will get a copy of us,
  //and us will fall out of scope
  FConst := TConsts.AddBlob('', FUserType, Self, DoubleToRealBinary(FRealValue));
end;

constructor TImmValue.CreateReal(AValue: TRealBinary);
begin
  FVarType := vtReal;
  FUserType := GetSystemType(FVarType);
  FConst := nil;
  FRealValue := RealBinaryToDouble(AValue);
end;

constructor TImmValue.CreateString(AString: String);
begin
  FUserType := GetSystemStringLiteralType;
  FVarType := FUserType.VarType;
  //We need to set first because the Const will get a copy of us,
  //and us will fall out of scope
  FConst := nil;
  FConst := TConsts.AddString('', GetSystemStringType, Self, AString);
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
    Result := (FConst.UserType as TArrayType).MetaSize + Length(FConst.StringValue)
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

procedure TImmValue.SetConst(AConst: TConst);
begin
  Assert(FConst = nil);
  FConst := AConst;
end;

function TImmValue.StringValue: String;
begin
  case FVarType of
    vtChar: Result := CharValue;
    vtEnumeration:
    begin
      Assert(Assigned(FUserType));
      Result := (UserType as TEnumeration).EnumItemToString(FIntValue);
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
      Result := (AUserType as TEnumeration).EnumItemToString(Value) + '(' + Value.ToString + ')';
    end;
  else
    Assert(False);
  end;
end;

function BlobArrayToString(const Blob: TBlob;AUserType: TArrayType;Offset: Integer): String;
var ElementSize: Integer;
  Count: Integer;
  I: Integer;
  Value: Integer;
begin
  Assert(AUserType.VarType = vtArrayType);
  ElementSize := AUserType.OfType.DataSize;
  case AUserType.ArrayKind of
    atArray:
    begin
      Count := (AUserType as TPascalArrayType).BoundsType.High-(AUserType as TPascalArrayType).BoundsType.Low+1;
      Offset := 0;
    end;
    atVector:
      case AUserType.ArraySize of
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
      case AUserType.ArraySize of
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
            Value := Value or (-1 xor Integer($ff));
      end;
      2:
      begin
        Value := Blob[Offset] + (Blob[Offset+1] shl 8);
        if IsSignedType(AUserType.OfType) then
          //SignExtend
          if Value and $8000 <> 0 then
            Value := Value or (-1 xor Integer(iCPUWordMask));
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
            Result := BlobArrayToString(FConst.BlobValue, AUserType as TArrayType, 0);
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
    FConst.UpdateUserType(NewType);
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

{ TConst }

constructor TConst.Create(const AName: String; AOwner: TScope;
  AUserType: TUserType;const AValue: TImmValue);
begin
  inherited Create(AName, AOwner, AUserType);
  FValue := AValue;
  Value.SetConst(Self);
  FIsString := False;
end;

constructor TConst.CreateBlob(const AName: String; AOwner: TScope;
  AUserType: TUserType; const AValue: TImmValue; const ABlobValue: TBlob);
begin
  Create(AName, AOwner, AUserType, AValue);
  FBlobValue := ABlobValue;
  FIsString := False;
end;

constructor TConst.CreateString(const AName: String; AOwner: TScope;
  AUserType: TUserType; const AValue: TImmValue; const AStringValue: String);
begin
  Create(AName, AOwner, AUserType, AValue);
  FStringValue := AStringValue;
  FIsString := True;
end;

function TConst.Description: String;
begin
  Result := UserType.Description + ' = ' + Value.ToString;
end;

function TConst.IdentType: TIdentType;
begin
  Result := itConst;
end;

function TConst.ToLabel: String;
var
  Prefix: String;
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

  Result := '__' + Prefix.Chars[0] + 'l_' + Owner.Name.ToLower + '_';
  if Name <> '' then
    Result := Result + Name
  else
    Result := Result + Prefix + Integer(Self).ToString;
end;

function TConst.ToString: String;
begin
  Result := 'const ';
  if Name <> '' then
    Result := Result + Name
   else
    Result := Result + '<anon>';
   Result := Result + ': ' + Description;
end;

procedure TConst.UpdateUserType(NewType: TUserType);
begin
  inherited UpdateUserType(NewType);
end;

{ TConsts }

class function TConsts.Add(const AName: String; UType: TUserType;const AValue: TImmValue): TConst;
var Scope: TScope;
begin
  Scope := ParseData.ParseScope;

  //Expression parser will return a TImmValue with an unnamed TConst. Handle the
  //case where that value is being assigned to a CONST
  if AValue.FConst <> nil then
  begin
    if AValue.FConst.Name = '' then
    begin
      AValue.FConst.AssignName(AName);
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

  Result := TConst.Create(AName, Scope, UType, AValue);
  Scope.Add(Result);
end;

class function TConsts.AddBlob(const AName: String; UType: TUserType;
  const AValue: TImmValue; const ABlobValue: TBlob): TConst;
var Scope: TScope;
begin
  Scope := ParseData.ParseScope;
  Result := TConst.CreateBlob(AName, Scope, UType, AValue, ABlobValue);
  Scope.Add(Result);
end;

class function TConsts.AddString(const AName: String; UType: TUserType;
  const AValue: TImmValue; const AStringValue: String): TConst;
var Scope: TScope;
begin
  Scope := ParseData.ParseScope;
  Result := TConst.CreateString(AName, Scope, UType, AValue, AStringValue);
  Scope.Add(Result);
end;

procedure CreateSystemConsts(SysUnit: TScope);
begin
  Assert(SysUnit is TUnit);
  Assert(ParseData.ILScope = SysUnit);
  ParseData.OpenILScope(TUnit(SysUnit));
  //These are now created as EnumItems by the type system
(*  TConsts.Add('False', GetSystemType(vtBoolean), TImmValue.CreateBoolean(False));
  TConsts.Add('True', GetSystemType(vtBoolean), TImmValue.CreateBoolean(True));
*)  TConsts.Add('Maxint', GetSystemType(vtInteger), TImmValue.CreateTyped(vtInteger, GetMaxValue(vtInteger)));
  TConsts.Add('Minint', GetSystemType(vtInteger), TImmValue.CreateTyped(vtInteger, GetMinValue(vtInteger)));
  TConsts.Add('nil', GetSystemType(vtPointer), TImmValue.CreateTyped(vtPointer, $0000));
  ParseData.CloseILScope(TUnit(SysUnit));
end;

initialization
end.

