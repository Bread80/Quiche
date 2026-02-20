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

  //Used to store binary data (for complex types)
  TBlob = array of Byte;

//===============TImmValue

//Record to store a typed constant value. Used within ILParams and as default
//parameters within function definitions
  TImmValue = record
  public
    //AValue is converted to the appropriate type, if possible,
    //For complex types, the only acceptable value for AValue is 0, which will
    //result in a default initialisation
    constructor CreateTyped(AType: TVarType;AValue: Integer);overload;
    constructor CreateTyped(AType: PUserType;AValue: Integer);overload;

    //For generic integer values
    constructor CreateInteger(AValue: Integer);

    constructor CreateChar(AValue: Char);
    constructor CreateBoolean(AValue: Boolean);
    constructor CreateTypeDef(AValue: PUserType);

    constructor CreateString(AString: String);
    constructor CreateBlob(AType: PUserType;ABlob: TBlob);

    //Can *only* be user for integer types (both new and current)
    procedure UpdateUserType(NewType: PUserType);
//    procedure UpdateVarType(NewType: TVarType);

    function UserType: PUserType;
    function VarType: TVarType;

    function IntValue: Integer;
    function BoolValue: Boolean;
    function CharValue: Char;
    function TypeDefValue: PUserType;
    //If VarType is vtChar or a vtArrayType of vtChar
    function StringValue: String;
    //If VarType is vtList ... More TODO
    function BlobValue: TBlob;
    //If VarType is vtArrayType
    function ArrayLength: Integer;

    //For (mostly) code generation
    //Only applicable to ordinal types
    function ToInteger: Integer;

    //Where data the stored as a pointer (ie Strings etc), returns a label for the
    //constant data
    function ToLabel: String;

    //Returns a string suitable for passing to the assembler.
    //Value returned must be a single byte. Value will be masked (with $ff) if necessary
    //This routine can return chars as string literals and makae code easier to read
    //than using numeric literals
    function ToStringByte: String;
    //Returns a 16-bit value masked with $ffff
    function ToStringWord: String;

    function GetConstList: PConstList;

    //For debugging. Sometimes for code generation
    function ToString: String;
  private
    FUserType: PUserType;

    case FVarType: TVarType of
      vtInt8, vtInteger, vtByte, vtWord, vtPointer, vtEnumeration:
        (FIntValue: Integer);
      vtReal: (); //TODO
      vtBoolean, vtFlag: (FBoolValue: Boolean);
      vtChar: (FCharValue: Char);
      vtTypeDef: (FTypeDefValue: PUserType);
      vtArrayType: (
        //Is the data is a string we'll store it as a string...
        FStringConstList: PConstList;  //Scope to which the constant belongs
        FStringIndex: Integer;  //Index into string constants list for Scope
                  //(We aren't allowed to put strings in a variant section. Instead
                  //we'll store them elsewhere and put an index to them here.

        //...other we'll store the data as a blob
        FBlobConstList: PConstList;
        FBlobIndex: Integer;
        );
  end;

//==================CONST

  PConst = ^TConst;
  TConst = record
    Name: String;
    UserType: PUserType;
    InScope: Boolean;
    Depth: Integer;
    Value: TImmValue;

    function VarType: TVarType;
  end;

  TConstList = record
  private
    //A slight hack to be able to generate labels for string literal data wihout
    //requiring a Scope pointer (which woud create circular references)
    ScopeName: String;

    FItems: TList<PConst>;
    MarkPosition: Integer;
    Strings: TStringList; //String literals defined within this scope
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

    function Add(const AName: String;UType: PUserType;const AValue: TImmValue): PConst;

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
  if FStringConstList <> nil then
    Result := Length(FStringConstList.Strings[FStringIndex])
  else
    Result := (GetTypeDataSize(FUserType) - FUserType.ArrayDef.MetaSize) div FUserType.ArrayDef.ElementSize;
end;

function TImmValue.BlobValue: TBlob;
begin
  case FVarType of
    vtArrayType, vtRecord:
    begin
      Assert(Assigned(FBlobConstList));
      Assert(FBlobIndex <> -1);

      Assert(False, 'TODO: Array blobs');
      Result := FBlobConstList.Blobs[FBlobIndex];
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

constructor TImmValue.CreateBlob(AType: PUserType; ABlob: TBlob);
begin
  FVarType := UTToVT(AType);
  FUserType := AType;
  FBlobConstList := GetCurrentScope.ConstList;
  Assert(Assigned(FBlobConstList));
  FBlobIndex := FBlobConstList.Blobs.Add(ABlob);

  FStringConstList := nil;
  FStringIndex := -1;
end;

constructor TImmValue.CreateBoolean(AValue: Boolean);
begin
  FVarType := vtBoolean;
  FUserType := GetSystemType(FVarType);
  FBoolValue := AValue;
end;

constructor TImmValue.CreateChar(AValue: Char);
begin
  FVarType := vtChar;
  FUserType := GetSystemType(FVarType);
  FCharValue := AValue;
end;

constructor TImmValue.CreateInteger(AValue: Integer);
begin
  FVarType := vtInteger;
  FUserType := GetSystemType(FVarType);
  FIntValue := AValue;
end;

constructor TImmValue.CreateTyped(AType: TVarType; AValue: Integer);
begin
  FUserType := GetSystemType(AType);
  FVarType := AType;
  case AType of
    vtInt8, vtInteger, vtByte, vtWord, vtPointer,
      vtTypedPointer, vtEnumeration: FIntValue := AValue;
    vtReal: Assert(False); //TODO
    vtBoolean, vtFlag: FBoolValue:= AValue <> 0;
    vtChar: FCharValue := chr(AValue);
    vtTypeDef: FTypeDefValue := PUserType(AValue);
    vtArrayType: //Only empty arrays can be created here (useful for empty strings)
      if AValue = 0 then
      begin
        FStringConstList := nil;
        FStringIndex := -1;
        FBlobConstList := nil;
        FBlobIndex := -1;
      end
      else
        Assert(False);
  else
    Assert(False);  //NOTE: We can't do complex types here!!
  end;
end;

constructor TImmValue.CreateTyped(AType: PUserType; AValue: Integer);
begin
  Assert(Assigned(AType));
  FUserType := AType;
  FVarType := AType.VarType;
  case FVarType of
    vtInt8, vtInteger, vtByte, vtWord, vtPointer,
      vtEnumeration, vtSetByte, vtSetWord,
      vtTypedPointer, vtFunction:
      FIntValue := AValue;
    vtReal: Assert(False); //TODO
    vtBoolean, vtFlag: FBoolValue:= AValue <> 0;
    vtChar: FCharValue := chr(AValue);
    vtTypeDef: FTypeDefValue := PUserType(AValue);
    vtArrayType, vtSetMem, vtRecord:
      if AValue = 0 then
      begin
        FStringConstList := nil;
        FStringIndex := -1;
        FBlobConstList := nil;
        FBlobIndex := -1;
      end
      else
        Assert(False);
  else
    Assert(False);
  end;
end;

constructor TImmValue.CreateString(AString: String);
begin
  FUserType := GetSystemStringLiteralType;
  FVarType := FUserType.VarType;
  FStringConstList := GetCurrentScope.ConstList;
  Assert(Assigned(FStringConstList));
  FStringIndex := FStringConstList.Strings.Add(AString);

  FBlobConstList := nil;
  FBlobIndex := -1;
end;

constructor TImmValue.CreateTypeDef(AValue: PUserType);
begin
  FVarType := vtTypeDef;
  FUserType := GetSystemType(FVarType);
  FTypeDefValue := AValue;
end;

function TImmValue.GetConstList: PConstList;
begin
  if FStringConstList <> nil then
    Result := FStringConstList
  else
    Result := FBlobConstList;
end;

function TImmValue.IntValue: Integer;
begin
  Assert(IsIntegerVarType(FVarType) or IsIntegerType(FUserType));
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
      Assert(Assigned(FStringConstList));
      if FStringIndex = -1 then
        Result := ''
      else
        Result := FStringConstList.Strings[FStringIndex];
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
  if FStringConstList <> nil then
    Result := '__sl_' + FStringConstList.ScopeName.ToLower + '_string' + FStringIndex.ToString
  else
    Assert(False);
end;

function IntValueToString(AUserType: PUserType;Value: Integer): String;
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

function BlobArrayToString(const Blob: TBlob;AUserType: PUserType;Offset: Integer): String;
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

  function ToUserTypedString(AUserType: PUserType): String;
  begin
    case UTToVT(AUserType) of
      vtTypeDef:
      if TypeDefValue <> nil then
        Result := TypeDefValue.Name
      else
        Result := 'nil';
      vtArrayType:
        if (FStringConstList <> nil) then
          if FStringIndex = -1 then
            Result := '<<UNASSIGNED>>'
          else
            Result := FStringConstList.Strings[FStringIndex]
        else
          if (FBlobConstList = nil) or (FBlobIndex = -1) then
            Result := '<<UNASSIGNED>>'
          else
            Result := BlobArrayToString(FBlobConstList.Blobs[FBlobIndex], AUserType, 0)
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

function TImmValue.TypeDefValue: PUserType;
begin
  Assert(FVarType = vtTypeDef);
  Result := FTypeDefValue;
end;

procedure TImmValue.UpdateUserType(NewType: PUserType);
begin
  Assert(IsIntegerType(NewType));
  Assert(IsIntegerType(FUserType));
  FVarType := UTToVT(NewType);
  FUserType := NewType;
end;
(*
procedure TImmValue.UpdateVarType(NewType: TVarType);
begin
  Assert(Def.QTypes.IsIntegerVarType(NewType));
  Assert(Def.QTypes.IsIntegerVarType(FVarType));
  FVarType := NewType;
  FUserType := GetSystemType(FVarType);
end;
*)
function TImmValue.UserType: PUserType;
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

function TConst.VarType: TVarType;
begin
  Result := UTToVT(UserType);
end;

{ TConstList }

function TConstList.Add(const AName: String; UType: PUserType;
  const AValue: TImmValue): PConst;
var CL: PConstList;
begin
  if IsPointeredType(UType) then
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

  //If not add it
  New(Result);
  FItems.Add(Result);
  Result.Name := AName;
  Result.UserType := UType;
  Result.InScope := True;
  if GetCurrentScope <> nil then
    Result.Depth := GetCurrentScope.Depth
  else
    Result.Depth := 0;
  Result.Value := AValue;
end;

procedure TConstList.Clear;
var V: PConst;
begin
  for V in FItems do
    Dispose(V);
  FItems.Clear;
  MarkPosition := -1;
  Strings.Clear;
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

  for I := 0 to FItems.Count-1 do
    case AValue.VarType of
      vtArrayType:
        if AValue.FStringConstList = FItems[I].Value.FStringConstList then
        begin
          if AValue.FStringIndex = FItems[I].Value.FStringIndex then
            EXIT(FItems[I]);
        end
        else
        if AValue.FBlobConstList = FItems[I].Value.FBlobConstList then
        begin
          if AValue.FBlobIndex = FItems[I].Value.FBlobIndex then
            EXIT(FItems[I]);
        end;
    else
      raise Exception.Create('Unknown type');
    end;

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
  Strings := TStringList.Create;
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
begin
  List.Add('False', GetSystemType(vtBoolean), TImmValue.CreateBoolean(False));
  List.Add('True', GetSystemType(vtBoolean), TImmValue.CreateBoolean(True));
  List.Add('Maxint', GetSystemType(vtInteger), TImmValue.CreateTyped(vtInteger, GetMaxValue(vtInteger)));
  List.Add('Minint', GetSystemType(vtInteger), TImmValue.CreateTyped(vtInteger, GetMinValue(vtInteger)));
  List.Add('nil', GetSystemType(vtPointer), TImmValue.CreateTyped(vtPointer, $0000));
end;

initialization
  Consts := nil;
end.

