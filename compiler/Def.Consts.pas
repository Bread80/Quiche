(*
  Compile time constants

  Currently repurposes the code and data structures for Variables
*)
unit Def.Consts;

interface
uses
  Generics.Collections, Classes,
  Def.QTypes, Def.UserTypes;

type PConstList = ^TConstList;

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
    constructor CreateEnumItem(AUserType: PUserType;Index: Integer);
    constructor CreateTypeDef(AValue: PUserType);

    constructor CreateString(AString: String);

    //Can *only* be user for integer types (both new and current)
    procedure UpdateUserType(NewType: PUserType);
//    procedure UpdateVarType(NewType: TVarType);

    function UserType: PUserType;
    function VarType: TVarType;

    function IntValue: Integer;
    function BoolValue: Boolean;
    function CharValue: Char;
    function TypeDefValue: PUserType;
    //If VarType is vtString or vChar
    function StringValue: String;
    //If VarType is vtList ... More TODO
    function BlobValue: String;

    //For (mostly) code generation
    //Only applicable to enumerated types
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

    //For debugging. Sometimes for code generation
    function ToString: String;
  private
    FUserType: PUserType;

    case FVarType: TVarType of
      vtInt8, vtInteger, vtByte, vtWord, vtPointer, vtEnumeration, vtSubRange:
        (FIntValue: Integer);
      vtReal: (); //TODO
      vtBoolean, vtFlag: (FBoolValue: Boolean);
      vtChar: (FCharValue: Char);
      vtTypeDef: (FTypeDefValue: PUserType);
      vtString: (
        FStringConstList: PConstList;  //Scope to which the constant belongs
        FStringIndex: Integer); //Index into string constants list for Scope
                  //(We aren't allowed to put strings in a variant section. Instead
                  //we'll store them elsewhere and put an index to them here.
      vtList: (
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
  end;

  TConstList = record
  private
    //A slight hack to be able to generate labels for string literal data wihout
    //requiring a Scope pointer (which woud create circular references)
    ScopeName: String;

    Items: TList<PConst>;
    MarkPosition: Integer;
    Strings: TStringList; //String literals defined within this scope
  public
    procedure Initialise;
    procedure Clear;
    //Scope depth has been DECrememented. Any in scope constants with higher scope depth
    //need to go out of scope
    procedure ScopeDepthDecced(NewDepth: Integer);

    function Add(const AName: String;UType: PUserType;const AValue: TImmValue): PConst;
    function FindByNameInScope(const AName: String): PConst;
  end;

//Currently scoped list of constants
var Consts: PConstList;

procedure InitialiseConsts;

//------------Scope related

//These routines are called by the Scopes routines to create, clear and set Scopes
function CreateConstList(const ScopeName: String): PConstList;
procedure SetCurrentConstList(List: PConstList);

//Add global/system consts to the /current/ const list (Ie current scope should be global scope)
procedure SetSystemConsts;

implementation
uses
  SysUtils,
  Def.Globals, Def.Scopes,
  CG.Data;

//----------------------TImmValue

function TImmValue.BlobValue: String;
begin
  case FVarType of
    vtList:
    begin
      Assert(Assigned(FBlobConstList));
      Assert(FBlobIndex <> -1);

      Assert(False, 'TODO: Array blobs');
//      Result := FBlobConstList.Blobs[FBlobIndex];
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
  Assert((FVarType = vtChar) or ((FVarType = vtSubRange) and (FUserType.OfType.VarType = vtChar)));
  Result := FCharValue;
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

constructor TImmValue.CreateEnumItem(AUserType: PUserType; Index: Integer);
begin
  Assert(AUserType.VarType = vtEnumeration);
  FUserType := AUserType;
  FVarType := vtEnumeration;
  FIntValue := Index;
end;

constructor TImmValue.CreateInteger(AValue: Integer);
begin
  FVarType := vtInteger;
  FUserType := GetSystemType(FVarType);
  FIntValue := AValue;
end;

constructor TImmValue.CreateString(AString: String);
begin
  FVarType := vtString;
  FUserType := GetSystemType(FVarType);
  FStringConstList := GetCurrentScope.ConstList;
  Assert(Assigned(FStringConstList));
  FStringIndex := FStringConstList.Strings.Add(AString);
end;

constructor TImmValue.CreateTyped(AType: TVarType; AValue: Integer);
begin
  FUserType := GetSystemType(AType);
  FVarType := AType;
  case AType of
    vtInt8, vtInteger, vtByte, vtWord, vtPointer,
      vtTypedPointer, vtEnumeration, vtSubRange: FIntValue := AValue;
    vtReal: Assert(False); //TODO
    vtBoolean, vtFlag: FBoolValue:= AValue <> 0;
    vtChar: FCharValue := chr(AValue);
    vtTypeDef: FTypeDefValue := PUserType(AValue);
    vtString:
      if AValue = 0 then
      begin
        FStringConstList := nil;
        FStringIndex := -1;
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
      vtEnumeration, vtSubRange, vtSetByte, vtSetWord,
      vtTypedPointer, vtFunction:
      FIntValue := AValue;
    vtReal: Assert(False); //TODO
    vtBoolean, vtFlag: FBoolValue:= AValue <> 0;
    vtChar: FCharValue := chr(AValue);
    vtTypeDef: FTypeDefValue := PUserType(AValue);
    vtString:
      if AValue = 0 then
      begin
        FStringConstList := nil;
        FStringIndex := -1;
      end
      else
        Assert(False);
    vtArray, vtVector, vtList, vtSetMem, vtRecord:
      if AValue = 0 then
      begin
        FBlobConstList := nil;
        FBlobIndex := -1;
      end
      else
        Assert(False);
  else
    Assert(False);
  end;
end;

constructor TImmValue.CreateTypeDef(AValue: PUserType);
begin
  FVarType := vtTypeDef;
  FUserType := GetSystemType(FVarType);
  FTypeDefValue := AValue;
end;

function TImmValue.IntValue: Integer;
begin
  Assert(IsIntegerVarType(FVarType) or IsIntegerType(FUserType));
  Result := FIntValue;
end;

function TImmValue.StringValue: String;
begin
  case FVarType of
    vtString:
    begin
      Assert(Assigned(FStringConstList));
      Assert(FStringIndex <> -1);
      Result := FStringConstList.Strings[FStringIndex];
    end;
    vtChar: Result := CharValue;
  else
    Assert(False);
  end;
end;

function TImmValue.ToInteger: Integer;
begin
  case FVarType of
    vtInt8, vtInteger, vtByte, vtWord, vtPointer,
      vtEnumeration, vtSubRange:
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
  case FVarType of //TODO: Add scope name to the result
    vtString: Result := '__sl_' + FStringConstList.ScopeName.ToLower + '_' + FStringIndex.ToString;
  else
    Assert(False);
  end;
end;

function TImmValue.ToString: String;

  function ToVarTypedString(AVarType: TVarType): String;
  begin
    case AVarType of
      vtByte: Result := '$' + IntToHex(IntValue, 2);
      vtWord, vtPointer, vtTypedPointer:
        Result := '$' + IntToHex(IntValue, 4);
      vtInt8, vtInteger: Result := IntValue.ToString;
      vtBoolean:
        if BoolValue then
          Result := 'True'
        else
          Result := 'False';
      vtChar:
        if CharInSet(CharValue, [#32..#126]) then
          Result := ''''+CharValue+''''
        else
          Result := '#' + ord(CharValue).ToString;
      vtTypeDef:
      if TypeDefValue <> nil then
        Result := TypeDefValue.Name
      else
        Result := 'nil';
      vtSubRange: Result := ToInteger.ToString;
      vtString:
        if (FStringConstList = nil) or (FStringIndex = -1) then
          Result := '<<UNASSIGNED>>'
        else
          Result := ''''+StringValue+'''';
      vtArray, vtList:
        if (FBlobConstList = nil) or (FBlobIndex = -1) then
          Result := '<<UNASSIGNED>>'
        else
          Result := BlobValue;
    else
      Assert(False);
    end;
  end;

  function ToUserTypedString(AUserType: PUserType): String;
  begin
    case UTToVT(AUserType) of
      vtEnumeration:
      begin
        Assert(Assigned(FUserType));
        Result := AUserType.EnumItems[FIntValue] + '(' + FIntValue.ToString + ')';
      end;
    else
      Result := ToVarTypedString(UTToVT(AUserType));
    end;
  end;

begin
  if Assigned(FUserType) then
    if FVarType = vtSubRange then
      Result := ToUserTypedString(FUserType.OfType)
    else
      Result := ToUserTypedString(FUserType)
  else
    Result := ToVarTypedString(FVarType);
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
  Assert(Def.QTypes.IsIntegerVarType(UTToVT(NewType)));
  Assert(Def.QTypes.IsIntegerVarType(UTToVT(FUserType)));
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

{ TConstList }


function TConstList.Add(const AName: String; UType: PUserType;
  const AValue: TImmValue): PConst;
begin
  New(Result);
  Items.Add(Result);
  Result.Name := AName;
  Result.UserType := UType;
  Result.InScope := True;
  Result.Depth := GetCurrentScope.Depth;
  Result.Value := AValue;
end;

procedure TConstList.Clear;
var V: PConst;
begin
  for V in Items do
    Dispose(V);
  Items.Clear;
  MarkPosition := -1;
  Strings.Clear;
end;

function TConstList.FindByNameInScope(const AName: String): PConst;
var I: Integer;
begin
  for I := 0 to Items.Count-1 do
    if (CompareText(Items[I].Name, AName) = 0) and Items[I].InScope then
      EXIT(Items[I]);

  Result := nil;
end;

procedure TConstList.Initialise;
begin
  Items := TList<PConst>.Create;
  MarkPosition := -1;
  Strings := TStringList.Create;
end;

procedure TConstList.ScopeDepthDecced(NewDepth: Integer);
var I: Integer;
begin
  I := Items.Count-1;
  while (I >= 0) and (Items[I].Depth > NewDepth) do
  begin
    Items[I].InScope := False;
    dec(I);
  end;
end;

procedure SetSystemConsts;
begin
  Consts.Add('False', GetSystemType(vtBoolean), TImmValue.CreateBoolean(False));
  Consts.Add('True', GetSystemType(vtBoolean), TImmValue.CreateBoolean(True));
  Consts.Add('Maxint', GetSystemType(vtInteger), TImmValue.CreateTyped(vtInteger, GetMaxValue(vtInteger)));
  Consts.Add('Minint', GetSystemType(vtInteger), TImmValue.CreateTyped(vtInteger, GetMinValue(vtInteger)));
  Consts.Add('nil', GetSystemType(vtPointer), TImmValue.CreateTyped(vtPointer, $0000));
end;

initialization
  Consts := nil;
end.
