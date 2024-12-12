(*
  Compile time constants

  Currently repurposes the code and data structures for Variables
*)
unit Def.Consts;

interface
uses
  Generics.Collections, Classes,
  Def.QTypes;

type PConstList = ^TConstList;

//===============TImmValue

//Record to store a typed constant value. Used within ILParams and as default
//parameters within function definitions
  TImmValue = record
  //AValue is converted to the appropriate type, if possible,
  //For complex types, the only acceptable value for AValue is 0, which will
  //result in a default initialisation
  constructor CreateTyped(AType: TVarType;AValue: Integer);
  //For generic integer values
  constructor CreateInteger(AValue: Integer);

  constructor CreateChar(AValue: Char);
  constructor CreateBoolean(AValue: Boolean);
  constructor CreateTypeDef(AValue: TVarType);

  constructor CreateString(AString: String);

  procedure UpdateVarType(NewType: TVarType);

  function VarType: TVarType;

  function IntValue: Integer;
  function BoolValue: Boolean;
  function CharValue: Char;
  function TypeDefValue: TVarType;
  //If VarType is vtString or vChar
  function StringValue: String;

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
    case FVarType: TVarType of
      vtInt8, vtInteger, vtByte, vtWord, vtPointer:
        (FIntValue: Integer);
      vtReal: (); //TODO
      vtBoolean, vtFlag: (FBoolValue: Boolean);
      vtChar: (FCharValue: Char);
      vtTypeDef: (FTypeDefValue: TVarType);
      vtString: (
        FConstList: PConstList;  //Scope to which the constant belongs
        FStringIndex: Integer); //Index into string constants list for Scope
                  //(We aren't allowed to put strings in a variant section. Instead
                  //we'll store them elsewhere and put an index to them here.
  end;

//==================CONST

  PConst = ^TConst;
  TConst = record
    Name: String;
    VarType: TVarType;
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

    function Add(const AName: String;AVarType: TVarType;const AValue: TImmValue): PConst;
    function FindByNameInScope(const AName: String): PConst;
  end;

//Currently scoped list of constants
var Consts: PConstList;

procedure InitialiseConsts;

//------------Scope related

//These routines are called by the Scopes routines to create, clear and set Scopes
function CreateConstList(const ScopeName: String): PConstList;
procedure SetCurrentConstList(List: PConstList);

implementation
uses
  SysUtils,
  Def.Globals, Def.Scopes,
  CodeGen;

//----------------------TImmValue

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

constructor TImmValue.CreateBoolean(AValue: Boolean);
begin
  FVarType := vtBoolean;
  FBoolValue := AValue;
end;

constructor TImmValue.CreateChar(AValue: Char);
begin
  FVarType := vtChar;
  FCharValue := AValue;
end;

constructor TImmValue.CreateTyped(AType: TVarType; AValue: Integer);
begin
  FVarType := AType;
  case AType of
    vtInt8, vtInteger, vtByte, vtWord, vtPointer: FIntValue := AValue;
    vtReal: Assert(False); //TODO
    vtBoolean, vtFlag: FBoolValue:= AValue <> 0;
    vtChar: FCharValue := chr(AValue);
    vtTypeDef: FTypeDefValue := TVarType(AValue);
    vtString:
      if AValue = 0 then
      begin
        FConstList := nil;
        FStringIndex := -1;
      end
      else
        Assert(False);
  else
    Assert(False);
  end;
end;

constructor TImmValue.CreateInteger(AValue: Integer);
begin
  FVarType := vtInteger;
  FIntValue := AValue;
end;

function TImmValue.IntValue: Integer;
begin
  Assert(IsIntegerType(FVarType));
  Result := FIntValue;
end;

constructor TImmValue.CreateString(AString: String);
begin
  FVarType := vtString;
  FConstList := GetCurrentScope.ConstList;
  Assert(Assigned(FConstList));
  FStringIndex := FConstList.Strings.Add(AString);
end;

constructor TImmValue.CreateTypeDef(AValue: TVarType);
begin
  FVarType := vtTypeDef;
  FTypeDefValue := AValue;
end;

function TImmValue.StringValue: String;
begin
  case VarType of
    vtString:
    begin
      Assert(Assigned(FConstList));
      Assert(FStringIndex <> -1);
      Result := FConstList.Strings[FStringIndex];
    end;
    vtChar: Result := CharValue;
  else
    Assert(False);
  end;
end;

function TImmValue.ToInteger: Integer;
begin
  case VarType of
    vtInt8, vtInteger, vtByte, vtWord, vtPointer : Result := IntValue;
    vtBoolean:
      if BoolValue then
        Result := valueTrue
      else
        Result := valueFalse;
    vtChar: Result := ord(CharValue);
    vtTypeDef: Result := ord(TypeDefValue);
  else
    Assert(False);
    Result := 0;
  end;
end;

function TImmValue.ToLabel: String;
begin
  case VarType of //TODO: Add scope name to the result
    vtString: Result := '__sl_' + FConstList.ScopeName.ToLower + '_' + FStringIndex.ToString;
  else
    Assert(False);
  end;
end;

function TImmValue.ToString: String;
begin
  case VarType of
    vtByte: Result := '$' + IntToHex(IntValue, 2);
    vtWord, vtPointer: Result := '$' + IntToHex(IntValue, 4);
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
      Result := VarTypeToName(TypeDefValue);
    vtString:
      if (FConstList = nil) or (FStringIndex = -1) then
        Result := '<<UNASSIGNED>>'
      else
        Result := ''''+StringValue+'''';
  else
    Assert(False);
  end;
end;

function TImmValue.ToStringByte: String;
begin
  case VarType of
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


function TImmValue.TypeDefValue: TVarType;
begin
  Assert(FVarType = vtTypeDef);
  Result := FTypeDefValue;
end;

procedure TImmValue.UpdateVarType(NewType: TVarType);
begin
  Assert(IsIntegerType(NewType));
  Assert(IsIntegerType(FVarType));
  FVarType := NewType;
end;

function TImmValue.VarType: TVarType;
begin
  Result := FVarType;
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


function TConstList.Add(const AName: String; AVarType: TVarType;
  const AValue: TImmValue): PConst;
begin
  New(Result);
  Items.Add(Result);
  Result.Name := AName;
  Result.VarType := AVarType;
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

initialization
  Consts := nil;
end.
