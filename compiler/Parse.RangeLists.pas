unit Parse.RangeLists;

interface
uses Def.IL, Def.UserTypes, Def.VarTypes,
  Parse.Errors, Parse.Literals, Parse.Source;

//======================================== RANGELISTS
//A range-list is a list of comma seperated values, where each value can be an individual
//item or a range <item1>..<item2>
//RangeLists are useful for:
//  * Array literals: no ranges allowed:
//    [<item1>,<item2>, ... ,<itemn>] (using square braces or parentheses)
//  * Set literals: ranges allowed:
//    [<item1>..<item2>,<item>]
//  * CASE statements: ranges allowed:
//    <item1>..<item2>,<item3> : <code>

type
  TRangeListOptions = (
    rloAllowRanges,       //Allow ranges
    rloConstantsOnly,     //All items must be compile time constant expressions
    rloEnumerablesOnly);  //Item type must be enumerable
  TRangeListOptionSet = set of TRangeListOptions;

//Parses an array or set literal. Parser is at the opening character ('[')
function ParseArrayOrSetLiteral(var Slug: TExprSlug): TQuicheError;

//function ParseRangeList(out RangeList: TRangeList;ItemType: PUserType;Terminator: Char;
//  Options: TRangeListOptionSet): TQuicheError;


//Where Slug's operand is a RangeList, converts the RangeList data to a value of AsType.
//AsType must be an array or set type.
//Currently the RangeList must consist solely of constant expressions.
//If AsType is a unbounded array, AsType will return a type with bounded data.
//If Astype is bounded, verifies the data size equals the declared size.
function HardenRangeListToType(var Slug: TExprSlug;var AsType: TUserType): TQuicheError;

//Where Slug's operand is a RangeType, converts the RangeList data to an array.
//The element type and size of the array are inferred from the RangeList data.
//AsType returns the type of the hardened data.
function HardenRangeListToInferredArrayType(var Slug: TExprSlug;out AsType: TArrayType): TQuicheError;


implementation
uses Def.Consts,
  Parse.Base, Parse.Expr, Parse.TypeChecker;

//===================================== PARSE RANGELISTS
function AppendRangeListItem(var RangeList: TRangeList;ItemType: TUserType;
  Options: TRangeListOptionSet): TQuicheError;
var Item: PRangeListItem;
  Slug: TExprSlug;
begin
  SetLength(RangeList, Length(RangeList)+1);
  Item := @RangeList[Length(RangeList)-1];
  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  //Parse item
  Item.Cursor := Parser.GetCursor;
  Result := ParseExprToSlug(Slug);
  if Result <> qeNone then
    EXIT;

  //TODO: ValidateItem

  Item.Operand := Slug.Operand;

  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;
end;

function DoParseRangeList(out RangeList: TRangeList;ItemType: TUserType;Terminator: Char;
  Options: TRangeListOptionSet): TQuicheError;
var
  Item: PRangeListItem;
  PrevItemType: TRangeListItemType;
  Ch: Char;
  ErrorType: TQuicheError;  //The error we generate depends on the options
begin
  if rloAllowRanges in Options then
    ErrorType := qeCommaOrDotDotExpected
  else
    ErrorType := qeCommaExpectedInArray;

  SetLength(RangeList, 0);
  PrevItemType := rlitItem; //Safe default
  while True do
  begin
    Result := AppendRangeListItem(RangeList, ItemType, Options);
    if Result <> qeNone then
      EXIT;

    Item := @RangeList[Length(RangeList)-1];

    //Expecting End-of-list or separator
    Ch := Parser.TestChar;
    if Ch = Terminator then
    begin
      Parser.SkipChar;
      EXIT(qeNone);
    end
    else if Ch = ',' then
    begin
      Parser.SkipChar;
      if PrevItemType = rlitRangeLow then
        Item.ItemType := rlitRangeHigh
      else
        Item.ItemType := rlitItem;
    end
    else if Ch = '.' then
    begin
      if not TestRangeOperator then
        EXIT(Err(ErrorType));

      if PrevItemType = rlitRangeLow then
        EXIT(Err(qeCommaExpectedInArray)); //DotDot not allowed here

      Item.ItemType := rlitRangeLow;
    end
    else
      EXIT(Err(ErrorType));

    PrevItemType := Item.ItemType;
  end;
end;

function ParseRangeList(out RangeList: TRangeList;ItemType: TUserType;Terminator: Char;
  Options: TRangeListOptionSet): TQuicheError;
begin
  //1. Parse list
  //  Validate against options
  Result := DoParseRangeList(RangeList, ItemType, Terminator, Options);
  if Result <> qeNone then
    EXIT;

  //2. Validate list
  //  Check all items are compatible types
  //  Ranges are in the correct order
  //  Check for duplicates (including dupes covered by ranges)

  //3. Simplify list
  //  If Ranges are allowed,
  //    Pack items into ranges(?)
  //    Sort list items (beware ranges!!)
end;

function ParseArrayOrSetLiteral(var Slug: TExprSlug): TQuicheError;
var RangeList: TRangeList;
begin
  Assert(Parser.TestChar = '[');
  Parser.SkipChar;
  Result := ParseRangeList(RangeList, nil, ']', [rloAllowRanges]);
  if Result <> qeNone then
    EXIT;

  //Assign the RangeList to the Slug
  //TODO: RangeLists are not always literals!
  Slug.Operand.Initialise;
  Slug.Operand.Kind := pkRangeList;
  Slug.RangeList := RangeList;
  Slug.ResultType := nil;
  Slug.ImplicitType := Slug.ResultType;
end;

//===================================== UTILTIES
//Validate that ArrayList is an array (ie that it cintains no ranges)
function ValidateRangeListAsArray(const RangeList: TRangeList): TQuicheError;
var Item: TRangeListItem;
begin
  for Item in RangeList do
    if Item.ItemType <> rlitItem then
    begin
      Parser.SetCursor(Item.Cursor);
      EXIT(Err(qeCommaExpectedInArray));
    end;

  Result := qeNone;
end;

//Where Left and Right and numeric types, finds the most suitable numeric type
//for representing them both
(*function GetCommonNumericType(Left, Right: PUserType): PUserType;
var LVT: TVarType;
  RVT: TVarType;
begin
  LVT := Left.VarType;
  RVT := Right.VarType;
  if LVT = vtReal then
    EXIT(Left);
  if RVT = vtReal then
    EXIT(Right);
  if IsSignedVarType(LVT) then
  begin
    if RVT in [vtWord, vtPointer] then
      //WARNING: Expanding integer values to reals
      EXIT(GetSystemType(vtReal))
    else if RVT = vtByte then
      EXIT(GetSystemType(vtInteger))
    else
      EXIT(Left);
  end
  //Left is unsigned
  else if IsSignedVarType(RVT) then
  begin
    if LVT in [vtWord, vtPointer] then
      //WARNING: Expanding integer values to reals
      EXIT(GetSystemType(vtReal))
    else if LVT = vtByte then
      EXIT(GetSystemType(vtInteger))
    else
      EXIT(Right);
  end
  else  //Both are unsigned
    if (LVT = vtByte) and (RVT = vtByte) then
      EXIT(Left)
    else if (LVT = vtPointer) or (RVT = vtPointer) then
      EXIT(GetSystemType(vtPOinter))
    else
      EXIT(GetSystemType(vtWord));
end;
*)
function GetRangeListNumericType(const RangeList: TRangeList;out CommonType: TUserType): TQuicheError;
var I: Integer;
  UT: TUserType;
  IsFloat: Boolean;
  IntValue: Integer;
  Min: Integer;
  Max: Integer;
begin
  IsFloat := False;
  Min := RangeList[0].Operand.Imm.IntValue;
  Max := Min;
  for I := 0 to Length(RangeList)-1 do
  begin
    UT := RangeList[I].Operand.GetUserType;
    if not IsNumericType(UT) then
    begin
      Parser.SetCursor(RangeList[I].Cursor);
      EXIT(ErrSub2(qeArrayElementTypeMismatch, UT.Description, '<numeric-type>'));
    end;

    IsFloat := IsFloat or (RangeList[0].Operand.GetVarType = vtReal);
    if not IsFloat then
    begin
      IntValue := RangeList[I].Operand.Imm.IntValue;
      if IntValue < Min then
        Min := IntValue
      else if IntValue > Max then
        Max := IntValue;
    end;
  end;

  if IsFloat then
    CommonType := GetSystemType(vtReal)
  else if not TryFindCommonIntegerType(Min, Max, CommonType) then
    //TODO: WARNING = Expanding Integers to Reals
    CommonType := GetSystemType(vtReal);

  Result := qeNone;
end;


function GetRangeListCommonType(const RangeList: TRangeList;out CommonType: TUserType): TQuicheError;
var Item: TRangeListItem;
  VT: TVarType;
begin
  if Length(RangeList) = 0 then
  begin
    CommonType := nil;
    EXIT(qeNone);
  end;

  Item := RangeList[0];
  VT := Item.Operand.GetVarType;
  if IsNumericVarType(VT) then
    EXIT(GetRangeListNumericType(RangeList, CommonType))
  else if IsOrdinalVarType(VT) then
  begin
    //TODO: Parser should already have validated that we have a common type
    CommonType := Item.Operand.GetUserType;
    EXIT(qeNone);
  end
(*    EXIT(GetRangeListOrdinalType(RangeList, CommonType))
*)  else
    Assert(False, 'TODO');
end;

function ValidateRangeListItemType(const RangeList: TRangeList; ItemType: TUserType): TQuicheError;
var Item: TRangeListItem;
begin
  for Item in RangeList do
  begin
    Assert(Item.Operand.Kind = pkImmediate);
    Result := TypeCheckImm(ItemType, Item.Operand.Imm);
    if Result <> qeNone then
      EXIT;
  end;
end;

function RangeListToArrayConstant(const RangeList: TRangeList;ArrayType: TArrayType): TImmValue;

  //Returns the updated Index
  //Absolutely no validation is performed, either on data size, index or value!
  function AddData(var Data: TBlob;Index: Integer;Value: Integer;ItemSize: Integer): Integer;
  begin
    case ItemSize of
      1:
      begin
        Data[Index] := Value and $ff;
        Result := Index + 1;
      end;
      2:
      begin
        Data[Index] := Value and $ff; //Low byte
        Data[Index+1] := Value shr 8 and $ff; //High byte
        Result := Index + 2;
      end;
    else
      raise EVarType.Create;
    end;
  end;

  //Returns the updated Index
  //Absolutely no validation is performed, either on data size, index or value!
  function AddMeta(var Data: TBlob;Index: Integer;Value: Integer;MetaSize: TArraySize): Integer;
  begin
    case MetaSize of
      asShort: Result := AddData(Data, Index, Value, 1);
      asLong: Result := AddData(Data, Index, Value, 2);
    else
      raise EVarType.Create;
    end;
  end;

var Data: TBlob;  //Array of Byte
  I: Integer;
  Item: TRangeListItem;
  ItemSize: Integer;
  Count: Integer;
begin
  SetLength(Data, ArrayType.DataSize);

  I := 0;
  //Add meta data
  case ArrayType.ArrayKind of
    atArray: ;  //No meta
    atVector:
      I := AddMeta(Data, I, (ArrayType as TVectorType).Length, ArrayType.ArraySize);
    atList:
    begin
      I := AddMeta(Data, I, (ArrayType as TLIstType).Capacity, ArrayType.ArraySize);
      I := AddMeta(Data, I, (ArrayType as TListType).Capacity, ArrayType.ArraySize);
    end;
  else
    raise EVarType.Create;
  end;

  ItemSize := ArrayType.OfType.DataSize;
  for Item in RangeList do
  begin
    Assert(IsRegisterType(ArrayType.OfType)); //TEMPORARY
    Assert(Item.Operand.Kind = pkImmediate);

    I := AddData(Data, I, Item.Operand.Imm.ToInteger, ItemSize);
  end;

  //If we have a List where the Range leaves empty entries
  if ArrayType.ArrayKind = atList then
    while I < Length(Data) do
      I := AddMeta(Data, I, 0, asShort);
(*
    for Count := 0 to ArrayType.ListCapacity - Length(RangeList) - 1 do
      I := AddMeta(Data, I, 0, ArrayType.ArrayDef.ArraySize);
*)
  Assert(I = Length(Data));

  Result.CreateBlob(ArrayType, Data);
end;

//===================================== HARDEN RANGELISTS

function HardenRangeListToArrayType(var Slug: TExprSlug;AsType: TArrayType;out ResultType: TUserType): TQuicheError;
var Newtype: TArrayType;
  BoundsType: TOrdinalType;
  Value: TImmValue;

  //For the new type:
  Kind: TArrayKind;
  Size: TArraySize;
  LengthOrCapacity: Integer;
begin
  Assert(AsType.VarType = vtArrayType);
  Assert(Slug.Operand.Kind = pkRangeList);

  Result := ValidateRangeListAsArray(Slug.RangeList);
  if Result <> qeNone then
    EXIT;

  Result := ValidateRangeListItemType(Slug.RangeList, AsType.OfType);
  if Result <> qeNone then
    EXIT;

  //Create type for the array: ArrayType = Vector[RangeList.Length] of ElementType
  if AsType.IsUnbounded then
  begin
    Kind := AsType.ArrayKind;
    if Length(Slug.RangeList) > 255 then
      Size := asLong
    else
      Size := asShort;
    //Set array bounds/length/capacity
    case AsType.ArrayKind of
      atArray:
      begin //Create a bounds type!!
        if Length(Slug.RangeList) > 255 then
          BoundsType := TTypes.CreateIntegerType(vtWord, 0, Length(Slug.RangeList)-1)
        else
          BoundsType := TTypes.CreateIntegerType(vtWord, 0, Length(Slug.RangeList)-1);
      end;
      atVector, atList: LengthOrCapacity := Length(Slug.RangeList);
    else
      raise EVarType.Create;
    end;
    NewType := TTypes.CreateArrayType(Kind, Size, BoundsType, LengthOrCapacity, AsType.OfType);
  end
  else  //Bounded array - verify the element count matches that of the RangeList
  begin
    case AsType.ArrayKind of
      atArray:
        if AsType.GetItemCount <> Length(Slug.RangeList) then
          EXIT(qeArrayLengthMismatch);
      atVector:
        if (AsType as TVectorType).Length <> Length(Slug.RangeList) then
          EXIT(qeArrayLengthMismatch);
      atList:
        if (AsType as TListType).Capacity < Length(Slug.RangeList) then
          EXIT(qeArrayLengthMismatch);
    else
      raise EVarType.Create;
    end;
    NewType := AsType;
  end;
  Value := RangeListToArrayConstant(Slug.RangeList, NewType);
  ResultType := NewType;

  Slug.SetImmediate(AsType, Value);
  Result := qeNone;
end;

function HardenRangeListToType(var Slug: TExprSlug;var AsType: TUserType): TQuicheError;
var Value: TImmValue;
  ResultType: TUserType;
begin
  Assert(AsType <> nil);
  Assert(Slug.ILItem = nil);
  Assert(Slug.Operand.Kind = pkRangeList);

  case AsType.VarType of
    vtSetByte, vtSetWord, vtSetMem: Assert(False, 'TODO');
    vtArrayType:  //TODO??? - this happens in expr evaluator??
    begin
      Result := HardenRangeListToArrayType(Slug, AsType as TArrayType, ResultType);
      if Result <> qeNone then
        EXIT;
      AsType := ResultType;
    end;
  else
    EXIT(ErrSub2(qeTypeMismatch,'<array-or-set>', AsType.ToString));
  end;
end;

function HardenRangeListToInferredArrayType(var Slug: TExprSlug;out AsType: TArrayType): TQuicheError;
var ElementType: TUserType;
  Value: TImmValue;

  Size: TArraySize;
begin
  Assert(Slug.Operand.Kind = pkRangeList);

  Result := ValidateRangeListAsArray(Slug.RangeList);
  if Result <> qeNone then
    EXIT;

  Result := GetRangeListCommonType(Slug.RangeList, ElementType);
  if Result <> qeNone then
    EXIT;

  //Create type for the array: ArrayType = Vector[RangeList.Length] of ElementType
  if Length(Slug.RangeList) > 255 then
    Size := asLong
  else
    Size := asShort;
  AsType := TTypes.CreateArrayType(atVector, Size, nil, Length(Slug.RangeList), ElementType);

  //Convert array data to a constant
  Value := RangeListToArrayConstant(Slug.RangeList, AsType);

  Slug.SetImmediate(AsType, Value);

end;

end.
