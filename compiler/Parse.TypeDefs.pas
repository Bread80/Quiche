unit Parse.TypeDefs;

interface
uses Def.UserTypes,
  Parse.Errors;

//Parses a type definition*
//If OrdinalTypesOnly is true will raise an error if the result is not an ordinal type
//Returns the created type.
//* - As opposed to a declaration. A declaration is part of a TYPE statement and includes
//a name for the type. A definition is the section after the = in a declaration and
//is unnamed. A name will be applied on return from this function if it is called as
//part of a declaration
function ParseTypeDefinition(out TheType: TUserType;OrdinalTypesOnly: Boolean = False): TQuicheError;

//Parse a TYPE statement
//
//<type-declaration> :== <identifier> = <type-definition>
//<type-definition> :== <type-identifier>
//                      | ( <identifier-list> )
//                      | set of <type-definition>
//                      | <range-expression>
//                      | range of <type-definition>
//                      | array<array-bounds> of <type-definition>
//                      | [LONG|SHORT] <list-keyword>[<capacity>] of <type-definition>
//                      | <list-type>[<capacity>]
//                      | <record-definition>
//TODO                      | <stream-definition>
//TODO                      | <function-definition>
function DoTYPE(const Ident: String): TQuicheError;

implementation
uses SysUtils,
  Def.Consts, Def.VarTypes, Def.Scopes, Def.ScopesEX, Def.Variables, Def.Functions, Def.Globals,
  Parse.Base, Parse.Source, Parse.Expr, Parse.FuncDef, Parse.VarDefs;

//Where ValueLow and ValueHigh are ordinal values, finds the optimal common type
//based on their type and value (for numeric types)
function FindCommonOrdinalType(ValueLow, ValueHigh: TImmValue;out CommonType: TUserType): TQuicheError;
begin
  CommonType := ValueLow.UserType;
  if IsNumericVarType(ValueLow.VarType) then
  begin
    if not IsNumericVarType(ValueHigh.VarType) then
      EXIT(ErrSub2(qeRangeBoundsTypeMismatch, ValueLow.UserType.Description, ValueHigh.UserType.Description));

    if not TryFindCommonIntegerType(ValueLow.IntValue, ValueHigh.IntValue, CommonType) then
      EXIT(ErrSub2(qeRangeExprValuesTooWide, ValueLow.ToString, ValueHigh.ToString));
  end
  else
    case ValueLow.VarType of
      vtBoolean, vtChar:
        if ValueLow.VarType <> ValueHigh.VarType then
          EXIT(ErrSub2(qeRangeBoundsTypeMismatch, ValueLow.UserType.Description, ValueHigh.UserType.Description));
      vtEnumeration:
        if ValueLow.UserType <> ValueHigh.UserType then
          EXIT(ErrSub2(qeRangeBoundsTypeMismatch, ValueLow.UserType.Description, ValueHigh.UserType.Description));
    else
      Assert(False);  //Unknown type?
    end;

  if ValueLow.ToInteger >= ValueHigh.ToInteger then
    //Left value must be < Right value
    EXIT(ErrSub2(qeRangeValuesMisordered, ValueLow.ToString, ValueHigh.ToString));
end;

function ParseSubRangeDefinition(out TheType: TUserType): TQuicheError;
var ValueLow: TImmValue;
  ValueHigh: TImmValue;
  CommonType: TUserType;  //Common type of upper and lower bounds
  Found: Boolean;
begin
  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;

  //Parse low bound
  Result := ParseConstantExpr(ValueLow);
  if Result = qeOperatorExpected then
    EXIT(Err(qeRangeOperatorExpected));
  if Result <> qeNone then
    EXIT;

  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;

  //Range operator ..
  if not TestRangeOperator then
    EXIT(Err(qeRangeOperatorExpected));

  if not IsOrdinalVarType(ValueLow.VarType) then
    EXIT(Err(qeOrdinalConstExprExpected));

  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  //Parse high bound
  Result := ParseConstantExpr(ValueHigh);
  if Result = qeConstantExpressionExpected then
    EXIT(Err(qeOrdinalConstExprExpected));
  if Result <> qeNone then
    EXIT;

  if not IsOrdinalVarType(ValueHigh.VarType) then
    EXIT(Err(qeOrdinalConstExprExpected));

  //Validate, and find a common type if required (ie for numeric types)
  Result := FindCommonOrdinalType(ValueLow, ValueHigh, CommonType);

  Assert(CommonType is TOrdinalType, 'Oops');
  TheType := TTypes.CreateSubRange(TOrdinalType(CommonType), ValueLow.ToInteger, ValueHigh.ToInteger);
end;

//If the first identifier is unknown, processes as an enumeration,
//If the first identifier is a constant, parses as a subrange (with constant expression)
//The leading ( has not been consumed
function ParseEnumDefinition(out TheType: TUserType): TQuicheError;
var Items: TArray<String>;
begin
  //Skip opening brace
  Parser.SkipChar;
  //Read enumeration items
  Result := ParseIdentifierList(True, Items);
  if Result <> qeNone then
    EXIT;

  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;

  if Parser.TestChar <> ')' then
    EXIT(Err(qeCommaOrCloseParensExpected));
  Parser.SkipChar;

  //Create type
  TheType := TTypes.CreateEnumeration(Items);
end;

function ParseOrdinalTypeDefinition(out TheType: TOrdinalType): TQuicheError;
var NewType: TUserType;
begin
  Result := ParseTypeDefinition(NewType, True);
  if Result <> qeNone then
    EXIT;
  TheType := NewType as TOrdinalType;
end;

function ParseSetDefinition(out TheType: TUserType): TQuicheError;
var OfType: TOrdinalType;
begin
  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;

  if not TestForIdent('of') then
    EXIT(Err(qeOfExpected));

  Result := ParseOrdinalTypeDefinition(OfType);
  if Result <> qeNone then
    EXIT;

  //TODO: When we implement data for sets we'll need to establish
  //whether the element count is suitable, and which internal set type to use

  TheType := TTypes.CreateSetType(vtSetMem, OfType);
end;

//Parses an array definition.
//If InBounds is True  -> We are within the [..] section of an array definition.
//                        TheType returns the Element type for an array (which
//                        might be an array definition, if we're parsing a
//                        multidimensional array definition.
//If InBounds is False -> We're outside a [..] section.
//                        TheType will return an Array definition (vtArrayType)

//Array definitions are basically a list of '<bounds> of <element>'
//where, for a multidimensional array, <element> will be another array type.
//Therefore we parse a bounds and recurse for the element and declare an array
//until we find the declaration of the inner element

//array [<low>..<high>] of <type>
//array [<low>..<high>,<low>..<high>] of <type>
//array [<low>..<high>][<low>..<high>] of <type>
//array [<low>..<high>] of array[<low>..<high>] of <type>
function ParseArrayDefinition(out TheType: TUserType;InBounds: Boolean = False): TQuicheError;

  //Parse a (Bounds:Element) pair
  function Recurse(out TheType: TUserType): TQuicheError;
  var Bounds: TOrdinalType;
    OfType: TUserType;
    Size: TArraySize;
  begin
    Parser.SkipChar;
    Result := ParseOrdinalTypeDefinition(Bounds);
    if Result <> qeNone then
      EXIT;
    Result := ParseArrayDefinition(OfType, True);
    if Result <> qeNone then
      EXIT;
    if (Bounds.High-Bounds.Low+1) > 255 then
      Size := asLong
    else
      Size := asShort;
    TheType := TTypes.CreateArrayType(atArray, Size, Bounds, -1, OfType);
  end;

var
  OfType: TUserType;
begin
  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  case Parser.TestChar of
    '[':
    begin
      if InBounds then
        EXIT(Err(qeArrayBoundsDefinition));

      Result := Recurse(TheType);
      EXIT;
    end;
    ',':
    begin
      if not InBounds then
        EXIT(Err(qeArrayBoundsDefinition));

      Result := Recurse(TheType);
      EXIT;
    end;
    ']':
    begin
      if not InBounds then
        EXIT(Err(qeArrayBoundsDefinition));

      Parser.SkipChar;
      Result := Parser.SkipWhite;
      if Result <> qeNone then
        EXIT;
      if Parser.TestChar = '[' then
      begin
        Result := ParseArrayDefinition(TheType, False);
        EXIT;
      end;
    end;
  else
    if InBounds then
      EXIT(Err(qeArrayBoundsDefinition));
  end;

  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;

  if not TestForIdent('of') then
    EXIT(Err(qeOfExpected));

  Result := ParseTypeDefinition(OfType, False);
  if Result <> qeNone then
    EXIT;

  if not InBounds then
    //Unbounded array
    TheType := TTypes.CreateUnboundedArrayType(atArray, asUnknown, OfType)
  else  //Element
    TheType := OfType;
end;

function ParseVectorOrListSize(out Size: Integer): TQuicheError;
var SizeValue: TImmValue;
begin
  Assert(Parser.TestChar = '[');

  Parser.SkipChar;
  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;
  Result := ParseConstantExpr(SizeValue);
  if Result <> qeNone then
    EXIT;

  //Vectors and list can only have numeric sizes
  if not IsIntegerVarType(SizeValue.VarType) then
    EXIT(Err(qeListCapacityError));
  Size := SizeValue.ToInteger;
  if Size < 1 then
    EXIT(Err(qeListCapacityError));
  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;
  if Parser.TestChar <> ']' then
    EXIT(Err(qeCloseSquareBraceExpected));
  Parser.SkipChar;
  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;
end;

//<list> :== list of <type>
//           list[<capacity>] of <type>
//           vector of <type>
//           vector[<length>] of <type>
//where <capacity>/<length> is any Integer value > 0. If <length>/<capacity> is not
//specified and unbounded array type will be created.
//ArrayType must be either atVector or atList.
//If ArraySize is asUnknown the size will be chosen based on options and specified
//length/capacity. If the options specify a Short size but the specified length/capacity
//is oversize then a Long size will be used instead.
//If ArraySize is given explicitly it will be adhered to and an error raised in the specified
//length/capacity is out of range.
function ParseVectorOrListDefinition(ArrayKind: TArrayKind;ArraySize: TArraySize;out TheType: TUserType): TQuicheError;
var LengthOrCapacity: Integer;
  OfType: TUserType;
  IsUnbounded: Boolean;
begin
  assert(ArrayKind in [atVector, atList]);

  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  IsUnbounded := Parser.TestChar <> '[';
  if IsUnbounded then
    LengthOrCapacity := -1
  else
  begin
    Result := ParseVectorOrListSize(LengthOrCapacity);
    if Result <> qeNone then
      EXIT;
  end;

  if not TestForIdent('of') then
    EXIT(Err(qeOFExpected));

  Result := ParseTypeDefinition(OfType, False);
  if Result <> qeNone then
    EXIT;

  if ArraySize = asUnknown then
  begin
    if LengthOrCapacity > 255 then
      ArraySize := asLong
    else
      ArraySize := optDefaultArraySize;
  end
  else
    if (LengthOrCapacity > 255) and (ArraySize <> asLong) then
      EXIT(Err(qeConstantOutOfRange));

  if IsUnbounded then
    TheType := TTypes.CreateUnboundedArrayType(ArrayKind, ArraySize, OfType)
  else
    TheType := TTypes.CreateArrayType(ArrayKind, ArraySize, nil, LengthOrCapacity, OfType);
end;

function BakeArrayType(FromType: TArrayType;out TheType: TUserType): TQuicheError;
var LengthOrCapacity: Integer;
begin
  Result := ParseVectorOrListSize(LengthOrCapacity);
  if Result <> qeNone then
    EXIT;

  TheType := TTypes.CreateArrayType(FromType.ArrayKind, FromType.ArraySize, nil,
    LengthOrCapacity, FromType.OfType);
end;

function ParseRecordFields(Scope: PScope): TQuicheError;
var Ident: String;
  Keyword: TKeyword;
  V: TVariable;
  Size: Integer;
  Offset: Integer;  //From start of record
begin
  Offset := 0;

  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  while True do
  begin
    Result := ParseIdentifier(#0, Ident);
    if Result <> qeNone then
      EXIT;

    Keyword := IdentToKeyword(Ident);
    case Keyword of
      keyUNKNOWN: //Data field definition
      begin
        V := nil;
        //(Techically we haven't read a VAR, but we're parsing a declaration as if one had)
        Result := ParseVarDeclaration(vsVarRead, asAssignNotAllowed, Ident, V, amStatic);
        if Result <> qeNone then
          EXIT;
        V.Offset := Offset;
        Size := V.UserType.DataSize;
        if Size <= 0 then
          EXIT(Err(qeConcreteOrPointerTypeRequired));
        Offset := Offset + Size;
      end;
(*      keyFUNCTION: Function
      keyPROCEDURE: Procedure
      keyCASE: Variants
*)      keyEND: EXIT(qeNONE);
    else
      EXIT(Err(qeInvalidKeyword));
    end;

    Result := Parser.NextStatement(False);
    if Result <> qeNone then
      EXIT;
  end;
end;

//<record> :== record
//               <field-list>
//             end
//<field> :== <variable-declaration> (??)
function ParseRecordDefinition(out TheType: TUserType): TQuicheError;
var PrevScope: PScope;
  Scope: PScope;
begin
  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  //Create scope
  PrevScope := GetCurrentScope;
  Scope := CreateRecordScope('<record>');

  Result := ParseRecordFields(Scope);
  if Result <> qeNone then
   EXIT;

  SetCurrentScope(PrevScope);

  //Create the type
  TheType := TTypes.CreateRecordType(ScopeToScopeHandle(Scope));
end;

//If IsProc True we're parsing a PROCEDURE, otherwise we're parsing a FUNCTION
function ParseFuncDefinition(IsProc: Boolean;out TheType: TUserType): TQuicheError;
var
  Func: PFunction;
begin
  Result := DoFUNCTION(IsProc, fptTypeDef, Func);
  if Result <> qeNone then
    EXIT;

  TheType := TTypes.CreateFunctionType(TFunctionHandle(Func));
end;

function ParseTypeDefinition(out TheType: TUserType;OrdinalTypesOnly: Boolean = False): TQuicheError;
var Ch: Char;
  IsPointed: Boolean;
  Ident: String;
  Keyword: TKeyword;
  IdentData: TIdentData;
  Cursor: TParseCursor;
  ArraySize: TArraySize;
begin
  TheType := nil;

  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  Ch := Parser.TestChar;
  IsPointed := Ch = '^';
  if IsPointed then
  begin
    if OrdinalTypesOnly then
      EXIT(Err(qeOrdinalTypeExpected));
    Parser.SkipChar;
    Ch := Parser.TestChar;
  end;

  if CharInSet(Ch, csIdentFirst) then
  begin //Identifier
    Cursor := Parser.GetCursor;
    Result := ParseIdentifier(#0, Ident);
    if Result <> qeNone then
      EXIT;

    Keyword := IdentToKeyword(Ident);
    if OrdinalTypesOnly then
      if Keyword in [keyARRAY, keyFUNCTION, keyLIST, keyLONG, keyPROCEDURE, keyRECORD,
        keySET, keySHORT, keyVECTOR] then
        EXIT(Err(qeOrdinalTypeExpected));

    case Keyword of
      keySHORT: ArraySize := asShort;
      keyLONG: ArraySize := asLong;
    else
      ArraySize := asUnknown;
    end;

    if ArraySize <> asUnknown then
    begin
      Result := Parser.SkipWhite;
      if Result <> qeNone then
        EXIT;

      Result := ParseIdentifier(#0, Ident);
      if Result <> qeNone then
        EXIT;

      Keyword := IdentToKeyword(Ident);
    end;

    //LIST and VECTOR can have a size prefix (LONG or SHORT)
    case Keyword of
      keyLIST:   Result := ParseVectorOrListDefinition(atList, ArraySize, TheType);
      keyVECTOR: Result := ParseVectorOrListDefinition(atVector, ArraySize, TheType);
    else
      if ArraySize <> asUnknown then
        EXIT(ErrSub(qeUndeclaredTypeOrInvalidTypeDef, Ident));

      case Keyword of
        keyARRAY: Result := ParseArrayDefinition(TheType);
        keyFUNCTION: Result := ParseFuncDefinition(False, TheType);
        keyPROCEDURE: Result := ParseFuncDefinition(True, TheType);
        keyRECORD: Result := ParseRecordDefinition(TheType);
        keySET: Result := ParseSetDefinition(TheType);
        keyUNKNOWN:
        begin //Not a keyword
          IdentData := GetCurrentScope.SearchUpAll(Ident, True);
          case IdentData.IdentType of
            itType:
            begin
              Assert(IdentData.Value <> nil);
              //If identifier is the name of a type we'll assume it's a type synonym
              //(or pointer)...
              if IsPointed then
                TheType := TTypes.SearchScopesForAnonTypedPointer(IdentData.AsType)
              else if (Parser.TestChar = '[') and
                (IdentData.AsType.VarType = vtArrayType) and
                ((IdentData.AsType as TArrayType).ArrayKind in [atVector, atList]) then
                Result := BakeArrayType(IdentData.AsType as TArrayType, TheType)
              else
                TheType := IdentData.AsType;
            end
          else
            if IsPointed and (IdentData.IdentType <> itType) then
              EXIT(Err(qePointedTypeNameExpected))
            else  //...otherwise we'll assume it's a range declaration
            begin
              Parser.SetCursor(Cursor); //Reset cursor. Messy but easy
              Result := ParseSubRangeDefinition(TheType);
              if Result = qeConstantExpressionExpected then
                EXIT(ErrSub(qeUndeclaredTypeOrInvalidTypeDef, Ident));
            end;
          end;
        end;
      else  //Any other reserved word
        EXIT(ErrSub(qeReservedWord, Ident));
      end
    end
  end
  else  //Not an identifier -
  begin
    if IsPointed then
      EXIT(Err(qePointedTypeNameExpected));

    //NOTE: There's a sytactic ambiguity between enumeration definitions, which begin
    //with a ( and a subrange definition which constant expression which begins
    //with a (. Eg.
    //type MyEnum = (X, Y)
    //type MyRange = (X + Y) * 2..256
    //Therefore:
    //If the definition begins with an opening brace it must be an enumeration definition,
    //if not it's a subrange. Examples such as the above will have to be rewritten
    //so as not to begin with a brace, eg.
    //type MyRange = 2 * (X + Y)..256
    if Parser.TestChar = '(' then
      Result := ParseEnumDefinition(TheType)
    else
    begin
      Result := ParseSubRangeDefinition(TheType);
      if Result = qeConstantExpressionExpected then
        EXIT(Err(qeInvalidTypeDefinition));
    end;
  end;
  if Result <> qeNone then
    EXIT;

  if OrdinalTypesOnly then
    if not IsOrdinalType(TheType) then
      EXIT(Err(qeOrdinalTypeExpected));
end;

function DoTYPE(const Ident: String): TQuicheError;
var TypeName: String;
  TheType: TUserType;
begin
  //Get the type name
  if Ident <> '' then
  begin
    Result := TestUniqueIdentifier(Ident);
    if Result <> qeNone then
      EXIT;

    TypeName := Ident
  end
  else
  begin
    Result := Parser.SkipWhite;
    if Result <> qeNone then
      EXIT;

    Result := ParseUniqueIdentifier(#0, TypeName);
    if Result <> qeNone then
      EXIT;
  end;

  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;

  if Parser.TestChar <> '=' then
    EXIT(Err(qeEqualExpectedInTYPE));

  Parser.SkipChar;
  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;

  Result := ParseTypeDefinition(TheType);
  if Result <> qeNone then
    EXIT;

  //Check they haven't declared a member with the name of the type
  if TheType.VarType = vtEnumeration then
    if TEnumeration(TheType).StringToEnumIndex(TypeName) <> -1 then
      EXIT(ErrSub(qeIdentifierRedeclared, TypeName));

  if TheType.Name = '' then
    //We have a newly declared type
    TheType.AssignName(TypeName)
  else
    //We have an existing type and need to
    //create a SYNONYM type pointing to original
    TTypes.CreateSynonym(TypeName, TheType);
(*    Types.AddSynonym(TypeName, TheType);*)
end;

end.
