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
function ParseTypeDefinition(out TheType: PUserType;OrdinalTypesOnly: Boolean = False): TQuicheError;

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
  Def.Consts, Def.VarTypes, Def.Scopes, Def.Variables, Def.Functions, Def.Globals,
  Parse.Base, Parse.Source, Parse.Expr, Parse.FuncDef, Parse.VarDefs;

function ParseSubRangeDefinition(out TheType: PUserType): TQuicheError;
var ValueLow: TImmValue;
  ValueHigh: TImmValue;
  CommonType: PUserType;  //Common type of upper and lower bounds
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

  //Range operator
  Result := TestRangeOperator(Found);
  if Result = qeOperatorExpected then
    EXIT(Err(qeRangeOperatorExpected));
  if Result <> qeNone then
    EXIT;
  if not Found then
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
  CommonType := ValueLow.UserType;
  if IsNumericVarType(ValueLow.VarType) then
  begin
    if not IsNumericVarType(ValueHigh.VarType) then
      EXIT(ErrSub2(qeRangeBoundsTypeMismatch, ValueLow.UserType.Description, ValueHigh.UserType.Description));

    //If we can't fit all Word values into an Integer type...
    //(This code is future proofed against wider Integer types)
    if GetMaxValue(vtInteger) < GetMaxValue(vtWord) then
      //...and the range of values is too wide to fit into any available Integer type
      if (ValueLow.IntValue < 0) and (ValueHigh.IntValue > 32767) then
        EXIT(ErrSub2(qeRangeExprValuesTooWide, ValueLow.ToString, ValueHigh.ToString));

    //Find most suitable common numeric type (if any)
    if ValueLow.IntValue < -128 then
      CommonType := ValueLow.UserType
    else if ValueHigh.IntValue > 32767 then
      CommonType := ValueHigh.UserType  //We want to preserve any Pointer typing so use parsed types
    else if ValueLow.IntValue < 0 then
      if ValueHigh.IntValue > 127 then
        CommonType := GetSystemType(vtInteger)
      else
        CommonType := GetSystemType(vtInt8)
    else  //ValueLow >= 0
      if ValueHigh.IntValue > 255 then
        CommonType := ValueHigh.UserType
      else
        CommonType := GetSystemType(vtByte);
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

  TheType := Types.AddOfType(CommonType.VarType, CommonType);
//  TheType := Types.AddOfType('', vtSubRange, CommonType);
  TheType.IsSubRange := True;
  TheType.Low := ValueLow.ToInteger;
  TheType.High := ValueHigh.ToInteger;
end;

//If the first identifier is unknown, processes as an enumeration,
//If the first identifier is a constant, parses as a subrange (with constant expression)
//The leading ( has not been consumed
function ParseEnumDefinition(out TheType: PUserType): TQuicheError;
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
  TheType := Types.Add(vtEnumeration);
  TheType.EnumItems := Items;
  TheType.Low := 0;
  TheType.High := Length(Items)-1;
end;

function ParseSetDefinition(out TheType: PUserType): TQuicheError;
var OfType: PUserType;
begin
  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;

  if not TestForIdent('of') then
    EXIT(Err(qeOfExpected));

  Result := ParseTypeDefinition(OfType, True);
  if Result <> qeNone then
    EXIT;

  //TODO: When we implement data for sets we'll need to establish
  //whether the element count is suitable, and which internal set type to use

  TheType := Types.AddOfType(vtSetMem, OfType);
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
function ParseArrayDefinition(out TheType: PUserType;InBounds: Boolean = False): TQuicheError;

  //Parse a (Bounds:Element) pair
  function Recurse(out TheType: PUserType): TQuicheError;
  var Bounds: PUserType;
    OfType: PUserType;
  begin
    Parser.SkipChar;
    Result := ParseTypeDefinition(Bounds, True);
    if Result <> qeNone then
      EXIT;
    Result := ParseArrayDefinition(OfType, True);
    if Result <> qeNone then
      EXIT;

    TheType := Types.AddOfType(vtArrayType, OfType);
    TheType.ArrayDef.ArrayType := atArray;
    if (TheType.High-TheType.Low+1) > 255 then
      TheType.ArrayDef.ArraySize := asLong
    else
      TheType.ArrayDef.ArraySize := asShort;
    TheType.ArrayDef.IsUnbounded := False;
    TheType.ArrayDef.ElementSize := GetTypeDataSize(OfType);
    TheType.BoundsType := Bounds;
  end;

var
  OfType: PUserType;
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
  begin //Unbounded array
    TheType := Types.AddOfType(vtArrayType, OfType);
    TheType.ArrayDef.ArrayType := atArray;
    TheType.ArrayDef.ArraySize := asUnknown;
    TheType.ArrayDef.IsUnbounded := True;
    TheType.ArrayDef.ElementSize := GetTypeDataSize(OfType);
    TheType.BoundsType := nil;
  end
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
function ParseVectorOrListDefinition(ArrayType: TArrayType;ArraySize: TArraySize;out TheType: PUserType): TQuicheError;
var Size: Integer;
  OfType: PUserType;
  IsUnbounded: Boolean;
begin
  assert(ArrayType in [atVector, atList]);

  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  IsUnbounded := Parser.TestChar <> '[';
  if IsUnbounded then
    Size := 0
  else
  begin
    Result := ParseVectorOrListSize(Size);
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
    if Size > 255 then
      ArraySize := asLong
    else
      ArraySize := optDefaultArraySize;
  end
  else
    if (Size > 255) and (ArraySize <> asLong) then
      EXIT(Err(qeConstantOutOfRange));

  TheType := Types.AddOfType(vtArrayType, OfType);
  TheType.ArrayDef.ArrayType := ArrayType;
  TheType.ArrayDef.ArraySize := ArraySize;
  TheType.ArrayDef.IsUnbounded := IsUnbounded;
  TheType.ArrayDef.ElementSize := GetTypeDataSize(OfType);
  case ArrayType of
    atVector: TheType.VectorLength := Size;
    atList: TheType.ListCapacity := Size;
  else
    Assert(False);
  end;
end;

function BakeArrayType(FromType: PUserType;out TheType: PUserType): TQuicheError;
var Size: Integer;
  BaseType: PUserType;
begin
  Result := ParseVectorOrListSize(Size);
  if Result <> qeNone then
    EXIT;

  BaseType := GetBaseType(FromType);
  TheType := Types.AddOfType(BaseType.VarType, BaseType.OfType);
  TheType.ArrayDef.ArrayType := BaseType.ArrayDef.ArrayType;
  TheType.ArrayDef.ArraySize := BaseType.ArrayDef.ArraySize;
  TheType.ArrayDef.IsUnbounded := False;
  TheType.ArrayDef.ElementSize := BaseType.ArrayDef.ElementSize;
  case TheType.ArrayDef.ArrayType of
    atVector: TheType.VectorLength := Size;
    atList:   TheType.ListCapacity := Size;
  else
    raise EVarType.Create; //Invalid type
  end;
end;

function ParseRecordFields(Scope: PScope): TQuicheError;
var Ident: String;
  Keyword: TKeyword;
  V: PVariable;
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
        Size := GetTypeSize(V.UserType);
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
function ParseRecordDefinition(out TheType: PUserType): TQuicheError;
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
  TheType := Types.Add(vtRecord);
  TheType.Scope := ScopeToScopeHandle(Scope);
end;

//If IsProc True we're parsing a PROCEDURE, otherwise we're parsing a FUNCTION
function ParseFuncDefinition(IsProc: Boolean;out TheType: PUserType): TQuicheError;
var
  Func: PFunction;
begin
  Result := DoFUNCTION(IsProc, fptTypeDef, Func);
  if Result <> qeNone then
    EXIT;

  TheType := Types.Add(vtFunction);
  TheType.Func := TFunctionHandle(Func);
end;

function ParseTypeDefinition(out TheType: PUserType;OrdinalTypesOnly: Boolean = False): TQuicheError;
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
          IdentData := GetCurrentScope.SearchAllInScope(Ident, True);
          case IdentData.IdentType of
            itType:
            begin
              //If identifier is the name of a type we'll assume it's a type synonym
              //(or pointer)...
              if IsPointed then
                TheType := GetPointerToType(IdentData.T)
              else if (Parser.TestChar = '[') and
                (IdentData.T.VarType = vtArrayType) and
                (IdentData.T.ArrayDef.ArrayType in [atVector, atList]) then
                Result := BakeArrayType(IdentData.T, TheType)
              else
                TheType := IdentData.T;
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
  TheType: PUserType;
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

  if TheType.VarType = vtEnumeration then
    if IdentToEnumIndex(TheType, TypeName) <> -1 then
      EXIT(ErrSub(qeIdentifierRedeclared, TypeName));

  if TheType.Name = '' then
    //We have a newly declared type
    TheType.Name := TypeName
  else
    //We have an existing type and need to
    //create a SYNONYM type pointing to original
    Types.AddSynonym(TypeName, TheType);
end;

end.
