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
//TODO                      | range of <type-definition>
//                      | array<array-bounds> of <type-definition>
//                      | <list-keyword>[<capacity>] of <type-definition>
//TODO                      | <list-type>[<capacity>]
//                      | <record-definition>
//TODO                      | <stream-definition>
//TODO                      | <function-definition>
function DoTYPE(const Ident: String): TQuicheError;

implementation
uses SysUtils,
  Def.Consts, Def.QTypes, Def.Scopes, Def.Variables, Def.Functions,
  Parse.Base, Parse.Source, Parse.Expr, Parse.FuncDef, Parse.VarDefs;

function ParseSubRangeDefinition(out TheType: PUserType): TQuicheError;
var ValueLow: TImmValue;
  ExprTypeLow: PUserType;
  ValueHigh: TImmValue;
  ExprTypeHigh: PUserType;
  CommonType: PUserType;  //Common type of upper and lower bounds
  Found: Boolean;
begin
  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;

  //Parse low bound
  ExprTypeLow := nil;
  Result := ParseConstantExpr(ValueLow, ExprTypeLow);
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

  Assert(Assigned(ExprTypeLow));
  if not IsOrdinalType(UTToVT(ExprTypeLow)) then
    EXIT(Err(qeOrdinalConstExprExpected));

  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  //Parse high bound
  ExprTypeHigh := nil;
  Result := ParseConstantExpr(ValueHigh, ExprTypeHigh);
  if Result = qeConstantExpressionExpected then
    EXIT(Err(qeOrdinalConstExprExpected));
  if Result <> qeNone then
    EXIT;

  Assert(Assigned(ExprTypeHigh));
  if not IsOrdinalType(UTToVT(ExprTypeHigh)) then
    EXIT(Err(qeOrdinalConstExprExpected));

  //Validate, and find a common type if required (ie for numeric types)
  CommonType := ExprTypeLow;
  if IsNumericType(UTToVT(ExprTypeLow)) then
  begin
    if not IsNumericType(UTToVT(ExprTypeHigh)) then
      EXIT(ErrSub2(qeRangeBoundsTypeMismatch, ExprTypeLow.Name, ExprTypeHigh.Name));

    //If we can't fit all Word values into an Integer type...
    //(This code is future proofed against wider Integer types)
    if GetMaxValue(vtInteger) < GetMaxValue(vtWord) then
      //...and the range of values is too wide to fit into any available Integer type
      if (ValueLow.IntValue < 0) and (ValueHigh.IntValue > 32767) then
        EXIT(ErrSub2(qeRangeExprValuesTooWide, ValueLow.ToString, ValueHigh.ToString));

    //Find most suitable common numeric type (if any)
    if ValueLow.IntValue < -128 then
      CommonType := ExprTypeLow
    else if ValueHigh.IntValue > 32767 then
      CommonType := ExprTypeHigh  //We want to preserve any Pointer typing so use parsed types
    else if ValueLow.IntValue < 0 then
      if ValueHigh.IntValue > 127 then
        CommonType := GetSystemType(vtInteger)
      else
        CommonType := GetSystemType(vtInt8)
    else  //ValueLow >= 0
      if ValueHigh.IntValue > 255 then
        CommonType := ExprTypeHigh
      else
        CommonType := GetSystemType(vtByte);
  end
  else
    case UTToVT(ExprTypeLow) of
      vtBoolean, vtChar:
        if UTToVT(ExprTypeLow) <> UTToVT(ExprTypeHigh) then
          EXIT(ErrSub2(qeRangeBoundsTypeMismatch, ExprTypeLow.Name, ExprTypeHigh.Name));
      vtEnumeration:
        if ExprTypeLow <> ExprTypeHigh then
          EXIT(ErrSub2(qeRangeBoundsTypeMismatch, ExprTypeLow.Name, ExprTypeHigh.Name));
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
//                        TheType will return an Array definition (vtArray or vtUnboundArray)


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

    TheType := Types.AddOfType(vtArray, OfType);
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
    TheType := Types.AddOfType(vtUnboundArray, OfType);
    TheType.BoundsType := nil;
  end
  else  //Element
    TheType := OfType;
end;

function ParseVectorOrListSize(out Size: Integer): TQuicheError;
var SizeValue: TImmValue;
  ExprType: PUserType;
begin
  Assert(Parser.TestChar = '[');

  Parser.SkipChar;
  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;
  ExprType := nil;
  Result := ParseConstantExpr(SizeValue, ExprType);
  if Result <> qeNone then
    EXIT;

  //Vectors and list can only have numeric sizes
  if not IsIntegerType(ExprType) then
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
//where <capacity> is any Integer value > 0
function ParseVectorOrListDefinition(ListType: TVarType;out TheType: PUserType): TQuicheError;
var Size: Integer;
  OfType: PUserType;
begin
  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  if Parser.TestChar = '[' then
  begin
    Result := ParseVectorOrListSize(Size);
    if Result <> qeNone then
      EXIT;
  end
  else
    Size := iUnboundedArray;

  if not TestForIdent('of') then
    EXIT(Err(qeOFExpected));

  Result := ParseTypeDefinition(OfType, False);
  if Result <> qeNone then
    EXIT;

  TheType := Types.AddOfType(ListType, OfType);
  case ListType of
    vtVector: TheType.VecLength := Size;
    vtList:   TheType.Capacity := Size;
  else
    assert(False);  //Invalid type
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
  case TheType.VarType of
    vtVector: TheType.VecLength := Size;
    vtList:   TheType.Capacity := Size;
  else
    Assert(False);  //Invalid type
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
  Scope: PScope;
  Cursor: TParseCursor;
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
    case Keyword of
      keyARRAY:
        if OrdinalTypesOnly then
          EXIT(Err(qeOrdinalTypeExpected))
        else
          Result := ParseArrayDefinition(TheType);
      keyFUNCTION:
        if OrdinalTypesOnly then
          EXIT(Err(qeOrdinalTypeExpected))
        else
          Result := ParseFuncDefinition(False, TheType);
      keyLIST:
        if OrdinalTypesOnly then
          EXIT(Err(qeOrdinalTypeExpected))
        else
          Result := ParseVectorOrListDefinition(vtList, TheType);
      keyPROCEDURE:
        if OrdinalTypesOnly then
          EXIT(Err(qeOrdinalTypeExpected))
        else
          Result := ParseFuncDefinition(True, TheType);
      keyRECORD:
        if OrdinalTypesOnly then
          EXIT(Err(qeOrdinalTypeExpected))
        else
          Result := ParseRecordDefinition(TheType);
      keySET:
        if OrdinalTypesOnly then
          EXIT(Err(qeOrdinalTypeExpected))
        else
          Result := ParseSetDefinition(TheType);
      keyVECTOR:
        if OrdinalTypesOnly then
          EXIT(Err(qeOrdinalTypeExpected))
        else
          Result := ParseVectorOrListDefinition(vtVector, TheType);
      keyUNKNOWN:
      begin //Not a keyword
        IdentData := SearchScopes(Ident, Scope, True);
        case IdentData.IdentType of
          itType:
          begin
            //If identifier is the name of a type we'll assume it's a type synonym
            //(or pointer)...
            if IsPointed then
              TheType := GetPointerToType(IdentData.T)
            else if (Parser.TestChar = '[') and (IdentData.T.VarType in [vtVector, vtList]) then
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
    if not IsOrdinalType(UTToVT(TheType)) then
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
