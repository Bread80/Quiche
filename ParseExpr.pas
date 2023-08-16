unit ParseExpr;

interface
uses MErrors, ILData, QTypes, Operators;

//Unary prefix operators
type
  TUnaryOperator = (uoNone, uoNegate, uoInvert);
  //Is the operand size explicitly specified within the expression, or
  //implied. Explicitly sized operands should retain that size, implicit ones
  //may be extended or shortened to optimise the code generation
  TParamOrigin = (poImplicit, poExplicit);


//The expression parser breaks the input stream down into chunks of an operand and an
//operation. A 'slug' (for want of a better term) is one of those chunks.
type
  PExprSlug = ^TExprSlug;
  TExprSlug = record
    Operand: TILParam;  //The operand
    ParamOrigin: TParamOrigin;

    Negate: Boolean;    //Used when reading operators - negate next parameter (-)?
    Invert: Boolean;    //As above, invert next parameter (NOT)?

    OpIndex: Integer;    //Index into Operators list
    OpData: POperator;

    OpType: TOpType;    //Type for the operator
    ResultType: TVarType; //Type for the result
    ImplicitType: TVarType;//?? Type for type inference

    ILItem: PILItem;  //Returns the ILItem of a sub-expression or unary operators
                      //(Otherwise, nil);
  end;

function ParseExpressionToSlug(Slug: PExprSlug;var ExprType: TVarType): TAssembleError;

(*
//Parses an expression (or sub-expression) until we run out of operators
//Returns the IL list item of the final operation. The caller will need to
//add the Dest info as needed (i.e to assign the result to a variable, or temp
//location.
//ExprType: If a value other than vtUnkown is passed in, validates that the result
//type of the expression is compatible.
//If not returns the ResultType of the xpression in ExprType

//Does NOT validate that there isn't 'junk' after the expression,
//it is up to the caller to detect the proper context and valid ending of
//the expression. (I.e. during the setting up of a FOR loop the expression will
//be terminated by the TO identifier. In an parameter list the expression will
//be terminated by the comma or close brace (or end of line). This code is useable
//no matter the context.
function ParseExpression(out ILItem: PILItem;var ExprType: TVarType;
  out ImplicitType: TVarType): TAssembleError;
*)
implementation
uses SysUtils, Variables, ParserBase, Eval;

//===============================================
//Expressions

//Where the Slug has an ILItem, this routine creates a temp var and sets the
//ILItem's Dest data to point to it.
procedure SlugAssignToTempVar(Slug: PExprSlug);
begin
  Assert(Slug.ILItem <> nil);

  Slug.ILItem.Dest.Loc := locTemp;
  Slug.ILItem.Dest.TempIndex := GetNextTempIndex;
  VarCreateTemp(Slug.ILItem.Dest.TempIndex, Slug.ResultType);
  Slug.Operand.Loc := locTemp;
  Slug.Operand.TempIndex := Slug.ILItem.Dest.TempIndex;
end;



//==============================Type prececence
//These routines determine what happens when be combine types through an operator.
//The routines determine the appropriate operator type and result type for the
//(sub)-expression. The resulting values are then (mostly) assigned to the left
//side slug.

//Returns the type to be used in deciding the implicit type of the expression
function GetOperandImplicitType(Slug: PExprSlug): TVarType;
var Variable: PVariable;
begin
  case Slug.Operand.Loc of
    locNone, locPhiVar: Result := vtUnknown;
    locImmediate:
    begin
      if Slug.ParamOrigin = poExplicit then
        EXIT(Slug.Operand.ImmType);

      if IsSignedType(Slug.Operand.ImmType) then
        EXIT(Slug.Operand.ImmType);

      if Slug.Operand.ImmValue >= 32768 then
        EXIT(vtWord)
      else
        EXIT(vtInteger);
    end;
    locVar:
    begin
      Variable := VarIndexToData(Slug.Operand.VarIndex);
      Result := Variable.VarType;
    end;
    locTemp:
    begin
      Variable := VarTempToData(Slug.Operand.TempIndex);
      Result := Variable.VarType;
    end;
  else
    raise Exception.Create('Unknown Location for Parameter');
  end;
end;

//The ultimate destination of the routines below. This sets the fields of the
//slug once the values for those fields have been determined
procedure SetSlugTypes(var Slug: PExprSlug;ResultType: TVarType;OpType: TOpType);
begin
  if Slug.ResultType = vtUnknown then
    Slug.ResultType := ResultType;
  Slug.OpType := OpType;
end;

//This routine considers the type and value of an immediate (constant) operand
//and returns the appropriate type for operations etc.
//This routine considers:
//The ImmType of the operand,
//The Origin of the operand (implicit or explicit),
//and the value of the operand
function MinImmType(Imm: PExprSlug): TVarType;
begin
  if (Imm.ParamOrigin = poExplicit) and (Imm.Operand.ImmType in [vtByte, vtWord, vtPointer]) then
    EXIT(Imm.Operand.ImmType);

  //Signed types (we ignore ParamOrgin if unsigned)
  if IsSignedType(Imm.Operand.ImmType) then
  begin
    if Imm.Operand.ImmValue >= $10000-128 then
      EXIT(vtInt8)
    else if Imm.Operand.ImmValue >= 32768 then
      EXIT(vtInteger)
    else if Imm.Operand.ImmValue <= 255 then
      EXIT(vtByte)
    else
      EXIT(vtWord);
  end;

  EXIT(Imm.Operand.ImmType);
end;

//Where one operand is a Variable, and the other an Immediate value, this routine
//chooses appropriate Type for the operation and result type and sets those values into
//the Left hand slug parameter.
//If Swap is false the routine expects the Variable parameter to be the left hand one,
//if Swap is True the routine expects the Variable parameter to be the right hand one.
//(And in both cases the other parameter is the immediate (constant) one.

//Returns False if there is no valid combination of types, otherwise True.
function SetMinCommonVarImmediateType(var Left: PExprSlug; Right: PExprSlug;Swap: Boolean): Boolean;
var VType, ImmType: TVarType;
  ImmValue: Word;
begin
  if Swap then
  begin
    VType := ILParamToVarType(@Right.Operand);
    ImmType := MinImmType(Left);
    ImmValue := Left.Operand.ImmValue;
  end
  else
  begin
    VType := ILParamToVarType(@Left.Operand);
    ImmType := MinImmType(Right);
    ImmValue := Right.Operand.ImmValue;
  end;
  //Choose a type which combines the /type/ of the variable (VType) and the
  //size and sign of the immediate value.
  //I.e. if the VType is an Int8 and the immediate is 0 .. 127 the common type is an Int8
  //If VType is an Int16 and the immediate is 0..32767 the common type is an Int16
  //I.e. We can ignore the type of the immediate value. But Explicit type???

  if IsNumericType(VType) and IsNumericType(ImmType) then
  begin
    Result := True;
     if (VType = vtReal) or (ImmType = vtReal) then
      SetSlugTypes(Left, vtReal, rtReal)
    else if (VType = vtPointer) or (ImmType = vtPointer) then
    begin
      if IsSignedType(VType) or IsSignedType(ImmType) then
        //One type is a pointer so we need a U16 result
        SetSlugTypes(Left, vtPointer, rtM16U16)
      else
        SetSlugTypes(Left, vtPointer, rtU16)
    end
    else
    case VType of
      vtByte:
        if ImmValue <= 255 then
          SetSlugTypes(Left, vtByte, rtU8)
        else if IsSignedType(ImmType) then
          SetSlugTypes(left, vtByte, rtS16)
        else
          SetSlugTypes(Left, vtByte, rtU16);
      vtWord, vtPointer:
        if IsSignedType(ImmType) and (ImmValue >= 32768) then
          SetSlugTypes(Left, vtWord, rtM16U16)
        else
          SetSlugTypes(Left, vtWord, rtU16);
      vtInt8:
        if (IsSignedType(ImmType) and (ImmValue >= $10000-128)) or (ImmValue <= 127) then
          SetSlugTypes(Left, vtInt8, rtS8)
        else if IsSignedType(ImmType) then
          SetSlugTypes(Left, vtInt8, rtS16)
        else
          SetSlugTypes(Left, vtInt8, rtM16S16);
      vtInteger:
        if IsSignedType(ImmType) or (ImmValue <= 32767) then
          SetSlugTypes(Left, vtInteger, rtS16)
        else
          SetSlugTypes(Left, vtInteger, rtM16S16);
    else
      raise Exception.Create('Invalid numeric type in SetMinCommonVarImmediateType');
    end;
  end
  else
  begin
    Result := VType = ImmType;
    if Result then
      SetSlugTypes(Left, VType, VarTypeToOpType(VType));
  end;
end;

//Determines the operator type and result type for an operation. Used in cases
//where both operators are in Variables, or both are immediate (constant) values.
//Once the types have been determined, sets the appropriate fields in the Slug parameter

//Typing rules (where both operands need to have the same type)
//If either side is Real then the operation is Real and the result is Real (for future use)
//If either side is pointer then the operation is U16 or M16 and the result is U16
//If either side is signed then the operation is S16 or M16 and the result is S16
//Both sides are unsigned, result will be U8 if both sides are Byte, otherwise U16
//(An M16 operation is one where one side is signed and the other unsigned)

//Returns False if there is no valid combination of types, otherwise True.
function SetMinCommonType(var Slug: PExprSlug; LType, RType: TVarType): Boolean;
begin
  if IsNumericType(LType) and IsNumericType(RType) then
  begin
    Result := True;
     if (LType = vtReal) or (RType = vtReal) then
      SetSlugTypes(Slug, vtReal, rtReal)
    else if (LType = vtPointer) or (RType = vtPointer) then
    begin
      if IsSignedType(LType) or IsSignedType(RType) then
        //One type is a pointer so we need a U16 result
        SetSlugTypes(Slug, vtPointer, rtM16U16)
      else
        SetSlugTypes(Slug, vtPointer, rtU16)
    end
    else if (LType = vtByte) and (RType = vtByte) then
      SetSlugTypes(Slug, vtByte, rtU8)
    else if (LType = vtInt8) and (RType = vtInt8) then
      SetSlugTypes(Slug, vtInt8, rtS8)
    else if (LType in [vtInt8, vtByte]) and (RType in [vtInt8, vtByte]) then
      SetSlugTypes(Slug, vtInteger, rtS16)
    else if (LType in [vtByte, vtWord]) and (RType in [vtByte, vtWord]) then
      SetSlugTypes(Slug, vtWord, rtU16)
    else if (LType in [vtByte, vtInteger, vtInteger, vtInt8]) and (RType in [vtByte, vtInteger, vtInt8, vtInteger]) then
      SetSlugTypes(Slug, vtInteger, rtS16)
    else
      //One type is signed, the other unsigned but not a pointer so we need an S16 result
      SetSlugTypes(Slug, vtInteger, rtM16S16);
  end
  else
  begin
    Result := LType = RType;
    if Result then
      SetSlugTypes(Slug, LType, VarTypeToOpType(LType));
  end;
end;

//Where both operands of an operator are immediate values, this routine
//determines the Type of the operation and result, and sets those values into the
//Left slug parameter.

//Returns False if there is no valid combination of types, otherwise True.
function SetMinCommonImmediateType(var Left: PExprSlug; Right: PExprSlug): Boolean;
var LType, RType: TVarType;
begin
  LType := MinImmType(Left);
  RType := MinImmType(Right);
  Result := SetMinCommonType(Left, LType, RType);
end;

//Where both operands are Variables, this routine chooses the appropriate type
//for the operation and result, and sets those values into the Left slug parameter.

//Returns False if there is no valid combination of types, otherwise True.
function SetMinCommonVarType(var Left: PExprSlug;Right: PExprSlug): Boolean;
var LType, RType: TVarType;
begin
  LType := ILParamToVarType(@Left.Operand);
  RType := ILParamToVarType(@Right.Operand);
  Result := SetMinCommonType(Left, LType, RType);
end;

//For a boolean/bitwise operator, sets the type/size of boolean operation required

//Returns False if there is no valid combination of types, otherwise True.
function SetBooleanOperatorType(var Left: PExprSlug;Right: PExprSlug): Boolean;
var LType, RType: TVarType;
begin
  LType := ILParamToVarType(@Left.Operand);
  RType := ILParamToVarType(@Right.Operand);

  if IsNumericType(LType) and IsNumericType(RType) then
  begin
    Result := True;
    if (LType = vtReal) or (RType = vtReal) then
      EXIT(False)
    else if IsWordType(LType) or IsWordType(RType) then
      SetSlugTypes(Left, vtWord, rtX16)
    else  //Both are single byte
      SetSlugTypes(Left, vtByte, rtX8);
  end
  else
  begin
    Result := (LType = vtBoolean) and (RType = vtBoolean);
    if Result then
      SetSlugTypes(Left, vtBoolean, rtX8);
  end;
end;

//Sets the types in the left slug (ResultType, OpType fields) for the operation when
//used with the given input operands: Slug.Operand <Slug.Operation> RightSlug.Operand
//Also validates that the operator can be used with the given operand types and
//that there is a suitable primitive available (as noted in the OpTypes field of
//the Operands list.
//Returns False if operands are incompatible with the operator/available primitives
function AssignSlugTypes(var Slug: PExprSlug; RightSlug: PExprSlug): Boolean;
begin
  //Operation has a fixed result type
  if Slug.OpData.ResultType <> teUnknown then
    Slug.ResultType := TypeEnumToVarType[Slug.OpData.ResultType]
  else
    Slug.ResultType := vtUnknown;
  Slug.OpType := rtUnknown;

  //Both operands need to be of the same type - ascertain the common type
  if Slug.OpData.MatchOperands then
  begin
//    if Slug.OpData.Logical then
      //Boolean operators are special :)
//      SetBooleanOperatorType(Slug, RightSlug)
//    else
    if Slug.Operand.Loc = locImmediate then
      if RightSlug.Operand.Loc = locImmediate then
        //Two immediate parameters
        SetMinCommonImmediateType(Slug, RightSlug)
      else
        //Right is variable and left is immediate
        SetMinCommonVarImmediateType(Slug, RightSlug, True)
    else //Slug not Immediate
      if RightSlug.Operand.Loc = locImmediate then
        //Left is variable and right is immediate
        SetMinCommonVarImmediateType(Slug, RightSlug, False)
      else
        //Both are variables
        SetMinCommonVarType(Slug, RightSlug);
  end
  else //Unmatched operands - use the type of the left operand
  begin
    if Slug.OpData.ResultSame then
    begin
      Slug.ResultType := ILParamToVarType(@Slug.Operand);
      Slug.OpType := VarTypeToOpType(Slug.ResultType);
    end
    else
      Slug.OpType := VarTypeToOpType(ILParamToVarType(@Slug.Operand));
  end;

  //Failure if we haven't found suitable types yet
  Result := (Slug.ResultType <> vtUnknown) and (Slug.OpType <> rtUnknown);

  if Result and not Slug.OpData.Logical then
  begin
    //If no primitive is available for our OpType then upscale the
    //operation to something which does exist. If nothing is available then
    //error
    while not (Slug.OpType in Slug.OpData.OpTypes) do
      case Slug.OpType of
      rtS8: Slug.OpType := rtS16;
      rtU8: Slug.OpType := rtU16;
      else
        Slug.OpType := rtUnknown;
        EXIT(False);
      end;
  end;
end;

//Validates whether the ExprType can be assigned to the variable (etc) with AssignType
function ValidateAssignmentTypes(AssignType, ExprType: TVarType): Boolean;
begin
  //Numeric types
  if IsNumericType(AssignType) and IsNumericType(ExprType) then
    EXIT(ExprType <> vtReal);
  if AssignType = vtBoolean then
    EXIT(ExprType = vtBoolean);

  if AssignType = vtString then
    EXIT(ExprType in [vtChar, vtString]);
  if AssignType = vtChar then
    EXIT(ExprType = vtChar);

  Result := False;
end;

//Tests whether the expression returned by Slug is compatible with the type
//given in ExprType. If it is returns errNone, otherwise returns a suitable error code
function ValidateExprType(ExprType: TVarType; Slug: PExprSlug): TAssembleError;
var Negative: Boolean;
  Valid: Boolean;
begin
  if Slug.OpIndex = OpIndexNone then
  begin //Single value, no expression
    if Slug.Operand.Loc = locImmediate then
    begin
      Negative := ((Slug.Operand.ImmType = vtInt8) and (Slug.Operand.ImmValue and $80 <> 0)) or
        ((Slug.Operand.ImmType = vtInteger) and (Slug.Operand.ImmValue >= $8000));
      case ExprType of
        vtByte: Valid := not Negative and (Slug.Operand.ImmValue < 256);
        vtWord, vtPointer: Valid := not Negative;
        vtInt8: Valid := (Negative and (Slug.Operand.ImmValue >= $10000-128)) or
          (Slug.Operand.ImmValue < $80);
        vtInteger: Valid := Negative or (Slug.Operand.ImmValue < $8000);
      else
        Valid := ValidateAssignmentTypes(ExprType, Slug.ResultType);
      end;
    end
    else
      Valid := ValidateAssignmentTypes(ExprType, Slug.ResultType);
  end
  else
    //Expression
    Valid := ValidateAssignmentTypes(ExprType, Slug.ResultType);

  if not Valid then
    Result := errIncompatibleTypes
  else
    Result := errNone;
end;


//======================================Parsing literals

//Parses and returns an integer literal
function ParseInteger(Slug: PExprSlug): TAssembleError;
var
  Ch: Char;
  Value: Integer;
begin
  Parser.Mark;
  Value := 0;
  while True do
  begin
    Ch := Parser.TestChar;
    case Ch of
      '0'..'9':
        if (Value < 6553) or ((Value = 6553) and (Ch < '6')) then
          Value := Value * 10 + (ord(Ch) - ord('0'))
        else
          EXIT(errInvalidDecimalNumber);
      '.','e','E': EXIT(errTypeNotYetSupported);
      '_': ; //Ignore
    else
      Slug.Operand.Loc := locImmediate;
      Slug.Operand.ImmValue := Value;
      if Value < 256 then
        Slug.Operand.ImmType := vtByte
      else
        Slug.Operand.ImmType := vtWord;
      Slug.ParamOrigin := poImplicit;
      Slug.ImplicitType := vtInteger;
      EXIT(errNone);
    end;

    Parser.SkipChar;
  end;
end;

//Where the '-' sign has already been parsed
function ParseNegativeInteger(Slug: PExprSlug): TAssembleError;
begin
  Result := ParseInteger(Slug);
  if Result <> errNone then
    EXIT;

  if Slug.Operand.ImmValue <= 32768 then
  begin
    Slug.Operand.ImmValue := $10000-Slug.Operand.ImmValue;
    Slug.Operand.ImmType := vtInteger;
    Slug.ParamOrigin := poImplicit;
    Slug.ImplicitType := vtInteger;
  end
  else
    EXIT(errInvalidDecimalNumber);
end;


//Parses and returns a hex literal
//Parser must be pointing to
function ParseHex(Slug: PExprSlug): TAssembleError;
var
  Ch: Char;
  Ignore: Boolean;
  Digit: Integer;
  Digits: Integer;
  Value: Word;
begin
  if Parser.TestChar = '$' then
    Parser.SkipChar;

  Parser.Mark;
  Value := 0;
  Digits := 0;
  while True do
  begin
    Ch := Parser.TestChar;
    Ignore := Ch = '_';
    if not Ignore then
    begin
      case Ch of
      '0'..'9': Digit := ord(Ch) - ord('0');
      'a'..'f': Digit := ord(Ch) - ord('a') + 10;
      'A'..'F': Digit := ord(Ch) - ord('A') + 10;
      else
        Slug.Operand.Loc := locImmediate;
        Slug.Operand.ImmValue := Value;
        if Digits <= 2 then
          Slug.Operand.ImmType := vtByte
        else
          Slug.Operand.ImmType := vtPointer;
        Slug.ParamOrigin := poExplicit;
        Slug.ImplicitType := Slug.Operand.ImmType;
        EXIT(errNone);
      end;

      inc(Digits);
      if Value < $1000 then
        Value := Value * 16 + Digit
      else
        EXIT(errInvalidHexNumber);
    end;

    Parser.SkipChar;
  end;
end;

//Parses and returns a binary literal
function ParseBinary(Slug: PExprSlug): TAssembleError;
var
  Ch: Char;
  Digits: Integer;
  Value: Word;
begin
  if Parser.TestChar = '%' then
    Parser.SkipChar;

  Parser.Mark;
  Value := 0;
  Digits := 0;
  while True do
  begin
    Ch := Parser.TestChar;
    if Ch in ['0','1'] then
    begin
      inc(Digits);
      if Value < 32768 then
        Value := (Value shl 1) + (ord(Ch)-ord('0'))
      else
        EXIT(errInvalidBinaryNumber);
    end
    else if Ch <> '_' then
    begin
      Slug.Operand.Loc := locImmediate;
      Slug.Operand.ImmValue := Value;
      if Digits <= 8 then
        Slug.Operand.ImmType := vtByte
      else
        Slug.Operand.ImmType := vtWord;
      Slug.ParamOrigin := poExplicit;
      Slug.ImplicitType := Slug.Operand.ImmType;
      EXIT(errNone);
    end;

    Parser.SkipChar;
  end;
end;

//Parses and returns a quoted string or character
//Currently only handles single character strings (i.e. chars)
function ParseString(Slug: PExprSlug): TAssembleError;
var
  Ch: Char;
  S: String;
begin
  Parser.Mark;
  if Parser.TestChar = '''' then
    Parser.SkipChar;

  S := '';
  while True do
  begin
    Ch := Parser.TestChar;
    if Ch = #0 then
      EXIT(errUnterminatedString);
    if Ch = '''' then
    begin
      Parser.SkipChar;
      if Parser.TestChar = '''' then
      begin
        S := S + '''';
        Parser.SkipChar;
      end
      else
      begin
        Slug.Operand.Loc := locImmediate;
        if Length(S) = 1 then
        begin
          Slug.Operand.ImmValue := Ord(S.Chars[0]);
          Slug.Operand.ImmType := vtChar;
          Slug.ParamOrigin := poExplicit;
          Slug.ImplicitType := vtChar;
        end
        else
        begin
          EXIT(errTypeNotYetSupported);
        end;
        EXIT(errNone);
      end;
    end
    else
    begin
      S := S + Ch;
      Parser.SkipChar;
    end;
  end;
end;

//Parses and returns a character literal (# prefix)
//Syntax: #<constant>
//where <constant> can be a decimal, hex or binary value (with the appropriate
//prefixes).
function ParseCharLiteral(Slug: PExprSlug): TAssembleError;
var
  Ch: Char;
begin
  Parser.Mark;
  if Parser.TestChar = '#' then
    Parser.SkipChar;

  Ch := Parser.TestChar;
  case Ch of
    '0'..'9': Result := ParseInteger(@Slug.Operand);
    '$': Result := ParseHex(@Slug.Operand);
    '%': Result := ParseBinary(@Slug.Operand);
  else
    EXIT(errInvalidCharLiteral);
  end;
  if Result <> errNone then
    EXIT;

  Slug.Operand.Loc := locImmediate;
  if Slug.Operand.ImmValue < 256 then
  begin
    Slug.Operand.ImmType := vtChar;
    Slug.ParamOrigin := poExplicit;
    Slug.ImplicitType := vtChar;

    Result := errNone;
  end
  else
    Result := errInvalidCharLiteral;
end;


//=================================Complex operands, and operators

function ParseOperand(Slug: PExprSlug;UnaryOp: TUnaryOperator): TAssembleError; forward;

//Parses identifiers as expression parameters. Identifiers can be constants,
//variables or functions
function ParseOperandIdentifier(Slug: PExprSlug;Ident: String): TAssembleError;
var Keyword: TKeyword;
  V: PVariable;
begin
  Keyword := IdentToKeyword(Ident);
  case Keyword of
    keyFalse, keyTrue:
      begin
        Slug.Operand.Loc := locImmediate;
        Slug.ParamOrigin := poExplicit;
        if Keyword = keyFALSE then
          Slug.Operand.ImmValue := valueFalse
        else
          Slug.Operand.ImmValue := valueTrue and $ffff;
        Slug.Operand.ImmType := vtBoolean;
        Slug.ImplicitType := vtBoolean;
        Result := errNone;
      end;
      //*** NOT operator handled elsewhere***
  else //Variable or function
    V := VarFindByName(Ident, Slug.Operand.VarIndex);
    if V <> nil then
    begin //Variable;
      Slug.Operand.Loc := locVar;
      Slug.Operand.VarSub := V.Sub;
      Slug.ParamOrigin := poExplicit;
      Slug.ResultType := V.VarType;
      Slug.ImplicitType := V.VarType;
      Result := errNone;
    end
    else //function?
      EXIT(errBug);
  end;
end;

function DoUnaryOp(UnaryOp: TUnaryOperator;var Slug: PExprSlug): TAssembleError;
begin
  if ((UnaryOp = uoNegate) and IsNumericType(Slug.ImplicitType)) or
    ((UnaryOP = uoInvert) and IsLogicalType(Slug.ImplicitType)) then
    Result := errNone
  else //Unary operator with invalid operand type
    EXIT(errInvalidExpression);

  if Slug.ILItem <> nil then
    //We already have an ILItem created. If so, we need to set the Dest to
    //assign it to a temp var and use that in our calculations
    SlugAssignToTempVar(Slug);

  Slug.ILItem := ILAppend(dtData);
  Slug.ILItem.Param1 := Slug.Operand;
  Slug.ILItem.Param2.Loc := locNone;

  case UnaryOp of
    uoNegate:
    begin
      OpSymbolToData('negate', Slug.ILItem.OpIndex, Slug.OpData);
      case ILParamToVarType(@Slug.Operand) of
        vtByte:
        begin
          Slug.ILItem.OpType := rtU8;
          Slug.OpType := rtU8;
        end;
        vtWord:
        begin
          Slug.ILItem.OpType := rtU16;
          Slug.OpType := rtU16;
        end;
      else
        Slug.ILItem.OpType := rtS16;
        Slug.OpType := rtS16;
      end;
      Slug.ResultType := vtInteger;
      Slug.ILItem.ResultType := rtU16;
    end;
    uoInvert:
    begin
      OpSymbolToData('not', Slug.ILItem.OpIndex, Slug.OpData);
      case ILParamToVarType(@Slug.Operand) of
        vtInt8, vtByte:
        begin
          Slug.ILItem.OpType := rtX8;
          Slug.OpType := rtX8;
          Slug.ResultType := vtByte;
          Slug.ILItem.ResultType := rtU8;
        end;
        vtBoolean:
        begin
          Slug.ILItem.OpType := rtBoolean;
          Slug.OpType := rtBoolean;
          Slug.ResultType := vtBoolean;
          Slug.ILItem.ResultType := rtBoolean;
        end;
      else
        Slug.ILItem.OpType := rtX16;
        Slug.OpType := rtX16;
        Slug.ResultType := vtWord;
        Slug.ILItem.ResultType := rtU16;
      end;
    end;
  end;
end;



//Parses an operand (of an expression) and returns the data in Operand
//Also parses sub-expressions (in parenthesis)
//Unary operators ('-' and 'not') result in a recursive call back into ParseOperand
//with the UnaryOp parameter updated appropriately. Multiple prefixes of the same operator
//will be cancelled out by the recursive call returning the UnaryOp to uoNone.
//The unary operator will be inserted into the IL when we reach the end of the chain of
//(identical) unary operators, with tail recursion adding any previous unary operators
function ParseOperand(Slug: PExprSlug;UnaryOp: TUnaryOperator): TAssembleError;
var
  Ch: Char;
  Ident: String;
begin
  Parser.SkipWhiteSpaceAll;
  Parser.Mark;

  Ch := Parser.TestChar;
  case Ch of
  '(':
    begin //Sub-expressions
      Parser.SkipChar;

      Result := ParseExpressionToSlug(Slug, Slug.ResultType{, Slug.ImplicitType});
      if Result <> errNone then
        EXIT;

      //If the expression parser generated any IL code
      if Slug.ILItem <> nil then
        SlugAssignToTempVar(Slug);

      if Parser.TestChar <> ')' then
        EXIT(errUnmatchedBrackets);
      Parser.SkipChar;
      Result := errNone;
    end;
//  csIdentFirst
  'a'..'z','A'..'Z','_': //Identifiers - variables, functions or keyword values
  begin
    Result := ParseIdentifier(#0, Ident);
    if Result <> errNone then
      EXIT;

    if CompareText(Ident, 'not') = 0 then
    begin
      case UnaryOp of
        uoNone: Result := ParseOperand(Slug, uoInvert);
        uoNegate: Result := ParseOperand(Slug, uoInvert);
        uoInvert: Result := ParseOperand(Slug, uoNone);
      else
        EXIT(errBug);
      end;
      if UnaryOp = uoInvert then
        UnaryOp := uoNone;
    end
    else
      Result := ParseOperandIdentifier(Slug, Ident);

    if Result <> errNone then
      EXIT;
  end;
//  csDecimalFirst
  '0'..'9': //Numeric constants
    if UnaryOp = uoNegate then
    begin
      Result := ParseNegativeInteger(Slug);
      UnaryOp := uoNone;
    end
    else
      Result := ParseInteger(Slug);
  '$': //Hex constant
    Result := ParseHex(Slug);
  '%': //Binary constant
    Result := ParseBinary(Slug);
  '.': //Real constant
    EXIT(errTypeNotYetSupported);
  '''': //Char or string
    Result := ParseString(Slug);
  '#': //Character literal
    Result := ParseCharLiteral(Slug);
  '@': //Address prefix
    EXIT(errBug);
  '+': //Unary plus
  begin
    Parser.SkipChar;
    EXIT(ParseOperand(Slug, UnaryOp));
  end;
  '-': //Unary subtract/negative
  begin
    Parser.SkipChar;
    case UnaryOp of
      uoNone: Result := ParseOperand(Slug, uoNegate);
      uoNegate: Result := ParseOperand(Slug, uoNone);
      uoInvert: Result := ParseOperand(Slug, uoNegate);
    else
      EXIT(errBug);
    end;
    if Result <> errNone then
      EXIT;
    if UnaryOp = uoNegate then
      UnaryOp := uoNone;
  end;
  else
    EXIT(errInvalidExpression);
  end;

  //If Negate or Invert: Do that here
  //NOTE: Recurse for '-' clears Negate, Recurse for 'NOT' clears Invert
  //Also, if constant is negated, or negate is otherwise processed, Negate must be cleared
  //Ditto for Invert/NOT
  if UnaryOp <> uoNone then
    Result := DoUnaryOp(UnaryOp, Slug);
end;

//Parse and returns a *binary* operator (i.e. not unary ones) and it's precedence
//If at the end of an expression returns opNone.
function ParseOperator(Slug: PExprSlug): TAssembleError;
var
  Ch: Char;
  Ident: String;
begin
  Parser.SkipWhiteSpace;
  Parser.Mark;

  Slug.OpIndex := opIndexNone;
  Slug.OpData := nil;

  //End of (sub)-expression
  if Parser.TestChar in [')',';',#0] then
    EXIT(errNone);

  Parser.Mark;
  if not Parser.ReadChar(Ch) then
    EXIT(errOperatorExpected);

  case Ch of
//    '@': op := ilAt;

    '*','/','+','-','=':
      begin
        if not OpSymbolToData(Ch, Slug.OpIndex, Slug.OpData) then
        //Caller determines if what follows the expression is acceptable
          Parser.Undo;
      end;
    '<':
      begin
        Ch := Parser.TestChar;
        if Ch = '>' then
        begin
          OpSymbolToData('<>', Slug.OpIndex, Slug.OpData);
          Parser.SkipChar;
        end
        else if Ch = '=' then
        begin
          OpSymbolToData('<=', Slug.OpIndex, Slug.OpData);
          Parser.SkipChar;
        end
        else
          OpSymbolToData('<', Slug.OpIndex, Slug.OpData);
      end;
    '>':
      begin
        Ch := Parser.TestChar;
        if Ch = '=' then
        begin
          OpSymbolToData('>=', Slug.OpIndex, Slug.OpData);
          Parser.SkipChar;
        end
        else
          OpSymbolToData('>', Slug.OpIndex, Slug.OpData);
      end;
    'a'..'z','A'..'Z':
      begin
        Result := ParseIdentifier(Ch, Ident);
        if Result <> errNone then
          EXIT;
        if not OpSymbolToData(Ident, Slug.OpIndex, Slug.OpData) then
        //Caller determines if what follows the expression is acceptable
          Parser.Undo;
      end;
  else
    EXIT(errUnknownOperator);
  end;
  Result := errNone;
end;

//Parse out and returns a single 'slug' - an operand and the operator following
//it (or opNone)
function ParseExprSlug(Slug: PExprSlug): TAssembleError;
begin
  Slug.ResultType := vtUnknown;
  Slug.OpType := rtUnknown;
  Slug.ILItem := nil;
  Result := ParseOperand(@Slug.Operand, uoNone);
  if Result <> errNone then
    EXIT;

  Result := ParseOperator(Slug);
end;



//==============================Expressions

//Compares the precedences of the passed in slug and the following slug.
//If the following slug(s) is(are) higher precedence then recurses to parse
//that(those) slug(s). If not appends the current data to the IL list and
//returns.
//When it returns the Slug parameter will have been updated to reflect the
//current situation - i.e. next operator (and it's precedence) and the location
//and value of the operand (i.e. the temp index location of the result of the
//deeper operations.
//ILItem returns the final operation of the expression. The caller will need
//to update the Dest info as needed
function ParseSubExpression(Slug: PExprSlug;out ILItem: PILItem): TAssembleError;
var RightSlug: TExprSlug;
  TempIndex: Integer;
  EvalResult: Integer;
  EvalType: TVarType;
begin
  while True do
  begin
    //Next slug
    Result := ParseExprSlug(@RightSlug);
    if Result <> errNone then
      EXIT;

    //If the rightslug returned an ILItem then we need to set it's Dest to a temp var
    if RightSlug.ILItem <> nil then
      //We already have an ILItem created. If so, we need to set the Dest to
      //assign it to a temp var and use that in our calculations
      SlugAssignToTempVar(@RightSlug);

    //Is next slug higher precedence
    while (RightSlug.OpIndex <> OpIndexNone) and (Slug.OpData.Precedence < RightSlug.OpData.Precedence) do
    begin
      Result := ParseSubExpression(@RightSlug, ILItem);
      if Result <> errNone then
        EXIT;

      if not AssignSlugTypes(Slug, @RightSlug) then
        EXIT(errInvalidOperands);

      if ILItem <> nil then
      begin
        //Add sub-expression to IL list
        //with dest as temp data
        TempIndex := GetNextTempIndex;
        VarCreateTemp(TempIndex, Slug.ResultType);
        ILItem.Dest.Loc := locTemp;
        ILItem.Dest.TempIndex := TempIndex;
        //Update right slug for next iteration
        RightSlug.Operand.Loc := locTemp;
        RightSlug.Operand.TempIndex := TempIndex;
      end;
    end;

    EvalType := vtUnknown;
    if (Slug.Operand.Loc = locImmediate) and (RightSlug.Operand.Loc = locImmediate) then
    begin
      //If possible, evaluate and replace Slug.Operand
      Result := EvalBi(Slug.OpIndex, @Slug.Operand, @RightSlug.Operand, EvalResult, EvalType);
      if Result <> errNone then
        EXIT;
      if EvalType <> vtUnknown then
      begin
        Slug.Operand.ImmValue := EvalResult;
        Slug.Operand.ImmType := EvalType;
        Slug.OpType := VarTypeToOpType(EvalType);
        Slug.ResultType := EvalType;
        Slug.ImplicitType := EvalType;
        Slug.OpIndex := RightSlug.OpIndex;
        Slug.OpData := RightSlug.OpData;
        if RightSlug.OpIndex = OpIndexNone then
        begin
          ILItem := nil;
          EXIT(errNone);
        end;
      end;
    end;

    if EvalType = vtUnknown then
    begin //not Evaluated
      if not AssignSlugTypes(Slug, @RightSlug) then
          EXIT(errInvalidOperands);

      //Add current operation to list.
      //Dest info will be added by later
      ILItem := ILAppend(dtData);
//      if not AssignSlugTypes(Slug, @RightSlug) then
//        EXIT(errInvalidOperands);
      ILItem.OpType := Slug.OpType;
      ILItem.OpIndex := Slug.OpIndex;
      ILItem.ResultType := VarTypeToOpType(Slug.ResultType);
      ILItem.Param1 := Slug.Operand;
      ILItem.Param2 := RightSlug.Operand;

      //End of expression or lower precedence
      if (RightSlug.OpIndex = opIndexNone) or
        (Slug.OpData.Precedence > RightSlug.OpData.Precedence) then
      begin
        //Note: Dest info will be added by the caller
        //Update slug and return
        Slug.OpIndex := RightSlug.OpIndex;
        Slug.OpData := RightSlug.OpData;
        Slug.OpType := RightSlug.OpType;

        //...and return
        EXIT(errNone);
      end;

      //Same precedence
      //Add item to IL list
      //with dest as temp data
      TempIndex := GetNextTempIndex;
      VarCreateTemp(TempIndex, Slug.ResultType);

      ILItem.Dest.Loc := locTemp;
      ILItem.Dest.TempIndex := TempIndex;

      //Update slug data...
      Slug.Operand.Loc := locTemp;
      Slug.Operand.TempIndex := TempIndex;
//      Slug.Operand.VarType := ResultType;
      //Right slug operation becomes left slug operation
      Slug.OpIndex := RightSlug.OpIndex;
      Slug.OpData := RightSlug.OpData;
    end;
  end;
end;


function ParseExpressionToSlug(Slug: PExprSlug;var ExprType: TVarType): TAssembleError;
var ILItem: PILItem;
begin
  Parser.Mark;

  //Read the first slug of the expression
  Result := ParseExprSlug(Slug);
  if Result <> errNone then
    EXIT;

  //If no operation then the expression is just a single item - either a literal
  //or variable.
  //We'll populate the operation data, but the dest data will be added by the caller
  if Slug.OpIndex = opIndexNone then
  begin
    if Slug.ResultType = vtUnknown then
      Slug.ResultType := ILParamToVarType(@Slug.Operand);
    if Slug.OpType = rtUnknown then
      if Slug.Operand.Loc = locImmediate then
        case GetTypeSize(Slug.ResultType) of
          1: Slug.OpType := rtX8;
          2: Slug.OpType := rtX16;
        else
          raise Exception.Create('Unknown Assignment type size');
        end
      else
        Slug.OpType := VarTypeToOpType(Slug.ResultType);
    if ExprType <> vtUnknown then
    begin
      Result := ValidateExprType(ExprType, Slug);
      if Result <> errNone then
        EXIT;
    end
    else
      ExprType := Slug.ResultType;
//    if Assigned(Slug.ILData) then
//      Slug.ILData.ResultType := lutVarTypeToOpType[ExprType];

    Slug.ImplicitType := GetOperandImplicitType(@Slug.Operand);

    EXIT(errNone);
  end;

  //If the slug returned an ILItem then we need to set it's Dest to a temp var
  if Slug.ILItem <> nil then
    //We already have an ILItem created. If so, we need to set the Dest to
    //assign it to a temp var and use that in our calculations
    SlugAssignToTempVar(Slug);

  //Loop until end of expression
  while True do
  begin
    Result := ParseSubExpression(Slug, ILItem);
    if Result <> errNone then
      EXIT;

{    if ExprType <> vtUnknown then
    begin
      Result := ValidateExprType(ExprType, Slug);
      if Result <> errNone then
        EXIT;
    end
;}//    else
//      ExprType := Slug.ResultType;

//    ImplicitType := ExprType;

    if Slug.OpIndex <> OpIndexNone then
    begin
      Slug.Operand.Loc := locTemp;
      Slug.Operand.TempIndex := GetNextTempIndex;
      VarCreateTemp(Slug.Operand.TempIndex, Slug.ResultType);//ExprType);

      ILItem.Dest.Loc := locTemp;
      ILItem.Dest.TempIndex := Slug.Operand.TempIndex;
    end
    else //Slug.Op = opNone
    begin
      if ILItem = nil then
      begin
        if ExprType <> vtUnknown then
        begin
          Result := ValidateExprType(ExprType, Slug);
          if Result <> errNone then
            EXIT;
        end;

        if ExprType <> vtUnknown then
          Slug.ResultType := ExprType;
//      Slug.ImplicitType := ExprType;
      end
      else // ILItem <> nil
      begin
        Slug.ILItem := ILItem;
        if ExprType <> vtUnknown then
          ILItem.ResultType := VarTypeToOpType(ExprType)
        else
          ILItem.ResultType := VarTypeToOpType(Slug.ImplicitType);
      end;

      EXIT(errNone);
    end;
  end;
end;
(*
//See header section
function ParseExpression(out ILItem: PILItem;var ExprType: TVarType;out ImplicitType: TVarType): TAssembleError;
var Slug: TExprSlug;
begin
  Parser.Mark;

  //Read the first slug of the expression
  Result := ParseExprSlug(@Slug);
  if Result <> errNone then
    EXIT;

  //If no operation then the expression is just a single item - either a literal
  //or variable.
  //We'll populate the operation data, but the dest data will be added by the caller
  if Slug.OpIndex = opIndexNone then
  begin
    if Slug.ResultType = vtUnknown then
      Slug.ResultType := ILParamToVarType(@Slug);
    if Slug.OpType = rtUnknown then
      if ILItem.Param1.Loc = locImmediate then
        case GetTypeSize(Slug.ResultType) of
          1: ILItem.OpType := rtX8;
          2: ILItem.OpType := rtX16;
        else
          raise Exception.Create('Unknown Assignment type size');
        end
      else
        ILItem.OpType := VarTypeToOpType(ILParamToVarType(@ILItem.Param1));
//      Slug.OpType := lutVarTypeToOpType[Slug.ResultType];
    if ExprType <> vtUnknown then
    begin
      Result := ValidateExprType(ExprType, @Slug);
      if Result <> errNone then
        EXIT;
    end
    else
      ExprType := Slug.ResultType;
    ImplicitType := GetOperandImplicitType(@Slug.Operand);

    //If an ILItem was created for the parameter then we don't need to create one now
    if Slug.ILItem <> nil then
      ILItem := Slug.ILItem
    else
    begin
      //Add item to IL list
      ILItem := ILAppend(dtData);
      ILItem.OpIndex := OpIndexAssign;
      if ILItem.Param1.Loc = locImmediate then
        case GetTypeSize(Slug.ResultType) of
          1: ILItem.OpType := rtX8;
          2: ILItem.OpType := rtX16;
        else
          raise Exception.Create('Unknown Assignment type size');
        end
      else
        ILItem.OpType := VarTypeToOpType(ILParamToVarType(@ILItem.Param1));
      //(Don't need ResultType for an assign)
      ILItem.Param1 := Slug.Operand;
      ILItem.Param2.Loc := locNone;
    end;

    EXIT(errNone);
  end;

  //If the slug returned an ILItem then we need to set it's Dest to a temp var
  if Slug.ILItem <> nil then
    //We already have an ILItem created. If so, we need to set the Dest to
    //assign it to a temp var and use that in our calculations
    SlugAssignToTempVar(@Slug);

  //Loop until end of expression
  while True do
  begin
    Result := ParseSubExpression(@Slug, ILItem);
    if Result <> errNone then
      EXIT;

    if ExprType <> vtUnknown then
    begin
      Result := ValidateExprType(ExprType, @Slug);
      if Result <> errNone then
        EXIT;
    end
    else
      ExprType := Slug.ResultType;

    ImplicitType := ExprType;

    if Slug.OpIndex <> OpIndexNone then
    begin
      Slug.Operand.Loc := locTemp;
      Slug.Operand.TempIndex := GetNextTempIndex;
      VarCreateTemp(Slug.Operand.TempIndex, ExprType);

      ILItem.Dest.Loc := locTemp;
      ILItem.Dest.TempIndex := Slug.Operand.TempIndex;
    end
    else //Slug.Op = opNone
    begin
      if ILItem = nil then
      begin
        //Add item to IL list
        ILItem := ILAppend(dtData);
        ILItem.OpType := VarTypeToOpType(ExprType);
        ILItem.OpIndex := OpIndexNone;
        //No ResultType needed here??
        ILItem.Param1 := Slug.Operand;
        ILItem.Param2.Loc := locNone;
      end;

      EXIT(errNone);
    end;
  end;
end;
*)
end.
