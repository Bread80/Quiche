unit ParseExpr;

interface
uses ParseErrors, ILData, QTypes, Operators;

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
//  PExprSlug = ^TExprSlug;
  TExprSlug = record
    //NOTE: If ILItem is non-nil then Operand will be ignored
    ILItem: PILItem;  //Returns the ILItem of a sub-expression or unary operators
                      //(Otherwise, nil);

    Operand: TILParam;  //The operand
    ParamOrigin: TParamOrigin;

    Negate: Boolean;    //Used when reading operators - negate next parameter (-)?
    Invert: Boolean;    //As above, invert next parameter (NOT)?

    OpIndex: Integer;    //Index into Operators list
    OpData: POperator;

    OpType: TOpType;    //Type for the operator
    ResultType: TVarType; //Type for the result
    ImplicitType: TVarType;//?? Type for type inference

    //Where the Slug has an ILItem, this routine creates a temp var and sets the
    //ILItem's Dest data to point to it.
    procedure AssignToHiddenVar;

    procedure SetImmediate(AImmValue: Integer;AImmType: TVarType);
  end;

function ParseExpressionToSlug(var Slug: TExprSlug;var ExprType: TVarType): TQuicheError;

//Parses an expression to an ILItem with a single parameter and no operation
//Dest and Operation data *must* be assigned by the caller
function ParseExprToILItem(out ILItem: PILItem;out VType: TVarType): TQuicheError;


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
*)
implementation
uses SysUtils, Variables, ParserBase, Eval, Globals, Scopes, ParseIntrinsics;

//===============================================
//Expressions

//Where the Slug has an ILItem, this routine creates a temp var and sets the
//ILItem's Dest data to point to it.
procedure TExprSlug.AssignToHiddenVar;
var Index: Integer;
begin
  Assert(ILItem <> nil);
  ILItem.Dest.CreateAndSetHiddenVar(ResultType, optDefaultVarStorage, Index);
  Operand.SetVarFromIndex(Index);
end;

procedure TExprSlug.SetImmediate(AImmValue: Integer;AImmType: TVarType);
begin
  Assert(ILItem = nil);
  Operand.SetImmediate(AImmValue, AImmType);
  ResultType := AImmType;
end;




//==============================Type prececence
//These routines determine what happens when be combine types through an operator.
//The routines determine the appropriate operator type and result type for the
//(sub)-expression. The resulting values are then (mostly) assigned to the left
//side slug.

//Returns the type to be used in deciding the implicit type of the expression
function GetOperandImplicitType(const Slug: TExprSlug): TVarType;
begin
  Result := Slug.Operand.GetVarType;
  if Slug.Operand.Loc = locImmediate then
  begin
    if Slug.ParamOrigin = poExplicit then
      EXIT;

    if IsSignedType(Slug.Operand.ImmType) then
      EXIT;

    if Slug.Operand.ImmValue >= 32768 then
      EXIT(vtWord)
    else
      EXIT(vtInteger);
  end;
end;

//The ultimate destination of the routines below. This sets the fields of the
//slug once the values for those fields have been determined
procedure SetSlugTypes(var Slug: TExprSlug;ResultType: TVarType;OpType: TOpType);
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
function MinImmType(const Imm: TExprSlug): TVarType;
begin
  if (Imm.ParamOrigin = poExplicit) and (Imm.Operand.ImmType in [vtByte, vtWord, vtPointer]) then
    EXIT(Imm.Operand.ImmType);

  //Signed types (we ignore ParamOrgin if unsigned)
  if IsSignedType(Imm.Operand.ImmType) then
  begin
    if Imm.Operand.ImmValue >= (GetMinValue(vtInt8) and $ffff){ $10000-128} then
      EXIT(vtInt8)
    else if Imm.Operand.ImmValue >= GetMaxValue(vtInteger) then
      EXIT(vtInteger)
    else if Imm.Operand.ImmValue <= GetMaxValue(vtByte) then
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
function SetMinCommonVarImmediateType(var Left: TExprSlug;const Right: TExprSlug;Swap: Boolean): Boolean;
var VType, ImmType: TVarType;
  ImmValue: Word;
begin
  if Swap then
  begin
    VType := Right.Operand.GetVarType;
    ImmType := MinImmType(Left);
    ImmValue := Left.Operand.ImmValue;
  end
  else
  begin
    VType := Left.Operand.GetVarType;
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
function SetMinCommonType(var Slug: TExprSlug; LType, RType: TVarType): Boolean;
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
function SetMinCommonImmediateType(var Left: TExprSlug;const Right: TExprSlug): Boolean;
var LType, RType: TVarType;
begin
  LType := MinImmType(Left);
  RType := MinImmType(Right);
  Result := SetMinCommonType(Left, LType, RType);
end;

//Where both operands are Variables, this routine chooses the appropriate type
//for the operation and result, and sets those values into the Left slug parameter.

//Returns False if there is no valid combination of types, otherwise True.
function SetMinCommonVarType(var Left: TExprSlug;const Right: TExprSlug): Boolean;
var LType, RType: TVarType;
begin
  LType := Left.Operand.GetVarType;
  RType := Right.Operand.GetVarType;
  Result := SetMinCommonType(Left, LType, RType);
end;

//For a boolean/bitwise operator, sets the type/size of boolean operation required

//Returns False if there is no valid combination of types, otherwise True.
function SetBooleanOperatorType(var Left: TExprSlug;const Right: TExprSlug): Boolean;
var LType, RType: TVarType;
begin
  LType := Left.Operand.GetVarType;
  RType := Right.Operand.GetVarType;

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
function AssignSlugTypes(var Left: TExprSlug;const Right: TExprSlug): Boolean;
begin
  //Operation has a fixed result type
  if Left.OpData.ResultType <> teUnknown then
  begin
    Left.ResultType := TypeEnumToVarType[Left.OpData.ResultType];
    Left.ImplicitType := Left.ResultType;
  end
  else
    Left.ResultType := vtUnknown;
  Left.OpType := rtUnknown;

  //Both operands need to be of the same type - ascertain the common type
  if Left.OpData.MatchOperands then
  begin
//    if Slug.OpData.Logical then
      //Boolean operators are special :)
//      SetBooleanOperatorType(Slug, RightSlug)
//    else
    if Left.Operand.Loc = locImmediate then
      if Right.Operand.Loc = locImmediate then
        //Two immediate parameters
        SetMinCommonImmediateType(Left, Right)
      else
        //Right is variable and left is immediate
        SetMinCommonVarImmediateType(Left, Right, True)
    else //Slug not Immediate
      if Right.Operand.Loc = locImmediate then
        //Left is variable and right is immediate
        SetMinCommonVarImmediateType(Left, Right, False)
      else
        //Both are variables
        SetMinCommonVarType(Left, Right);
  end
  else //Unmatched operands - use the type of the left operand
  begin
    if Left.OpData.ResultSame then
    begin
      Left.ResultType := Left.Operand.GetVarType;
      Left.OpType := VarTypeToOpType(Left.ResultType);
    end
    else
      Left.OpType := VarTypeToOpType(Left.Operand.GetVarType);
  end;

  //Failure if we haven't found suitable types yet
  Result := (Left.ResultType <> vtUnknown) and (Left.OpType <> rtUnknown);

  if Result and not Left.OpData.Logical then
  begin
    //If no primitive is available for our OpType then upscale the
    //operation to something which does exist. If nothing is available then
    //error
    while not (Left.OpType in Left.OpData.OpTypes) do
      case Left.OpType of
      rtS8: Left.OpType := rtS16;
      rtU8: Left.OpType := rtU16;
      else
        Left.OpType := rtUnknown;
        EXIT(False);
      end;
  end;
end;


//Tests whether the expression returned by Slug is compatible with the type
//given in ExprType. If it is returns errNone, otherwise returns a suitable error code
function ValidateExprType(ExprType: TVarType;const Slug: TExprSlug): TQuicheError;
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
        Valid := ValidateAssignmentType(ExprType, Slug.ResultType);
      end;
    end
    else
      Valid := ValidateAssignmentType(ExprType, Slug.ResultType);
  end
  else
    //Expression
    Valid := ValidateAssignmentType(ExprType, Slug.ResultType);

  if not Valid then
    Result := ErrMsg(qeTypeMismatch, 'Incompatible types: ' + VarTypeToName(Slug.ResultType) +
      ' and ' + VarTypeToName(ExprType))
  else
    Result := qeNone;
end;


//======================================Parsing literals

//Parses and returns an integer literal
function ParseInteger(var Slug: TExprSlug): TQuicheError;
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
          EXIT(Err(qeInvalidDecimalNumber));
      '.','e','E': EXIT(ErrMsg(qeTODO, 'Floating point numbers are not yet supported :('));
      '_': ; //Ignore
    else
      if Value < 256 then
        Slug.SetImmediate(Value, vtByte)
      else
        Slug.SetImmediate(Value, vtWord);
      Slug.ParamOrigin := poImplicit;
      Slug.ImplicitType := vtInteger;
      EXIT(qeNone);
    end;

    Parser.SkipChar;
  end;
end;

//Where the '-' sign has already been parsed
function ParseNegativeInteger(var Slug: TExprSlug): TQuicheError;
begin
  Result := ParseInteger(Slug);
  if Result <> qeNone then
    EXIT;

  if Slug.Operand.ImmValue <= 32768 then
  begin
    Slug.SetImmediate($10000-Slug.Operand.ImmValue, vtInteger);
    Slug.ParamOrigin := poImplicit;
    Slug.ImplicitType := vtInteger;
  end
  else
    EXIT(Err(qeInvalidDecimalNumber));
end;


//Parses and returns a hex literal
//Parser must be pointing to
function ParseHex(var Slug: TExprSlug): TQuicheError;
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
        if Digits < 2 then
          Slug.SetImmediate(Value, vtByte)
        else
          Slug.SetImmediate(Value, vtPointer);
        Slug.ParamOrigin := poExplicit;
        Slug.ImplicitType := Slug.Operand.ImmType;
        EXIT(qeNone);
      end;

      inc(Digits);
      if Value < $1000 then
        Value := Value * 16 + Digit
      else
        EXIT(Err(qeInvalidHexNumber));
    end;

    Parser.SkipChar;
  end;
end;

//Parses and returns a binary literal
function ParseBinary(var Slug: TExprSlug): TQuicheError;
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
        EXIT(Err(qeInvalidBinaryNumber));
    end
    else if Ch <> '_' then
    begin
      if Digits <= 8 then
        Slug.SetImmediate(Value, vtByte)
      else
        Slug.SetImmediate(Value, vtWord);
      Slug.ParamOrigin := poExplicit;
      Slug.ImplicitType := Slug.Operand.ImmType;
      EXIT(qeNone);
    end;

    Parser.SkipChar;
  end;
end;

//Parses and returns a quoted string or character
//Currently only handles single character strings (i.e. chars)
function ParseString(var Slug: TExprSlug): TQuicheError;
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
      EXIT(Err(qeUnterminatedString));
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
        if Length(S) = 1 then
        begin
          Slug.SetImmediate(Ord(S.Chars[0]), vtChar);
          Slug.ParamOrigin := poExplicit;
          Slug.ImplicitType := vtChar;
        end
        else
        begin
          EXIT(ErrMsg(qeTODO, 'Strings are not yet supported :('));
        end;
        EXIT(qeNone);
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
function ParseCharLiteral(var Slug: TExprSlug): TQuicheError;
var
  Ch: Char;
begin
  Parser.Mark;
  if Parser.TestChar = '#' then
    Parser.SkipChar;

  Ch := Parser.TestChar;
  case Ch of
    '0'..'9': Result := ParseInteger(Slug);
    '$': Result := ParseHex(Slug);
    '%': Result := ParseBinary(Slug);
  else
    EXIT(Err(qeInvalidCharLiteral));
  end;
  if Result <> qeNone then
    EXIT;

  if Slug.Operand.ImmValue < 256 then
  begin
    Slug.Operand.ImmType := vtChar;
    Slug.ParamOrigin := poExplicit;
    Slug.ImplicitType := vtChar;

    Result := qeNone;
  end
  else
    Result := Err(qeInvalidCharLiteral);
end;


//=================================Complex operands, and operators

function ParseOperand(var Slug: TExprSlug;UnaryOp: TUnaryOperator): TQuicheError; forward;

//Parses identifiers as expression parameters. Identifiers can be constants,
//variables or functions
function ParseOperandIdentifier(var Slug: TExprSlug;Ident: String): TQuicheError;
var IdentType: TIdentType;
  Scope: PScope;
  Item: Pointer;
  Index: Integer;
  V: PVariable;
  OpIndex: Integer;
  VarType: TVarType;
begin
  //System constants - TODO - this needs to be somewhere else!
  if CompareText(Ident, 'False') = 0 then
  begin
    Slug.SetImmediate(valueFalse, vtBoolean);
    Slug.ParamOrigin := poExplicit;
    Slug.ImplicitType := vtBoolean;
    EXIT(qeNone);
  end;
  if CompareText(Ident, 'True') = 0 then
  begin
    Slug.SetImmediate(valueTrue, vtBoolean);
    Slug.ParamOrigin := poExplicit;
    Slug.ImplicitType := vtBoolean;
    EXIT(qeNone);
  end;
  if CompareText(Ident, 'MinInt') = 0 then
  begin
    Slug.SetImmediate(GetMinValue(vtInteger), vtInteger);
    Slug.ParamOrigin := poExplicit;
    Slug.ImplicitType := vtInteger;
    EXIT(qeNone);
  end;
  if CompareText(Ident, 'MaxInt') = 0 then
  begin
    Slug.SetImmediate(GetMaxValue(vtInteger), vtInteger);
    Slug.ParamOrigin := poExplicit;
    Slug.ImplicitType := vtInteger;
    EXIT(qeNone);
  end;


  //Variable or function or const
  //Search everything we can see
  if SearchScopes(Ident, IdentType, Scope, Item, Index) then
  begin
    case IdentType of
      itVar:
      begin
        V := PVariable(Item);
        Slug.Operand.SetVar(Index, V.WriteCount);
        Slug.ParamOrigin := poExplicit;
        Slug.ResultType := V.VarType;
        Slug.ImplicitType := V.VarType;
        EXIT(qeNone);
      end;
      itFunction: EXIT(ErrMsg(qeTODO, 'Function dispatch (in expressions) not yet implemeneted'));
      itConst: EXIT(ErrMsg(qeTODO, 'Constant lookup (in expressions) not yet implemented'));
      itType: EXIT(ErrMsg(qeTODO, 'Typecasts for user types not yet implemented. Type names not allowed in expressions'));
    else
      EXIT(ErrMsg(qeBUG, 'Invalid/unknown IdentType in ParseOperandIentifier'));
    end;
  end
  else
  begin
    //Test for type names. If it is a typecast it will be followed by open bracket.
    //If not it's a typename
    if Parser.TestChar <> '(' then
    begin
      VarType := StringToVarType(Ident);
      if VarType <> vtUnknown then
      begin
        Slug.SetImmediate(VarType, vtType);
        Slug.ParamOrigin := poExplicit;
        Slug.ImplicitType := vtType;
        EXIT(qeNone);
      end;
    end;

    //Test for intrinsic functions
    OpIndex := OpFunctionToIndex(Ident);
    if OpIndex <> -1 then
      if OpIndexToData(OpIndex).OpGroup = ogProc then
        EXIT(ErrOpUsage(ermCantAssignProcedure, OpIndex))
      else
        Result := ParseIntrinsic(OpIndex, True, Slug)
    else
    begin
      //TODO: Search builtin function library
      EXIT(ErrMsg(qeTODO, 'Library function lookup not yet implemented'));

      //If we get here then it's 'Identifier not found/not declared'
    end;
  end;
end;

function DoUnaryOp(UnaryOp: TUnaryOperator;var Slug: TExprSlug): TQuicheError;
var EvalResult: Integer;
  EvalType: TVarType;
begin
  case UnaryOp of
    uoNegate:
      Slug.OpIndex := OpIndexNegate;
    uoInvert:
      Slug.OpIndex := OpIndexNOT;
  else
    raise Exception.Create('Unknown unary operator');
  end;

  if ((UnaryOp = uoNegate) and IsNumericType(Slug.ImplicitType)) or
    ((UnaryOP = uoInvert) and IsLogicalType(Slug.ImplicitType)) then
    Result := qeNone
  else //Unary operator with invalid operand type
    EXIT(ErrOpUsage('Incorrect parameter type: ' + VarTypeToName(Slug.ImplicitType), Slug.OpIndex));


  Slug.OpData := OpIndexToData(Slug.OpIndex);

  if (Slug.ILItem = nil) and (Slug.Operand.Loc = locImmediate) then
  begin
    Result := EvalUnary(Slug.OpIndex, @Slug.Operand, EvalResult, EvalType);
    if Result <> qeNone then
      EXIT;
    Slug.SetImmediate(EvalResult, EvalType);
    Slug.OpIndex := OpIndexNone;
    Slug.OpData := nil;
    Slug.ImplicitType := EvalType;
    EXIT;
  end;

  if Slug.ILItem <> nil then
    //We already have an ILItem created. If so, we need to set the Dest to
    //assign it to a temp var and use that in our calculations
    Slug.AssignToHiddenVar;

  Slug.ILItem := ILAppend(dtData, Slug.OpIndex);
  Slug.ILItem.Param1 := Slug.Operand;
  Slug.ILItem.Param2.Loc := locNone;
  Slug.ILItem.OpType := VarTypeToOpType(Slug.ResultType);
  Slug.ILItem.ResultType := Slug.ILItem.OpType;

//  Slug.ResultType := ILParamToVarType(@Slug.Operand);
//  Slug.OpType := VarTypeToOpType(Slug.ResultType);
end;



//Parses an operand (of an expression) and returns the data in Operand
//Also parses sub-expressions (in parenthesis)
//Unary operators ('-' and 'not') result in a recursive call back into ParseOperand
//with the UnaryOp parameter updated appropriately. Multiple prefixes of the same operator
//will be cancelled out by the recursive call returning the UnaryOp to uoNone.
//The unary operator will be inserted into the IL when we reach the end of the chain of
//(identical) unary operators, with tail recursion adding any previous unary operators
function ParseOperand(var Slug: TExprSlug;UnaryOp: TUnaryOperator): TQuicheError;
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
      if Result <> qeNone then
        EXIT;

      //If the expression parser generated any IL code
      if Slug.ILItem <> nil then
        Slug.AssignToHiddenVar;

      if Parser.TestChar <> ')' then
        EXIT(Err(qeUnmatchedBrackets));
      Parser.SkipChar;
      Result := qeNone;
    end;
//  csIdentFirst
  'a'..'z','A'..'Z','_': //Identifiers - variables, functions or keyword values
  begin
    Result := ParseIdentifier(#0, Ident);
    if Result <> qeNone then
      EXIT;

    if CompareText(Ident, 'not') = 0 then
    begin
      case UnaryOp of
        uoNone: Result := ParseOperand(Slug, uoInvert);
        uoNegate: Result := ParseOperand(Slug, uoInvert);
        uoInvert: Result := ParseOperand(Slug, uoNone);
      else
        EXIT(ErrMsg(qeBUG, 'Unknown/invalid unary operator'));
      end;
      if UnaryOp = uoInvert then
        UnaryOp := uoNone;
    end
    else
      Result := ParseOperandIdentifier(Slug, Ident);

    if Result <> qeNone then
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
    EXIT(ErrMsg(qeTODO, 'Floating point numbers not yet supported'));
  '''': //Char or string
    Result := ParseString(Slug);
  '#': //Character literal
    Result := ParseCharLiteral(Slug);
  '@': //Address prefix
    EXIT(ErrMsg(qeTODO, '''@'' operator not yet supported'));
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
      EXIT(errMsg(qeBUG, 'Unknown/invalid unary operator'));
    end;
    if Result <> qeNone then
      EXIT;
    if UnaryOp = uoNegate then
      UnaryOp := uoNone;
  end;
  else
    EXIT(Err(qeOperandExpected));
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
function ParseOperator(var Slug: TExprSlug): TQuicheError;
var
  Ch: Char;
  Ident: String;
begin
  Parser.SkipWhiteSpace;
  Parser.Mark;

  Slug.OpIndex := opIndexNone;
  Slug.OpData := nil;

  //End of (sub)-expression
  if Parser.TestChar in [')',';',',',#0] then
    EXIT(qeNone);

  Parser.Mark;
  if not Parser.ReadChar(Ch) then
    EXIT(Err(qeOperatorExpected));

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
        if Result <> qeNone then
          EXIT;
        if not OpSymbolToData(Ident, Slug.OpIndex, Slug.OpData) then
        //Caller determines if what follows the expression is acceptable
          Parser.Undo;
      end;
  else
    EXIT(Err(qeUnknownOperator));
  end;
  Result := qeNone;
end;

//Parse out and returns a single 'slug' - an operand and the operator following
//it (or opNone)
function ParseExprSlug(var Slug: TExprSlug): TQuicheError;
begin
  Slug.ResultType := vtUnknown;
  Slug.OpType := rtUnknown;
  Slug.ILItem := nil;
  Result := ParseOperand(Slug, uoNone);
  if Result <> qeNone then
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
function ParseSubExpression(var Left: TExprSlug;out ILItem: PILItem): TQuicheError;
var Right: TExprSlug;
  VarIndex: Integer;
  EvalResult: Integer;
  EvalType: TVarType;
begin
  while True do
  begin
    //Next slug
    Result := ParseExprSlug(Right);
    if Result <> qeNone then
      EXIT;

    //If the rightslug returned an ILItem then we need to set it's Dest to a temp var
    if Right.ILItem <> nil then
      //We already have an ILItem created. If so, we need to set the Dest to
      //assign it to a temp var and use that in our calculations
      Right.AssignToHiddenVar;

    //Is next slug higher precedence
    while (Right.OpIndex <> OpIndexNone) and (Left.OpData.Precedence < Right.OpData.Precedence) do
    begin
      Result := ParseSubExpression(Right, ILItem);
      if Result <> qeNone then
        EXIT;

      if not AssignSlugTypes(Left, Right) then
        EXIT(ErrOpUsage('No operator available for operand types ' +
          VarTypeToName(Left.Operand.GetVarType) + ' and ' +
          VarTypeToName(Right.Operand.GetVarType), Left.OpIndex));

      if ILItem <> nil then
      begin
        //Add sub-expression to IL list
        //with dest as temp data
        ILItem.Dest.CreateAndSetHiddenVar(Left.ResultType, optDefaultVarStorage, VarIndex);
        //Update right slug for next iteration
        Right.Operand.SetVarFromIndex(VarIndex);
      end;
    end;

    EvalType := vtUnknown;
    if (Left.Operand.Loc = locImmediate) and (Right.Operand.Loc = locImmediate) then
    begin
      //If possible, evaluate and replace Slug.Operand
      Result := EvalBi(Left.OpIndex, @Left.Operand, @Right.Operand, EvalResult, EvalType);
      if Result <> qeNone then
        EXIT;
      if EvalType <> vtUnknown then
      begin
        Left.SetImmediate(EvalResult, EvalType);
        Left.OpType := VarTypeToOpType(EvalType);
        Left.ImplicitType := EvalType;
        Left.OpIndex := Right.OpIndex;
        Left.OpData := Right.OpData;
        if Right.OpIndex = OpIndexNone then
        begin
          ILItem := nil;
          EXIT(qeNone);
        end;
      end;
    end;

    if EvalType = vtUnknown then
    begin //not Evaluated
      if not AssignSlugTypes(Left, Right) then
        EXIT(ErrOpUsage('No operator available for operand types ' +
          VarTypeToName(Left.Operand.GetVarType) + ' and ' +
          VarTypeToName(Right.Operand.GetVarType), Left.OpIndex));

      //Add current operation to list.
      //Dest info will be added by later
      ILItem := ILAppend(dtData, Left.OpIndex);
//      if not AssignSlugTypes(Slug, @RightSlug) then
//        EXIT(errInvalidOperands);
      ILItem.OpType := Left.OpType;
      ILItem.ResultType := VarTypeToOpType(Left.ResultType);
      ILItem.Param1 := Left.Operand;
      ILItem.Param2 := Right.Operand;

      //End of expression or lower precedence
      if (Right.OpIndex = opIndexNone) or
        (Left.OpData.Precedence > Right.OpData.Precedence) then
      begin
        //Note: Dest info will be added by the caller
        //Update slug and return
        Left.OpIndex := Right.OpIndex;
        Left.OpData := Right.OpData;
        Left.OpType := Right.OpType;

        //...and return
        EXIT(qeNone);
      end;

      //Same precedence
      //Add item to IL list
      //with dest as temp data
      ILItem.Dest.CreateAndSetHiddenVar(Left.ResultType, optDefaultVarStorage, VarIndex);

      //Update slug data...
      Left.Operand.SetVarFromIndex(VarIndex);
//      Slug.Operand.VarType := ResultType;
      //Right slug operation becomes left slug operation
      Left.OpIndex := Right.OpIndex;
      Left.OpData := Right.OpData;
    end;
  end;
end;

//For cases where the expression is a single item with no operation, either a
//literal value or a variable/identifier
//Updates the Slug's data as appropriate
function FixupSlugNoOperation(var Slug: TExprSlug;var ExprType: TVarType): TQuicheError;
begin
  if Slug.ResultType = vtUnknown then
    Slug.ResultType := Slug.Operand.GetVarType;
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
    if Result <> qeNone then
      EXIT;
  end
  else
    ExprType := Slug.ResultType;
//  if Assigned(Slug.ILData) then
//    Slug.ILData.ResultType := lutVarTypeToOpType[ExprType];

  EXIT(qeNone);
end;

function ParseExpressionToSlug(var Slug: TExprSlug;var ExprType: TVarType): TQuicheError;
var ILItem: PILItem;
  VarIndex: Integer;
begin
  Parser.Mark;

  //Read the first slug of the expression
  Result := ParseExprSlug(Slug);
  if Result <> qeNone then
    EXIT;

  //If no operation then the expression is just a single item - either a literal
  //or variable.
  //We'll populate the operation data, but the dest data will be added by the caller
  if Slug.OpIndex = opIndexNone then
    EXIT(FixupSlugNoOperation(Slug, ExprType));


  //If the slug returned an ILItem then we need to set it's Dest to a temp var
  if Slug.ILItem <> nil then
    //We already have an ILItem created. If so, we need to set the Dest to
    //assign it to a temp var and use that in our calculations
    Slug.AssignToHiddenVar;

  //Loop until end of expression
  repeat
    Result := ParseSubExpression(Slug, ILItem);
    if Result <> qeNone then
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
      ILItem.Dest.CreateAndSetHiddenVar(Slug.ResultType, optDefaultVarStorage, VarIndex);
      Slug.Operand.SetVarFromIndex(VarIndex);
    end;
  until Slug.OpIndex = opIndexNone;


  if ILItem = nil then
  begin
    if ExprType <> vtUnknown then
    begin
      Result := ValidateExprType(ExprType, Slug);
      if Result <> qeNone then
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

  Result := qeNone;
end;

//Parses an expression to an ILItem
function ParseExprToILItem(out ILItem: PILItem;out VType: TVarType): TQuicheError;
var
  ExprType: TVarType;
  Slug: TExprSlug;
begin
  ExprType := vtUnknown;
  Result := ParseExpressionToSlug(Slug, ExprType);
  if Result <> qeNone then
    EXIT;

  if Slug.ILItem <> nil then
    Slug.AssignToHiddenVar;

  VType := Slug.ResultType;
  ILItem := ILAppend(dtData, OpIndexNone);
  ILItem.Param1 := Slug.Operand;
  ILItem.Param2.Loc := locNone;
  //Op data and Dest to be assigned by caller
end;

end.
