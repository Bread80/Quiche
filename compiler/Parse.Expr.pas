unit Parse.Expr;

interface
uses Def.IL, Def.Operators, Def.QTypes, Def.Consts, Def.Variables,
  Parse.Errors;

//Unary prefix operators
type
  //Subset of operators. Only opNegate and opComplement are permitted.
  //(opAddr is processes differently)
  TUnaryOperator = TOperator;

  //Is the operand size explicitly specified within the expression, or
  //implied. Explicitly sized operands should retain that size, implicit ones
  //may be extended or shortened to optimise the code generation
  TParamOrigin = (poImplicit, poExplicit);


//The expression parser breaks the input stream down into chunks of an operand and an
//operation. A 'slug' (for want of a better term) is one of those chunks.
type
  TExprSlug = record
    //NOTE: If ILItem is non-nil then Operand will be ignored
    ILItem: PILItem;  //Returns the ILItem of a sub-expression or unary operators
                      //(Otherwise, nil);

    Operand: TILParam;  //The operand
    ParamOrigin: TParamOrigin;

    Negate: Boolean;    //Used when reading operators - negate next parameter (-)?
    Invert: Boolean;    //As above, invert next parameter (NOT)?

    Op: TOperator;    //Index into Operators list

    ResultType: TVarType; //Type for the result
    ImplicitType: TVarType;//?? Type for type inference

    procedure Initialise;

    //Where the Slug has an ILItem, this routine creates a temp var and sets the
    //ILItem's Dest data to point to it.
    procedure AssignToHiddenVar;

    procedure SetImmediate(AImmType: TVarType);

    //Converts the Slug to an ILItem but does not assign a Dest
    //(If the Slug already has an ILItem returns it, otherwise
    //creates and assigns one).
    function ToILItemNoDest: PILItem;

    function OpData: POpData;
  end;

//Creates IL to assign the slug to a variable.
//If Variable is nil it will be created.
//If VType is nil:
//  If the variable is being created it will be assigned the implicit type of the expression
//  Otherwise the variable will be checked for compatibility with the expressions result type
procedure AssignSlugToVariable(const Slug: TExprSlug;var Variable: PVariable;
  VType: TVarType);

//Tests whether the expression returned by Slug is compatible with the type
//given in ExprType. If it is returns errNone, otherwise returns a suitable error code
function ValidateExprType(ExprType: TVarType;const Slug: TExprSlug): TQuicheError;

function ParseExpressionToSlug(var Slug: TExprSlug;var ExprType: TVarType): TQuicheError;

//Parses an expression to an ILItem with a single parameter and no operation
//Dest and Operation data *must* be assigned by the caller
function ParseExprToILItem(out ILItem: PILItem;out VType: TVarType): TQuicheError;

//Parses a constant expression and returns it in Value.
//If the expression is not a constant expression, or if the type is incompatible with
//ExprType returns an error.
//On entry a value of vtUnknown may be passed for ExprType. If so ExprType will
//return the type of the expression parsed
function ParseConstantExpression(out Value: TImmValue;var ExprType: TVarType): TQuicheError;

//Parses an expression (After the <varname> := has been parsed) and creates IL
//to assign it to the Variable which has been passed in. VarIndex is the index of
//the Variable
//If Variable is nil and VarIndex is -1, creates a new variable, assigning it a
//type based on the result of the expression. The caller will then need to fill
//in the variables Name, and any other necessary details.
//WARNING: When creating and initialising a variable the variable MUST be created
//*after* the expression has been evaluated. If the variable is created before
//then it will be possible to reference the variable within the expression which
//would, of course, be an bug.
function ParseAssignmentExpr(var Variable: PVariable;VType: TVarType): TQuicheError;



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
uses SysUtils,
  Def.Globals, Def.Functions, Def.Scopes,
  Lib.Primitives,
  Parse.Base, Parse.Eval, Parse.FuncCall, Parse.Source;

//===================================== Slugs

//Where the Slug has an ILItem, this routine creates a temp var and sets the
//ILItem's Dest data to point to it.
procedure TExprSlug.AssignToHiddenVar;
begin
  Assert(ILItem <> nil);
  Operand.SetVarSource(ILItem.AssignToHiddenVar(ResultType));
end;

procedure TExprSlug.Initialise;
begin
  ILItem := nil;
  Operand.Initialise;
  Op := OpUnknown;
  ResultType := vtUnknown;
  ImplicitType := vtUnknown;
end;

function TExprSlug.OpData: POpData;
begin
  Result := @Operations[Op];
end;

procedure TExprSlug.SetImmediate(AImmType: TVarType);
begin
  Assert(ILItem = nil);
  Operand.Kind := pkImmediate;
  ResultType := AImmType;
end;

function TExprSlug.ToILItemNoDest: PILItem;
begin
  if ILItem <> nil then
  begin
    Result := ILItem;
    Result.Dest.Initialise;
  end
  else
  begin
    Result := ILAppend(opUnknown);
    Result.Param1 := Operand;
    Result.ResultType := ResultType;
  end;
end;

procedure AssignSlugToVariable(const Slug: TExprSlug;var Variable: PVariable;
  VType: TVarType);
var VarVersion: Integer;
  ILItem: PILItem;
begin
  if Variable = nil then
  begin
    if VType = vtUnknown then
      Variable := VarCreateUnknown(Slug.ImplicitType)
    else
      Variable := VarCreateUnknown(VType);
  end;

  VarVersion := Variable.IncVersion;

  if Slug.ILItem <> nil then
  begin
    ILItem := Slug.ILItem;
    if ILItem.Op = OpUnknown then
      if (ILItem.Param1.Kind = pkImmediate) and (ILItem.Param2.Kind = pkNone) then
        ILItem.Op := OpStoreImm
      else
        ILItem.Op := OpMove;
  end
  else
  begin
    if Slug.Operand.Kind = pkImmediate then
      ILItem := ILAppend(OpStoreImm)
    else
      ILItem := ILAppend(OpMove);
    ILItem.Param1 := Slug.Operand;
    ILItem.Param2.Kind := pkNone;
    ILItem.ResultType := Slug.ResultType;
  end;

  ILItem.Dest.SetVarDestAndVersion(Variable, VarVersion);

  //Overflows for an immediate assignment must be validated by the parser
  if ILItem.Op = OpStoreImm then
    ILItem.Flags := ILItem.Flags - [cgOverflowCheck];
end;

//======================================Parsing literals

//Where the sign symbol is part of the number - ie. where there is no whitespace
//between it and the first digit
type TSign = (sgnNone, sgnMinus, sgnPlus);

function ParseDecimal(var Slug: TExprSlug;Sign: TSign): TQuicheError;
var S: String;
  Value: Integer;
  Ch: Char;
  Large: Boolean; //If first digit is '0' expand type to vInteger/vtWord
begin
  if Sign = sgnMinus then
    S := '-'
  else
    S := '';
  Large := Parser.TestChar = '0';
  while True do
  begin
    Ch := Parser.TestChar;
    case Ch of
      '0'..'9': S := S + Ch;
      '.','e','E':  EXIT(ErrTODO('Floating point numbers are not yet supported :('));
      '_': ;  //Ignore
    else
      if not TryStrToInt(S, Value) then
        EXIT(Err(qeInvalidDecimalNumber));
      if (Sign <> sgnNone) or Large then
        Slug.ParamOrigin := poExplicit
      else
        Slug.ParamOrigin := poImplicit;

      if (Sign <> sgnNone) or optDefaultSignedInteger then
      begin
        if optDefaultSmallestInteger and not Large and (Value >= -128) and (Value <= 127) then
          Slug.ImplicitType := vtInt8
        else
          Slug.ImplicitType := vtInteger;
      end
      else
      begin
        if optDefaultSmallestInteger and not Large and (Value <= 255) then
          Slug.ImplicitType := vtByte
        else
          Slug.ImplicitType := vtWord;
      end;
      Slug.Operand.Kind := pkImmediate;
      Slug.Operand.Imm.CreateTyped(Slug.ImplicitType, Value);
      Slug.ResultType := Slug.ImplicitType;
      EXIT(qeNone);
    end;

    Parser.SkipChar;
  end;
end;



//OLD VERSION - retained as it will be useful for on device parsing
//Will require updating for sign prefixes and implicit types

//Parses and returns an integer literal
(*function ParseDecimal(var Slug: TExprSlug): TQuicheError;
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
      begin
        Value := Value * 10 + (ord(Ch) - ord('0'));
        if Value > GetMaxValue(vtWord) then
          EXIT(Err(qeInvalidDecimalNumber));
      end;
      '.','e','E': EXIT(ErrTODO('Floating point numbers are not yet supported :('));
      '_': ; //Ignore
    else
      if Value < 256 then
        Slug.SetImmediate(vtByte)
      else
        Slug.SetImmediate(vtInteger);
      Slug.Operand.Imm.IntValue := Value;
      Slug.ParamOrigin := poImplicit;
      if Value > 32767 then
        Slug.ImplicitType := vtWord
      else
        Slug.ImplicitType := vtInteger;
      EXIT(qeNone);
    end;

    Parser.SkipChar;
  end;
end;
*)

//Where the '-' sign has already been parsed AND there is whitespace between
//the sign and the digits
function ParseNegativeDecimal(var Slug: TExprSlug): TQuicheError;
begin
  Result := ParseDecimal(Slug, sgnNone);
  if Result <> qeNone then
    EXIT;

  if Slug.Operand.Imm.IntValue <= -(GetMinValue(vtInteger)) then
  begin
    Slug.Operand.Kind := pkImmediate;
    Slug.Operand.Imm.CreateTyped(vtInteger, -Slug.Operand.Imm.IntValue);
    Slug.ParamOrigin := poImplicit;
    Slug.ImplicitType := vtInteger;
    Slug.ResultType := vtInteger;
  end
  else
    EXIT(Err(qeInvalidDecimalNumber));
end;


//Parses and returns a hex literal
//Parser must be pointing to the preceding $
function ParseHex(var Slug: TExprSlug): TQuicheError;
var
  Ch: Char;
  Ignore: Boolean;
  Digit: Integer;
  Digits: Integer;
  Value: Word;
begin
  Parser.Mark;

  //Skip the $
  Parser.SkipChar;

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
        Slug.Operand.Kind := pkImmediate;
        if Digits <= 2 then
          Slug.Operand.Imm.CreateTyped(vtByte, Value)
        else
          Slug.Operand.Imm.CreateTyped(vtPointer, Value);
        Slug.ParamOrigin := poExplicit;
        Slug.ImplicitType := Slug.Operand.Imm.VarType;
        Slug.ResultType := Slug.ImplicitType;
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
//Parse must be pointing at the preceding %
function ParseBinary(var Slug: TExprSlug): TQuicheError;
var
  Ch: Char;
  Digits: Integer;
  Value: Word;
begin
  Parser.Mark;
  //Skip %
  Parser.SkipChar;

  Value := 0;
  Digits := 0;
  while True do
  begin
    Ch := Parser.TestChar;
    if CharInSet(Ch, ['0','1']) then
    begin
      inc(Digits);
      if Value < 32768 then
        Value := (Value shl 1) + (ord(Ch)-ord('0'))
      else
        EXIT(Err(qeInvalidBinaryNumber));
    end
    else if Ch <> '_' then
    begin
      Slug.Operand.Kind := pkImmediate;
      if Digits <= 8 then
        Slug.Operand.Imm.CreateTyped(vtByte, Value)
      else
        Slug.Operand.Imm.CreateTyped(vtWord, Value);
      Slug.ParamOrigin := poExplicit;
      Slug.ImplicitType := Slug.Operand.Imm.VarType;
      Slug.ResultType := Slug.ImplicitType;
      EXIT(qeNone);
    end;

    Parser.SkipChar;
  end;
end;

//Parses and returns a quoted string or character
function ParseQuotedStringOrChar(var S: String): TQuicheError;
var
  Ch: Char;
begin
  //Skip leading quote
  Parser.SkipChar;

  while True do
  begin
    Ch := Parser.TestChar;
    if Ch = #0 then
      EXIT(Err(qeUnterminatedString));
    if Ch = '''' then
    begin //Quote char
      Parser.SkipChar;
      if Parser.TestChar = '''' then
      begin
        S := S + '''';
        Parser.SkipChar;
      end
      else  //Valid end of string
        EXIT(qeNone);
    end
    else
    begin
      S := S + Ch;
      Parser.SkipChar;
    end;
  end;
end;

//Parses and returns a character or string
//(# prefix)
//Syntax: #<constant>
//where <constant> can be a decimal, hex or binary value (with the appropriate
//prefixes).
//Currently only handles single character strings (i.e. chars)
function ParseStringOrChar(var Slug: TExprSlug): TQuicheError;
var
  S: String;
  Ch: Char;
  VarType: TVarType;
begin
  Parser.Mark;
  S := '';

  while True do
  begin
    if Parser.TestChar = '''' then
      ParseQuotedStringOrChar(S)
    else if Parser.TestChar = '#' then
    begin
      Parser.SkipChar;

      Ch := Parser.TestChar;
      case Ch of
        '0'..'9': Result := ParseDecimal(Slug, sgnNone);
        '$': Result := ParseHex(Slug);
        '%': Result := ParseBinary(Slug);
      else
        EXIT(Err(qeInvalidCharLiteral));
      end;
      if Result <> qeNone then
        EXIT;

      if Slug.Operand.Imm.IntValue < 256 then
        S := S + chr(Slug.Operand.Imm.IntValue);
    end
    else  //End of string data
    begin
      if Length(S) = 1 then
        VarType := vtChar
      else
        VarType := vtString;

      Slug.Operand.Kind := pkImmediate;
      if VarType = vtChar then
        Slug.Operand.Imm.CreateChar(S.Chars[0])
      else
        Slug.Operand.Imm.CreateString(S);
      Slug.ParamOrigin := poExplicit;
      Slug.ImplicitType := VarType;
      Slug.ResultType := VarType;
      EXIT(qeNone);
    end;
  end;
end;


//=================================Complex operands, and operators

function ParseOperand(var Slug: TExprSlug;UnaryOp: TUnaryOperator): TQuicheError; forward;

//Parses identifiers as expression parameters.
//Identifiers can be constants, variables or functions (or Types (TODO))
function ParseOperandIdentifier(var Slug: TExprSlug;Ident: String): TQuicheError;
var IdentType: TIdentType;
  Scope: PScope;
  Item: Pointer;
  C: PConst;
  V: PVariable;
  VarType: TVarType;
begin
  //Test for type names (used as data values).
  //If it is a typecast it will be followed by open bracket.
  //If not it's a typename
  //Typecasts will be processed as functions in the next section
  VarType := StringToVarType(Ident);
  if VarType <> vtUnknown then
  begin
    if Parser.TestChar <> '(' then
    begin //Not a typecast
      Slug.Operand.Kind := pkImmediate;
      Slug.Operand.Imm.CreateTypeDef(VarType);
      Slug.ParamOrigin := poExplicit;
      Slug.ImplicitType := vtTypeDef;
      Slug.ResultType := vtTypeDef;
      EXIT(qeNone);
    end;
  end;

  //Const or Variable or function
  //Search everything we can see
  Item := SearchScopes(Ident, IdentType, Scope);
  if assigned(Item) then
  begin
    case IdentType of
      itConst:
      begin
        C := PConst(Item);
        Slug.SetImmediate(C.VarType);
        Slug.Operand.Imm := C.Value;
        Slug.ParamOrigin := poExplicit;
        Slug.ResultType := C.VarType;
        Slug.implicitType := C.VarType;
        EXIT(qeNone);
      end;
      itVar:
      begin
        V := PVariable(Item);
        Slug.Operand.SetVarSource(V);
        Slug.ParamOrigin := poExplicit;
        Slug.ResultType := V.VarType;
        Slug.ImplicitType := V.VarType;
        EXIT(qeNone);
      end;
      itFunction:
      begin
        Result := DoParseFunctionCall(PFunction(Item), Slug);
        if Result <> qeNone then
          EXIT;
      end;
      itType: EXIT(ErrTODO('Typecasts for user types not yet implemented. Type names not allowed in expressions'));
    else
      EXIT(ErrBUG('Invalid/unknown IdentType in ParseOperandIentifier'));
    end;
  end
  else
    EXIT(ErrSub(qeUndefinedIdentifier, Ident));
end;

function DoUnaryOp(UnaryOp: TUnaryOperator;var Slug: TExprSlug): TQuicheError;
var VType: TVarType;
  ResultType: TVarType;
begin
  Assert(UnaryOp in UnaryOps);
  Result := qeNone;
  Slug.Op := UnaryOp;

  ResultType := vtUnknown;
  if not PrimFindParseUnary(Slug.Op, Slug, VType, ResultType) then
    EXIT(ErrOpUsageSub(qeOpIncompatibleType, VarTypeToName(Slug.ImplicitType), Slug.Op));

  if (Slug.ILItem = nil) and (Slug.Operand.Kind = pkImmediate) then
  begin
    Result := EvalUnary(Slug.Op, @Slug.Operand, Slug.Operand.Imm);
    if Result <> qeNone then
      EXIT;
    Slug.Op := OpUnknown;
    Slug.ImplicitType := Slug.Operand.Imm.VarType;
  end
  else  //Parameter is not immediate
  begin
    Slug.ResultType := ResultType;
    Slug.ImplicitType := ResultType;

    if Slug.ILItem <> nil then
      //We already have an ILItem created. If so, we need to set the Dest to
      //assign it to a temp var and use that in our calculations
      Slug.AssignToHiddenVar;

    Slug.ILItem := ILAppend(Slug.Op);
    Slug.ILItem.Param1 := Slug.Operand;
    Slug.ILItem.Param2.Kind := pkNone;
    Slug.ILItem.ResultType := Slug.ResultType;
  end;
end;

//Parses the @ operator. Value must be something which is addressable.
function ParseAddrOf(var Slug: TExprSlug): TQuicheError;
var UnaryOp: TUnaryOperator;
begin //Sub-expressions
  Parser.Mark;
  Parser.SkipChar;

  UnaryOp := opUnknown;
  Result := ParseOperand(Slug, UnaryOp);
  if Result <> qeNone then
    EXIT;

  if UnaryOp <> opUnknown then
    EXIT(Err(qeAt));
  if Slug.Op <> opUnknown then
    EXIT(Err(qeAt));
  if Slug.Operand.Kind <> pkVarSource then
     EXIT(Err(qeAt));

  Slug.ILItem := ILAppend(OpAddrOf);
  Slug.ILItem.Param1 := Slug.Operand;
  Slug.ILItem.Param1.Kind := pkVarAddr;
  Slug.ILItem.ResultType := vtPointer;
  Slug.Operand.Kind := pkNone;
  Slug.ResultType := vtPointer;
  Slug.ImplicitType := vtPointer;

  Result := qeNone;
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
  Result := Parser.SkipWhiteNL;
  if Result <> qeNone then
    EXIT;

  Parser.Mark;

  Ch := Parser.TestChar;
  case Ch of
    '(':
      begin //Sub-expressions
        Parser.SkipChar;

        Result := ParseExpressionToSlug(Slug, Slug.ResultType{, Slug.ImplicitType});
        if Result <> qeNone then
          EXIT;

        if not Parser.SkipCharIf(')') then
          EXIT(Err(qeUnmatchedBrackets));
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
          opUnknown: Result := ParseOperand(Slug, opComplement);
          opNegate: Result := ParseOperand(Slug, opComplement);
          opComplement: Result := ParseOperand(Slug, opUnknown);
        else
          EXIT(ErrBUG('Unknown/invalid unary operator'));
        end;
        if UnaryOp = opComplement then
          UnaryOp := opUnknown;
      end
      else
        Result := ParseOperandIdentifier(Slug, Ident);

      if Result <> qeNone then
        EXIT;
    end;
//    csDecimalFirst
    '0'..'9': //Numeric constants
      if UnaryOp = opNegate then
      begin
        Result := ParseNegativeDecimal(Slug);
        UnaryOp := opUnknown;
      end
      else
        Result := ParseDecimal(Slug, sgnNone);
    '$': //Hex constant
      Result := ParseHex(Slug);
    '%': //Binary constant
      Result := ParseBinary(Slug);
    '.': //Real constant
      EXIT(ErrTODO('Floating point numbers not yet supported'));
    '''','#': //Char or string
      Result := ParseStringOrChar(Slug);
    '@': //Address prefix
      Result := ParseAddrOf(Slug);
    '+': //Unary plus
    begin
      Parser.SkipChar;
      if CharInSet(Parser.TestChar, ['0'..'9']) then
        Result := ParseDecimal(Slug, sgnPlus)
      else
        EXIT(ParseOperand(Slug, UnaryOp));
    end;
    '-': //Unary subtract/negative
    begin
      Parser.SkipChar;
      if CharInSet(Parser.TestChar, ['0'..'9']) then
        Result := ParseDecimal(Slug, sgnMinus)
      else
      begin
        case UnaryOp of
          opUnknown: Result := ParseOperand(Slug, opNegate);
          opNegate: Result := ParseOperand(Slug, opUnknown);
          opComplement: Result := ParseOperand(Slug, opNegate);
        else
          EXIT(errBUG('Unknown/invalid unary operator'));
        end;
        if Result <> qeNone then
          EXIT;
        if UnaryOp = opNegate then
          UnaryOp := opUnknown;
      end;
    end;
  else
    EXIT(Err(qeOperandExpected));
  end;

  //If Negate or Invert: Do that here
  //NOTE: Recurse for '-' clears Negate, Recurse for 'NOT' clears Invert
  //Also, if constant is negated, or negate is otherwise processed, Negate must be cleared
  //Ditto for Invert/NOT
  if UnaryOp <> opUnknown then
    Result := DoUnaryOp(UnaryOp, Slug);
end;

//Parse and returns a *binary* operator (i.e. not unary ones) and it's precedence
//If at the end of an expression returns opNone.
function ParseInfixOperator(var Slug: TExprSlug): TQuicheError;
var Ident: String;
begin
  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;
  Parser.Mark;

  Slug.Op := opUnknown;

  //End of (sub)-expression
  if CharInSet(Parser.TestChar, [')',';',',',#0]) then
    EXIT(qeNone);
  if Parser.EOLN then
    EXIT(qeNone);

  if TestSymbolFirst then
  begin
    Result := ParseSymbol(Ident);
    Slug.Op := SymbolToOperator(Ident);
  end
  else if TestIdentFirst then
  begin
    Result := ParseIdentifier(#0, Ident);
    Slug.Op := IdentToInfixOperator(Ident);
  end
  else
    EXIT(Err(qeOperatorExpected));

  if Result <> qeNone then
    EXIT;
  if Slug.Op = opUnknown then
  begin
    Parser.Undo;
    EXIT(qeNone);
  end;

  Result := qeNone;
end;

//Parse out and returns a single 'slug' - an operand and the operator following
//it (or opNone)
function ParseExprSlug(out Slug: TExprSlug): TQuicheError;
begin
  Slug.Initialise;
  Result := ParseOperand(Slug, opUnknown);
  if Result <> qeNone then
    EXIT;

  Result := ParseInfixOperator(Slug);
end;


//==============================Expressions

//Tests whether the expression returned by Slug is compatible with the type
//given in ExprType. If it is returns errNone, otherwise returns a suitable error code
function ValidateExprType(ExprType: TVarType;const Slug: TExprSlug): TQuicheError;
var Valid: Boolean;
begin
  if Slug.Op = OpUnknown then
  begin //Single value, no expression
    if Slug.Operand.Kind = pkImmediate then
    begin
      if IsIntegerType(ExprType) and IsIntegerType(Slug.Operand.Imm.VarType) then
      begin //Integer value in range
        Valid := (Slug.Operand.Imm.IntValue >= GetMinValue(ExprType)) and
          (Slug.Operand.Imm.IntValue <= GetMaxValue(ExprType));
        if not Valid then
          EXIT(ErrSub2(qeConstantAssignmentOutOfRange, Slug.Operand.ImmValueToString,
            VarTypeToName(ExprType)));
      end
      else  //One or both are not integer types
      begin
        Valid := ValidateAssignmentType(ExprType, Slug.ResultType);
        if not Valid then
          EXIT(ErrSub2(qeTypeMismatch, VarTypeToName(Slug.ResultType),
            VarTypeToName(ExprType)));
      end
    end
    else
      Valid := ValidateAssignmentType(ExprType, Slug.ResultType);
  end
  else
    //Expression
    Valid := ValidateAssignmentType(ExprType, Slug.ResultType);

  if not Valid then
    Result := ErrSub2(qeTypeMismatch, VarTypeToName(Slug.ResultType),
      VarTypeToName(ExprType))
  else
    Result := qeNone;
end;

//Sets the types in the left slug (ResultType, OpType fields) for the operation when
//used with the given input operands: Slug.Operand <Slug.Operation> RightSlug.Operand
//Also validates that the operator can be used with the given operand types and
//that there is a suitable primitive available (as noted in the OpTypes field of
//the Operands list.
//Returns False if operands are incompatible with the operator/available primitives
function AssignSlugTypes(var Left, Right: TExprSlug): Boolean;
var
  LType: TVarType;
  RType: TVarType;
  ResultType: TVarType;
begin
  ResultType := vtUnknown;  //Find routines with any result type
  Result := PrimFindParse(Left.Op, Left, Right, LType, RType, ResultType);
  if not Result then
    EXIT;

  //Update types for constants
  if (Left.ILItem = nil) and (Left.Operand.Kind = pkImmediate) then
    Left.Operand.Imm.UpdateVarType(LType);
  if (Right.ILItem = nil) and (Right.Operand.Kind = pkImmediate) then
    Right.Operand.Imm.UpdateVarType(RType);

  Left.ResultType := ResultType;
  Left.ImplicitType := ResultType;
  Right.ImplicitType := ResultType;
end;

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
  Evalled: Boolean;
begin
  while True do
  begin
    //Next slug
    Result := ParseExprSlug(Right);
    if Result <> qeNone then
      EXIT;

    //If the right slug returned an ILItem then we need to set it's Dest to a temp var
    if Right.ILItem <> nil then
      if Right.ILItem.Dest.Kind = pkNone then
        //We already have an ILItem created. If so, we need to set the Dest to
        //assign it to a temp var and use that in our calculations
        Right.AssignToHiddenVar;

    //While next slug is higher precedence
    while (Right.Op <> OpUnknown) and (Left.OpData.Precedence < Right.OpData.Precedence) do
    begin
      Result := ParseSubExpression(Right, ILItem);
      if Result <> qeNone then
        EXIT;

      if not AssignSlugTypes(Left, Right) then
        EXIT(ErrOpUsageSub2(qeOpIncompatibleTypes,
          VarTypeToName(Left.Operand.GetVarType),
          VarTypeToName(Right.Operand.GetVarType), Left.Op));

      if ILItem <> nil then
        //Add sub-expression to IL list
        //with dest as temp data
        //Update right slug for next iteration
        Right.Operand.SetVarSource(ILItem.AssignToHiddenVar(Left.ResultType));
    end;

    //Evaluate constant expressions
    Evalled := False;
    if (Left.Operand.Kind = pkImmediate) and (Right.Operand.Kind = pkImmediate) then
    begin
      //If possible, evaluate and replace Slug.Operand
      Result := EvalBi(Left.Op, @Left.Operand, @Right.Operand, Left.Operand.Imm);
      if Result <> qeNone then
        EXIT;
(*      if EvalType <> vtUnknown then
      begin
        Left.SetImmediate(EvalResult, EvalType);
*)        Left.ImplicitType := Left.Operand.Imm.VarType;
        Left.ResultType := Left.ImplicitType;
        Left.Op := Right.Op;
        if Right.Op = OpUnknown then
        begin
          ILItem := nil;
          EXIT(qeNone);
        end;
(*      end;
 *)   end;

    //If we didn't evaluate as a constant expression
    if not Evalled then
    begin
      //Modifies operand types and result type based on available primitives
      if not AssignSlugTypes(Left, Right) then
        EXIT(ErrOpUsageSub2(qeOpIncompatibleTypes,
          VarTypeToName(Left.Operand.GetVarType),
          VarTypeToName(Right.Operand.GetVarType), Left.Op));

      //Add current operation to list.
      //Dest info will be added by later
      ILItem := ILAppend(Left.Op);
      ILItem.ResultType := Left.ResultType;
      ILItem.Param1 := Left.Operand;
      ILItem.Param2 := Right.Operand;

      //Exit if end of expression or lower precedence
      if (Right.Op = opUnknown) or
        (Left.OpData.Precedence > Right.OpData.Precedence) then
      begin
        //Note: Dest info will be added by the caller
        //Update slug and return
        Left.Op := Right.Op;

        //...and return
        EXIT(qeNone);
      end;

      //Same precedence:
      //Add item to IL list
      //with dest as temp data (hidden variable)
      //Update slug data...
      Left.Operand.SetVarSource(ILItem.AssignToHiddenVar(Left.ResultType));

      //Right slug operation becomes left slug operation
      Left.Op := Right.Op;
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

  if ExprType <> vtUnknown then
  begin
    Result := ValidateExprType(ExprType, Slug);
    if Result <> qeNone then
      EXIT;
  end
  else
    ExprType := Slug.ResultType;

  Result := qeNone;
end;

function ParseExpressionToSlug(var Slug: TExprSlug;var ExprType: TVarType): TQuicheError;
var ILItem: PILItem;
begin
  Parser.Mark;

  //Read the first slug of the expression
  Result := ParseExprSlug(Slug);
  if Result <> qeNone then
    EXIT;

  //If no operation then the expression is just a single item - either a literal
  //or variable.
  //We'll populate the operation data, but the dest data will be added by the caller
  if Slug.Op = opUnknown then
    EXIT(FixupSlugNoOperation(Slug, ExprType));


  //If the slug returned an ILItem then we need to set it's Dest to a temp var
  if Slug.ILItem <> nil then
    if Slug.ILItem.Dest.Kind = pkNone then
      //We already have an ILItem created. If so, we need to set the Dest to
      //assign it to a temp var and use that in our calculations
      Slug.AssignToHiddenVar;

  //Loop until end of expression
  repeat
    Result := ParseSubExpression(Slug, ILItem);
    if Result <> qeNone then
      EXIT;

    //Unless end of expression :- assign value to hidden variable
    if Slug.Op <> OpUnknown then
      Slug.Operand.SetVarSource(ILItem.AssignToHiddenVar(Slug.ResultType));
  until Slug.Op = opUnknown;


  if ILItem = nil then
  begin
    if ExprType <> vtUnknown then
    begin
      Result := ValidateExprType(ExprType, Slug);
      if Result <> qeNone then
        EXIT;
    end;
  end
  else // ILItem <> nil
//???ValidateExprType??
    Slug.ILItem := ILItem;

  if ExprType <> vtUnknown then
//    ILItem.ResultType := ExprType
  else
    ExprType := Slug.ImplicitType;

  Result := qeNone;
end;

//Parses an expression to an ILItem
function ParseExprToILItem(out ILItem: PILItem;out VType: TVarType): TQuicheError;
var
  ExprType: TVarType;
  Slug: TExprSlug;
begin
  Slug.Initialise;

  ExprType := vtUnknown;
  Result := ParseExpressionToSlug(Slug, ExprType);
  if Result <> qeNone then
    EXIT;

  VType := Slug.ResultType;
  if Slug.ILItem <> nil then
    ILItem := Slug.ILItem
  else
  begin
    ILItem := ILAppend(OpUnknown);
    ILItem.Param1 := Slug.Operand;
    ILItem.Param2.Kind := pkNone;
  end;

  //Op data and Dest to be assigned by caller
end;

function ParseConstantExpression(out Value: TImmValue;var ExprType: TVarType): TQuicheError;
var Slug: TExprSlug;
begin
  Result := ParseExpressionToSlug(Slug, ExprType);
  if Result <> qeNone then
    EXIT;

  if Slug.Operand.Kind <> pkImmediate then
    EXIT(Err(qeConstantExpressionExpected));

  Value := Slug.Operand.Imm;
end;

function ParseAssignmentExpr(var Variable: PVariable;VType: TVarType): TQuicheError;
var
  ExprType: TVarType;
  Slug: TExprSlug;
begin
  ExprType := VType;
  Result := ParseExpressionToSlug(Slug, ExprType);
  if Result <> qeNone then
    EXIT;

  if ExprType = vtString then
    //TODO: If we are assigning a string literal to a variable (or passing as a var
    //parameter) then we need to get the code generator to generate /modifable/
    //string data (and to make sure that data is stored in RAM).
    EXIT(ErrTODO('Strings variables are not currently supported.'));

  //Verify assignment is in range. Only really required for integers due to
  //implicit typing rules
  if VType = vtUnknown then
  begin
    Result := ValidateExprType(ExprType, Slug);
    if Result <> qeNone then
      EXIT;
  end;

  AssignSlugToVariable(Slug, Variable, VType);
end;

end.
