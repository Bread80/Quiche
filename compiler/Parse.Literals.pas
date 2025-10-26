unit Parse.Literals;

interface
uses Def.Operators, Def.IL, Def.UserTypes,
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


//The expression parser breaks the input stream down into 'slugs'. Each slug
//consists of an operand and an operation. For infix operators the operation is
//the operator which follows the operand. For prefix operators the sub-expression
//will be parsed first (using recursion) and the operation added after the returned
//slug.
//If there is no operation opNone will be used.
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

    ResultType: PUserType; //Type for the result
    ImplicitType: PUserType;//?? Type for type inference

    //Used where the result value of a function call used pass-by-reference.
    //The result will be assigned to a variable (possibly a temp variable). The
    //function dispatch parser has no knownledge of that variable - indeed the variable
    //might not have been created yet.
    //Normally this is not a problem, but if the Result is pass by reference then
    //the VarRef of (pointer to) the variable needs to be passed into the function
    //at call dispatch time. When that happens the function dispatch mechanism will
    //assign ResultByRefParam to the IL param which loads that VarRef prior to the function
    //call. On return from the function dispatch mechanism, once the assignment variable
    //is known ResultByRefParam must be patched to load it's VarRef.
    //At all other times ResultByRefParam will be nil.
    ResultByRefParam: PILParam;


    procedure Initialise;

    //Where the Slug has an ILItem, this routine creates a temp var and sets the
    //ILItem's Dest data to point to it.
    procedure AssignToHiddenVar;

    procedure SetImmediate(AImmType: PUserType);

    //Converts the Slug to an ILItem but does not assign a Dest
    //(If the Slug already has an ILItem returns it, otherwise
    //creates and assigns one).
    function ToILItemNoDest: PILItem;

    function OpData: POpData;
  end;

//Where the sign symbol is part of the number - ie. where there is no whitespace
//between it and the first digit
type TSign = (sgnNone, sgnMinus, sgnPlus);

function ParseDecimal(var Slug: TExprSlug;Sign: TSign): TQuicheError;

//Where the '-' sign has already been parsed AND there is whitespace between
//the sign and the digits
function ParseNegativeDecimal(var Slug: TExprSlug): TQuicheError;

//Parses and returns a hex literal
//Parser must be pointing to the preceding $
function ParseHex(var Slug: TExprSlug): TQuicheError;

//Parses and returns a binary literal
//Parse must be pointing at the preceding %
function ParseBinary(var Slug: TExprSlug): TQuicheError;

//Parses and returns a quoted string or character
function ParseQuotedStringOrChar(var S: String): TQuicheError;

//Parses and returns a character or string
//(# prefix)
//Syntax: #<constant>
//where <constant> can be a decimal, hex or binary value (with the appropriate
//prefixes).
//Currently only handles single character strings (i.e. chars)
function ParseStringOrChar(var Slug: TExprSlug): TQuicheError;

implementation
uses SysUtils,
  Def.Variables, Def.Globals, Def.VarTypes,
  Parse.Source, Parse.Base;

//===================================== Slugs

procedure TExprSlug.AssignToHiddenVar;
var V: PVariable;
begin
  Assert(ILItem <> nil);
  if ResultByRefParam <> nil then
  begin //If we have a function whose return value is pass-by-reference
    Assert(ResultByRefParam.Kind = pkNone);
    //Create a temp variable
    V := Vars.AddHidden(ResultType);
    V.IncVersion;
    //Patch it into the argument /load/ IL for the function call
    ResultByRefParam.SetVarRef(V);
    //And return it in the operand
    Operand.SetVarSource(V);
  end
  else
    Operand.SetVarSource(ILItem.AssignToHiddenVar(ResultType));
end;

procedure TExprSlug.Initialise;
begin
  ILItem := nil;
  Operand.Initialise;
  Op := OpUnknown;
  ResultType := nil;
  ImplicitType := nil;
  ResultByRefParam := nil;
end;

function TExprSlug.OpData: POpData;
begin
  Result := @Operations[Op];
end;

procedure TExprSlug.SetImmediate(AImmType: PUserType);
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

//======================================Parsing literals

function ParseDecimal(var Slug: TExprSlug;Sign: TSign): TQuicheError;
var S: String;
  Value: Integer;
  Ch: Char;
  Large: Boolean; //If first digit is '0' expand type to vtInteger/vtWord
  Cursor: TParseCursor;
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
      'e','E':  EXIT(ErrTODO('Floating point numbers are not yet supported :('));
      '_': ;  //Ignore
    else  //End of /integer/ decimal (could be real)
      if Ch = '.' then
      begin //Do we have a floating point number? (Or, maybe, a range operator or
            //a dotted syntax expression
        Cursor := Parser.GetCursor;
        Parser.SkipChar;
        if CharInSet(Parser.TestChar, ['0'..'9']) then
          EXIT(ErrTODO('Floating point numbers are not yet supported :('));
        Parser.SetCursor(Cursor);
      end;

      if not TryStrToInt(S, Value) then
        EXIT(Err(qeInvalidDecimalNumber));
      if (Sign <> sgnNone) or Large then
        Slug.ParamOrigin := poExplicit
      else
        Slug.ParamOrigin := poImplicit;

      if (Sign <> sgnNone) or optDefaultSignedInteger then
      begin
        if optDefaultSmallestInteger and not Large and (Value >= -128) and (Value <= 127) then
          Slug.ImplicitType := GetSystemType(vtInt8)
        else
          Slug.ImplicitType := GetSystemType(vtInteger);
      end
      else
      begin
        if optDefaultSmallestInteger and not Large and (Value <= 255) then
          Slug.ImplicitType := GetSystemType(vtByte)
        else
          Slug.ImplicitType := GetSystemType(vtWord);
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
    Slug.ImplicitType := GetSystemType(vtInteger);
    Slug.ResultType := GetSystemType(vtInteger);
  end
  else
    EXIT(Err(qeInvalidDecimalNumber));
end;

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
        Slug.ImplicitType := Slug.Operand.Imm.UserType;
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
      Slug.ImplicitType := Slug.Operand.Imm.UserType;
      Slug.ResultType := Slug.ImplicitType;
      EXIT(qeNone);
    end;

    Parser.SkipChar;
  end;
end;

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

function ParseStringOrChar(var Slug: TExprSlug): TQuicheError;
var
  S: String;
  Ch: Char;
  UserType: PUserType;
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
        UserType := GetSystemType(vtChar)
      else
        UserType := GetSystemType(vtString);

      Slug.Operand.Kind := pkImmediate;
      if UTToVT(UserType) = vtChar then
        Slug.Operand.Imm.CreateChar(S.Chars[0])
      else
        Slug.Operand.Imm.CreateString(S);
      Slug.ParamOrigin := poExplicit;
      Slug.ImplicitType := UserType;
      Slug.ResultType := UserType;
      EXIT(qeNone);
    end;
  end;
end;

end.
