unit QuicheParser;

interface
uses MSourceReader, ILData, MErrors;

procedure LoadFromFile(Filename: String);
procedure LoadFromString(Source: String);

function ParserEOF: Boolean;

//Parses an expression (or sub-expression) until we run out of operators
//Returns the IL list item of the final operation. The caller will need to
//add the Dest info as needed (i.e to assign the result to a variable, or temp
//location

//Does NOT validate that there isn't 'junk' after the expression,
//it is up to the caller to detect the proper context and valid ending of
//the expression. (I.e. during the setting up of a FOR loop the expression will
//be terminated by the TO identifier. In an parameter list the expression will
//be terminated by the comma or close brace (or end of line). This code is useable
//no matter the context.
function ParseExpression(out ILItem: PILItem): TAssembleError;

function ParseStatement(Ident: String): TAssembleError;

type TBlockState = (bsSingle, bsBeginRead);
function ParseBlock(BlockState: TBlockState): TAssembleError;

implementation
uses SysUtils, Variables;

var Parser: TSourceReader;

procedure LoadFromString(Source: String);
begin
  Parser.LoadFromString(Source);
end;

procedure LoadFromFile(Filename: String);
begin
  Parser.OpenFile(Filename);
end;

function ParserEOF: Boolean;
begin
  Result := Parser.EOF;
end;


type TKeyword = (keyUNKNOWN, keyIF, keyTHEN, keyELSE);
const KeywordStrings: array[low(TKeyword)..high(TKeyword)] of String = (
  '',  //Placeholder for Unknown value
  'if','then','else');

function IdentToKeyword(Ident: String): TKeyword;
begin
  for Result := low(TKeyword) to high(TKeyword) do
    if CompareText(KeywordStrings[Result], Ident) = 0 then
      EXIT;

  Result := keyUnknown;
end;

function TestForIdent(Ident: String): Boolean;
var
  Ch: Char;
  S: String;
begin
  Parser.SkipWhiteSpace;
  S := '';
  Parser.Mark;

  if Parser.TestChar in csIdentFirst then
  begin
    repeat
      Parser.ReadChar(Ch);
      S := S + Ch;
    until not (Parser.TestChar in csIdentOther);

    Result := CompareText(Ident, S) = 0;
  end
  else
    Result := False;

  if not Result then
    Parser.Undo;
end;

//Parses and returns an integer literal
function ParseInteger(out Value: Integer): TAssembleError;
var
  Ch: Char;
  S: String;
  SignMult: Integer;
begin
{  Ch := Parser.TestChar;
  if Ch in ['-','+'] then
  begin
    S := Ch;
    Parser.SkipChar;
  end
  else
    S := '';
}  while True do
  begin
    Ch := Parser.TestChar;
    if Ch in ['0'..'9'] then
      S := S + Ch
    else
      if TryStrToInt(S, Value) then
        EXIT(errNone)
      else
        EXIT(errInvalidDecimalNumber);
    Parser.SkipChar;
  end;
end;

//Parses and returns an identifier.
//If First is anything other than #0 this will be used as the first character
//of the Identifier. This is useful where the character has already been consumed,
//from the Parser, for analysis.
function ParseIdentifier(First: Char;out Ident: String): TAssembleError;
var Ch: Char;
begin
  if First <> #0 then
    Ident := First
  else
  begin
    if not Parser.ReadChar(Ch) then
      EXIT(errIdentifierExpected);
    if Ch in csIdentFirst then
      Ident := Ch
    else
      EXIT(errIdentifierExpected);
  end;

  while True do
  begin
    Ch := Parser.TestChar;
    if Ch in csIdentOther then
      Ident := Ident + Ch
    else
      EXIT(errNone);
    Parser.SkipChar;
  end;
end;

//Parses an operand (of an expression)
//Returns with Loc containg the location of the operand (i.e whether it is a
//literal or variable). Data returns either the value (immediate data) or
//a reference if anything else (such as a variable)
function ParseOperand(out Loc: TILLocation;out Data: Integer;out VarSub: Integer): TAssembleError;
var
  Ch: Char;
//  Prefix: Char;
  Ident: String;
  V: PVariable;
//  Index: Integer;
//  Value: Integer; //For integer literals
  ILItem: PILItem;
begin
//  Prefix := #0;
  Parser.SkipWhiteSpace;

  Ch := Parser.TestChar;

  //Unary operators/prefixes
//  if Ch in ['+','-'] then
//  begin
//    Prefix := Ch;
//    Parser.SkipChar;
//    Parser.SkipWhiteSpace;
//      Result := ParseOperand(Loc, Data);

//    Ch := Parser.TestChar;
//  end;

  //Sub-expressions
  if Ch = '(' then
  begin
    Parser.SkipChar;
    Loc := locTemp;
    Data := -1;
    Result := ParseExpression(ILItem);
    if Result <> errNone then
      EXIT;

    Loc := locTemp;
    Data := GetNextTempIndex;
    ILItem.DestLoc := Loc;
    ILItem.DestData := Data;

    if Parser.TestChar <> ')' then
      EXIT(errUnmatchedBrackets);
    Parser.SkipChar;
  end
  //Identifiers
  else if Ch in csIdentFirst then
  begin
    Result := ParseIdentifier(#0, Ident);
    if Result <> errNone then
      EXIT;

    //*** NOT operator ***
    V := FindOrAddVar(Ident, Data, VarSub);
    Loc := locVar;
    Result := errNone;
  end


  //Numeric constants
  else if Ch in csDecimalFirst then
  begin
    Result := ParseInteger(Data);
    if Result <> errNone then
      EXIT;

    Loc := locImmediate;
    Result := errNone;
  end
  else
    EXIT(errInvalidExpression);

//  case Prefix of
//  '+': AddIL(ilUnaryPlus, 0);
//  '-': AddIL(ilNegate, 0);
//  end;

end;

//For ASCII operators, this converts the operator name to the operator enumeration value.
//ONLY handles binary operators (i.e. not unary operators).
//If the string isn't cound returns opNone
function IdentToOp(Ident: String): TILOperation;
const
  ListMax = 7;
  Ops: array[0..ListMax] of TILOperation = (opIn, opOr, opXor, opDiv, opMod, opAnd, opSHR, opSHL);
  OpNames: array [0..ListMax] of String =      ('in', 'or', 'xor', 'div', 'mod', 'and', 'shr', 'shl');
var I: Integer;
begin
  for I := 0 to ListMax do
    if CompareText(Ident, OpNames[I]) = 0 then
      EXIT(Ops[I]);

  Result := opNone;
end;

//Parse and returns a *binary* operator (i.e. not unary ones) and it's precedence
//If at the end of an expression returns opNone.
function ParseOperator(out Op: TILOperation;out Precedence: Integer): TAssembleError;
var
  Ch: Char;
  Ident: String;
begin
  Parser.SkipWhiteSpace;

  Op := opNone;
  Precedence := -1;
  //End of (sub)-expression
  if Parser.TestChar in [')',';',#0] then
    EXIT(errNone);

  Parser.Mark;
  if not Parser.ReadChar(Ch) then
    EXIT(errOperatorExpected);

  case Ch of
//    '@': op := ilAt;

    '*': Op := opMultiply;
    '/': Op := opDivide;

    '+': Op := opAdd;
    '-': Op := opSubtract;

    '=': Op := opEqual;
    '<':
      begin
        Ch := Parser.TestChar;
        if Ch = '>' then
        begin
          Op := opNotEqual;
          Parser.SkipChar;
        end
        else if Ch = '=' then
        begin
          Op := opLessEqual;
          Parser.SkipChar;
        end
        else
          Op := opLess;
      end;
    '>':
      begin
        Ch := Parser.TestChar;
        if Ch = '=' then
        begin
          Op := opGreaterEqual;
          Parser.SkipChar;
        end
        else
          Op := opGreater;
      end;
    'a'..'z','A'..'Z':
      begin
        Result := ParseIdentifier(Ch, Ident);
        if Result <> errNone then
          EXIT;
        Op := IdentToOp(Ident);
        if Op = opNone then
        begin
          Parser.Undo;
          EXIT(errNone);
        end;
      end;
  else
    EXIT(errUnknownOperator);
  end;
  Precedence := ILOpPrecedences[Op];

  Result := errNone;
end;

//The expression parser breaks the input stream down into chunks of an operand and an
//operation. A 'slug' (for want of a better term) is one of those chunks.
type
  PExprSlug = ^TExprSlug;
  TExprSlug = record
    Loc: TILLocation;     //What does the Data field refer to?
    Data: Integer;        //Data for an operand (or where to find that data)
    VarSub: Integer;      //Variable substript (variable write index) if variable

    Op: TILOperation;     //The operation
    Precedence: Integer;  //The precedence of the operation (cached)
  end;

//Parse out and returns a single 'slug' - an operand and the operator following
//it (or opNone)
function ParseExprSlug(out Slug: TExprSlug): TAssembleError;
begin
  Result := ParseOperand(Slug.Loc, Slug.Data, Slug.VarSub);
  if Result <> errNone then
    EXIT;

  Result := ParseOperator(Slug.Op, Slug.Precedence);
end;

//Compares the precedences of the passed in slug and the following slug.
//If the following slug(s) is(are) higher precedence then recurses to parse
//that(those) slug(s). If not appends the current data to the IL list and
//returns.
//When it returns the Slug parameter will have been updated to reflect the
//current situation - i.e. next operator (and it's precedence) and the location
//and value of the operand (i.e. the temp index location of the result of the
//deeper operations.
//ILItem returns the final operationof the expression. The caller will need
//to update the Dest info as needed
function ParseSubExpression(Slug: PExprSlug;out ILItem: PILItem): TAssembleError;
var RightSlug: TExprSlug;
  DestData: Integer;
begin
  while True do
  begin
    //Next slug
    Result := ParseExprSlug(RightSlug);
    if Result <> errNone then
      EXIT;

    //Is next slug higher precedence
    while Slug.Precedence < RightSlug.Precedence do
    begin
      Result := ParseSubExpression(@RightSlug, ILItem);
      if Result <> errNone then
        EXIT;

      //Add sub-expression to IL list
      //with dest as temp data
      DestData := GetNextTempIndex;
      ILItem.DestLoc := locTemp;
      ILItem.DestData := DestData;
      //Update right slug for next iteration
      RightSlug.Loc := locTemp;
      RightSlug.Data := DestData;
    end;

    //Add current operation to list.
    //Dest info will be added by later
    ILItem := ILAlloc(dtData);
    ILItem.Op := Slug.Op;
    ILItem.Param1Loc := Slug.Loc;
    ILItem.Param1Data := Slug.Data;
    ILItem.Param1Sub := Slug.VarSub;
    ILItem.Param2Loc := RightSlug.Loc;
    ILItem.Param2Data := RightSlug.Data;
    ILItem.Param2Sub := RightSlug.VarSub;

    //End of expression or lower precedence
    if (RightSlug.Op = opNone) or (Slug.Precedence > RightSlug.Precedence) then
    begin
      //Note: Dest info will be added by the caller
      //Update slug and return
      Slug.Op := RightSlug.Op;
      Slug.Precedence := RightSlug.Precedence;

      //...and return
      EXIT(errNone);
    end
    else //Same precedence
    begin
      //Add item to IL list
      //with dest as temp data
      DestData := GetNextTempIndex;

      ILItem.DestLoc := locTemp;
      ILItem.DestData := DestData;

    //Update slug data...
      Slug.Loc := locTemp;
      Slug.Data := DestData;
      //Right slug operation becomes left slug operation
      Slug.Op := RightSlug.Op;
      Slug.Precedence := RightSlug.Precedence;
     end;

  end;
end;

//See header section
function ParseExpression(out ILItem: PILItem): TAssembleError;
var Slug: TExprSlug;
begin
  //Read the first slug of the expression
  Result := ParseExprSlug(Slug);
  if Result <> errNone then
    EXIT;

  //If no operation then the expression is just a single item - either a literal
  //or variable.
  //We'll populate the operation data, but the dest data will be added by the caller
  if Slug.Op = opNone then
  begin
    //Add item to IL list
    ILItem := ILAlloc(dtData);
    ILItem.Op := opAssign;
    ILItem.Param1Loc := Slug.Loc;
    ILItem.Param1Data := Slug.Data;
    ILItem.Param1Sub := Slug.VarSub;
    ILItem.Param2Loc := locNone;
    ILItem.Param2Data := -1;
    ILItem.Param2Sub := -1;

    EXIT(errNone);
  end;

  //Loop until end of expression
  while True do
  begin
    Result := ParseSubExpression(@Slug, ILItem);
    if Result <> errNone then
      EXIT;

    if Slug.Op <> opNone then
    begin
      Slug.Loc := locTemp;
      Slug.Data := GetNextTempIndex;

      ILItem.DestLoc := locTemp;
      ILItem.DestData := Slug.Data;
    end
    else
      EXIT(errNone);
  end;
end;

function DoIF: TAssembleError;
var
  Branch: PILItem;
  ThenBranch: PILItem;
  ElseBranch: PILItem;
  //BlockIDs
  BranchID: Integer;    //Block ID of the conditional branch
  ThenLastID: Integer;  //Last block of the THEN branch
  ElseLastID: Integer;  //Last block of the ELSE branch
//  MergeID: Integer;     //The block where both paths merge together
  //ILItem indexes
  BranchIndex: Integer; //Item containing the conditional branch (i.e. end of previous code)
  ThenLastIndex: Integer; //Last item in the THEN path
  ElseLastIndex: Integer; //Last item in the ELSE path. -1 if no ELSE path
begin
  Result := ParseExpression(Branch);
  if Result <> errNone then
    EXIT;

  //Convert item to a branch
  Branch.DestType := dtBranch;

  //Test for THEN
  Parser.SkipWhiteSpace;
  if not TestForIdent('then') then
    EXIT(errTHENExpected);

  BranchID := GetCurrBlockID;
  BranchIndex := ILGetCount-1;

  //THEN Block
  NewBlock := True;
  Branch.TrueBlock := GetCurrBlockID + 1;
  Result := ParseBlock(bsSingle);
  if Result <> errNone then
    EXIT;
  //Branch to merge block
  ThenBranch := ILAlloc(dtBranch);
  ThenBranch.FalseBlock := -1;  //Unconditional branch
  ThenLastID := GetCurrBlockID;
  ThenLastIndex := ILGetCount-1;

  //Test for ELSE
  Parser.SkipWhiteSpaceAll;
  if TestForIdent('else') then
  begin //If so, ELSE block
    NewBlock := True;
    Branch.FalseBlock := GetCurrBlockID + 1;

    //ParseBlock
    Result := ParseBlock(bsSingle);
    if Result <> errNone then
      EXIT;
    //Branch to merge block
    ElseBranch := ILAlloc(dtBranch);
    ElseBranch.FalseBlock := -1;  //Unconditional branch
    ElseLastID := GetCurrBlockID;
    ElseLastIndex := ILGetCount-1;

    //Branch to merge block
    NewBlock := True;
    ElseBranch.TrueBlock := GetCurrBlockID + 1;
    Parser.SkipWhiteSpaceAll;
  end
  else //No ELSE - if condition failed it jumps straight to the merge block
  begin
    NewBlock := True;
//    ElseBranch := nil;
    ElseLastID := -1;
    ElseLastIndex := -1;

    Branch.FalseBlock := GetCurrBlockID + 1;
  end;

  //Fixup branches at end of THEN and ELSE blocks
  NewBlock := True;
  ThenBranch.TrueBlock := GetCurrBlockID + 1;
//  MergeID := GetCurrBlockID + 1;

  //If we have two paths then we need to fixup any variable reads
  if ElseLastIndex >= 0 then
    BranchFixup(BranchIndex + 1, ThenLastIndex, ThenLastIndex + 1, ElseLastIndex);

  //phi expression(s) - for each variable
  if ElseLastIndex >= 0 then
    PhiWalk(ThenLastIndex, ElseLastIndex, BranchIndex, ThenLastID, ElseLastID)
  else
    PhiWalk(ThenLastIndex, -1, BranchIndex, ThenLastID, BranchID);
end;

function ParseStatement(Ident: String): TAssembleError;
var
  Ch: Char;
  Keyword: TKeyword;
  VarIndex: Integer;
  VarSub: Integer;
  ILItem: PILItem;
begin
  if Ident = '' then
  begin
    Parser.SkipWhiteSpace;

    Ch := Parser.TestChar;
    if Ch in csIdentFirst then
    begin
      Result := ParseIdentifier(#0, Ident);
      if Result <> errNone then
        EXIT;
    end
    else
      EXIT(errIdentifierExpected);
  end;

  //Do we have a keyword?
  Keyword := IdentToKeyword(Ident);
  if Keyword <> keyUNKNOWN then
  begin
    case Keyword of
    keyIF: Result := DoIF;
    else
      EXIT(errInvalidKeyword);
    end;
    EXIT;
  end
  else
  begin
    Parser.SkipWhiteSpace;
    if Parser.TestChar = ':' then
    begin
      Parser.SkipChar;
      if Parser.TestChar = '=' then
      begin
        Parser.SkipChar;
        Result := ParseExpression(ILItem);
        if Result <> errNone then
          EXIT;

        Parser.SkipWhiteSpace;
//        if not (Parser.TestChar in [';',#0]) then
//          EXIT(errNone);//          EXIT(errUnexpectedChar);

        FindOrAddVar(Ident, VarIndex, VarSub);
        VarSub := VarIndexIncWriteCount(VarIndex);
        ILItem.DestLoc := locVar;
        ILItem.DestData := VarIndex;
        ILItem.DestSub := VarSub;
      end
      else
        EXIT(errAssignmentExpected);
    end
    else
      EXIT(errSyntaxError);
  end;
end;

function ParseBlock(BlockState: TBlockState): TAssembleError;
var
  Ch: Char;
  Ident: String;
begin
  while True do
  begin
    Parser.SkipWhiteSpaceAll;

    Ch := Parser.TestChar;
    if Ch in csIdentFirst then
    begin
      Result := ParseIdentifier(#0, Ident);
      if Result <> errNone then
        EXIT;

      if (BlockState = bsBeginRead) and (CompareText(Ident, 'end') = 0) then
        EXIT(errNone);
      if CompareText(Ident, 'begin') = 0 then
      begin
        Parser.SkipWhiteSpaceAll;

        Result := ParseBlock(bsBeginRead);
        if Result <> errNone then
          EXIT;
        if BlockState = bsSingle then
          EXIT(errNone);
      end
      else
      begin
        Result := ParseStatement(Ident);
        if Result <> errNone then
          EXIT;

        repeat
          Parser.SkipWhiteSpaceAll;
          Ch := Parser.TestChar;
          if Ch = ';' then
            Parser.SkipChar;
        until Parser.EOF or not (Ch in [#0,';']);

        if BlockState = bsSingle then
          EXIT(errNone);
        if Parser.EOF then
          EXIT(errEndExpected);
      end;
    end
    else
      EXIT(errIdentifierExpected);
  end;
end;

initialization
  Parser := TSourceReader.Create;
finalization
  Parser.Free;
end.
