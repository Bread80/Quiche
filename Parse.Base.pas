unit Parse.Base;

interface
uses Classes,
  Def.Functions, Def.IL, Def.QTypes, Def.Variables,
  Parse.Errors, Parse.Source,
  Z80.CPU;

//NameSpace of the code we are currently parsing. I.e. the file name
var NameSpace: String;

//Load source code text from a file
procedure LoadFromFile(Filename: String);
procedure LoadFromStrings(SL: TStrings);
//Load source code text from a string
procedure LoadFromString(Source: String);

function ParserEOF: Boolean;

var Parser: TQuicheSourceReader;


//Keywords listed in alphabetical order (for convenience)
type TKeyword = (keyUNKNOWN,
  keyAND, keyBEGIN,
  keyCONST, keyDIV, keyDO, keyDOWNTO,
  keyELSE, keyEND, {keyEXTERN,} keyFOR, {keyFORWARD, }keyFUNCTION,
  keyIF, keyIN,
  keyMOD, keyNOT, keyOR, keyPROCEDURE,
  keySHL, keySHR,
  keyTHEN, keyTO, keyVAR, keyXOR,
  keyFALSE, keyTRUE, keyMAXINT, keyMININT //These are Consts and shouldn't be here!
  );

//Converts an identifier to it's keyXXX constant.
//If no match found, returns keyUnknown.
function IdentToKeyword(Ident: String): TKeyword;

//Test for := assignment operator
//Parser must be position on the first char of the identifier (':')
//If found, the parser consumes the characters,
//If not found, leaves the parser position unchanged
function TestAssignment: Boolean;

//Tests for a specific identifier.
//Parser must be position on the first char of the identifier
//If found, the parser consumes the characters,
//if not found, leaves the parser position unchanged.
function TestForIdent(Ident: String): Boolean;

//Test for a Type Suffix Symbol.
//If found returns the type and consumes the characters,
//if not returns vtUnknown and leaves the Parser unchanged
function TestForTypeSymbol(out VarType: TVarType): TQuicheError;

function TestSymbolFirst: Boolean;

function ParseSymbol(out Ident: String): TQuicheError;

//Is the next char to be consumed the start of an Identifier?
function TestIdentFirst: Boolean;

//Parses and returns an identifier.
//If First is anything other than #0 this will be used as the first character
//of the Identifier. This is useful where the character has already been consumed,
//from the Parser, for analysis.
function ParseIdentifier(First: Char;out Ident: String): TQuicheError;

//Parses the next item as a keyword.
//If the next item is not a keyword returns Keyword = keyUnknown
function ParseKeyword(out Keyword: TKeyword): TQuicheError;

//Parses the next item as a variable type.
//If the next item is not a known type returns VT = vtUnknown
function ParseVarType(out VT: TVarType): TQuicheError;

//Attribute data
var AttrCorrupts: TCPURegSet;

//To be called /after/ the leading '[' has been consumed
//Valid attributes:
//[Corrupts <register list>]
//where <register list> is a list of register names (A,B,C,D,E,H,L,BC,DE,HL,F,IX,IY)
//optionally separated by whitespace or commas
//Specifies a list of registers which are corrupted by the following code
//[PreservesAll]
//Specifies that the following code does not corrupt any registers
function ParseAttribute: TQuicheError;


//=====
//Skip mode enables IL generation to easily be skipped or rolled back by
//stopping certain internal actions. Skip mode is used to avoid generating code
//which will never be exectuted

//Resets skip mode to initial state (off)
procedure InitialiseSkipMode;

//Get current skip mode state
function SkipMode: Boolean;

//Enable parameter enables code to conditionally enter skip mode without
//caring whether the skip mode is actually needed
//Enables Skip mode if Enable is true.
//Does nothing if Enable is False
//Either way, returns previous skip mode. This value MUST be passed to SkipModeEnd
function SkipModeStart(Enable: Boolean): Boolean;

//To be called at the end of Skip mode. PrevSkipMode MUST be the value returned by the
//previous call to SkipModeStart
procedure SkipModeEnd(PrevSkipMode: Boolean);

implementation
uses SysUtils, IOUtils, Generics.Collections;

procedure LoadFromString(Source: String);
begin
  Parser.LoadFromString(Source);
  NameSpace := '';
end;

procedure LoadFromStrings(SL: TStrings);
begin
  Parser.LoadFromStrings(SL);
  NameSpace := '';
end;

procedure LoadFromFile(Filename: String);
begin
  Parser.OpenFile(Filename);
  NameSpace := TPath.GetFilename(Filename);
end;

function ParserEOF: Boolean;
begin
  Result := Parser.EOF;
end;


//===============================================
//Parsing


function IdentToKeyword(Ident: String): TKeyword;
const KeywordStrings: array[low(TKeyword)..high(TKeyword)] of String = (
  '',  //Placeholder for Unknown value
  'and', 'begin', 'const', 'div', 'do', 'downto',
  'else', 'end', 'for', 'function',
  'if', 'in',
  'mod', 'not', 'or', 'procedure',
  'shl', 'shr',
  'then', 'to', 'var', 'xor',
  'false','true','maxint','minint');
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
  S := '';
  Parser.Mark;

  if CharInSet(Parser.TestChar, csIdentFirst) then
  begin
    repeat
      Parser.ReadChar(Ch);
      S := S + Ch;
    until not CharInSet(Parser.TestChar, csIdentOther);

    Result := CompareText(Ident, S) = 0;
  end
  else
    Result := False;

  if not Result then
    Parser.Undo;
end;

function TestAssignment: Boolean;
begin
  Parser.SkipWhite; //<--TO BE REMOVED WHEN ASSIGNMENT REWRITTEN
  Parser.Mark;

  if Parser.TestChar = ':' then
  begin
    Parser.SkipChar;
    if Parser.TestChar = '=' then
    begin
      Parser.SkipChar;
      EXIT(True);
    end;
  end;
  Parser.Undo;
  Result := False;
end;

function TestSymbolFirst: Boolean;
begin
  Parser.SkipWhite; //<--TO BE REMOVED
  Result := CharInSet(Parser.TestChar, csSymbolFirst);
end;

function ParseSymbol(out Ident: String): TQuicheError;
var Ch: Char;
begin
  Ident := '';

  Parser.SkipWhite; //<--TO BE REMOVED
  Parser.Mark;

  if not Parser.ReadChar(Ch) then
    EXIT(Err(qeOperatorExpected));
  if CharInSet(Ch, csSymbolFirst) then
    Ident := Ch
  else
    EXIT(Err(qeIdentifierExpected));

  while True do
  begin
    Ch := Parser.TestChar;
    if CharInSet(Ch, csSymbolOther) then
      Ident := Ident + Ch
    else
      EXIT(qeNone);
    Parser.SkipChar;
  end;
end;

function TestIdentFirst: Boolean;
begin
  Parser.SkipWhite; //<--TO BE REMOVED
  Result := CharInSet(Parser.TestChar, csIdentFirst);
end;

function ParseIdentifier(First: Char;out Ident: String): TQuicheError;
var Ch: Char;
begin
  if First <> #0 then
    Ident := First
  else
  begin
    Parser.SkipWhite; //<--TO BE REMOVED
    Parser.Mark;

    if not Parser.ReadChar(Ch) then
      EXIT(Err(qeIdentifierExpected));
    if CharInSet(Ch, csIdentFirst) then
      Ident := Ch
    else
      EXIT(Err(qeIdentifierExpected));
  end;

  while True do
  begin
    Ch := Parser.TestChar;
    if CharInSet(Ch, csIdentOther) then
      Ident := Ident + Ch
    else
      EXIT(qeNone);
    Parser.SkipChar;
  end;
end;

function ParseKeyword(out Keyword: TKeyword): TQuicheError;
var Ident: String;
begin
  Result := ParseIdentifier(#0, Ident);
  if Result = qeNone then
    Keyword := IdentToKeyword(Ident);
end;

function ParseVarType(out VT: TVarType): TQuicheError;
var Ident: String;
begin
  Result := ParseIdentifier(#0, Ident);
  if Result <> qeNone then
    EXIT;

  VT := StringToVarType(Ident);
  if VT = vtUnknown then
    EXIT(ErrSub(qeUnknownType, Ident));
end;

function TestForTypeSymbol(out VarType: TVarType): TQuicheError;
var Ch: Char;
begin
  Ch := Parser.TestChar;
  case Ch of
    '%': VarType := vtInteger;
    '$': VarType := vtString;
    '#': VarType := vtWord;
    '!': VarType := vtReal;
    '^': VarType := vtPointer;
    '?': VarType := vtBoolean;
  else
    VarType := vtUnknown;
  end;

  if VarType <> vtUnknown then
  begin
    Parser.SkipChar;
    if VarType in [vtString, vtWord, vtInteger] then
    begin
      if Parser.TestChar = Ch then
      begin
        case VarType of
          vtString: VarType := vtChar;
          vtWord: VarType := vtByte;
          vtInteger: VarType := vtInt8;
        end;
        Parser.SkipChar;
      end;
    end;

    if VarType in [vtReal, vtString] then
      EXIT(ErrMsg(qeTodo, 'Type not yet supported: ' + VarTypeToName(VarType)));
  end;

  Result := qeNone;
end;

//Called after '[Corrupts' has been parsed
function ParseCorruptsAttribute: TQuicheError;
type TIXIYState = (xsNone, xsIRead, xsWaitingXY);
var
  Ch: Char;
  IXIYState: TIXIYState;
  WhiteSpace: Boolean;
  Reg: TCPUReg;
const
  strCorruptsAttrError = 'Invalid corrupts attribute: ''%s''. Valid values are A,B,C,D,E,F,H,L,IX,IY,L,X,Y. Whitespace and comma separators are allowed.';
  strXorYRequired = 'X or Y required after I in Corrupts attribute (IX or IY register)';
begin
  AttrCorrupts := [];
  Parser.SkipWhite;  //<--TO BE REMOVED
  IXIYState := xsNone;
  Reg := rNone;
  while True do
  begin
    Parser.Mark;
    if Parser.Readchar(Ch) then
    begin
      WhiteSpace := False;
      case Upcase(Ch) of
        #0..#32,',': WhiteSpace := True;  //Whitespace and separators
        ']':
          if IXIYState <> xsNone then
            EXIT(ErrMsg(qeAttributeError, StrXOrYRequired))
          else
            EXIT(qeNone);
        'A': Reg := rA;
        'B': Reg := rB;
        'C': Reg := rC;
        'D': Reg := rD;
        'E': Reg := rE;
        'F': Reg := rFlags;
        'H': Reg := rH;
        'I':
          if IXIYState <> xsNone then
            EXIT(ErrMsg(qeAttributeError, StrXOrYRequired))
          else
            IXIYState := xsIRead;
        'L': Reg := rL;
        'X':
          if IXIYState = xsWaitingXY then
            Reg := rIX
          else
            EXIT(ErrMsg(qeAttributeError, StrXOrYRequired));
        'Y':
          if IXIYState = xsWaitingXY then
            Reg := rIY
          else
            EXIT(ErrMsg(qeAttributeError, StrXOrYRequired));
      else
        EXIT(ErrMsg(qeAttributeError, Format(strCorruptsAttrError, [Ch])));
      end;

      if IXIYState = xsWaitingXY then
      begin
        if Reg in [rIX, rIY] then
          AttrCorrupts := AttrCorrupts + [Reg]
        else
          EXIT(ErrMsg(qeAttributeError, StrXOrYRequired));
        IXIYState := xsNone;
      end
      else
        if (not WhiteSpace) and (IXIYState = xsNone) then
          AttrCorrupts := AttrCorrupts + [Reg];

      if IXIYState = xsIRead then
        IXIYState := xsWaitingXY;
    end
    else
      EXIT(ErrMsg(qeAttributeError, Format(strCorruptsAttrError, [Ch])))
  end;
end;


function ParseAttribute: TQuicheError;
var Ident: String;
begin
  Result := ParseIdentifier(#0, Ident);
  if Result <> qeNone then
    EXIT(ErrMsg(qeAttributeError, 'Attribute name expected'));

  if CompareText(Ident, 'corrupts') = 0 then
    EXIT(ParseCorruptsAttribute)
  else if CompareText(Ident, 'preservesall') = 0 then
  begin
{    Parser.SkipWhiteSpace;
    Result := ParseIdentifier(#0, Ident);
    if Result <> qeNone then
      EXIT;
}    Parser.SkipWhite;
    if Parser.TestChar <> ']' then
      EXIT(ErrMsg(qeAttributeError, '''['' expected at end of attribute'));
    Parser.SkipChar;
  end
  else
    EXIT(ErrMsg(qeAttributeError, 'Unknown attribute: ' + Ident));
end;

var CurrSkipMode: Boolean;

procedure InitialiseSkipMode;
begin
  CurrSkipMode := False;
end;

//Get current skip mode state
function SkipMode: Boolean;
begin
  Result := CurrSkipMode;
end;

//Enables Skip mode if Enable is true.
//Does nothing if Enable is False
//Either way, returns previous skip mode
function SkipModeStart(Enable: Boolean): Boolean;
begin
  //Only when turning on skip mode!
  if Enable and not CurrSkipMode then
  begin //Mark current positions
    ILMark;
    VarMark;
  end;
  Result := CurrSkipMode;
  if Enable then
    CurrSkipMode := True;
end;

procedure SkipModeEnd(PrevSkipMode: Boolean);
begin
  //Only when disabling SkipMode
  if CurrSkipMode and not PrevSkipMode then
  begin
    ILRollback;
    VarRollback;
  end;
  CurrSkipMode := PrevSkipMode;
end;


initialization
  Parser := TQuicheSourceReader.Create;
  InitialiseSkipMode;
finalization
  Parser.Free;
end.
