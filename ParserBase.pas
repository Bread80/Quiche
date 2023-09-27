unit ParserBase;

interface
uses Classes, SourceReader, ParseErrors, QTypes, Functions, ILData, Variables;

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
  keyELSE, keyEND, keyEXTERN,keyFOR, keyFORWARD, keyFUNCTION,
  keyIF, keyIN,
  keyMOD, keyNOT, keyOR, keyOUT, keyPROCEDURE,
  keySHL, keySHR,
  keyTHEN, keyTO, keyVAR, keyXOR,
  keyFALSE, keyTRUE, keyMAXINT, keyMININT //These are Consts and shouldn't be here!
  );

//Converts an identifier to it's keyXXX constant.
//If no match found, returns keyUnknown.
function IdentToKeyword(Ident: String): TKeyword;

//Test for := assignment operator
//If found, the parser consumes the characters,
//If not found, leaves the parser position unchanged
function TestAssignment: Boolean;

//Tests for a specific identifier.
//If found, the parser consumes the characters,
//if not found, leaves the parser position unchanged.
function TestForIdent(Ident: String): Boolean;

//Test for a Type Suffix Symbol.
//If found returns the type and consumes the characters,
//if not returns vtUnknown and leaves the Parser unchanged
function TestForTypeSymbol(out VarType: TVarType): TQuicheError;

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
var AttrCorrupts: TUsedRegSet;

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


const KeywordStrings: array[low(TKeyword)..high(TKeyword)] of String = (
  '',  //Placeholder for Unknown value
  'and', 'begin', 'const', 'div', 'do', 'downto',
  'else', 'end', 'extern', 'for', 'forward', 'function',
  'if', 'in',
  'mod', 'not', 'or', 'out', 'procedure',
  'shl', 'shr',
  'then', 'to', 'var', 'xor',
  'false','true','maxint','minint');

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
  Parser.SkipWhiteSpaceAll;
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

function TestAssignment: Boolean;
begin
  Parser.Mark;
  Parser.SkipWhiteSpaceAll;

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
  EXIT(False);
end;

function ParseIdentifier(First: Char;out Ident: String): TQuicheError;
var Ch: Char;
begin
  if First <> #0 then
    Ident := First
  else
  begin
    Parser.SkipWhiteSpaceAll;
    Parser.Mark;

    if not Parser.ReadChar(Ch) then
      EXIT(Err(qeIdentifierExpected));
    if Ch in csIdentFirst then
      Ident := Ch
    else
      EXIT(Err(qeIdentifierExpected));
  end;

  while True do
  begin
    Ch := Parser.TestChar;
    if Ch in csIdentOther then
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
  if Result = qeNone then
  begin
    VT := StringToVarType(Ident);
    if VT = vtUnknown then
      EXIT(ErrSub(qeUnknownType, Ident));
  end;
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

function ParseAttribute: TQuicheError;
var Ident: String;
  Ch: Char;
  InIXIY: Boolean;
  WhiteSpace: Boolean;
  Reg: TUsedReg;
const strCorruptsAttrError = 'Invalid corrupts attribute: ''%s''. Valid values are A,B,C,D,E,F,H,L,IX,IY,L,X,Y. Whitespace and comma separators are allowed.';
begin
  ParseIdentifier(#0, Ident);
  if CompareText(Ident, 'corrupts') = 0 then
  begin
    AttrCorrupts := [];
    Parser.SkipWhiteSpace;
    InIXIY := False;
    while True do
      if Parser.Readchar(Ch) then
      begin
        WhiteSpace := False;
        Ch := UpCase(Ch);
        case Ch of
          #0..#32,',': WhiteSpace := True;  //Whitespace and separators
          ']': EXIT(qeNone);
          'A': Reg := urA;
          'B': Reg := urB;
          'C': Reg := urC;
          'D': Reg := urD;
          'E': Reg := urE;
          'F': Reg := urFlags;
          'H': Reg := urH;
          'I':
            if InIXIY then
              EXIT(ErrMsg(qeAttributeError, Format(strCorruptsAttrError, ['I'])))
            else
              InIXIY := True;
          'L': Reg := urL;
          'X': Reg := urIX;
          'Y': Reg := urIY;
        else
          EXIT(ErrMsg(qeAttributeError, Format(strCorruptsAttrError, [Ch])));
        end;

        if not WhiteSpace then
          if InIXIY then
          begin
            if Reg in [urIX, urIY] then
              AttrCorrupts := AttrCorrupts + [Reg]
            else
            if Ch <> 'I' then
              EXIT(ErrMsg(qeAttributeError, Format(strCorruptsAttrError, [Ch])));
            InIXIY := False;
          end
          else
            AttrCorrupts := AttrCorrupts + [Reg];
      end
      else
        EXIT(Err(qeAttributeError))
  end
  else if CompareText(Ident, 'preservesall') = 0 then
  begin
    Parser.SkipWhiteSpace;
    Result := ParseIdentifier(#0, Ident);
    if Result <> qeNone then
      EXIT;
    Parser.SkipWhiteSpace;
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
