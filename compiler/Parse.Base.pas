unit Parse.Base;

interface
uses Classes, Generics.Collections, SysUtils,
  Def.Compiler, Def.IL, Def.VarTypes, Def.Variables, Def.UserTypes, Def.Consts, Def.Scopes,
  Parse.Errors, Parse.Source,
  Z80.Hardware;

//NameSpace of the code we are currently parsing. I.e. the file name
(*var NameSpace: String;*)

//Load source code text from a file
procedure LoadFromFile(Filename: String);
procedure LoadFromStrings(SL: TStrings);
//Load source code text from a string
procedure LoadFromString(Source: String);

function ParserEOF: Boolean;

var Parser: TQuicheSourceReader;


//Keywords listed in alphabetical order (for convenience)
type TKeyword = (keyUNKNOWN,
  keyAND, keyARRAY, keyBEGIN,
  keyCONST, keyDIV, keyDO, keyDOWNTO,
  keyELSE, keyEND, keyFOR, keyFUNCTION,
  keyIF, keyIN, keyLIST, keyLONG,
  keyMOD, keyNOT, keyOR, keyPROCEDURE, keyPROGRAM,
  keyRECORD, keyREPEAT,
  keySET, keySHL, keySHORT, keySHR,
  keyTHEN, keyTO, keyTYPE, keyUNTIL, keyVAR, keyXOR,
  keyVECTOR,
  keyWHILE
  );

//Converts an identifier to it's keyXXX constant.
//If no match found, returns keyUnknown.
function IdentToKeyword(Ident: String): TKeyword;

//Test for := assignment operator
//Parser must be position on the first char of the identifier (':')
//If found, the parser consumes the characters,
//If not found, leaves the parser position unchanged
function TestAssignment: Boolean;

//Tests for ..
//If Found the symbol will be consumed, otherwise consumes nothing
function TestRangeOperator: Boolean;

//Tests for a specific identifier.
//Parser must be position on the first char of the identifier
//If found, the parser consumes the characters,
//if not found, leaves the parser position unchanged.
function TestForIdent(Ident: String): Boolean;

//Is the next char the first char of a symbol?
function TestSymbolFirst: Boolean;

function ParseSymbol(out Ident: String): TQuicheError;

//Tests whether the identifier has already been declared in the current scope,
//and whether it is a keyword. If so returns a suitable error
function TestUniqueIdentifier(const Ident: String): TQuicheError;

//Is the next char to be consumed the start of an Identifier?
function TestIdentFirst: Boolean;

//Parses and returns an identifier.
//If First is anything other than #0 this will be used as the first character
//of the Identifier. This is useful where the character has already been consumed,
//from the Parser, for analysis.
function ParseIdentifier(First: Char;out Ident: String): TQuicheError;
//If Ident = '' then parses an identifier and returns it in Ident
//Otherwise leaves Ident untouched
function ParseIdentifierIfNone(var Ident: String): TQuicheError;

//Parses an identifier. If the identifier is already defined in the current
//scope returns a suitable error
function ParseUniqueIdentifier(First: Char;out Ident: String): TQuicheError;
//If Ident is '' reads an identifier, otherwises uses value passed in Ident
//Tests whether the identifier is unique in the curren scope and returns a
///suitable error if not
function ParseUniqueIdentifierIfNone(var Ident: String): TQuicheError;

//Parses the next item as a keyword.
//If the next item is not a keyword returns Keyword = keyUnknown
function ParseKeyword(out Keyword: TKeyword): TQuicheError;

//Test for a Type Suffix Symbol.
//If found returns the type and consumes the characters,
//if not returns vtUnknown and leaves the Parser cursor unchanged
function TestForTypeSymbol(out UserType: TUserType): TQuicheError;

//Parses a comma separated list of identifiers.
//Each must not be a keyword. Each must be unique within the list.
//If Unique is True each must be undeclared in the current scope.
function ParseIdentifierList(Unique: Boolean;out Items: TArray<String>): TQuicheError;

//Is the next char to be consumed the first char of a literal.
//Includes numbers, strings, chars, arrays, lists etc.
function TestLiteralFirst: Boolean;

//Attribute data
var AttrPreserves: TCPURegSet;

//To be called /after/ the leading '[' has been consumed
//Valid attributes:
//[Corrupts <register list>]
//where <register list> is a list of register names (A,B,C,D,E,H,L,BC,DE,HL,F,IX,IY)
//optionally separated by whitespace or commas
//Specifies a list of registers which are corrupted by the following code
//[PreservesAll]
//Specifies that the following code does not corrupt any registers
function ParseAttribute: TQuicheError;

//IL will be converted to assembler by calling OnScopeDone. This is called at the
//end of a function declaration, or  the end of the program for root level code.
type TAssembleCallback = TFunc<TILScope, Boolean>;

type TParseData = class
  private
    FILScopes: TStack<TILScope>;
    FParseScopes: TStack<TScope>;
    FOnScopeDone: TAssembleCallback;
    //Do any processing which is required when changing scopes (Uses stack TOSes)
    procedure SetCurrentScopes;
    function GetILScope: TILScope;
    function GetParseScope: TScope;
  public
    constructor Create;
    destructor Destroy;override;

    //Make Scope the currenr scope
    procedure OpenILScope(Scope: TILScope);
    function CloseILScope(Scope: TILScope): TILScope;

    function OpenParseScope: TScope;
    //Closes the currently open parse scope. If the scope contains no declarations
    //it will be trimmed (ie deleted).
    //If the scope was trimmed returns nil, otherwise returns Scope
    function CloseParseScope(Scope: TScope): TScope;
    procedure RollBackParseScope(Scope: TScope);

    //Current Program, Unit or Function (whilst parsing)
    property ILScope: TILScope read GetILScope;
    //The current code block (whilst parsing). Initially the same as ILScope
    property ParseScope: TScope read GetParseScope;

    property OnScopeDone: TAssembleCallback read FOnScopeDone write FOnScopeDone;

  end;

function ParseData: TParseData;

procedure InitParseData;

implementation
uses
  {$ifndef fpc}IOUtils,{$endif}
  Parse;

procedure LoadFromString(Source: String);
begin
  Parser.LoadFromString(Source);
//  NameSpace := '';
end;

procedure LoadFromStrings(SL: TStrings);
begin
  Parser.LoadFromStrings(SL);
//  NameSpace := '';
end;

procedure LoadFromFile(Filename: String);
begin
  Parser.OpenFile(Filename);
{$ifdef fpc}
//  Namespace := ExtractFilePath(Filename);
{$else}
//  NameSpace := TPath.GetFilename(Filename);
{$endif}
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
  'and', 'array', 'begin', 'const', 'div', 'do', 'downto',
  'else', 'end', 'for', 'function',
  'if', 'in', 'list', 'long',
  'mod', 'not', 'or', 'procedure', 'program',
  'record', 'repeat',
  'set', 'shl', 'short', 'shr',
  'then', 'to', 'type', 'until', 'var', 'xor',
  'vector',
  'while');
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

function TestRangeOperator: Boolean;
var Cursor: TParseCursor;
begin
  Cursor := Parser.GetCursor;
  if Parser.TestChar = '.' then
  begin
    Parser.SkipChar;
    if Parser.TestChar = '.' then
    begin
      Parser.SkipChar;
      EXIT(True);
    end;
  end;
  Parser.SetCursor(Cursor);
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

function ParseIdentifierIfNone(var Ident: String): TQuicheError;
begin
  if Ident = '' then
    Result := ParseIdentifier(#0, Ident)
  else
    Result := qeNone;
end;

function TestUniqueIdentifier(const Ident: String): TQuicheError;
begin
  if IdentToKeyword(Ident) <> keyUNKNOWN then
    EXIT(ErrSub(qeReservedWord, Ident));

  if ParseData.ParseScope.FindIdentifierUpLocal(Ident) <> nil then
    EXIT(ErrSub(qeIdentifierRedeclared, Ident));

  Result := qeNone;
end;

function ParseUniqueIdentifier(First: Char;out Ident: String): TQuicheError;
begin
  Result := ParseIdentifier(First, Ident);
  if Result = qeNone then
    Result := TestUniqueIdentifier(Ident);
end;

function ParseUniqueIdentifierIfNone(var Ident: String): TQuicheError;
begin
  Result := ParseIdentifierIfNone(Ident);
  if Result = qeNone then
    Result := TestUniqueIdentifier(Ident);
end;

function ParseKeyword(out Keyword: TKeyword): TQuicheError;
var Ident: String;
begin
  Result := ParseIdentifier(#0, Ident);
  if Result = qeNone then
    Keyword := IdentToKeyword(Ident);
end;

function TestForTypeSymbol(out UserType: TUserType): TQuicheError;
var Ch: Char;
  VarType: TVarType;
  Cursor: TParseCursor;
begin
  Cursor := Parser.GetCursor;

  Ch := Parser.TestChar;
  case Ch of
    '%': VarType := vtInteger;
    '$': VarType := vtChar;   //We'll convert to string in a moment
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
    if VarType in [vtChar, vtWord, vtInteger] then
    begin
      //Double sigil
      if Parser.TestChar = Ch then
      begin
        case VarType of
          vtChar:   ; //Stay as is
          vtWord:     VarType := vtByte;
          vtInteger:  VarType := vtInt8;
        end;
        Parser.SkipChar;
      end
      else  //Not double sigil
        if VarType = vtChar then
        begin
          UserType := GetSystemStringType;
          EXIT(qeNone);
        end;
    end;
  end;

  if VarType = vtUnknown then
    Parser.SetCursor(Cursor);
  UserType := GetSystemType(VarType);
  Result := qeNone;
end;

function ParseIdentifierList(Unique: Boolean;out Items: TArray<String>): TQuicheError;
var Ident: String;
  S: String;
begin
  SetLength(Items, 0);

  Result := Parser.SkipWhite;
  if Result <> qeNone then
    EXIT;

  while True do
  begin
    if Unique then
      Result := ParseUniqueIdentifier(#0, Ident)
    else
      Result := ParseIdentifier(#0, Ident);
    if Result <> qeNone then
      EXIT;

    //Check ident is unique in the list
    for S in Items do
      if CompareText(S, Ident) = 0 then
        EXIT(ErrSub(qeIdentifierRedeclared, Ident));

    //Add to list
    SetLength(Items, Length(Items)+1);
    Items[Length(Items)-1] := Ident;

    Result := Parser.SkipWhite;
    if Result <> qeNone then
      EXIT;

    if Parser.TestChar <> ',' then
      EXIT(qeNone);
    Parser.SkipChar;

    Result := Parser.SkipWhiteNL;
    if Result <> qeNone then
      EXIT;
  end;
end;

function TestLiteralFirst: Boolean;
begin
  //May need updating for reals
  Result := CharInSet(Parser.TestChar, ['0'..'9','$','#','%','"','''','[','(']);
end;

//Called after '[Corrupts' has been parsed
function ParseCorruptsAttribute: TQuicheError;
type TIXIYState = (xsNone, xsIRead, xsWaitingXY);
var
  Ch: Char;
  IXIYState: TIXIYState;
  WhiteSpace: Boolean;
  Reg: TCPUReg;
begin
  AttrPreserves := CPURegsAll;
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
      //TODO: Carry flag and Zero flag
        #0..#32,',': WhiteSpace := True;  //Whitespace and separators
        ']':
          if IXIYState <> xsNone then
            EXIT(Err(qeXOrYExpected))
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
            EXIT(Err(qeXOrYExpected))
          else
            IXIYState := xsIRead;
        'L': Reg := rL;
        'X':
          if IXIYState = xsWaitingXY then
            Reg := rIX
          else
            EXIT(Err(qeXOrYExpected));
        'Y':
          if IXIYState = xsWaitingXY then
            Reg := rIY
          else
            EXIT(Err(qeXOrYExpected));
      else
        EXIT(ErrSub(qeInvalidCorruptsAttr, Ch));
      end;

      if IXIYState = xsWaitingXY then
      begin
        if Reg in [rIX, rIY] then
          AttrPreserves := AttrPreserves - [Reg]
        else
          EXIT(Err(qeXOrYExpected));
        IXIYState := xsNone;
      end
      else
        if (not WhiteSpace) and (IXIYState = xsNone) then
          AttrPreserves := AttrPreserves - [Reg];

      if IXIYState = xsIRead then
        IXIYState := xsWaitingXY;
    end
    else
      EXIT(ErrSub(qeInvalidCorruptsAttr, Ch));
  end;
end;

function ParseAttribute: TQuicheError;
var Ident: String;
begin
  Result := ParseIdentifier(#0, Ident);
  if Result <> qeNone then
    EXIT(ErrSub(qeInvalidAttrName, Ident));

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
      EXIT(Err(qeCloseSquareExpectedAttr));
    Parser.SkipChar;
  end
  else
    EXIT(ErrSub(qeInvalidAttrName, Ident));
end;


var ParseDataVar: TParseData;

function ParseData: TParseData;
begin
  Assert(Assigned(ParseDataVar));
  Result := ParseDataVar;
end;

procedure InitParseData;
begin
  if Assigned(ParseDataVar) then
    ParseDataVar.Free;
  ParseDataVar := TParseData.Create;
end;

{ TParseData }

function TParseData.CloseILScope(Scope: TILScope): TILScope;
begin
  Assert(FILScopes.Peek = Scope);
  CloseParseScope(Scope);
  FILScopes.Pop;
  Result := ILScope;
  SetCurrentScopes;
end;

function TParseData.CloseParseScope(Scope: TScope): TScope;
var ILIndex: Integer;
begin
  Assert(FParseScopes.Peek = Scope);
//  Assert(not Scope.IsEnded);
  ILIndex := ILScope.ILList.Count;
  Result := Scope.EndCodeBlock(ILIndex);
  FParseScopes.Pop;
  SetCurrentScopes;
end;

constructor TParseData.Create;
begin
  inherited;
  FILScopes := TStack<TILScope>.Create;
  FParseScopes := TStack<TScope>.Create;
end;

destructor TParseData.Destroy;
begin
  FILScopes.Free;
  FParseScopes.Free;
  inherited;
end;

function TParseData.GetILScope: TILScope;
begin
  if FILScopes.Count = 0 then
    Result := nil
  else
    Result := FILScopes.Peek;
end;

function TParseData.GetParseScope: TScope;
begin
  if FParseScopes.Count = 0 then
    Result := nil
  else
    Result := FParseScopes.Peek;
end;

procedure TParseData.OpenILScope(Scope: TILScope);
begin
  FILScopes.Push(Scope);
  FParseScopes.Push(Scope);
  SetCurrentScopes;
end;

function TParseData.OpenParseScope: TScope;
var ILIndex: Integer; //Current IL Position (ie first item in new scope)
begin
  ILIndex := ILScope.ILList.Count;
  Result := ParseScope.BeginCodeBlock(ILIndex);
  FParseScopes.Push(Result);
  SetCurrentScopes;
end;

procedure TParseData.RollBackParseScope(Scope: TScope);
begin
//  Assert(Scope.IsEnded);
  ILRollback(Scope.ILIndexBegin);
  Assert(Assigned(Scope.Owner));
  Scope.Owner.RemoveChild(Scope);
end;

procedure TParseData.SetCurrentScopes;
begin
  if FILScopes.Count > 0 then
    SetCurrentILList(ILScope.ILList);
end;

initialization
  Parser := TQuicheSourceReader.Create;
  ParseDataVar := nil;
finalization
  FreeAndNil(ParseDataVar);
  Parser.Free;
end.
