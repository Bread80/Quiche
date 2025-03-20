{$ifdef fpc}
  {$mode delphi}
{$endif}
unit Parse.Source;

interface
uses Classes,
  Parse.Errors;

const
  csWhiteSpace = [#0..' '];
  csIdentFirst = ['A'..'Z','a'..'z',{'.',}'_'];
  csIdentOther = ['0'..'9','A'..'Z','a'..'z',{'.',}'_'];
  csDecimalFirst = ['0'..'9'];
  csHexChars = ['0'..'9','a'..'f','A'..'F'];
  csSymbolFirst = ['*','/','+','-','=','<','>'];
  csSymbolOther = ['=','>'];  //For <= and <>

type TParseState = (psCode, psCurlyComment, psBraceComment);

//Used to preserve parser state if we need to undo, and for reporting errors
type TParseCursor = record
    Filename: String;
    LineNo: Integer;
    Column: Integer;
    Indent: Integer;  //Indent level of the selected line

    constructor Create(const AFilename: String;ALineNo, AColumn, AIndent: Integer);
  end;

//Line oriented source code reader
//Can read data from a file or string
//Most functions won't read past the end of the current line
type TBaseReader = class
  private
    FFilename: String;
    FLineNo: Integer;
    FColumn: Integer;
    FIndent: Integer;
    FLines: TStringList;
    FLine: String;
    FEOF: Boolean;
    FState: TParseState;

    FMarkLineNo: Integer;
    FMarkPos: Integer;
    FMarkCursor: TParseCursor;
    function GetSource(Index: Integer): String;
    function GetEOLN: Boolean;
  public
    destructor Destroy;override;
    function OpenFile(AFilename: String): Boolean;
    //Closes any open file, sets AString as the source data and Resets
    procedure LoadFromString(AString: String);
    //Reset to the start of the current file or string
    procedure LoadFromStrings(SL: TStrings);
    procedure Reset;

    //Skips to the start of the next line.
    //Returns False if we hit the end of the file/string
    function NextLine: Boolean;
    //Reads the next character, skipping to the next line if needed
    //Returns False if EOF
    function NextChar(out Ch: Char): Boolean;
    //Reads the next character on the current line.
    //Returns False if EOLN or EOF
    function ReadChar(out Ch: Char): Boolean;
    //Skip current character on current line
    //Returns True if a char was skipped, False if EOLN or EOF
    function SkipChar: Boolean;
    //If the next char is the the given char skips it.
    //Returns true if the next char was the given one
    function SkipCharIf(Ch: Char): Boolean;
    //Get the next char on the current line without moving the current position
    //If no char available (EOF, EOLN) returns #0
    function TestChar: Char;

    //Skips whitespace but not new lines.
    //Returns True if we are at the end of the line
    function SkipWhiteChars: Boolean;


    function GetCursor: TParseCursor;
    //Restore the cursor to a previous position
    procedure SetCursor(const Cursor: TParseCursor);

    //Stores current read position, for possible Undo call
    //Note that only one Mark position is available. A second call will overwrite
    //previous setting
    procedure Mark;
    //Restores the read position to the current Mark position.
    //Can be used after a test for text or an identifier fails.
    procedure Undo;

    function CursorToString: String;

    property FileName: String read FFileName;
    property EOLN: Boolean read GetEOLN;
    property EOF: Boolean read FEOF;
    property Pos: Integer read FColumn;
    property LineNo: Integer read FLineNo;
    property Line: String read FLine;
    //Saved cursor position
    property MarkCursor: TParseCursor read FMarkCursor;
    property MarkLineNo: Integer read FMarkLineNo;
    property MarkPos: Integer read FMarkPos;
    property Source[Index: Integer]: String read GetSource;
  end;

  TGenericReader = class(TBaseReader)
  public
    //Reads and returns an identifier.
    //An identifier begins with an alpha or underscore,
    //and continues while alpha, numeric or underscore
    function ReadIdentifier: String;
    //Returns a hex ($ prefix) or decimal number
    function ReadNumber: String;

    //Reads and returns all chars from cursor to end of line
    function ReadLine: String;
  end;

  TQuicheSourceReader= class(TBaseReader)
  private
    function DoSkipWhiteLine: TQuicheError;

    // { .. } comment
    //Also parses compiler directives (TODO)
    //If NewLines then multiple lines of comment camn be consumed,
    //   If not the function will terminate (qeNone) if a new line is encountered
    //     with FState as psCurlyComment
    //Returns False if EOF or (EOLN and NewLines is False).
    //Returns True if the comment terminated correctly at a }
    function SkipCurlyCommentLine(out State: TParseState): TQuicheError;
    // (* *) commant. Initial ( must have been read. Bonus if the initial * has also been read
    function SkipBraceCommentLine(out State: TParseState): TQuicheError;
    // // .. EOLN/EOF commment
    //Doesn't NOT read the EOLN characters (I.e. does not advance to the next line, and EOLN
    //or EOF will always be True after calling this.
    procedure SkipSlashComment;
  public
    //Skip whitespace and comments but NOT newlines (does NOT skip comments which
    //span new lines)
    function SkipWhite: TQuicheError;
    //Skip whitespace and comments including new lines (including comments which
    //span new lines)
    function SkipWhiteNL: TQuicheError;
    //Skips whitespace and comments up to and including the start of the next line
    //Returns an error if there was content (other than whitespace and comments) before the
    //line end
    function SkipToNextLine: TQuicheError;
    //Skips whitespace, comments, newlines and semicolons
    //If SeparatorRequired is True an error will be returned if neither a semicolon
    //new line is read.
    function NextStatement(SeparatorRequired: Boolean): TQuicheError;
    //End-of-Statement
    //Returns True if:
    //Next char is a statement separator (semicolon ; )
    //End-of-line
    //End-of-file
    //TODO: may be extended to parse exnted-to-next-line marker ( \ char?)
    //Note: Does NOT skip any whitespace
    function EOS: Boolean;
  end;

implementation
uses SysUtils,
  Parse.Directives;

{ TParseCursor }

constructor TParseCursor.Create(const AFilename: String; ALineNo, AColumn,
  AIndent: Integer);
begin
  Filename := AFilename;
  LineNo := ALineNo;
  Column := AColumn;
  Indent := AIndent;
end;

{ TBaseReader }

function TBaseReader.CursorToString: String;
begin
  Result := 'Line ' + LineNo.ToString + ', column ' + FColumn.ToString;
end;

destructor TBaseReader.Destroy;
begin
  FLines.Free;
  inherited;
end;

function TBaseReader.GetCursor: TParseCursor;
begin
  Result.Create(FFilename, FLineNo, FColumn, FIndent);
end;

function TBaseReader.GetEOLN: Boolean;
begin
  Result := FEOF or (FColumn >= Length(Line));
end;

function TBaseReader.GetSource(Index: Integer): String;
begin
  if Index <= FLines.Count then
    Result := FLines[Index-1]
  else
    Result := ';<No such line>';
end;

procedure TBaseReader.LoadFromString(AString: String);
begin
  if FLines = nil then
    FLines := TStringList.Create;
  FFilename := '';
  FLines.Text := AString;
  Reset;
end;

procedure TBaseReader.LoadFromStrings(SL: TStrings);
begin
  if FLines = nil then
    FLines := TStringList.Create;
  FLines.Assign(SL);
  FFilename := '';
  Reset;
end;

procedure TBaseReader.Mark;
begin
  FMarkLineNo := LineNo;
  FMarkPos := Pos;
  FMarkCursor := GetCursor;
end;

function TBaseReader.NextChar(out Ch: Char): Boolean;
begin
  while FColumn >= Length(FLine) do
    if not NextLine then
      EXIT(False);

  Result := ReadChar(Ch);
end;

function TBaseReader.NextLine: Boolean;
begin
  if FLines = nil then
    EXIT(False);
  if FLineNo < FLines.Count then
  begin
    FLineNo := FLineNo + 1;
    FLine := FLines[FLineNo-1];
    FColumn := 0;
  end
  else
    FEOF := True;
  Result := not FEOF;
end;

function TBaseReader.OpenFile(AFilename: String): Boolean;
begin
  if FLines <> nil then
    raise Exception.Create('File already open');
  FFilename := AFilename;
  FLines := TStringList.Create;
  FLines.LoadFromFile(Filename);
  FFilename := Filename;
  Reset;
  Result := True;
end;

function TBaseReader.ReadChar(out Ch: Char): Boolean;
begin
  Result := FColumn < Length(FLine);
  if Result then
  begin
    Ch := FLine.Chars[FColumn];
    inc(FColumn);
  end
  else
  begin
    Ch := #0;
    if not Assigned(FLines) then
      FEOF := True
    else
      FEOF := FLineNo >= FLines.Count;
  end;
end;

procedure TBaseReader.Reset;
begin
  FEOF := False;
  FColumn := 0;
  if FLines <> nil then
  begin
    FLineNo := 1;
    FLine := FLines[0];
  end
  else
  begin
    FEOF := True;
    FLineNo := -1;
  end;
  FMarkLineNo := -1;
  FState := psCode;
end;

procedure TBaseReader.SetCursor(const Cursor: TParseCursor);
begin
  Assert(Filename = Cursor.Filename, 'Can''t set cursor into a different file');
  FLineNo := Cursor.LineNo;
  FColumn := Cursor.Column;
  FIndent := Cursor.Indent;
  if Assigned(FLines) then
    FLine := FLines[FLineNo-1];
end;

function TBaseReader.SkipChar: Boolean;
begin
  Result :=  FColumn < Length(FLine);
  if Result then
    inc(FColumn);
end;

function TBaseReader.SkipCharIf(Ch: Char): Boolean;
begin
  Result := TestChar = Ch;
  if Result then
    SkipChar;
end;

function TBaseReader.SkipWhiteChars: Boolean;
begin
  while (FColumn < Length(FLine)) and CharInSet(FLine.Chars[FColumn], csWhiteSpace) do
    inc(FColumn);
  Result := FColumn >= Length(FLine);
end;

function TBaseReader.TestChar: Char;
begin
  if FColumn < Length(FLine) then
    Result := FLine.Chars[FColumn]
  else
    Result := #0;
end;

procedure TBaseReader.Undo;
begin
  //This shouldn't be able to happen.
  //Wee need nested/stacked Mark points
  if FMarkLineNo < 0 then
    EXIT;
//  Assert(FMarkLineNo >= 0, 'Unmatched Parser.Undo/Parser.Mark');
  FLineNo := FMarkLineNo;
  FColumn := FMarkPos;
  if FLines <> nil then
    FLine := FLines[FLineNo-1];
  FMarkLineNo := -1;
end;

{ TGenericReader }

function TGenericReader.ReadIdentifier: String;
var Ch: Char;
begin
  Result := '';
  if not CharInSet(TestChar, ['a'..'z','A'..'Z','_']) then
    EXIT;
  repeat
    ReadChar(Ch);
    Result := Result + Ch;
  until not CharInSet(TestChar,['a'..'z','A'..'Z','0'..'9','_']);
end;

function TGenericReader.ReadLine: String;
var Ch: Char;
begin
  Result := '';
  while ReadChar(Ch) do
    Result := Result + Ch;
end;

function TGenericReader.ReadNumber: String;
var Chars: set of Char;
  Ch: Char;
begin
  Result := '';
  if TestChar = '$' then
  begin
    Result := '$';
    Chars := ['0'..'9','a'..'f','A'..'F'];
  end
  else
    Chars := ['0'..'9'];

  while CharInSet(TestChar, Chars) do
    if ReadChar(Ch) then
      Result := Result + Ch;
end;

{ TQuicheSourceReader }
(* Comment *)
// Comment
function TQuicheSourceReader.DoSkipWhiteLine: TQuicheError;
var Cursor: TParseCursor;
begin
  Result := qeNone;
  case FState of
    psCode: ;
    psCurlyComment: Result := SkipCurlyCommentLine(FState);
    psBraceComment: Result := SkipBraceCommentLine(FState);
  else
    Assert(False);
  end;
  if Result <> qeNone then
    EXIT;

  while not EOLN do
  begin
    case TestChar of
      #1..#12,#14..' ': SkipChar;
      '{': //Curly comment
      begin
        SkipChar;
        Result := SkipCurlyCommentLine(FState);
        if Result <> qeNone then
          EXIT;

        if FState <> psCode then
          if EOF then
            EXIT(Err(qeUnterminatedComment))
          else
            EXIT(qeNone);
      end;
      '(':  //(* ... *) comment?
      begin
        Cursor := GetCursor;
        SkipChar;
        if TestChar = '*' then
        begin
          SkipChar;
          Result := SkipBraceCommentLine(FState);
          if Result <> qeNone then
            EXIT;
          if FState <> psCode then
            if EOF then
              EXIT(Err(qeUnterminatedComment))
            else
              EXIT(qeNone);
        end
        else
        begin
          SetCursor(Cursor);
          EXIT(qeNone);
        end;
      end;
      '/':  // // comment
      begin
        Cursor := GetCursor;
        SkipChar;
        if TestChar = '/' then
        begin
          SkipSlashComment;
          //We MUST be at end of line
          EXIT(qeNone);
        end
        else
          SetCursor(Cursor);
      end;
      '\':  //Line continuation
      begin
        Cursor := GetCursor;
        SkipChar;
        while not CharInSet(TestChar, [#0,#13]) do
        begin
          if not CharInSet(TestChar, [#1..#32]) then
          begin //If text after continuation then its not a continuation!
            SetCursor(Cursor);
            EXIT(Err(qeTextAfterContinuationChar));
          end;
          SkipChar;
        end;
        NextLine;
      end;
    else
      EXIT(qeNone);
    end;
  end;

  //EOF
  if EOF and (FState <> psCode) then
    Result := qeUnterminatedComment
  else
    Result := qeNone;
end;

function TQuicheSourceReader.EOS: Boolean;
begin
  Result := CharInSet(TestChar, [#0,';']);
end;

function TQuicheSourceReader.NextStatement(SeparatorRequired: Boolean): TQuicheError;
var NewLine: Boolean;
  Done: Boolean;
begin
  NewLine := False;
  while True do
  begin
    Done := False;
    repeat
      Result := DoSkipWhiteLine;
      if Result <> qeNone then
        EXIT;
      if EOF then
      begin
        Done := True;
        NewLine := True;
      end
      else if EOLN then
      begin
        NewLine := True;
        NextLine;
      end
      else
        Done := FState = psCode;
    until Done;

    if TestChar = ';' then
    begin
      NewLine := True;
      SkipChar;
    end
    else
      if NewLine or not SeparatorRequired then
        EXIT(qeNone)
      else
        EXIT(Err(qeEndOfStatementExpected));
  end;
end;

function TQuicheSourceReader.SkipBraceCommentLine(out State: TParseState): TQuicheError;
var Ch: Char;
begin
  Result := qeNone;

  if TestChar = '$' then
  begin //Compiler directive(s)
    SkipChar;
    Result := ParseDirective;
  end;

  while not EOLN do
  begin
    if ReadChar(Ch) then
      if Ch = '*' then
        if TestChar = ')' then
        begin
          SkipChar;
          State := psCode;
          EXIT;
        end;
  end;
  State := psBraceComment;
end;

function TQuicheSourceReader.SkipCurlyCommentLine(out State: TParseState): TQuicheError;
var Ch: Char;
begin //Compiler directive(s)
  Result := qeNone;

  if TestChar = '$' then
  begin //Compiler directive(s)
    SkipChar;
    Result := ParseDirective;
  end;

  while not EOLN do
  begin
    if ReadChar(Ch) then
      if Ch = '}' then
      begin
        State := psCode;
        EXIT;
      end;
  end;
  State := psCurlyComment;
end;

procedure TQuicheSourceReader.SkipSlashComment;
var Ch: Char;
begin
  repeat
    if not ReadChar(Ch) then
      EXIT;
  until CharInSet(Ch, [#13,#10]);
end;

function TQuicheSourceReader.SkipToNextLine: TQuicheError;
var NewLine: Boolean;
begin
  NewLine := False;
  repeat
    Result := DoSkipWhiteLine;
    if Result <> qeNone then
      EXIT;
    if EOLN then
    begin
      NextLine;
      NewLine := True;
    end;
  until FState = psCode;
  if NewLine then
    Result := qeNone
  else
    Result := qeNewlineExpected
end;

function TQuicheSourceReader.SkipWhite: TQuicheError;
begin
  Result := DoSkipWhiteLine;
end;

function TQuicheSourceReader.SkipWhiteNL: TQuicheError;
begin
  while True do
  begin
    Result := DoSkipWhiteLine;
    if Result <> qeNone then
      EXIT;
    if EOF then
      EXIT(qeNone);
    if EOLN then
      NextLine
    else if FState = psCode then
      EXIT(qeNone);
  end;
end;

end.
