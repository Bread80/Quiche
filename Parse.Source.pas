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
type TSourceReader = class
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
    //DEPRECATED
    function SkipWhiteSpace: Boolean;
    //Get the next char on the current line without moving the current position
    //If no char available (EOF, EOLN) returns #0
    function TestChar: Char;

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

  TQuicheSourceReader= class(TSourceReader)
  private
    function DoSkipWhiteLine: TQuicheError;

    // { .. } comment
    //Also parses compiler directives (TODO)
    //If NewLines then multiple lines of comment camn be consumed,
    //   If not the function will terminate (qeNone) if a new line is encountered
    //     with FState as psCurlyComment
    //Returns False if EOF or (EOLN and NewLines is False).
    //Returns True if the comment terminated correctly at a }
    function SkipCurlyCommentLine: TParseState;
    // (* *) commant. Initial ( must have been read. Bonus if the initial * has also been read
    function SkipBraceCommentLine: TParseState;
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
uses SysUtils;

{ TParseCursor }

constructor TParseCursor.Create(const AFilename: String; ALineNo, AColumn,
  AIndent: Integer);
begin
  Filename := AFilename;
  LineNo := ALineNo;
  Column := AColumn;
  Indent := AIndent;
end;

{ TSourceReader }

destructor TSourceReader.Destroy;
begin
  FLines.Free;
  inherited;
end;

function TSourceReader.GetCursor: TParseCursor;
begin
  Result.Create(FFilename, FLineNo, FColumn, FIndent);
end;

function TSourceReader.GetEOLN: Boolean;
begin
  Result := FEOF or (FColumn >= Length(Line));
end;

function TSourceReader.GetSource(Index: Integer): String;
begin
  if Index <= FLines.Count then
    Result := FLines[Index-1]
  else
    Result := ';<No such line>';
end;

procedure TSourceReader.LoadFromString(AString: String);
begin
  if FLines = nil then
    FLines := TStringList.Create;
  FFilename := '';
  FLines.Text := AString;
  Reset;
end;

procedure TSourceReader.LoadFromStrings(SL: TStrings);
begin
  if FLines = nil then
    FLines := TStringList.Create;
  FLines.Assign(SL);
  FFilename := '';
  Reset;
end;

procedure TSourceReader.Mark;
begin
  FMarkLineNo := LineNo;
  FMarkPos := Pos;
  FMarkCursor := GetCursor;
end;

function TSourceReader.NextChar(out Ch: Char): Boolean;
begin
  while FColumn >= Length(FLine) do
    if not NextLine then
      EXIT(False);

  Result := ReadChar(Ch);
end;

function TSourceReader.NextLine: Boolean;
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

function TSourceReader.OpenFile(AFilename: String): Boolean;
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

function TSourceReader.ReadChar(out Ch: Char): Boolean;
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

procedure TSourceReader.Reset;
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

procedure TSourceReader.SetCursor(const Cursor: TParseCursor);
begin
  Assert(Filename = Cursor.Filename, 'Can''t set cursor into a different file');
  FLineNo := Cursor.LineNo;
  FColumn := Cursor.Column;
  FIndent := Cursor.Indent;
  if Assigned(FLines) then
    FLine := FLines[FLineNo-1];
end;

function TSourceReader.SkipChar: Boolean;
begin
  Result :=  FColumn < Length(FLine);
  if Result then
    inc(FColumn);
end;

function TSourceReader.SkipCharIf(Ch: Char): Boolean;
begin
  Result := TestChar = Ch;
  if Result then
    SkipChar;
end;

function TSourceReader.SkipWhiteSpace: Boolean;
begin
  while (FColumn < Length(FLine)) and CharInSet(FLine.Chars[FColumn], csWhiteSpace) do
    inc(FColumn);
  Result := FColumn >= Length(FLine);
end;

function TSourceReader.TestChar: Char;
begin
  if FColumn < Length(FLine) then
    Result := FLine.Chars[FColumn]
  else
    Result := #0;
end;

procedure TSourceReader.Undo;
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

{ TQuicheSourceReader }
(* Comment *)
// Comment
function TQuicheSourceReader.DoSkipWhiteLine: TQuicheError;
var Cursor: TParseCursor;
begin
  case FState of
    psCode: ;
    psCurlyComment: FState := SkipCurlyCommentLine;
    psBraceComment: FState := SkipBraceCommentLine;
  else
    Assert(False);
  end;

  while not EOLN do
  begin
    case TestChar of
      #1..#12,#14..' ': SkipChar;
      '{': //Curly comment
      begin
        SkipChar;
        FState := SkipCurlyCommentLine;
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
          FState := SkipBraceCommentLine;
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

function TQuicheSourceReader.SkipBraceCommentLine: TParseState;
var Ch: Char;
begin
  if TestChar = '$' then
    ;    //Compiler directives here <<--- TODO

  while not EOLN do
  begin
    if ReadChar(Ch) then
      if Ch = '*' then
        if TestChar = ')' then
        begin
          SkipChar;
          EXIT(psCode);
        end;
  end;
  EXIT(psBraceComment);
end;

function TQuicheSourceReader.SkipCurlyCommentLine: TParseState;
var Ch: Char;
begin
  if TestChar = '$' then
    ;    //Compiler directives here <<--- TODO

  while not EOLN do
  begin
    if ReadChar(Ch) then
      if Ch = '}' then
        EXIT(psCode);
  end;
  EXIT(psCurlyComment);
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
begin
  repeat
    Result := DoSkipWhiteLine;
    if Result <> qeNone then
      EXIT;
    if EOLN then
      NextLine;
  until FState = psCode;
  Result := qeNone;
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
