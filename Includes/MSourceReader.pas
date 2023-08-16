unit MSourceReader;

interface
  uses Classes, MErrors;

//Line oriented source code reader
//Can read data from a file or string
//Most functions won't read past the end of the current line

type TSourceReader = class
  private
    FFilename: String;
    FLines: TStringList;
    FLineNo: Integer;
    FLine: String;
    FEOF: Boolean;
    FPos: Integer;

    FMarkLineNo: Integer;
    FMarkPos: Integer;
    function GetSource(Index: Integer): String;
  public
    destructor Destroy;override;
    function OpenFile(AFilename: String): TAssembleError;
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
    //Skips whitespace chars on the current line (ie. doesn't advance to the next line)
    //Returns True if we are at the end of the line
    function SkipWhiteSpace: Boolean;
    //Skips all whitespace, including new lines.
    procedure SkipWhiteSpaceAll;
    //Get the next char on the current line without moving the current position
    //If no char available (EOF, EOLN) returns #0
    function TestChar: Char;

    //Stores current read position, for possible Undo call
    //Note that only one Mark position is available. A second call will overwrite
    //previous setting
    procedure Mark;
    //Restores the read position to the current Mark position.
    //Can be used after a test for text or an identifier fails.
    procedure Undo;

    property FileName: String read FFileName;
    property EOF: Boolean read FEOF;
    property Pos: Integer read FPos;
    property LineNo: Integer read FLineNo;
    property Line: String read FLine;
    property MarkLineNo: Integer read FMarkLineNo;
    property MarkPos: Integer read FMarkPos;
    property Source[Index: Integer]: String read GetSource;
  end;

  TAssemSourceReader = class(TSourceReader)
  public
    //Skips to the next instruction ignoring whitespace, comments and newlines.
    //Leaves the current pointer at the first non-whitespace char.
    //Can skip multiple lines if empty or only containing comments.
    function NextInstruction: TAssembleError;
    //Skip the current instruction (etc.) to the next. Terminates at either:
    //A colon (next instruction), a semicolon (comment), end of line, or end of file
    //Intelligently skips comments and strings.
    //Note: an EX AF,AF' results in the rest of the line being skipped!
    function SkipInstruction: TAssembleError;
    function ReadIdentifier(out Identifier: String): TAssembleError;
    //Reads a parameter ending at any of EOF, EOLN, a comma, semicolon (comment)
    //or colon (next instruction)
    //Strips any spaces before and after brackets ( ) to enable easy comparisons
    //to register etc references
    //Comma returns true if the parameter ended with a comma, indicating another parameter
    function ReadParameter(out Value: String;out Comma: Boolean): TAssembleError;
    //Reads a string, returning it without the delimeters (ie. quote characters)
    //Delimiter can be ' or " or any (other character). String ends at the matching delimeter
    //The string cannot contain the delimiter
    //On entry the reader should be positioned after the opening delimeter,
    //on exit the reader is positioned after the closing delimiter
    function ReadString(var Value: String;Delimiter: Char): TAssembleError;
  end;

implementation
uses SysUtils;

{ TSourceReader }

destructor TSourceReader.Destroy;
begin
  FLines.Free;
  inherited;
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
end;

function TSourceReader.NextChar(out Ch: Char): Boolean;
begin
  while FPos >= Length(FLine) do
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
    FPos := 0;
  end
  else
    FEOF := True;
  Result := not FEOF;
end;

function TSourceReader.OpenFile(AFilename: String): TAssembleError;
begin
  if FLines <> nil then
    raise Exception.Create('File already open');
  FFilename := AFilename;
  FLines := TStringList.Create;
  FLines.LoadFromFile(Filename);
  FFilename := Filename;
  Reset;
  Result := errNone;
end;

function TSourceReader.ReadChar(out Ch: Char): Boolean;
begin
  Result := FPos < Length(FLine);
  if Result then
  begin
    Ch := FLine.Chars[FPos];
    inc(FPos);
  end
  else
    Ch := #0;
end;

procedure TSourceReader.Reset;
begin
  FEOF := False;
  FPos := 0;
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
end;

function TSourceReader.SkipChar: Boolean;
begin
  Result :=  FPos < Length(FLine);
  if Result then
    inc(FPos);
end;

function TSourceReader.SkipWhiteSpace: Boolean;
begin
  while (FPos < Length(FLine)) and CharInSet(FLine.Chars[FPos], csWhiteSpace) do
    inc(FPos);
  Result := FPos >= Length(FLine);
end;

procedure TSourceReader.SkipWhiteSpaceAll;
begin
  while True do
  begin
    if SkipWhiteSpace then
      NextLine
    else
      EXIT;
    if FEOF then
      EXIT;
  end;
end;

function TSourceReader.TestChar: Char;
begin
  if FPos < Length(FLine) then
    Result := FLine.Chars[FPos]
  else
    Result := #0;
end;

procedure TSourceReader.Undo;
begin
  FLineNo := FMarkLineNo;
  FPos := FMarkPos;
  if FLines <> nil then
    FLine := FLines[FLineNo-1];
  FMarkLineNo := -1;
end;

{ TAssemSourceReader }

function TAssemSourceReader.ReadString(var Value: String;
  Delimiter: Char): TAssembleError;
begin
  Value := '';
  while (FPos < Length(FLine)) and (FLine.Chars[FPos] <> Delimiter) do
  begin
    Value := Value + FLine.Chars[FPos];
    inc(FPos);
  end;

  if FPos >= Length(FLine) then
      EXIT(errUnterminatedString);
  inc(FPos);
  Result := errNone;
end;

function TAssemSourceReader.SkipInstruction: TAssembleError;
var Dummy: String;
begin
  while not EOF do
  begin
    SkipWhiteSpace;

    if (FPos >= Length(FLine)) or (CharInSet(FLine.Chars[FPos], [':',';'])) then
      EXIT(errNone)
    else if CharInSet(FLine.Chars[FPos], ['''', '"']) then
      ReadString(Dummy, FLine.Chars[FPos])
    else
      inc(FPos);
  end;
  Result := errNone;
end;

function TAssemSourceReader.ReadIdentifier(out Identifier: String): TAssembleError;
begin
  Identifier := '';
  if (FPos < Length(FLine)) and CharInSet(FLine.Chars[FPos], csIdentFirst) then
    Identifier := FLine.Chars[FPos]
  else
    EXIT(errIdentifierExpected);

  FPos := FPos + 1;

  while (FPos < Length(FLine)) and CharInSet(FLine.Chars[FPos], csIdentOther) do
  begin
    Identifier := Identifier + FLine.Chars[FPos];
    FPos := FPos + 1;
  end;
  Result := errNone;
end;

function TAssemSourceReader.ReadParameter(out Value: String;
  out Comma: Boolean): TAssembleError;
var
  Ch: Char;
  Sub: String;
  SkipSpace: Boolean;
  DoneWhiteSpace: Boolean;
begin
  DoneWhiteSpace := False;
  SkipSpace := True;
  Value := '';
  Comma := False;
  while FPos < Length(FLine) do
  begin
    Ch := FLine.Chars[FPos];
    case Ch of
    ',':
      begin
        Comma := True;
        BREAK;
      end;
    ';',':':
      BREAK;
    '"','''':
      if (Ch = '''') and (CompareText(Value, 'af') = 0) then
        Value := Value + ''''
      else
      begin
        inc(FPos);
        Result := ReadString(Sub, Ch);
        if Result <> errNone then
          EXIT;
        Value := Value + Ch + Sub + Ch;
        dec(FPos);
      end;
    #0..' ':
      DoneWhiteSpace := True;
    '(',')':
    begin
      DoneWhiteSpace := False;
      SkipSpace := True;
      Value := Value + Ch;
    end
    else
      if DoneWhiteSpace and not SkipSpace then
        Value := Value + ' ';
      DoneWhiteSpace := False;
      SkipSpace := False;
      Value := Value + Ch;
    end;

    inc(FPos);
  end;

  Result := errNone;
end;

function TAssemSourceReader.NextInstruction: TAssembleError;
begin
  while True do
  begin
    SkipWhiteSpace;

    if (FPos >= Length(FLine)) or (FLine.Chars[FPos] = ';') then
    begin
      if not NextLine then
        EXIT(errNone)
    end
    else if (FLine.Chars[FPos] = ':') then
      SkipChar
    else
      EXIT(errNone);
  end;
end;

end.
