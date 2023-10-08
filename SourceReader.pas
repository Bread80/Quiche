unit SourceReader;

interface
uses Classes;

const
  csWhiteSpace = [#0..' '];
  csIdentFirst = ['A'..'Z','a'..'z',{'.',}'_'];
  csIdentOther = ['0'..'9','A'..'Z','a'..'z',{'.',}'_'];
  csDecimalFirst = ['0'..'9'];
  csHexChars = ['0'..'9','a'..'f','A'..'F'];

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
    //Skips whitespace chars on the current line (ie. doesn't advance to the next line)
    //Returns True if we are at the end of the line
    function SkipWhiteSpace: Boolean;virtual;
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

  TQuicheSourceReader= class(TSourceReader)
  private
    // { .. } comment
    //Also parses compiler directives (TODO)
    procedure SkipCurlyComment;
    // (* *) commant. Initial ( must have been read. Bonus if the initial * has also been read
    procedure SkipBraceComment;
    // // .. EOLN/EOF commment
    //Doesn't NOT read the EOLN characters (I.e. does not advance to the next line, and EOLN
    //or EOF will always be True after calling this.
    procedure SkipSlashComment;
  public
    //Skips whitespace within a line.
    //Doesn't advance to the next line
    //Returns True if the reader (after skipping) is at EOLN or EOF
    function SkipWhiteSpace: Boolean;override;
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
  Result := FPos < Length(FLine);
  if Result then
  begin
    Ch := FLine.Chars[FPos];
    inc(FPos);
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

{ TQuicheSourceReader }
(* Comment *)
// Comment
function TQuicheSourceReader.EOS: Boolean;
begin
  Result := TestChar in [#0,';'];
end;

procedure TQuicheSourceReader.SkipBraceComment;
var Ch: Char;
begin
  while True do
  begin
    if not ReadChar(Ch) then
      //EOF
      EXIT;
    if Ch = '*' then
      if TestChar = ')' then
      begin
        SkipChar;
        EXIT;
      end;
  end;
end;

procedure TQuicheSourceReader.SkipCurlyComment;
var Ch: Char;
begin
  if not ReadChar(Ch) then
    EXIT;
//  if Ch = '$' then
    //Compiler directives here <<--- TODO

  while Ch <> '}' do
    if not ReadChar(Ch) then
      EXIT;
end;

procedure TQuicheSourceReader.SkipSlashComment;
var Ch: Char;
begin
  repeat
    if not ReadChar(Ch) then
      EXIT;
  until Ch in [#13,#10];
end;

function TQuicheSourceReader.SkipWhiteSpace: Boolean;
begin
  while not EOF do
  begin
    case TestChar of
      #0,#13: EXIT(True);
      #1..#12,#14..' ': SkipChar;
      '{':
        SkipCurlyComment;
      '(':
      begin
        Mark;
        SkipChar;
        if TestChar = '*' then
        begin
          SkipChar;
          SkipBraceComment;
        end
        else
        begin
          Undo;
          EXIT(False);
        end;
      end;
      '/':
      begin
        Mark;
        SkipChar;
        if TestChar = '/' then
        begin
          SkipSlashComment;
          //We MUST be at end of line
          EXIT(True);
        end
        else
        begin
          Undo;
          EXIT(False);
        end;
      end;
    else
      EXIT(False);
    end;
  end;

  //EOF
  Result := True;
end;

end.
