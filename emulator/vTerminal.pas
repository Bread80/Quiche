unit vTerminal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.Edit, FMX.Objects;

type
  TTerminalForm = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    edInBuffer: TEdit;
    edOutBuffer: TEdit;
    cbShowControlCodes: TCheckBox;
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    FMaxCols: Integer;
    FMaxRows: Integer;
    FCurCol: Integer; //0..MaxCols
    FCurRow: Integer; //0..MaxRows
    FCursorEnabled: Boolean;

    FInControlCode: Boolean;

    procedure AddLine;
    procedure Cls;
    procedure CursorOn;
    procedure CursorOff;
    procedure ScrollUp;
    procedure SetCursorPos(ACol, ARow: Integer);
    procedure ValidateCursor;
    procedure UpdateCursor;
    procedure PaintRawString(const S: String);
    procedure PaintRawChar(C: Char);
    procedure PaintChar(C: Char);

    function ProcessControlCode(const Code: String): Boolean;
  public
    procedure TermOut(Ch: Char);
    function TermIn: Integer;

    procedure Reset;
  end;

var
  TerminalForm: TTerminalForm;

implementation
uses FMX.Memo.Types;

{$R *.fmx}

procedure TTerminalForm.AddLine;
var S: String;
  I: Integer;
begin
  S := '';
  for I := 1 to FMaxCols do
    S := S + ' ';
  Memo1.Lines.Add(S);
end;

procedure TTerminalForm.Cls;

begin
  Memo1.Lines.Clear;

  while Memo1.Lines.Count < FMaxRows do
    AddLine;
  SetCursorPos(0,0);
end;

procedure TTerminalForm.CursorOff;
begin
  Memo1.Caret.Width := 0;
  FCursorEnabled := False;
end;

procedure TTerminalForm.CursorOn;
begin
  Memo1.Caret.Width := 8;
  FCursorEnabled := True;
  ValidateCursor;
end;

procedure TTerminalForm.FormCreate(Sender: TObject);

begin
  FMaxCols := 80;
  FMaxRows := 25;

  Cls;
end;

procedure TTerminalForm.Memo1KeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 0 then
    edInBuffer.Text := edInBuffer.Text + KeyChar
  else
    //Exclude SHIFT
    if not (Key in [16]) then
      edInBuffer.Text := edInBuffer.Text + Char(Key);
  Key := 0;
  KeyChar := #0;
end;

procedure TTerminalForm.PaintChar(C: Char);
begin
  case C of
    #10: FCurRow := FCurRow + 1;
    #13: FCurCol := 0;
  else
    PaintRawChar(C);
  end;
  UpdateCursor;
end;

procedure TTerminalForm.PaintRawChar(C: Char);
begin
  ValidateCursor;
  Memo1.Lines[FCurRow] := Memo1.Lines[FCurRow].Substring(0,FCurCol) + C + Memo1.Lines[FCurRow].Substring(FCurCol + 1);
  FCurCol := FCurCol + 1;
  UpdateCursor;
end;

procedure TTerminalForm.PaintRawString(const S: String);
var C: Char;
begin
  for C in S do
    PaintRawChar(C);
end;

const ctrlCLS = #27 + '[2J';

function TTerminalForm.ProcessControlCode(const Code: String): Boolean;

  function SplitNumbers(Code: String): TArray<Integer>;
  var Raw: TArray<String>;
    I: Integer;
  begin
    Raw := Code.Substring(2,Length(Code) - 3).Split([';']);
    SetLength(Result, Length(Raw));
    for I := 0 to Length(Raw) - 1 do
      if not TryStrToInt(Raw[I], Result[I]) then
      begin
        SetLength(Raw, 0);
        EXIT;
      end;
  end;

var
  Params: TArray<String>;
  Numbers: TArray<Integer>;
  LastChar: Char;
  NextLastChar: Char;
begin
  Result := True;
  if Code = ctrlCLS then
    Cls
  else if Code.StartsWith(#27'[') then
  begin
    LastChar := Code.Chars[Length(Code)-1];
    NextLastChar := Code.Chars[Length(Code)-2];
    case LastChar of
      'h':
        case NextLastChar of
          '5': CursorOn;
        else
          Result := False;
        end;
      'l':
        case NextLastChar of
          '5': CursorOff;
        else
          Result := False;
        end;
      'H':  //Set cursor position
      begin
        Numbers := SplitNumbers(Code);
        if Length(Numbers) <> 2 then
          EXIT(False)
        else
          SetCursorPos(Numbers[1], Numbers[0]);
      end
    else
      Result := False;
    end
  end
  else
    Result := False;
end;

procedure TTerminalForm.Reset;
begin
  Cls;
  edInBuffer.Text := '';
  edOutBuffer.Text := '';
  CursorOff;
end;

procedure TTerminalForm.ScrollUp;
begin
  Memo1.Lines.Delete(0);
  AddLine;
  FCurRow := FCurRow - 1;
  UpdateCursor;
end;

procedure TTerminalForm.SetCursorPos(ACol, ARow: Integer);
begin
  FCurCol := ACol;
  FCurRow := ARow;
end;

function TTerminalForm.TermIn: Integer;
begin
  if Length(edInBuffer.Text) = 0 then
    Result := -1
  else
  begin
    Result := Integer(edInBuffer.Text.Chars[0]);
    edInBuffer.Text := edInBuffer.Text.Substring(1);
  end;
end;

procedure TTerminalForm.TermOut(Ch: Char);
begin
  if (Ch = #27) and FInControlCode then
  begin
    PaintRawString(edOutBuffer.Text);
    edOutBuffer.Text := '';
  end;
  if ((Ch = #27) or FInControlCode) and not cbShowControlCodes.IsChecked then
  begin
    edOutBuffer.Text := edOutBuffer.Text + Ch;
    if ProcessControlCode(edOutBuffer.Text) then
    begin
      FInControlCode := False;
      edOutBuffer.Text := '';
    end
    else
    begin
      FInControlCode := not CharInSet(Ch, ['A'..'Z','a'..'z']);
      if not FInControlCode then
      begin
        PaintRawString(edOutBuffer.Text);
        edOutBuffer.Text := '';
      end;
    end;
  end
  else
    PaintChar(Ch);
end;

procedure TTerminalForm.UpdateCursor;
begin
  Memo1.CaretPosition := TCaretPosition.Create(FCurRow, FCurCol);
end;

procedure TTerminalForm.ValidateCursor;
begin
  if FCurCol >= FMaxCols then
  begin
    FCurCol := 0;
    FCurRow := FCurRow + 1;
  end;
  if FCurRow >= FMaxRows then
    ScrollUp;
  UpdateCursor;
end;

end.
