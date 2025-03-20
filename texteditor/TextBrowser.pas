unit TextBrowser;

interface
uses Classes, System.Types, System.UITypes, FMX.Types, FMX.Layouts, FMX.Controls,
  FMX.Graphics, FMX.StdCtrls, FMX.Presentation.Style, FMX.Text,
  EditableText, ViewableText;

type
  PTBTheme = ^TTBTheme;
  TTBTheme = record
    //Text colours
    Text,

    //Backgound colours
    CursorRowHighlight,
    SelectionBG,

    //Other
    Caret
    : TAlphaColor
  end;

var DefaultTheme: TTBTheme;
  DarkTheme: TTBTheme;

type
  TTextDisplay = class(TStyledControl{Presentation}{, ITextInput})
  private
    FText: TEditableText;

    FFont: TFont;
    FRowHeight: Single;  //Height of a single row
    FCharWidth: Single;  //Width of a character

    FCaret: TCaret;
    FOnViewChanged: TEventProc;
    FTheme: TTBTheme;
    procedure SetTheme(const Value: TTBTheme);
  protected
    //Dispatch OnViewChanged events
    procedure ViewChanged;

    //Where X and Y are relative to the client area of the control
    procedure SetCursor(X, Y: Single);

    procedure DoFontChanged(Sender: TObject);
    procedure DoUpdateCursor;

    procedure SetTextMetrics;
    procedure Resize;override;
    procedure Paint;override;
    procedure DoEnter;override;
    procedure DoExit;override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);override;
    procedure DialogKey(var Key: Word; Shift: TShiftState); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DblClick;override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single);override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;var Handled : Boolean); override;

    property OnViewChanged: TEventProc read FOnViewChanged write FOnViewChanged;

  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;

    procedure acFontLarger;
    procedure acFontSmaller;

    property Text: TEditableText read FText;
    property Font: TFont read FFont;
    property Theme: TTBTheme read FTheme write SetTheme;
  end;

  TTextBrowser = class(TStyledControl)
  private
    FHScrollBar: TScrollBar;
    FVScrollBar: TScrollBar;
    FDisplay: TTextDisplay;
    function GetText: TEditableText;
    function GetFont: TFont;
    function GetTheme: TTBTheme;
    procedure SetTheme(const Value: TTBTheme);
  protected
    procedure DoActivate;override;
    procedure DoEnter;override;
    procedure DoVScrollBarChange(Sender: TObject);
    procedure DoHScrollBarChange(Sender: TObject);

    procedure UpdateScrollBars;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;

    property Text: TEditableText read GetText;
    property Font: TFont read GetFont;
    property Theme: TTBTheme read GetTheme write SetTheme;
  end;

implementation
uses Math, SysUtils, FMX.TextLayout;

{ TTextBrowser }

procedure TTextDisplay.acFontLarger;
var Size: Single;
begin
  Size := Min(Font.Size * 1.1, 100);
  if Size > 5 then
    Size := RoundTo(Size, 0);
  Font.Size := Size;
end;

procedure TTextDisplay.acFontSmaller;
var Size: Single;
begin
  Size := Max(Font.Size * 0.9, 1);
  if Size > 5 then
    Size := RoundTo(Size, 0);
  Font.Size := Size;
end;

constructor TTextDisplay.Create(AOwner: TComponent);
begin
  inherited;
  SetTheme(DefaultTheme);

  CanFocus := True;
  FText := TEditableText.Create;
  FText.OnRepaint := Repaint;
  FText.OnUpdateCursor := DoUpdateCursor;

  FFont := TFont.Create;
  FFont.Size := 12;
  FFont.Family := 'Courier New';
  FFont.Style := [];
  FFont.OnChanged := DoFontChanged;

  SetTextMetrics;

  FCaret := TCaret.Create(Self);
  FCaret.Visible := True;
  FCaret.Color := Theme.Caret;
  FCaret.Size := TSizeF.Create(2, FRowHeight);
  FCaret.Show;
end;

procedure TTextDisplay.DblClick;
begin
  inherited;

  FText.acMouseDblClick;
end;

destructor TTextDisplay.Destroy;
begin
  FText.Free;
  inherited;
end;

procedure TTextDisplay.DialogKey(var Key: Word; Shift: TShiftState);
var Handled: Boolean;
begin
  if Shift = [] then
  begin
    Handled := True;
    case Key of
      vkTab: FText.acTab;
    else
      Handled := False;
    end;

    if Handled then
      Key := 0;
  end;

  inherited;

end;

procedure TTextDisplay.DoEnter;
begin
  inherited;
  FCaret.Show;
end;

procedure TTextDisplay.DoExit;
begin
  inherited;
  FCaret.Hide;
end;

procedure TTextDisplay.DoFontChanged(Sender: TObject);
begin
  SetTextMetrics;

  Resize;

  Repaint;
end;

procedure TTextDisplay.DoUpdateCursor;
begin
  FCaret.Pos := TPointF.Create((FText.Cursor.X - FText.Origin.X) * FCharWidth,
    (FText.Cursor.Y - FText.Origin.Y) * FRowHeight);

  ViewChanged;
  Repaint;
end;

procedure TTextDisplay.KeyDown(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
var Handled: Boolean;
begin
  inherited;

  Handled := True;
  //Shift with a cursor movement extends or modifies the selection region.
  //Cursor movement without Shift clears the selection region.
  if ssShift in Shift then
    FText.acBeginSelecting;

  if KeyChar <> #0 then
    FText.acInsertChar(KeyChar)
  else
    if (ssCtrl in Shift) and (ssShift in Shift) then
    case Key of
      vkZ:      FText.acRedo;
    else
      Handled := False;
    end
  else
    if ssCtrl in Shift then
    case Key of
      //Cursor movement
      vkPrior:  FText.acTopOfScreen;   //Top of screen
      vkNext:   FText.acBottomOfScreen;//Bottom of screen
      vkUp:     FText.acScrollUp;      //Scroll screen up
      vkDown:   FText.acScrollDown;    //Scroll screen down
      vkLeft:   FText.acWordLeft;      //Word left
      vkRight:  FText.acWordRight;     //Word right
      vkHome:   FText.acStartOfFile;   //Start of file
      vkEnd:    FText.acEndOfFile;     //End of file

      //Control keys
      vkA:      FText.acSelectAll;
      vkC:      FText.acCopy;
      vkX:      FText.acCut;
      vkV:      FText.acPaste;
      vkY:      FText.acDeleteCurrentLine;
      vkZ:      FText.acUndo;

      //UI controls
      vkAdd:      acFontLarger;       //Numeric keypad
      vkSubtract: acFontSmaller;      //Numeric keypad
    else
      Handled := False;
    end
  else //Not Ctrl
    case Key of
      //Cursor movement
      vkPrior: FText.acPgUp;
      vkNext: FText.acPgDn;
      vkUp: FText.acUp;
      vkDown: FText.acDown;
      vkLeft: FText.acLeft;
      vkRight: FText.acRight;
      vkHome: FText.acStartOfLine;
      vkEnd: FText.acEndOfLine;
      vkTab: FText.acTab;

      vkReturn: FText.acNewLine;
      vkBack: FText.acDeleteLeft;
      vkDelete: FText.acDeleteRight;
    else
      Handled := False;
    end;

  FText.acEndSelecting;

  if Handled then
  begin
    Key := 0;
    KeyChar := #0;
  end;
end;

procedure TTextDisplay.MouseClick(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if (Button = TMousebutton.mbLeft) and (Shift = []) then
    FText.acMouseLeftClick(FText.Origin.X + Trunc(X / FCharWidth),
    FText.Origin.Y + Trunc(Y / FRowHeight));
end;

procedure TTextDisplay.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if Button = TMouseButton.mbLeft then
  begin
    FText.acMouseDown(Trunc(X / FCharWidth) - FText.Origin.X, Trunc(Y / FRowHeight) + FText.Origin.Y);
    Capture;
  end;
end;

procedure TTextDisplay.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

  FText.acMouseMove(Trunc(X / FCharWidth) - FText.Origin.X, Trunc(Y / FRowHeight) + FText.Origin.Y);
end;

procedure TTextDisplay.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  if Button = TMouseButton.mbLeft then
  begin
    FText.acMouseUp(Trunc(X / FCharWidth) - FText.Origin.X, Trunc(Y / FRowHeight) + FText.Origin.Y);
    ReleaseCapture;
  end;
end;

procedure TTextDisplay.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  inherited;

  if not Handled then
  begin
    FText.acSetOrigin(FText.Origin.X,
      Min(Max(FText.Origin.Y - Max(abs(WheelDelta) div 20, 1) * Sign(WheelDelta), 0), FText.LineCount-1));
    Handled := True;
  end;
end;

procedure TTextDisplay.Resize;
begin
  inherited;

  FText.SetVisible(Max(Trunc(Width / FCharWidth), 1),
    Max(Trunc(Height / FRowHeight), 1) - 1);
  //The following enables you to PgUp/PgDn less than a full screen if desired
  FText.PageScrollAmount := Max(FText.Visible.Y, 1);

  FText.acSetCursor(FText.Cursor.X, FText.Cursor.Y);
end;

procedure TTextDisplay.Paint;
var
  CursorBrush: TBrush;  //Brush for cursor highlight row
  SelBrush: TBrush;     //Brush for selection region
  Rect: TRectF;         //Rect for current row or part row
  FileLine: Integer;    //Line number within file
  Row: TTextLayout;     //TextLayout for drawing text
  RowY: Single;         //Current cursor position
begin
  inherited;

  Row := TTextLayoutManager.DefaultTextLayout.Create;
  Row.BeginUpdate;
  Row.Font.Size := FFont.Size;
  Row.Font.Family := FFont.Family;
  Row.Font.Style := FFont.Style;
  Row.Color := Theme.Text;
  Row.EndUpdate;

  CursorBrush := TBrush.Create(TBrushKind.Solid, Theme.CursorRowHighlight);
  SelBrush := TBrush.Create(TBrushKind.Solid, Theme.SelectionBG);

  RowY := 0;
  FileLine := FText.Origin.Y;

  while (RowY < Canvas.Height) and (FileLine < FText.LineCount) do
  begin
    //Set default rect for line
    Rect := TRectF.Create(0, RowY, Canvas.Width, RowY + Row.Height);

    if FileLine = FText.Cursor.Y then
      //Cursor row highlight
      Canvas.FillRect(Rect, 0, 0, [], 1, CursorBrush);

    //Selection background
    //Do we have a selection and are we on a row within it?
    if FText.HaveSelection and (Fileline >= FText.SelStart.Y) and (Fileline <= FText.SelEnd.Y) then
    begin
      if Fileline = FText.SelStart.Y then
        Rect.Left := Max((FText.SelStart.X - FText.Origin.X) * FCharWidth, 0);

      if Fileline = FText.SelEnd.Y then
        if FText.SelEnd.X <> -1 then
          Rect.Right := (FText.SelEnd.X - FText.Origin.X) * FCharWidth;

      Canvas.FillRect(Rect, 0, 0, [], 1, SelBrush);
    end;

    //Draw text
    Row.TopLeft := TPointF.Create(-FText.Origin.X * FCharWidth, RowY);
    Row.Text := FText[FileLine];
    Row.RenderLayout(Canvas);

    inc(FileLine);
    RowY := RowY + Row.Height;
  end;

  Row.Free;
end;

procedure TTextDisplay.SetCursor(X, Y: Single);
begin
  FText.acSetCursor(FText.Origin.X + Trunc(X / FCharWidth),
    FText.Origin.Y + Trunc(Y / FRowHeight));
end;

procedure TTextDisplay.SetTextMetrics;
var Row: TTextLayout;
begin
  Row := TTextLayoutManager.DefaultTextLayout.Create;
  Row.BeginUpdate;
  Row.Font.Size := FFont.Size;
  Row.Font.Family := FFont.Family;
  Row.Font.Style := FFont.Style;
  Row.Text := ' ';
  Row.EndUpdate;
  FRowHeight := Row.Height;
  FCharWidth := Row.Width;
  Row.Free;

  if Assigned(FCaret) then
    FCaret.Size := TSizeF.Create(2, FRowHeight);
end;

procedure TTextDisplay.SetTheme(const Value: TTBTheme);
begin
  FTheme := Value;
  if Assigned(FCaret) then
    FCaret.Color := Theme.Caret;
  Repaint;
end;

procedure TTextDisplay.ViewChanged;
begin
  if Assigned(OnViewChanged) then
    OnViewChanged;
end;

{ TTextBrowser }

procedure TTextBrowser.DoActivate;
begin
  inherited;
  FDisplay.SetFocus;
end;

constructor TTextBrowser.Create(AOwner: TComponent);
begin
  inherited;
//  CanFocus := True;
  ClipChildren := True;

  FDisplay := TTextDisplay.Create(Self);
  FDisplay.Parent := Self;
  FDisplay.Align := TAlignLayout.Client;

  FVScrollBar := TScrollBar.Create(Self);
  FVScrollBar.Parent := Self;
  FVScrollBar.Orientation := TOrientation.Vertical;
  FVScrollBar.Align := TAlignLayout.Right;
  FVScrollBar.Width := 16;
  FVScrollBar.OnChange := DoVScrollBarChange;

  FHScrollBar := TScrollBar.Create(Self);
  FHScrollBar.Parent := Self;
  FHScrollBar.Align := TAlignLayout.Bottom;
  FHScrollBar.Height := 16;
  FHScrollBar.Max := 200;
  FHScrollBar.OnChange := DoHScrollBarChange;

  FDisplay.OnViewChanged := UpdateScrollBars;
end;

destructor TTextBrowser.Destroy;
begin
  FDisplay.Free;
  inherited;
end;

procedure TTextBrowser.DoEnter;
begin
  inherited;
  FDisplay.SetFocus;
end;

procedure TTextBrowser.DoHScrollBarChange(Sender: TObject);
begin
  FDisplay.Text.acSetOrigin(Trunc(FHScrollBar.Value), FDisplay.Text.Origin.Y);
end;

procedure TTextBrowser.DoVScrollBarChange(Sender: TObject);
begin
  FDisplay.Text.acSetOrigin(FDisplay.Text.Origin.X, Trunc(FVScrollBar.Value));
end;

function TTextBrowser.GetFont: TFont;
begin
  Result := FDisplay.Font;
end;

function TTextBrowser.GetText: TEditableText;
begin
  Result := FDisplay.Text;
end;

function TTextBrowser.GetTheme: TTBTheme;
begin
  Result := FDisplay.Theme;
end;

procedure TTextBrowser.SetTheme(const Value: TTBTheme);
begin
  FDisplay.Theme := Value;
end;

procedure TTextBrowser.UpdateScrollBars;
begin
  FVScrollBar.Max := FDisplay.Text.LineCount + FDisplay.Text.Visible.Y;
  FVScrollBar.ViewportSize := FDisplay.Text.Visible.Y;
  FVScrollBar.Value := FDisplay.Text.Origin.Y;

  //Can't be bothered to count line lengths. For now we'll just ignore
//  FHScrollBar.Max := FDisplay.Text.LineCount + FDisplay.Text.VisibleRows;
  FHScrollBar.ViewportSize := FDisplay.Text.Visible.X;
  FHScrollBar.Value := FDisplay.Text.Origin.X;
end;

initialization
  with DefaultTheme do
  begin
    Text := TAlphaColorRec.Black;

    CursorRowHighlight:= TAlphaColorRec.Silver;
    SelectionBG:= TAlphaColorRec.PowderBlue;

    Caret:= TAlphaColorRec.Black;
  end;

  with DarkTheme do
  begin
    Text := TAlphaColorRec.AntiqueWhite;

    CursorRowHighlight:= TAlphaColorRec.Black;
    SelectionBG:= TAlphaColorRec.DarkOliveGreen;

    Caret:= TAlphaColorRec.Cream;
  end;
end.
