unit EditableText;

interface
uses Classes, Types, FMX.Platform, Generics.Collections, ViewableText;

type
  TEditAction = (eaDeleteLeft, eaDeleteRight, eaDeleteLine, eaDeleteBlock,
    eaInsertChar, eaInsertBlock);

  PUndoItem = ^TUndoItem;
  TUndoItem = record
    Action: TEditAction;
    Pos: TPoint;
    Text: String;
  end;

  type TUndoList = class
  private
    FItems: TList<PUndoItem>;
    //Index of Item after the first item to be Undone.
    //Ie. Undo will return Items[Index-1]
    //    Redo will return Items[Index]
    //    Add will add item as Items[Index]
    //      (if there are Redo items with indexes >= Index then
    //      will be deleted).
    //If adding an item would move past the maximum item count (if there is one)
    //items at the start of the list will be deleted and Index adjusted accordingly.
    FIndex: Integer;
    FInUndo: Boolean;
  public
    constructor Create;
    destructor Destroy;override;

    procedure Add(Action: TEditAction;Pos: TPoint;const Text: String);
    //Retrieves data for the next item to be undone.
    //If there are no items returns nil.
    //If the caller processes the Undo then ProcessUndo *must* be called
    //to update internal data
    function BeginUndo: PUndoItem;
    procedure ProcessUndo;
    procedure CancelUndo;
    function GetRedoPos: TPoint;
    function GetRedoText: String;

    //Can be set by owner as desired. When InUndo is True any calls to Add will
    //be ignored. Any calls will raise an Assertion.
    property InUndo: Boolean read FInUndo;
  end;


  TEditableText = class(TViewableText)
  private
    FCursor: TPoint;      //Cursor position in file

    FSelStart: TPoint;    //Start of text selection. If X = -1 then no text is selected
    FSelEnd: TPoint;      //End of text selection
    //If True various cursor movements will act to enlarge or modify the selection
    //region. If False those cursor movements will act to cancel the selection region
    FInSelecting: Boolean;

    FVisible: TPoint;     //Number of whole rows and columns visible on screen
    FPageScrollAmount: Integer;
    FOrigin: TPoint;      //Column and row displayed at the top-left of the window

    FUndoList: TUndoList;

    //Blocks the MouseDown that occurs after a DblClick so the word selection
    //doesn't get cleared/trashed
    FInDblClick: Boolean;

    //Settings/Options
    FTabWidth: Integer;    //Width of a tabstop

    FOnUpdateCursor: TEventProc;

    //If true new lines will start with the indent of the line above,
    //otherwise they'll start with no indent. See GetCurrentIndent
    FAutoIndent: Boolean;
    //Returns the current Indent level.
    //Indent is defined by GetIndentOfLine.
    //Search backwards from current line to the first non-empty line.
    //If there are non non-empty lines prior to the current, returns 0
    function GetCurrentIndent: Integer;

    function GetHaveSelection: Boolean;
    procedure SetTabWidth(const Value: Integer);
    procedure Repaint;
    procedure SaveToFile;
    procedure SaveToFileAs(const AFilename: String);
    procedure SetFileOpenTabWidth(const Value: Integer);
  protected
     procedure BeforeSave;override;

    //Add blank lines to the end of the file until the number of lines matches
    //LineCount. We need this for times when the cursor is beyond the last line
    //of the file and the user starts editing
    function EndPad(LineCountTo: Integer): String;
    //Trim blank lines from the end of the file
    procedure EndTrim;

    procedure SetLines(Index: Integer; const Value: String);

    //Move P to the previous character in the file, moving to the end of the
    //previous line if necessary.
    //Returns False if there is no previous point (ie. start of file)
    function PrevPoint(var P: TPoint): Boolean;
    function NextPoint(var P: TPoint): Boolean;
    procedure WordStart(var P: TPoint);
    procedure WordEnd(var P: TPoint);

    //Called for any cursor upwards or left-wards movement.
    //* Moves the cursor to the new position
    //* Clears Selection if not InSelecting
    //* If InSelected creates or adjusts the Selection as needed
    procedure MoveCursorUpTo(NewX, NewY: Integer);
    //As MoveCursorUpTo but for downwards or right-wards movement
    procedure MoveCursorDownTo(NewX, NewY: Integer);

    //If Cut is False copies the text between Start and ToEnd to Result.
    //If Cut is True also deletes the text
    function CutCopy(Start, ToEnd: TPoint;Cut: Boolean): String;

    procedure Insert(Position: TPoint;Text: String);

    //Update the cursor position. Scrolls and redraws the text as necessary
    //If ForceRedraw is False the screen will only be redrawn if it needs to be
    //scrolled (vertically or horizontally). If True the screen will always be redrawn
    procedure UpdateCursor(NewCol, NewRow: Integer;ForceRedraw: Boolean = False);
  public
    constructor Create;
    destructor Destroy;override;

    //Cursor movement
    procedure acUp;
    procedure acDown;
    procedure acPgUp;
    procedure acPgDn;
    procedure acLeft;
    procedure acRight;
    procedure acStartOfLine;
    procedure acEndOfLine;
    procedure acTopOfScreen;
    procedure acBottomOfScreen;
    procedure acScrollUp;
    procedure acScrollDown;
    procedure acWordLeft;
    procedure acWordRight;
    procedure acStartOfFile;
    procedure acEndOfFile;

    procedure acMouseLeftClick(X, Y: Integer);
    procedure acMouseDblClick;
    procedure acMouseDown(X, Y: Integer);
    procedure acMouseMove(X, Y: Integer);
    procedure acMouseUp(X, Y: Integer);

    //Editing
    procedure acNewLine;
    procedure acDeleteRight;
    procedure acDeleteLeft;
    procedure acInsertChar(Ch: Char);
    procedure acTab;
    procedure acDeleteLine(Index: Integer);
    procedure acDeleteCurrentLine;

    //Selections
    //Set or adjust the start point of the selection
    //If the values supplied are _after_ the current SelStart, adjusts the
    //end point to the supplied position.
    procedure acSetSelStart(X, Y: Integer);
    //Set of adjust the end point of the selection
    //If the values supplied are _before_ the current SelEnd, adjusts the
    //start point to the supplied position.
    procedure acSetSelEnd(X, Y: Integer);
    procedure acSelectAll;
    procedure acBeginSelecting;
    procedure acEndSelecting;
    //If IsSelecting is False, clears the current selection.
    //If IsSelecting is True does nothing.
    procedure acClearSelection;
    //Delete text within the selection. No Undo buffer yet. Be careful
    procedure acDeleteSelected;
    procedure acCut;
    procedure acCopy;
    procedure acPaste;

    procedure acUndo;
    procedure acRedo;

    //Set the cursor position. Scrolls the screen as needed to make the cursor
    //visible
    procedure acSetCursor(X, Y: Integer);
    //Updates the Origin and repaints the window
    procedure acSetOrigin(X, Y: Integer);


    //Updates the Visible dimensions. Doesn't repaint anything.
    //Should only be called by the TTextBrowser. Used for cursor positioning
    //and screen repaints
    procedure SetVisible(X, Y: Integer);
    procedure SetOrigin(X, Y: Integer);

    property Origin: TPoint read FOrigin;
    property Visible: TPoint read FVisible;
    property PageScrollAmount: Integer read FPageScrollAmount write FPageScrollAmount;

    property Cursor: TPoint read FCursor;
    property SelStart: TPoint read FSelStart;
    property SelEnd: TPoint read FSelEnd;
    //Returns True if a region of text is selected
    property HaveSelection: Boolean read GetHaveSelection;

    property OnUpdateCursor: TEventProc read FOnUpdateCursor write FOnUpdateCursor;
  published
    //Options and settings
    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property AutoIndent: Boolean read FAutoIndent write FAutoIndent;
  end;

implementation
uses SysUtils, Math, RTTI;

const WordChars = ['a'..'z','A'..'Z','0','9','_'];

{ TEditableFile }

procedure TEditableText.acBeginSelecting;
begin
  FInSelecting := True;
end;

procedure TEditableText.acBottomOfScreen;
begin //Ctrl+End
  MoveCursorDownTo(Cursor.X, Origin.Y + Visible.Y);
end;

procedure TEditableText.acClearSelection;
begin
  if HaveSelection and not FInSelecting then
  begin
    FSelStart.X := -1;
    Repaint;
  end;
end;

procedure TEditableText.acCopy;
var
  uClipBoard : IFMXClipboardService;
  AValue: TValue;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, uClipBoard) then
  begin
    AValue := CutCopy(SelStart, SelEnd, False);
    uClipBoard.SetClipboard(AValue);
  end;
end;

procedure TEditableText.acCut;
var
  uClipBoard : IFMXClipboardService;
  AValue: TValue;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, uClipBoard) then
  begin
    AValue := CutCopy(SelStart, SelEnd, True);
    uClipBoard.SetClipboard(AValue);
  end;
end;

procedure TEditableText.acDeleteCurrentLine;
begin
  acDeleteLine(Cursor.Y);
end;

procedure TEditableText.acDeleteLeft;
var Length: Integer;
  Deleted: String;  //Text which has been deleted

begin //Backspace
  if HaveSelection then
  begin
    acDeleteSelected;
    EXIT;
  end;

  Deleted := '';
  EndPad(Cursor.Y);

  if Cursor.X = 0 then
  begin
    if Cursor.Y > 0 then
    begin //Merge lines
      Length := Lines[Cursor.Y-1].Length;
      Lines[Cursor.Y-1] := Lines[Cursor.Y-1] + Lines[Cursor.Y];
      Deleted := #13;
      DeleteLine(Cursor.Y);
      UpdateCursor(Length, Cursor.Y-1, True);
    end
  end
  else
  begin
    Deleted := Lines[Cursor.Y].Substring(Cursor.X-1, 1);
    if Cursor.X = 1 then
    begin
      Lines[Cursor.Y] := Lines[Cursor.Y].Substring(1);
      UpdateCursor(Cursor.X-1, Cursor.Y, True);
    end
    else
    begin
      Lines[Cursor.Y] := Lines[Cursor.Y].Substring(0,Cursor.X-1) + Lines[Cursor.Y].Substring(Cursor.X);
      Lines[Cursor.Y] := Lines[Cursor.Y].TrimRight;
      UpdateCursor(Cursor.X-1, Cursor.Y, True);
    end;
  end;

  EndTrim;
  if Deleted <> '' then
    FUndoList.Add(eaDeleteLeft, Cursor, Deleted);
end;

procedure TEditableText.acDeleteLine(Index: Integer);
begin
  if Cursor.Y >= LineCount then
    EXIT;

  FUndoList.Add(eaDeleteLine, TPoint.Create(0, Index), Lines[Index] + #13);
  DeleteLine(Index);
  EndTrim;
  UpdateCursor(0, Min(Index, LineCount-1));
end;

procedure TEditableText.acDeleteRight;
var Length: Integer;
  Deleted: String;  //The deleted text
begin //Delete
  if HaveSelection then
  begin
    acDeleteSelected;
    EXIT;
  end;

  Deleted := '';
  EndPad(Cursor.Y);

  Length := Lines[Cursor.Y].Length;
  if Cursor.X >= Length then
  begin //At or beyond end of line
    if Cursor.Y >= LineCount-1 then
      //End of file - do nothing
    else
    begin //!! If cursor is beyond the end of line pad with spaces
      Lines[Cursor.Y] := Lines[Cursor.Y].PadRight(Cursor.X) + Lines[Cursor.Y+1];
      Lines[Cursor.Y] := Lines[Cursor.Y].TrimRight;
      Deleted := #13;
      DeleteLine(Cursor.Y+1);
      UpdateCursor(Cursor.X, Cursor.Y, True);
    end;
  end
  else
  begin
    Deleted := Lines[Cursor.Y].Chars[Cursor.X];
    if Cursor.X = Length-1 then
    begin
      Lines[Cursor.Y] := Lines[Cursor.Y].SubString(0,Cursor.X);
      UpdateCursor(Cursor.X, Cursor.Y, True)
    end
    else
    begin
      Lines[Cursor.Y] := Lines[Cursor.Y].Substring(0,Cursor.X) + Lines[Cursor.Y].Substring(Cursor.X+1);
      UpdateCursor(Cursor.X, Cursor.Y, True);
    end;
  end;

  EndTrim;
  if Deleted <> '' then
    FUndoList.Add(eaDeleteRight, Cursor, Deleted);
end;

procedure TEditableText.acDeleteSelected;
begin
  if not HaveSelection then
    EXIT;

  CutCopy(SelStart, SelEnd, True);
end;

procedure TEditableText.acDown;
begin
  MoveCursorDownTo(Cursor.X, Cursor.Y+1);
end;

procedure TEditableText.acEndOfFile;
begin //Ctrl+End
  MoveCursorDownTo(Lines[LineCount-1].Length, LineCount-1);
end;

procedure TEditableText.acEndOfLine;
begin //End
  MoveCursorDownTo(Lines[Cursor.Y].Length, Cursor.Y);
end;

procedure TEditableText.acEndSelecting;
begin
  FInSelecting := False;
end;

procedure TEditableText.acInsertChar(Ch: Char);
var Padding: String;
begin
  acDeleteSelected;
  EndPad(Cursor.Y);

  //!! if the cursor is to beyond the end of text on the line, pad with spaces
  if Cursor.X > Length(Lines[Cursor.Y]) then
    Padding := StringOfChar(' ', Cursor.X - (Length(Lines[Cursor.Y])))
  else
    Padding := '';
  FUndoList.Add(eaInsertChar, Cursor, Padding + Ch);

  Lines[Cursor.Y] := Lines[Cursor.Y].Substring(0, Cursor.X) + Padding +
    Ch + Lines[Cursor.Y].Substring(Cursor.X);
  Lines[Cursor.Y] := Lines[Cursor.Y].TrimRight;
  EndTrim;
  UpdateCursor(Cursor.X+1, Cursor.Y, True);
end;

procedure TEditableText.acLeft;
begin
  if Cursor.X > 0 then
    MoveCursorUpTo(Cursor.X - 1, Cursor.Y)
  else  //Move to end of previous line
    if Cursor.Y > 0 then
      MoveCursorUpTo(Lines[Cursor.Y-1].Length, Cursor.Y-1);
end;

procedure TEditableText.acMouseDblClick;
var Current: TPoint;
begin
  FInDblClick := True;
  acClearSelection;
  acBeginSelecting;
  Current := Cursor;
  WordStart(Current);
  FSelStart := Current;
  WordEnd(Current);
  FSelEnd := Current;
  Repaint;
end;

procedure TEditableText.acMouseDown(X, Y: Integer);
var SelX, SelY: Integer;
begin
  if FInDblClick then
  begin
    FInDblClick := False;
    EXIT;
  end;

  acClearSelection;
  FCursor.X := X;
  FCursor.Y := Y;

  SelY := Min(Y, LineCount-1);
  SelX := Min(X, Lines[SelY].Length);
  FSelStart.X := SelX;
  FSelStart.Y := SelY;
  FSelEnd.X := SelX;
  FSelEnd.Y := SelY;
  FInSelecting := True;

  if Assigned(OnRepaint) then
    OnRepaint;
  if Assigned(OnUpdateCursor) then
    OnUpdateCursor;
end;

procedure TEditableText.acMouseLeftClick(X, Y: Integer);
begin
  acClearSelection;
  UpdateCursor(Max(X, 0), Max(Y, 0));
end;

procedure TEditableText.acMouseMove(X, Y: Integer);
var LX, LY: Integer;
begin
  if FInSelecting then
  begin
    LX := X;//Max(X, 0);
    LY := Y;//Min(Max(Y, 0), LineCount-1);
    if (LY > Cursor.Y) or ((LY = Cursor.Y) and (LX > Cursor.X)) then
      MoveCursorDownTo(LX, LY)
    else if (LY < Cursor.Y) or ((LY = Cursor.Y) and (LX < Cursor.X)) then
      MoveCursorUpTo(LX, LY);
  end;
end;

procedure TEditableText.acMouseUp(X, Y: Integer);
begin
  FInSelecting := False;
end;

procedure TEditableText.acNewLine;
var NewCol: Integer;
begin
  acDeleteSelected;
  EndPad(Cursor.Y);

  FUndoList.Add(eaInsertChar, Cursor, #13);
  InsertLine(Cursor.Y+1, Lines[Cursor.Y].Substring(Cursor.X));
  Lines[Cursor.Y] := Lines[Cursor.Y].Substring(0,Cursor.X);
  if AutoIndent then
    NewCol := GetCurrentIndent
  else
    NewCol := 0;
  Lines[Cursor.Y] := Lines[Cursor.Y].TrimRight;
  EndTrim;

  UpdateCursor(NewCol, Cursor.Y+1, True);
end;

procedure TEditableText.acPaste;
var uClipBoard : IFMXClipboardService;
  AValue: TValue;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, uClipBoard) then
  begin
    AValue := uClipBoard.GetClipboard;
    if AValue.IsType<String> then
    begin
      acDeleteSelected;
      Insert(Cursor, AValue.AsString);
      ExpandTabs;
    end;
  end;
end;

procedure TEditableText.acPgDn;
begin
  MoveCursorDownTo(Cursor.X, Cursor.Y + PageScrollAmount);
end;

procedure TEditableText.acPgUp;
begin
  MoveCursorUpTo(Cursor.X, Cursor.Y - PageScrollAmount);
end;

procedure TEditableText.acRedo;
var Pos: TPoint;
  Text: String;
begin
  Pos := FUndoList.GetRedoPos;
  if Pos.X = -1 then
    EXIT;
  if (Pos.X <> Cursor.X) or (Pos.Y <> Cursor.Y) then
    acSetCursor(Pos.X, Pos.Y)
  else
  begin
    Text := FUndoList.GetRedoText;
    Assert(not FUndoList.InUndo, 'Attempting to Redo when already Undoing');
    try
//      FUndoList.InUndo := True;
      Insert(Pos, Text);
    finally
//      FUndoList.InUndo := False;
    end;
  end;
end;

procedure TEditableText.acRight;
begin
  MoveCursorDownTo(Cursor.X+1, Cursor.Y);
end;

procedure TEditableText.acScrollDown;
begin //Ctrl+Down
  acClearSelection;
  if Origin.Y < LineCount then
    SetOrigin(Origin.X, Origin.Y+1);
end;

procedure TEditableText.acScrollUp;
begin //Ctrl+Up
  if Origin.Y > 0 then
    SetOrigin(Origin.X, Origin.Y - 1);
end;

procedure TEditableText.acSelectAll;
begin
  FSelStart.X := 0;
  FSelStart.Y := 0;
  FSelEnd.Y := LineCount-1;
  FSelEnd.X := Lines[LineCount-1].Length;
  Repaint;
end;

procedure TEditableText.acSetCursor(X, Y: Integer);
begin
  UpdateCursor(X, Y, False);
end;

procedure TEditableText.acSetOrigin(X, Y: Integer);
begin
  SetOrigin(X, Y);
  if Assigned(OnUpdateCursor) then
    OnUpdateCursor;
end;

procedure TEditableText.acSetSelEnd(X, Y: Integer);
begin
  FSelEnd.X := X;
  FSelEnd.Y := Y;
end;

procedure TEditableText.acSetSelStart(X, Y: Integer);
begin
  FSelStart.X := X;
  FSelStart.Y := Y;
end;

procedure TEditableText.acStartOfFile;
begin //Ctrl+Home
  MoveCursorUpTo(0,0);
end;

procedure TEditableText.acStartOfLine;
begin //Home
  MoveCursorUpTo(0, Cursor.Y);
end;

procedure TEditableText.acTab;
var Spaces: Integer;
begin
  acDeleteSelected;
  EndPad(Cursor.Y);

  Spaces := (TabWidth - Cursor.X mod TabWidth);
  //!! if the cursor is to beyond the end of text on the line, pad with spaces
  Lines[Cursor.Y] := Lines[Cursor.Y].Substring(0, Cursor.X).PadRight(Cursor.X + Spaces) +
    Lines[Cursor.Y].Substring(Cursor.X);
  Lines[Cursor.Y] := Lines[Cursor.Y].TrimRight;
  EndTrim;

  UpdateCursor(Cursor.X+Spaces, Cursor.Y, True);
end;

procedure TEditableText.acTopOfScreen;
begin //Ctrl+PgUp
  MoveCursorUpTo(Cursor.X, Origin.Y);
end;

procedure TEditableText.acUndo;
var
  Item: PUndoItem;
begin
  Item := FUndoList.BeginUndo;

  if not Assigned(Item) then
    //Nothing to undo
    FUndoList.CancelUndo
  else
  begin
    case Item.Action of
      eaDeleteLeft, eaDeleteRight, eaDeleteLine, eaDeleteBlock:
      if (Item.Pos.X <> Cursor.X) or (Item.Pos.Y <> Cursor.Y) then
      begin
        FUndoList.CancelUndo;
        acSetCursor(Item.Pos.X, Item.Pos.Y);
      end
      else
      begin
        Insert(Item.Pos, Item.Text);
        if Item.Action = eaDeleteRight then
          UpdateCursor(Item.Pos.X, Item.Pos.Y);
        FUndoList.ProcessUndo;
      end;
    else
      FUndoList.CancelUndo;
      Assert(False, 'Unknown Edit Action');
    end;
  end;

  Assert(not FUndoList.InUndo, 'Failed to cancel or process Undo');
end;

procedure TEditableText.acUp;
begin
  MoveCursorUpTo(FCursor.X, FCursor.Y - 1);
end;

procedure TEditableText.acWordLeft;
var Current: TPoint;
begin //Ctrl+Left
  Current := Cursor;
  WordStart(Current);
  MoveCursorUpTo(Current.X, Current.Y);
end;

procedure TEditableText.acWordRight;
var Current: TPoint;
begin //Ctrl+Right
  Current := Cursor;
  WordEnd(Current);
  MoveCursorDownTo(Current.X, Current.Y);
end;



procedure TEditableText.BeforeSave;
begin
  inherited;
  EndTrim;
end;

constructor TEditableText.Create;
begin
  inherited Create;
  FUndoList := TUndoList.Create;
  FAutoIndent := True;
  FTabWidth := 2;
  FSelStart.X := -1;  //Nothing selected
end;

function TEditableText.CutCopy(Start, ToEnd: TPoint; Cut: Boolean): String;
var LastLine: String;
  Y: Integer;
begin
  if Start.Y = ToEnd.Y then
  begin
    Result := Lines[Start.Y].Substring(Start.X, ToEnd.X-Start.X);
    if Cut then
    begin
      Lines[Start.Y] := Lines[Start.Y].Substring(0, Start.X) + Lines[Start.Y].Substring(ToEnd.X);
      UpdateCursor(Start.X, Start.Y);
      acClearSelection;
    end
  end
  else
  begin
    Result := Lines[ToEnd.Y].Substring(0, ToEnd.X);
    if Cut then
    begin
      LastLine := Lines[ToEnd.Y].Substring(ToEnd.X);
      DeleteLine(ToEnd.Y);
    end;

    for Y := ToEnd.Y-1 downto Start.Y + 1 do
    begin
      Result := Lines[Y] + #13 + Result;
      if Cut then
        DeleteLine(Y);
    end;
    Result := Lines[Start.Y].Substring(Start.X) + #13 + Result;
    if Cut then
    begin
      Lines[Start.Y] := Lines[Start.Y].Substring(0, Start.X) + LastLine;
      UpdateCursor(Start.X, Start.Y);
      acClearSelection;
    end;
  end;
  EndTrim;

  if Cut and (Result <> '') then
    FUndoList.Add(eaDeleteBlock, Start, Result);
end;

destructor TEditableText.Destroy;
begin
  FUndoList.Free;
  inherited;
end;

function TEditableText.EndPad(LineCountTo: Integer): String;
begin
  Result := '';
  //TODO: Add Undo data
  while LineCount <= LineCountTo do
  begin
    AppendLine('');
    Result := Result + #13;
  end;
end;

procedure TEditableText.EndTrim;
begin
  while (LineCount > 0) and (Lines[LineCount-1].TrimRight = '') do
    DeleteLine(LineCount-1);
  if LineCount = 0 then
    AppendLine('');
end;

function TEditableText.GetCurrentIndent: Integer;
var LineNo: Integer;
begin
  LineNo := Cursor.Y;
  while LineNo >= 0 do
  begin
    Result := GetIndentOfLine(LineNo);
    if Result <> -1 then
      EXIT;
    dec(LineNo);
  end;

  Result := 0;
end;

function TEditableText.GetHaveSelection: Boolean;
begin
  Result := SelStart.X <> -1;
end;

procedure TEditableText.Insert(Position: TPoint; Text: String);
var SL: TStringList;
  I: Integer;
  Y: Integer;
  LastLine: String;
  Padding: String;
begin
  Padding := EndPad(Position.Y);
  SL := TStringList.Create;
  try
    //If Text ends with a newline token and SL treats it as the end of that line.
    //We need it to treat that as a final, empty, line. To work around this we
    //Add a space to the end of the input Text and remove it (from the last line
    //of SL) in the second line
    SL.Text := Text + ' ';
    SL[SL.Count-1] := SL[SL.Count-1].Substring(0, SL[SL.Count-1].Length-1);

    if Position.X > Length(Lines[Position.Y]) then
      Padding := StringOfChar(' ', Position.X-Length(Lines[Position.Y]))
    else
      Padding := '';
    FUndoList.Add(eaInsertBlock, Position, Text);

    Y := Cursor.Y;
    LastLine := Lines[Y].Substring(Cursor.X);
    Lines[Y] := Lines[Y].Substring(0, Cursor.X) + Padding + SL[0];
    if SL.Count = 1 then
    begin
      Lines[Y] := Lines[Y] + LastLine;
      Repaint;
      UpdateCursor(Cursor.X + SL[0].Length, Y);
    end
    else
    begin
      inc(Y);
      for I := 1 to SL.Count-2 do
      begin
        InsertLine(Y, SL[I]);
        inc(Y);
      end;
      InsertLine(Y, SL[SL.Count-1] + LastLine);

      Repaint;
      UpdateCursor(Cursor.X + SL[SL.Count-1].Length, Y);
    end;
  finally
    SL.Free;
  end;
  EndTrim;
end;


procedure TEditableText.MoveCursorDownTo(NewX, NewY: Integer);
begin
  //For comments see MoveCursorUpTo. This is the same but reversed
//  NewX := Max(NewX, 0);
//  NewY := Min(NewY, FLines.Count-1);
  if FInSelecting and HaveSelection then
  begin
    if (NewY < SelEnd.Y) or ((NewY = SelEnd.Y) and (NewX < SelEnd.X)) then
    begin
      acSetSelStart(NewX, NewY);
      UpdateCursor(NewX, NewY);
      EXIT;
    end
    else if (Cursor.Y < SelEnd.Y) or ((Cursor.Y = SelEnd.Y) and (Cursor.X < SelEnd.X)) then
      acSetSelStart(SelEnd.X, SelEnd.Y);
  end;

  if FInSelecting then
  begin
    if not HaveSelection then
      acSetSelStart(FCursor.X, FCursor.Y);
    acSetSelEnd(NewX, NewY)
  end
  else
    acClearSelection;

  UpdateCursor(NewX, NewY);
end;

procedure TEditableText.MoveCursorUpTo(NewX, NewY: Integer);
begin
  NewX := Max(NewX, 0);
  NewY := Max(NewY, 0);
  if FInSelecting and HaveSelection then
  begin
    //Handle the case where the cursor has been moved down (SelEnd > SelStart)
    //and is now moving up again. If so we need to update the _end_ to the new
    //cursor position
    if (NewY > SelStart.Y) or ((NewY = SelStart.Y) and (NewX > SelStart.X)) then
    begin
      acSetSelEnd(NewX, NewY);
      UpdateCursor(NewX, NewY);
      EXIT;
    end
    //Same as above, the cursor has moved down and is now moving up again, but this
    //time the cursor has moved beyond the old SetStart. Here we need to set
    //SelEnd to the old SelStart, then update SelStart to (NewX, NewY)
    else if (Cursor.Y > SelStart.Y) or ((Cursor.Y = SelStart.Y) and (Cursor.X > SelStart.X)) then
      acSetSelEnd(SelStart.X, SelStart.Y);
  end;

  if FInSelecting then
  begin
    //If we don't have a selection then initialise SelEnd
    if not HaveSelection then
      acSetSelEnd(Cursor.X, FCursor.Y);
    acSetSelStart(NewX, NewY);
  end
  else
    acClearSelection;

  UpdateCursor(NewX, NewY);
end;

function TEditableText.NextPoint(var P: TPoint): Boolean;
begin
  Result := not ((P.Y >= LineCount) and (P.X >= Length(Lines[LineCount-1])));
  if P.X < Length(Lines[P.Y]) then
    P.X := P.X + 1
  else if P.Y < LineCount then
  begin
    P.Y := P.Y + 1;
    while (P.Y < LineCount) and (Length(Lines[P.Y]) = 0) do
      P.Y := P.Y + 1;
    if P.Y >= LineCount then
      P.Y := LineCount-1;
    P.X := 0;
  end;
end;

function TEditableText.PrevPoint(var P: TPoint): Boolean;
begin
  Result := not ((P.X = 0) and (P.Y = 0));
  if P.X > 0 then
    P.X := P.X - 1
  else if P.Y > 0 then
  begin
    P.Y := P.Y - 1;
    while (P.Y >= 0) and (Length(Lines[P.Y]) = 0) do
      P.Y := P.Y - 1;
    if P.Y = -1 then
      P.Y := 0;
    P.X := Max(Length(Lines[P.Y])-1, 0);
  end;
end;


procedure TEditableText.Repaint;
begin

end;

procedure TEditableText.SaveToFile;
begin

end;

procedure TEditableText.SaveToFileAs(const AFilename: String);
begin

end;

procedure TEditableText.SetFileOpenTabWidth(const Value: Integer);
begin

end;

procedure TEditableText.SetLines(Index: Integer; const Value: String);
begin

end;

procedure TEditableText.SetOrigin(X, Y: Integer);
begin
  FOrigin.X := X;
  FOrigin.Y := Y;
  Repaint;
end;

procedure TEditableText.SetTabWidth(const Value: Integer);
begin
  if Value >= 1 then
    FTabWidth := Value;
end;

procedure TEditableText.SetVisible(X, Y: Integer);
begin
  FVisible.X := X;
  FVisible.Y := Y;
end;

procedure TEditableText.UpdateCursor(NewCol, NewRow: Integer;
  ForceRedraw: Boolean);
var
  NewOrigin: TPoint;
begin
  NewOrigin := Origin;

  if NewCol < Origin.X then
  begin
    NewOrigin.X := NewCol;
    ForceRedraw := True;
  end
  else if NewCol > Origin.X + Visible.X then
  begin
    NewOrigin.X := NewCol - Visible.X;
    ForceRedraw := True;
  end;

  if NewRow < Origin.Y then
  begin
    NewOrigin.Y := NewRow;
    ForceRedraw := True;
  end
  else if NewRow > Origin.Y + Visible.Y then
  begin
    NewOrigin.Y := NewRow - Visible.Y;
    ForceRedraw := True;
  end;

  FCursor.X := NewCol;
  FCursor.Y := NewRow;

  SetOrigin(NewOrigin.X, NewOrigin.Y);

  if ForceRedraw and Assigned(OnRepaint) then
    OnRepaint;
  if Assigned(OnUpdateCursor) then
    OnUpdateCursor;
end;

procedure TEditableText.WordEnd(var P: TPoint);
begin //Ctrl+Left
  while not CharInSet(CharAtPoint(P), WordChars) do
    if not NextPoint(P) then
      BREAK;
  while CharInSet(CharAtPoint(P), WordChars) do
    if not NextPoint(P) then
      BREAK;
end;

procedure TEditableText.WordStart(var P: TPoint);
var Prev: TPoint;
begin //Ctrl+Left
  while PrevPoint(P) do
    if CharInSet(CharAtPoint(P), WordChars) then
      BREAK;
  Prev := P;
  while CharInSet(CharAtPoint(P), WordChars) do
  begin
    Prev := P;
    if not PrevPoint(P) then
      BREAK;
  end;
  P := Prev;
end;

{ TUndoList }

procedure TUndoList.Add(Action: TEditAction;Pos: TPoint;const Text: String);
var Item: PUndoItem;
begin
  if InUndo then
    EXIT;

  New(Item);
  Item.Action := Action;
  Item.Pos := Pos;
  Item.Text := Text;
  while FItems.Count > FIndex do
  begin
    Dispose(FItems[FItems.Count-1]);
    FItems.Delete(FItems.Count-1);
  end;
  FItems.Add(Item);
  FIndex := FItems.Count;
end;

function TUndoList.BeginUndo: PUndoItem;
begin
  Assert(not InUndo);

  FInUndo := True;
  if FIndex > 0 then
    Result := FItems[FIndex-1]
  else
    Result := nil;
end;

procedure TUndoList.CancelUndo;
begin
  Assert(InUndo);
  FInUndo := False;
end;

constructor TUndoList.Create;
begin
  inherited Create;
  FItems := TList<PUndoItem>.Create;
  FIndex := 0;
end;

destructor TUndoList.Destroy;
var Item: PUndoItem;
begin
  for Item in FItems do
    Dispose(Item);
  inherited;
end;

function TUndoList.GetRedoPos: TPoint;
begin
  Assert(not InUndo);

  if FIndex < FItems.Count then
    Result := FItems[FIndex].Pos
  else
    Result := TPoint.Create(-1, -1);
end;

function TUndoList.GetRedoText: String;
begin
  Assert(not InUndo);

  if FIndex < FItems.Count then
  begin
    Result := FItems[FIndex].Text;
    inc(FIndex);
  end
  else
    Result := '';
end;

procedure TUndoList.ProcessUndo;
begin
  Assert(InUndo);

  if FIndex > 0 then
  begin
    dec(FIndex);
    FInUndo := False;
  end;
end;

end.
