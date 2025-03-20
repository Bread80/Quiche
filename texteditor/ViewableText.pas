(*
Abstract-ish class allowing text to be loaded, saved, and viewed and queried.

Generally will need to be subclassed to provide functionality to browse end edit
as required.
*)
unit ViewableText;

interface
uses Classes, Types, FMX.Platform, Generics.Collections;

type
  TEventProc = procedure of object;

type TViewPort = class;

  TViewableText = class
  private
    FFileOpenTabWidth: Integer; //Width of tab to assume when opening files
    FLines: TStringList;  //The text being editted

    FViewPorts: TList<TViewPort>;
    FOnRepaint: TEventProc;

    FFilename: String;
    procedure SetLine(Index: Integer; const Value: String);
  protected
    //Override to preform actions before a file is saved
    procedure BeforeSave;virtual;
    //Override to perform actions after a file is loaded
    procedure AfterLoad;virtual;

    //Expands any tabs in Line using current TabWidth setting
    function TabsToSpaces(const Line: String): String;
    //Expands all tab characters within the text.
    //ie. Calls TabsToSpaces for every line in the file
    procedure ExpandTabs;
    procedure SetFileOpenTabWidth(const Value: Integer);

    function GetLineCount: Integer;
    function GetLines(Index: Integer): String;
    procedure SetLines(Index: Integer; const Value: String);
    procedure DeleteLine(Index: Integer);
    procedure InsertLine(Index: Integer;Text: String);
    procedure AppendLine(Text: String);

    //Returns the char at the selection position
    function CharAtPoint(P: TPoint): Char;

    //Returns the Indent of the given line.
    //Indent is the column index of the first non-space character
    //If there line is empty returns -1
    function GetIndentOfLine(LineNo: Integer): Integer;

    //Redraws the entire display and all viewports
    procedure Repaint;
  public
    constructor Create;
    destructor Destroy;override;

    procedure LoadFromFile(const AFilename: String);
    procedure SaveToFile;
    procedure SaveToFileAs(const AFilename: String);

    //Return entire text as a string
    function AsString: String;

    //Add a view port to the list of viewports
    procedure AddViewport(AViewport: TViewport);
    //Remove a viewport from the list if viewports
    procedure RemoveViewport(AViewport: TViewport);


    property Lines[Index: Integer]: String read GetLines write SetLines;default;
    property LineCount: Integer read GetLineCount;

    property FileOpenTabWidth: Integer read FFileOpenTabWidth write SetFileOpenTabWidth;

    //The prefered way to change the filename is via SaveToFileAs.
    //Use this with caution
    property Filename: String read FFilename write FFilename;

    property Viewports: TList<TViewport> read FViewports;

    //Events - intended for use by TTextBrowser
    property OnRepaint: TEventProc read FOnRepaint write FOnRepaint;
  end;

  TViewPort = class
  private
    FText: TViewableText;

    FCursor: TPoint;      //Cursor position in file
    FBounds: TPoint;     //Number of whole rows and columns visible on screen
    FPageScrollAmount: Integer;
    FOrigin: TPoint;      //Column and row displayed at the top-left of the window

    FOnUpdateCursor: TEventProc;

    FOnRepaint: TEventProc;
  protected

    //Update the cursor position. Scrolls and redraws the text as necessary
    //If ForceRedraw is False the screen will only be redrawn if it needs to be
    //scrolled (vertically or horizontally). If True the screen will always be redrawn
    procedure UpdateCursor(NewCol, NewRow: Integer;ForceRedraw: Boolean = False);virtual;
  public
    constructor Create(AText: TViewableText);
    destructor Destroy;override;

    //Set the cursor position. Scrolls the screen as needed to make the cursor
    //visible
    procedure acSetCursor(X, Y: Integer);
    //Updates the Origin and repaints the window
    procedure acSetOrigin(X, Y: Integer);

    //Updates the visible dimensions. Doesn't repaint anything.
    //Should only be called by the TTextBrowser. Used for cursor positioning
    //and screen repaints
    procedure SetBounds(X, Y: Integer);
    procedure SetOrigin(X, Y: Integer);

    procedure Repaint;

    property Text: TViewableText read FText;

    property OnRepaint: TEventProc read FOnRepaint write FOnRepaint;

    property Origin: TPoint read FOrigin;
    property Bounds: TPoint read FBounds;
    property PageScrollAmount: Integer read FPageScrollAmount write FPageScrollAmount;
    property Cursor: TPoint read FCursor;

    property OnUpdateCursor: TEventProc read FOnUpdateCursor write FOnUpdateCursor;
  end;

implementation
uses SysUtils;

{ TBrowsableText }

procedure TViewableText.AddViewport(AViewport: TViewport);
begin
  Assert(FViewPorts.IndexOf(AViewPort) < 0, 'Viewport already in list');
  FViewports.Add(AViewport);
end;

procedure TViewableText.AfterLoad;
begin
  ExpandTabs;
end;

procedure TViewableText.AppendLine(Text: String);
begin
  FLines.Add(Text);
end;

function TViewableText.AsString: String;
begin
  Result := FLines.Text;
end;

procedure TViewableText.BeforeSave;
begin
  //Nowt
end;

function TViewableText.CharAtPoint(P: TPoint): Char;
begin
  if Length(Lines[P.Y]) = 0 then
    Result := #0
  else
    Result := Lines[P.Y].Chars[P.X];
end;

constructor TViewableText.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
  FFileOpenTabWidth := 4;
end;

procedure TViewableText.DeleteLine(Index: Integer);
begin
  Flines.Delete(Index);
end;

destructor TViewableText.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TViewableText.ExpandTabs;
var I: Integer;
begin
  for I := 0 to FLines.Count-1 do
  begin
    //Trim any trailing whitespace from lines
    FLines[I] := FLines[I].TrimRight;

    FLines[I] := TabsToSpaces(FLines[I]);
  end;
end;

function TViewableText.GetIndentOfLine(LineNo: Integer): Integer;
var Line: String;
begin
  Result := 0;
  Line := FLines[LineNo];
  while Result < Line.Length do
  begin
    if ord(Line.Chars[Result]) > 33 then
      EXIT;
    inc(Result);
  end;

  Result := -1;
end;

function TViewableText.GetLines(Index: Integer): String;
begin
  Result := FLines[Index];
end;

procedure TViewableText.InsertLine(Index: Integer; Text: String);
begin
  FLines.Insert(Index, Text);
end;

function TViewableText.GetLineCount: Integer;
begin
  Result := FLines.Count;
end;

procedure TViewableText.LoadFromFile(const AFilename: String);
begin
  FFilename := AFilename;
  FLines.LoadFromFile(Filename);
  AfterLoad;
  Repaint;
end;

procedure TViewableText.RemoveViewport(AViewport: TViewport);
begin
  Assert(FViewports.IndexOf(AViewport) >= 0, 'Viewport not in list');
  FViewports.Remove(AViewport);
end;

procedure TViewableText.Repaint;
var Viewport: TViewport;
begin
  if Assigned(OnRepaint) then
    OnRepaint;
  if Assigned(ViewPorts) then
    for Viewport in Viewports do
      Viewport.Repaint;
end;

procedure TViewableText.SaveToFile;
begin
  BeforeSave;
  FLines.SaveToFile(FFilename);
end;

procedure TViewableText.SaveToFileAs(const AFilename: String);
begin
  FFilename := AFilename;
  SaveToFile;
end;

procedure TViewableText.SetFileOpenTabWidth(const Value: Integer);
begin
  if Value >= 1 then
    FFileOpenTabWidth := Value;
end;

procedure TViewableText.SetLine(Index: Integer; const Value: String);
begin

end;

procedure TViewableText.SetLines(Index: Integer; const Value: String);
begin
  FLines[Index] := Value;
end;

function TViewableText.TabsToSpaces(const Line: String): String;
var C: Char;
begin
  Result := '';
  for C in Line do
    if C = #9 then
      Result := Result.PadRight(Result.Length + FileOpenTabWidth - (Result.Length mod FileOpenTabWidth))
    else
      Result := Result + C;
end;


{ TViewPort }

procedure TViewPort.acSetCursor(X, Y: Integer);
begin
  UpdateCursor(X, Y, False);
end;

procedure TViewPort.acSetOrigin(X, Y: Integer);
begin
  SetOrigin(X, Y);
  if Assigned(OnUpdateCursor) then
    OnUpdateCursor;
end;

constructor TViewPort.Create(AText: TViewableText);
begin
  inherited Create;
  FText := AText;
end;

destructor TViewPort.Destroy;
begin

  inherited;
end;

procedure TViewPort.Repaint;
begin
  if Assigned(OnRepaint) then
    OnRepaint;
end;

procedure TViewPort.SetBounds(X, Y: Integer);
begin
  FBounds.X := X;
  FBounds.Y := Y;
end;

procedure TViewPort.SetOrigin(X, Y: Integer);
begin
  FOrigin.X := X;
  FOrigin.Y := Y;
  Repaint;
end;

procedure TViewPort.UpdateCursor(NewCol, NewRow: Integer; ForceRedraw: Boolean);
var
  NewOrigin: TPoint;
begin
  NewOrigin := Origin;

  if NewCol < Origin.X then
  begin
    NewOrigin.X := NewCol;
    ForceRedraw := True;
  end
  else if NewCol > Origin.X + Bounds.X then
  begin
    NewOrigin.X := NewCol - Bounds.X;
    ForceRedraw := True;
  end;

  if NewRow < Origin.Y then
  begin
    NewOrigin.Y := NewRow;
    ForceRedraw := True;
  end
  else if NewRow > Origin.Y + Bounds.Y then
  begin
    NewOrigin.Y := NewRow - Bounds.Y;
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

end.
