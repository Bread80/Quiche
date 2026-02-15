unit vSourceFileView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, mHardware,
  FMX.TabControl;

type
  TSourceFileView = class(TForm)
    Tabs: TTabControl;
    procedure FormCreate(Sender: TObject);
  private
    procedure DoUpdate;
    //If the file is not showing, open it.
    //Position caret on the given line in the file.
    procedure GotoLine(const Filename: String;FileLine: Integer);
  public
    procedure UpdateProc(UpdateType: TUpdateType);
  end;

var
  SourceFileView: TSourceFileView;

implementation
uses IOUtils, TextBrowser;

{$R *.fmx}

{ TForm2 }

procedure TSourceFileView.DoUpdate;
var Filename: String;
  FileLine: Integer;
begin
  if Hardware.TryAddrToSourceFile(Hardware.Z80.Z80.PC, Filename, FileLine) then
    if FileExists(Filename) then
      GotoLine(Filename, FileLine);
end;

procedure TSourceFileView.FormCreate(Sender: TObject);
begin
  Hardware.AddUpdateProc(UpdateProc);
end;

procedure TSourceFileView.GotoLine(const Filename: String; FileLine: Integer);
var I: Integer;
  TabItem: TTabItem;
  Editor: TTextBrowser;
begin
  //Search tabs for file
  for I := 0 to Tabs.TabCount - 1 do
//  tabs.tabs[0].children[1].children[0].classname
    if Tabs.Tabs[I].Children[1].Children[0] is TTextBrowser then
    begin
      Editor := Tabs.Tabs[I].Children[1].Children[0] as TTextBrowser;
      if CompareText(Editor.Text.Filename, Filename) = 0 then
      begin
        Tabs.ActiveTab := Tabs.Tabs[I];
        Editor.Text.acSetCursor(0, FileLine - 1);
        EXIT;
      end;
    end;

  //Tab not found - Create it
  TabItem := Tabs.Add;
  TabItem.Text := TPath.GetFilenameWithoutExtension(Filename);
  Editor := TTextBrowser.Create(Self);
  Editor.Parent := TabItem;
  Editor.Align := TAlignLayout.Client;
  Editor.Text.LoadFromFile(Filename);
  Editor.Text.acSetCursor(0, FileLine - 1);
end;

procedure TSourceFileView.UpdateProc(UpdateType: TUpdateType);
begin
  DoUpdate;
end;

end.
