unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects,
  FMX.TextLayout, FMX.Layouts, FMX.ScrollBox, FMX.Memo,
  TextBrowser, FMX.Edit, FMX.EditBox, FMX.SpinBox;

type
  TForm1 = class(TForm)
    TextArea: TLayout;
    Panel1: TPanel;
    Label1: TLabel;
    spFontSize: TSpinBox;
    procedure FormCreate(Sender: TObject);
    procedure spFontSizeChange(Sender: TObject);
  private
    Browser: TTextBrowser;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Browser := TTextBrowser.Create(Self);
  Browser.Parent := TextArea;
  Browser.Align := TAlignLayout.Client;
  Browser.Text.LoadFromFile('C:\DropBox\Delphi\TextEditor\TextBrowser.pas');
  Browser.SetFocus;
end;

procedure TForm1.spFontSizeChange(Sender: TObject);
begin
  Browser.Font.Size := spFontSize.Value;
end;

end.
