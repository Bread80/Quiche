program TextEditor;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  TextBrowser in 'TextBrowser.pas',
  EditableText in 'EditableText.pas',
  ViewableText in 'ViewableText.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
