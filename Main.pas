unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Edit;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Source: TLabel;
    mmSource: TMemo;
    Panel2: TPanel;
    Label1: TLabel;
    mmIL: TMemo;
    Panel3: TPanel;
    btnStatement: TButton;
    Label2: TLabel;
    edError: TEdit;
    Label3: TLabel;
    mmOutput: TMemo;
    btnBlock: TButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Panel4: TPanel;
    btnRun: TButton;
    procedure btnStatementClick(Sender: TObject);
    procedure btnBlockClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses ILData, Variables, QuicheParser, MErrors, ILExec;

{$R *.fmx}

procedure TForm1.btnBlockClick(Sender: TObject);
var Err: TAssembleError;
begin
  ClearILList;
  ClearVars;
  LoadFromString(mmSource.Text);
  Err := errNone;
  while (Err = errNone) and not ParserEOF do
    Err := ParseBlock(bsSingle);

  ILToStrings(mmIL.Lines);
  edError.Text := Integer(Err).ToString + ': ' + Errors[Err];

  Focused := mmSource;
end;

procedure TForm1.btnRunClick(Sender: TObject);
begin
  Execute;

  mmOutput.Lines.Assign(ExecOutput);
  VarsToStrings(mmOutput.Lines);

end;

procedure TForm1.btnStatementClick(Sender: TObject);
var Err: TAssembleError;
begin
  ClearILList;
  ClearVars;
  LoadFromString(mmSource.Text);
  Err := ParseStatement('');

  ILToStrings(mmIL.Lines);
  edError.Text := Integer(Err).ToString + ': ' + Errors[Err];

  Focused := mmSource;
end;

end.
