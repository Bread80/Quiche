unit IDE.OptionsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FMX.Controls.Presentation;

type
  TOptions = class(TForm)
    Panel1: TPanel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    cbSourceCode: TCheckBox;
    cbILCode: TCheckBox;
    cbFragmentNames: TCheckBox;
    cbCPUState: TCheckBox;
    Button1: TButton;
    btnSave: TButton;
    Button3: TButton;
    cbBlockInfo: TCheckBox;
    cbPrimitiveNames: TCheckBox;
    cbVarMap: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormSaveState(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Options: TOptions;

implementation
uses IDE.Compiler, IDE.Config;

{$R *.fmx}

procedure TOptions.btnSaveClick(Sender: TObject);
begin
  with IDE.Compiler.GetConfig.CodeGen do
  begin
    BlockInfo := cbBlockInfo.IsChecked;
    SourceCode := cbSourceCode.IsChecked;
    ILCode := cbILCode.IsChecked;
    FragmentNames := cbFragmentNames.IsChecked;
    PrimitiveNames := cbPrimitiveNames.IsChecked;
    CPUState := cbCPUState.IsChecked;
    VarMap := cbVarMap.IsChecked;
  end;
end;

procedure TOptions.FormSaveState(Sender: TObject);
begin
  if ModalResult = mrOk then
    btnSaveClick(Self);
end;

procedure TOptions.FormShow(Sender: TObject);
begin
  with IDE.Compiler.GetConfig.CodeGen do
  begin
    cbBlockInfo.IsChecked := BlockInfo;
    cbSourceCode.IsChecked := SourceCode;
    cbILCode.IsChecked := ILCode;
    cbFragmentNames.IsChecked := FragmentNames;
    cbPrimitiveNames.IsChecked := PrimitiveNames;
    cbCPUState.IsChecked := CPUState;
    cbVarMap.IsChecked := VarMap;
  end;
end;

end.
