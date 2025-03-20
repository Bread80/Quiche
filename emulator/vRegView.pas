unit vRegView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  mZ80State, mDisassembler, mSymbols, mHardware, vMemView,
  FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TRegView = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    RegAh: TEdit;
    RegBC: TEdit;
    RegDE: TEdit;
    RegHL: TEdit;
    RegIX: TEdit;
    RegIY: TEdit;
    RegSP: TEdit;
    RegPC: TEdit;
    RegAah: TEdit;
    RegBCa: TEdit;
    RegDEa: TEdit;
    RegHLa: TEdit;
    RegI: TEdit;
    RegR: TEdit;
    RegC: TEdit;
    RegB: TEdit;
    RegD: TEdit;
    RegE: TEdit;
    RegH: TEdit;
    RegL: TEdit;
    RegBa: TEdit;
    RegCa: TEdit;
    RegDa: TEdit;
    RegEa: TEdit;
    RegHa: TEdit;
    RegLa: TEdit;
    RegAi: TEdit;
    RegAai: TEdit;
    FlagS: TLabel;
    FlagZ: TLabel;
    FlagY: TLabel;
    FlagH: TLabel;
    FlagX: TLabel;
    FlagN: TLabel;
    FlagPV: TLabel;
    FlagC: TLabel;
    RegAc: TEdit;
    Opcode: TLabel;
    Symbol: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RegBCEnter(Sender: TObject);
    procedure RegDEEnter(Sender: TObject);
    procedure RegHLEnter(Sender: TObject);
    procedure RegIXEnter(Sender: TObject);
    procedure RegIYEnter(Sender: TObject);
    procedure RegSPEnter(Sender: TObject);
    procedure RegBCaEnter(Sender: TObject);
    procedure RegDEaEnter(Sender: TObject);
    procedure RegHLaEnter(Sender: TObject);
    procedure RegPCEnter(Sender: TObject);
  private
    FPrevRegs: TZ80Regs;
    FDisassembler: TZ80Disassembler;
    FMemView: TMemView;
  protected
    procedure UpdateProc(AUpdateType: TUpdateType);
    procedure UpdateOpcode(Z80: TZ80State);
  public
    procedure Update(Z80: TZ80State);
    property MemView: TMemView read FMemView write FMemView;
  end;

var
  RegView: TRegView;

implementation

{$R *.fmx}

{ TForm2 }

procedure TRegView.FormCreate(Sender: TObject);
begin
  FDisassembler := TZ80Disassembler.Create;

  Hardware.AddUpdateProc(UpdateProc);
end;

procedure TRegView.FormDestroy(Sender: TObject);
begin
  Hardware.RemoveUpdateProc(UpdateProc);
  FDisassembler.Free;
end;

procedure TRegView.RegBCaEnter(Sender: TObject);
begin
  if Assigned(FMemView) then
    FMemView.RPIndex := Reg16BCa;
end;

procedure TRegView.RegBCEnter(Sender: TObject);
begin
  if Assigned(FMemView) then
    FMemView.RPIndex := Reg16BC;
end;

procedure TRegView.RegDEaEnter(Sender: TObject);
begin
  if Assigned(FMemView) then
    FMemView.RPIndex := Reg16DEa;
end;

procedure TRegView.RegDEEnter(Sender: TObject);
begin
  if Assigned(FMemView) then
    FMemView.RPIndex := Reg16DE;
end;

procedure TRegView.RegHLaEnter(Sender: TObject);
begin
  if Assigned(FMemView) then
    FMemView.RPIndex := Reg16HLa;
end;

procedure TRegView.RegHLEnter(Sender: TObject);
begin
  if Assigned(FMemView) then
    FMemView.RPIndex := Reg16HL;
end;

procedure TRegView.RegIXEnter(Sender: TObject);
begin
  if Assigned(FMemView) then
    FMemView.RPIndex := Reg16IX;
end;

procedure TRegView.RegIYEnter(Sender: TObject);
begin
  if Assigned(FMemView) then
    FMemView.RPIndex := Reg16IY;
end;

procedure TRegView.RegPCEnter(Sender: TObject);
begin
  if Assigned(FMemView) then
    FMemView.RPIndex := Reg16PC;
end;

procedure TRegView.RegSPEnter(Sender: TObject);
begin
  if Assigned(FMemView) then
    FMemView.RPIndex := Reg16SP;
end;

procedure TRegView.Update(Z80: TZ80State);
var SymbolName: String;
begin
  RegAh.Text := IntToHex(Z80.A,2);
  RegAi.Text := IntToStr(Z80.A);
  if Z80.A in [32..127] then
    RegAc.Text := Chr(Z80.A)
  else
    RegAc.Text := '??';
  RegBC.Text := IntToHex(Z80.BC,4);
  RegB.Text := IntToStr(Z80.B);
  RegC.Text := IntToStr(Z80.C);
  RegDE.Text := IntToHex(Z80.DE,4);
  RegD.Text := IntToStr(Z80.D);
  RegE.Text := IntToStr(Z80.E);
  RegHL.Text := IntToHex(Z80.HL,4);
  RegH.Text := IntToStr(Z80.H);
  RegL.Text := IntToStr(Z80.L);
  RegIX.Text := IntToHex(Z80.IX,4);
  RegIY.Text := IntToHex(Z80.IY,4);
  RegSP.Text := IntToHex(Z80.SP,4);
  RegPC.Text := IntToHex(Z80.PC,4);
  RegAah.Text := IntToHex(Z80.Aa,2);
  RegAai.Text := IntToStr(Z80.Aa);
  RegBCa.Text := IntToHex(Z80.BCa,4);
  RegBa.Text := IntToStr(Z80.Ba);
  RegCa.Text := IntToStr(Z80.Ca);
  RegDEa.Text := IntToHex(Z80.DEa,4);
  RegDa.Text := IntToStr(Z80.Da);
  RegEa.Text := IntToStr(Z80.Ea);
  RegHLa.Text := IntToHex(Z80.HLa,4);
  RegHa.Text := IntToStr(Z80.Ha);
  RegLa.Text := IntToStr(Z80.La);
  RegI.Text := IntToHex(Z80.I,2);
  RegR.Text := IntToHex(Z80.R,2);

  if Z80.FlagS then
    FlagS.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    FlagS.TextSettings.Font.Style := [];
  if Z80.FlagZ then
    FlagZ.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    FlagZ.TextSettings.Font.Style := [];
  if Z80.FlagY then
    FlagY.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    FlagY.TextSettings.Font.Style := [];
  if Z80.FlagH then
    FlagH.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    FlagH.TextSettings.Font.Style := [];
  if Z80.FlagX then
    FlagX.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    FlagX.TextSettings.Font.Style := [];
  if Z80.FlagN then
    FlagN.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    FlagN.TextSettings.Font.Style := [];
  if Z80.FlagPV then
    FlagPV.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    FlagPV.TextSettings.Font.Style := [];
  if Z80.FlagC then
    FlagC.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    FlagC.TextSettings.Font.Style := [];

  if Z80.A <> FPrevRegs.A then
    RegAh.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegAh.TextSettings.Font.Style := [];
  RegAi.TextSettings.Font.Style := RegAh.TextSettings.Font.Style;
  RegAc.TextSettings.Font.Style := RegAh.TextSettings.Font.Style;
  if Z80.B <> FPrevRegs.B then
    RegB.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegB.TextSettings.Font.Style := [];
  if Z80.C <> FPrevRegs.C then
    RegC.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegC.TextSettings.Font.Style := [];
  if Z80.BC <> FPrevRegs.BC then
    RegBC.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegBC.TextSettings.Font.Style := [];
  if Z80.D <> FPrevRegs.D then
    RegD.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegD.TextSettings.Font.Style := [];
  if Z80.E <> FPrevRegs.E then
    RegE.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegE.TextSettings.Font.Style := [];
  if Z80.DE <> FPrevRegs.DE then
    RegDE.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegDE.TextSettings.Font.Style := [];
  if Z80.H <> FPrevRegs.H then
    RegH.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegH.TextSettings.Font.Style := [];
  if Z80.L <> FPrevRegs.L then
    RegL.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegL.TextSettings.Font.Style := [];
  if Z80.HL <> FPrevRegs.HL then
    RegHL.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegHL.TextSettings.Font.Style := [];
  if Z80.IX <> FPrevRegs.IX then
    RegIX.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegIX.TextSettings.Font.Style := [];
  if Z80.IY <> FPrevRegs.IY then
    RegIY.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegIY.TextSettings.Font.Style := [];
  if Z80.SP <> FPrevRegs.SP then
    RegSP.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegSP.TextSettings.Font.Style := [];
  if Z80.PC <> FPrevRegs.PC then
    RegPC.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegPC.TextSettings.Font.Style := [];
  if Z80.aA <> FPrevRegs.aA then
    RegAah.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegAah.TextSettings.Font.Style := [];
  RegAai.TextSettings.Font.Style := RegAah.TextSettings.Font.Style;
  if Z80.Ba <> FPrevRegs.Ba then
    RegBa.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegBa.TextSettings.Font.Style := [];
  if Z80.Ca <> FPrevRegs.Ca then
    RegCa.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegCa.TextSettings.Font.Style := [];
  if Z80.BCa <> FPrevRegs.BCa then
    RegBCa.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegBCa.TextSettings.Font.Style := [];
  if Z80.Da <> FPrevRegs.Da then
    RegDa.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegDa.TextSettings.Font.Style := [];
  if Z80.Ea <> FPrevRegs.Ea then
    RegEa.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegEa.TextSettings.Font.Style := [];
  if Z80.DEa <> FPrevRegs.DEa then
    RegDEa.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegDEa.TextSettings.Font.Style := [];
  if Z80.Ha <> FPrevRegs.Ha then
    RegHa.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegHa.TextSettings.Font.Style := [];
  if Z80.La <> FPrevRegs.La then
    RegLa.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegLa.TextSettings.Font.Style := [];
  if Z80.HLa <> FPrevRegs.HLa then
    RegHLa.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegHLa.TextSettings.Font.Style := [];
  if Z80.I <> FPrevRegs.I then
    RegI.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegI.TextSettings.Font.Style := [];
  if Z80.R <> FPrevRegs.R then
    RegR.TextSettings.Font.Style := [TFontStyle.fsBold]
  else
    RegR.TextSettings.Font.Style := [];

  UpdateOpcode(Z80);
  if Hardware.TryAddrToSymbol(Z80.PC, SymbolName) then
    Symbol.Text := SymbolName
  else
    Symbol.Text := '<No label found>';

  FPrevRegs := Z80.Regs;
end;

procedure TRegView.UpdateOpcode(Z80: TZ80State);
var
  PC: Word;
  Address: String;
  Source: String;
  Bytes: String;
  Chars: String;
  Comments: String;
begin
  PC := Z80.PC;
  FDisassembler.DisassembleToString(PC,
    function(Addr: Word): Byte
    begin
      Result := Hardware.ReadMemoryByte(Addr);
    end,
    Address, Source, Bytes, Chars, Comments);

  Opcode.Text := Source;
end;

procedure TRegView.UpdateProc(AUpdateType: TUpdateType);
begin
  Update(Hardware.Z80.Z80);
end;

end.
