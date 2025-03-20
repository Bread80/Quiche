unit vBreakpoints;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Controls.Presentation, FMX.StdCtrls, mHardware;

type
  TBreakpoints = class(TForm)
    Label1: TLabel;
    edAddr: TEdit;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure edAddrChange(Sender: TObject);
  private
    FInPause: Boolean;
    FAddr: Word;
    procedure BreakProc;
  public
  end;

implementation
uses mZ80State;
{$R *.fmx}

procedure TBreakpoints.edAddrChange(Sender: TObject);
var A: Integer;
begin
  TryStrToInt('$'+edAddr.Text, A);
  if (A >= 0) and (A <= $ffff) then
  begin
    FAddr := A;
    Label2.Text := IntToHex(FAddr, 4);
  end;
end;

procedure TBreakpoints.FormCreate(Sender: TObject);
begin
  FInPause := False;
  Hardware.BreakpointProc := BreakProc;
end;

procedure TBreakpoints.BreakProc;
begin
  if not FInPause then
  begin
    FInPause := True;
//    Label2.Text := IntToHex(FAddr, 4);
    if (FAddr <> 0) and (Hardware.Z80.Z80.PC = FAddr) then
      Hardware.Pause;
    fInPause := False;
  end;
end;

end.
