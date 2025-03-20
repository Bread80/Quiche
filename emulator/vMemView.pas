unit vMemView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, mZ80State,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  mHardware, FMX.ListBox, FMX.Edit;

type TAddressType = (atAbsolute, atRegister);

type
  TMemView = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Label1: TLabel;
    edAddr: TEdit;
    Label2: TLabel;
    cbReg: TComboBox;
    cbLocked: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Memo1Resized(Sender: TObject);
    procedure edAddrChangeTracking(Sender: TObject);
    procedure cbRegChange(Sender: TObject);
    procedure cbLockedChange(Sender: TObject);
  private
    FAddr: Word;
    //Base address for the viewport
    FViewAddr: Word;
    //The address pointed to by the register or absolute value (Addr)
    FCurAddr: Word;
    FLocked: Boolean;
    FAddressType: TAddressType;
    FRPIndex: TRPIndex;
    FInUpdate: Boolean;
    FAddrTyping: Boolean;
    procedure DoUpdate;
    procedure SetAddr(const Value: Word);
    procedure SetAddressType(const Value: TAddressType);
    procedure SetLocked(const Value: Boolean);
    procedure SetRPIndex(const Value: TRPIndex);
  public
    procedure UpdateProc(UpdateType: TUpdateType);

    property AddressType: TAddressType read FAddressType write SetAddressType;
    property Addr: Word read FAddr write SetAddr;
    property RPIndex: TRPIndex read FRPIndex write SetRPIndex;
    property Locked: Boolean read FLocked write SetLocked;
  end;

var
  MemView: TMemView;

implementation

{$R *.fmx}

const
  LineHeight = 16;
  BytesPerLine = 8;

{ TMemViewForm }

procedure TMemView.cbLockedChange(Sender: TObject);
begin
  if not FInUpdate then
  begin
    FLocked := cbLocked.IsChecked;
    DoUpdate;
  end;
end;

procedure TMemView.cbRegChange(Sender: TObject);
begin
  if not FInUpdate then
  begin
    if (cbReg.ItemIndex >= 0) and (cbReg.ItemIndex < RPCount) then
    begin
      FAddressType := atRegister;
      RPIndex := cbReg.ItemIndex;
    end
    else
      FAddressType := atAbsolute;

    DoUpdate;
  end;
end;

procedure TMemView.DoUpdate;
var
  LineNo: Integer;
  LineCount: Integer;
  Ptr: Word;
  B: Integer;
  Data: Byte;
  S: String;
  Ascii: String;
begin
  if (FAddressType = atRegister) then
    FCurAddr := Hardware.Z80.Z80.GetRPByIndex(RPIndex);
  if not FAddrTyping and not FLocked then
  begin
    FInUpdate := True;
    edAddr.Text := IntToHex(FCurAddr);
    FInUpdate := False;
  end;
  if not FLocked then
    FViewAddr := FCurAddr;

  Memo1.Lines.Clear;
  LineCount := trunc(Memo1.Height) div LineHeight;

  Ptr := (FViewAddr - (LineCount div 2) * BytesPerLine);
  Ptr := Ptr - (Ptr mod BytesPerLine);
  LineNo := 0;
  while LineNo <= LineCount do
  begin
    S := IntToHex(Ptr, 4);

    Ascii := '';
    for B := 0 to BytesPerLine - 1 do
    begin
      if Ptr = FCurAddr then
        S := S + '*'
      else
        S := S + ' ';
      Data := Hardware.ReadMemoryByte(Ptr);
      S := S + IntToHex(Data, 2);
      if (Data >= 32) and (Data <= 127) then
        Ascii := Ascii + chr(Data)
      else
        Ascii := Ascii + '.';
      Ptr := (Ptr + 1) and $ffff;
    end;
    Memo1.Lines.Add(S + ' ' + Ascii);
    Inc(LineNo);
  end;
end;

procedure TMemView.edAddrChangeTracking(Sender: TObject);
var NewAddr: Integer;
begin
  if not FInUpdate then
    if TryStrToInt('$' + edAddr.Text, NewAddr) then
      if (NewAddr >= 0) and (NewAddr <= $ffff) then
      begin
        FAddr := NewAddr;
        FAddressType := atAbsolute;
        FAddrTyping := True;
        DoUpdate;
        FAddrTyping := False;
      end;
end;

procedure TMemView.FormCreate(Sender: TObject);
var I: Integer;
begin
  for I := 0 to RPCount -1 do
    cbReg.Items.Add(RPNames[I]);
  cbReg.Items.Add('..');
  Hardware.AddUpdateProc(UpdateProc);
end;

procedure TMemView.Memo1Resized(Sender: TObject);
begin
  DoUpdate;
end;

procedure TMemView.SetAddr(const Value: Word);
begin
  FAddr := Value;
  DoUpdate;
end;

procedure TMemView.SetAddressType(const Value: TAddressType);
begin
  FAddressType := Value;
  DoUpdate;
end;

procedure TMemView.SetLocked(const Value: Boolean);
begin
  FLocked := Value;
  FInUpdate := True;
  cbLocked.IsChecked := Value;
  FInUpdate := False;
  DoUpdate;
end;

procedure TMemView.SetRPIndex(const Value: TRPIndex);
begin
  if not Locked then
  begin
    FRPIndex := Value;
    FInUpdate := True;
    cbReg.ItemIndex := Value;
    FInUpdate := False;
    FAddressType := atRegister;
    DoUpdate;
  end;
end;

procedure TMemView.UpdateProc(UpdateType: TUpdateType);
begin
  DoUpdate;
end;

end.
