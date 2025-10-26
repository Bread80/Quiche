unit mZ80State;

interface
uses SysUtils;

type
  TIndexModifier = (imMain, imIX, imIY);
  TInstructionSet = (isMain, isED, isCB);

type
  TReg8 = 0..7;
  TReg16 = 0..3;
  TInterruptMode = 0..2;

const

  Reg8HL = 6;

  RSCount = 26;
  Reg8C = 0;
  Reg8B = 1;
  Reg8E = 2;
  Reg8D = 3;
  Reg8L = 4;
  Reg8H = 5;
  Reg8F = 6;
  Reg8A = 7;
  Reg8Ca = 8;
  Reg8Ba = 9;
  Reg8Ea = 10;
  Reg8Da = 11;
  Reg8La = 12;
  Reg8Ha = 13;
  Reg8Fa = 14;
  Reg8Aa = 15;
  Reg8IXL = 16;
  Reg8IXH = 17;
  Reg8IYL = 18;
  Reg8IYH = 19;
  Reg8PCL = 20;
  Reg8PCH = 21;
  Reg8SPL = 22;
  Reg8SPH = 23;
  Reg8R = 24;
  Reg8I = 25;

  RPCount = 12;

  //Indexes of 16 bit registers;
  Reg16BC  = 0;
  Reg16DE  = 1;
  Reg16HL  = 2;
  Reg16AF  = 3;
  Reg16BCa = 4;
  Reg16DEa = 5;
  Reg16HLa = 6;
  Reg16AFa = 7;
  Reg16IX  = 8;
  Reg16IY  = 9;
  Reg16PC  = 10;
  Reg16SP  = 11;

type TRPIndex = 0..RPCount - 1;

const RPNames: array of String = ['BC','DE','HL','AF', 'BCa','DEa','HLa','AFa','IX','IY','PC','SP'];

//Flag masks
const
  SMask = $80;
  ZMask = $40;
  YMask = $20;
  HMask = $10;
  XMask = $08;
  PVMask = $04;
  NMask = $02;
  CMask = $01;

//Convert c or cc index from opcode into a bitmask for a flag
const FlagMasks: array[0..3] of Byte = (ZMask, CMask, PVMask, SMask);

type TZ80Regs = packed record
  EXDEHLState: Boolean;
  EXAFAFState: Boolean;
  EXXState: Boolean;
  IM: TInterruptMode;
  IFF2: Boolean;
  IFF1: Boolean;
  case Integer of
    0: (C, B, E, D, L, H, F, A,
        Ca, Ba, Ea, Da, La, Ha, Fa, Aa,
        IXL, IXH, IYL, IYH,
        PCL, PCH, SPL, SPH,
        R, I: Byte);
    1: (BC, DE, HL, AF,
        BCa, DEa, HLa, AFa,
        IX, IY, PC, SP,
        IR: Word);
    //Allows indirect indexed access to 8-bit registers using
    //Reg8x constants
    2: (RS: array[0..RSCount-1] of Byte);
    //Allows indirect indexed access to 16-bit registers using
    //Reg16x constants
    3: (RP: array[0..RPCount-1] of Word);
  end;

type
  PReg8Pointers = ^TReg8Pointers;
  TReg8Pointers = array[0..7] of PByte;

  PReg16Pointers = ^TReg16Pointers;
  TReg16Pointers = array[0..3] of PWord;

type TZ80State = class
  private
    FRegs: TZ80Regs;

    procedure InitRegPointers;
    procedure SetFlag(Mask: Byte;Value: Boolean);
    function GetFlagC: Boolean;
    function GetFlagH: Boolean;
    function GetFlagN: Boolean;
    function GetFlagPV: Boolean;
    function GetFlagS: Boolean;
    function GetFlagZ: Boolean;
    procedure SetFlagH(const Value: Boolean);
    procedure SetFlagN(const Value: Boolean);
    procedure SetFlagPV(const Value: Boolean);
    procedure SetFlagS(const Value: Boolean);
    procedure SetFlagZ(const Value: Boolean);
    procedure SetFlagC(const Value: Boolean);
    function GetFlagX: Boolean;
    function GetFlagY: Boolean;
    procedure SetFlagX(const Value: Boolean);
    procedure SetFlagY(const Value: Boolean);
  public
    Reg8Base: TReg8Pointers;
    Reg8IX: TReg8Pointers;
    Reg8IY: TReg8Pointers;
    RegRSPBase: TReg16Pointers;
    RegRSPIX: TReg16Pointers;
    RegRSPIY: TReg16Pointers;
    RegRAFBase: TReg16Pointers;
    RegRAFIX: TReg16Pointers;
    RegRAFIY: TReg16Pointers;

    constructor Create;

    procedure ColdReset;
    procedure WarmReset;

    //Read or write an 8 bit register encoded into the low 3 bits of r
    procedure SetReg8(r: TReg8;Value: byte;IndexModifier: TIndexModifier = imMain);
    function GetReg8(r: TReg8;IndexModifier: TIndexModifier = imMain): byte;
    procedure SetRegRSP(r: TReg16;Value: Word;IndexModifier: TIndexModifier = imMain);
    function GetRegRSP(r: TReg16;IndexModifier: TIndexmodifier): Word;
    procedure SetRegRAF(r: TReg16;Value: Word;IndexModifier: TIndexModifier = imMain);
    function GetRegRAF(r: TReg16;IndexModifier: TIndexmodifier): Word;

    function GetRPByIndex(RP: Integer): Word;inline;

    function TestFlag(r: TReg8): Boolean;

    //Read PC and increment it
    function PCInc: Word;
    function RInc: byte;

    procedure EXX;
    procedure EXAFAF;
    procedure EXHLDE;

    //Reads from a string in the same format as written by ToString, i.e.
    //A F  B C  D E  H L  IX   IY   PC   SP   IR    A'F' B'C' D'E' H'L'
    //FFFF FFFF  FFFF FFFF FFFF FFFF 0000 FFFF FF00 FFFF FFFF FFFF FFFF
    //SZYHXPNC
    //
    //Error checking is non existent.
    procedure FromString(AState: String);
    function ToString: String;override;

    property Regs: TZ80Regs read FRegs write FRegs;

    property A: byte read FRegs.A write FRegs.A;
    property F: byte read FRegs.F write FRegs.F;
    property B: byte read FRegs.B write FRegs.B;
    property C: byte read FRegs.C write FRegs.C;
    property D: byte read FRegs.D write FRegs.D;
    property E: byte read FRegs.E write FRegs.E;
    property H: byte read FRegs.H write FRegs.H;
    property L: byte read FRegs.L write FRegs.L;
    property Aa: byte read FRegs.Aa write FRegs.Aa;
    property Fa: byte read FRegs.Fa write FRegs.Fa;
    property Ba: byte read FRegs.Ba write FRegs.Ba;
    property Ca: byte read FRegs.Ca write FRegs.Ca;
    property Da: byte read FRegs.Da write FRegs.Da;
    property Ea: byte read FRegs.Ea write FRegs.Ea;
    property Ha: byte read FRegs.Ha write FRegs.Ha;
    property La: byte read FRegs.La write FRegs.La;
    property AF: word read FRegs.AF write FRegs.AF;
    property BC: word read FRegs.BC write FRegs.BC;
    property DE: word read FRegs.DE write FRegs.DE;
    property HL: word read FRegs.HL write FRegs.HL;
    property AFa: word read FRegs.AFa write FRegs.AFa;
    property BCa: word read FRegs.BCa write FRegs.BCa;
    property DEa: word read FRegs.DEa write FRegs.DEa;
    property HLa: word read FRegs.HLa write FRegs.HLa;
    property IXH: byte read FRegs.IXH write FRegs.IXH;
    property IXL: byte read FRegs.IXL write FRegs.IXL;
    property IYH: byte read FRegs.IYH write FRegs.IYH;
    property IYL: byte read FRegs.IYL write FRegs.IYL;
    property IX: word read FRegs.IX write FRegs.IX;
    property IY: word read FRegs.IY write FRegs.IY;
    property PC: word read FRegs.PC write FRegs.PC;
    property SP: word read FRegs.SP write FRegs.SP;
    property I: byte read FRegs.I write FRegs.I;
    property R: byte read FRegs.R write FRegs.R;
    property IR: word read FRegs.IR write FRegs.IR;

    property EXXState: Boolean read FRegs.EXXState;
    property EXAFAFState: Boolean read FRegs.EXAFAFState;
    property EXDEHLState: Boolean read FRegs.EXDEHLState;

    property FlagS: Boolean read GetFlagS write SetFlagS;
    property FlagZ: Boolean read GetFlagZ write SetFlagZ;
    //Undocumented X flag
    property FlagX: Boolean read GetFlagX write SetFlagX;
    property FlagH: Boolean read GetFlagH write SetFlagH;
    //Undocumented Y flag
    property FlagY: Boolean read GetFlagY write SetFlagY;
    //Undocumented2
    //P and V are the equivalent
    property FlagPV: Boolean read GetFlagPV write SetFlagPV;
//    property FlagV: Boolean read GetFlagPV write SetFlagPV;
    property FlagN: Boolean read GetFlagN write SetFlagN;
    property FlagC: Boolean read GetFlagC write SetFlagC;

    property IM: TInterruptMode read FRegs.IM write FRegs.IM;
    //Interrupts are enabled if IFF1 = IFF2 = True
    //and disabled if IFF1 = IFF2 = False;
    //An NMI copies IFF1 to IFF2 and clears IFF1 (which disabled INTs)
    //RETN (RETurn from Nmi) restores IFF2 to IFF1
    property IFF1: Boolean read FRegs.IFF1 write FRegs.IFF1;
    property IFF2: Boolean read FRegs.IFF2 write FRegs.IFF2;
end;

type EZ80Exception = class(Exception);

implementation


{ TZ80State }

procedure TZ80State.ColdReset;
begin
  AF := $ffff;
  BC := $ffff;
  DE := $ffff;
  HL := $ffff;
  AFa := $ffff;
  BCa := $ffff;
  DEa := $ffff;
  HLa := $ffff;
  IX := $ffff;
  IY := $ffff;
  SP := $ffff;
  PC := 0;
  IR := $ff00;
  IFF1 := False;
  IFF2 := False;
  IM := 0;
  FRegs.EXAFAFState := False;
  FRegs.EXXState := False;
  FRegs.EXDEHLState := False;
end;

constructor TZ80State.Create;
begin
  inherited;
  InitRegPointers;

  ColdReset;
end;

procedure TZ80State.EXAFAF;
var
  Temp: word;
begin
  Temp := AF;
  AF := AFa;
  AFa := Temp;
  FRegs.EXAFAFState := not EXAFAFState;
end;

procedure TZ80State.EXHLDE;
var
  Temp: word;
begin
  Temp := DE;
  DE := HL;
  HL := Temp;
  FRegs.EXDEHLState := not EXDEHLState;
end;

procedure TZ80State.EXX;
var
  Temp: Word;
begin
  Temp := BC;
  BC := BCa;
  BCa := Temp;
  Temp := DE;
  DE := DEa;
  DEa := Temp;
  Temp := HL;
  HL := HLa;
  HLa := Temp;
  FRegs.EXXState := not EXXState;
end;

procedure TZ80State.FromString(AState: String);
var
  LLines: TArray<String>;
  LLine: TArray<String>;
begin
//A F  B C  D E  H L  IX   IY   PC   SP   IR    A'F' B'C' D'E' H'L'
//FFFF FFFF FFFF FFFF FFFF FFFF 0000 FFFF FF00 FFFF FFFF FFFF FFFF
//SZYHXPNC
  LLines := AState.Split([#13,#10]);
  if LLines[1] <> '' then
    LLine := LLines[1].Split([' '])
  else
    LLine := LLines[2].Split([' ']);
  AF := StrToInt('$'+LLine[0]);
  BC := StrToInt('$'+LLine[1]);
  DE := StrToInt('$'+LLine[2]);
  HL := StrToInt('$'+LLine[3]);
  IX := StrToInt('$'+LLine[4]);
  IY := StrToInt('$'+LLine[5]);
  PC := StrToInt('$'+LLine[6]);
  SP := StrToInt('$'+LLine[7]);
  IR := StrToInt('$'+LLine[8]);
  AFa := StrToInt('$'+LLine[9]);
  BCa := StrToInt('$'+LLine[10]);
  DEa := StrToInt('$'+LLine[11]);
  HLa := StrToInt('$'+LLine[12]);
end;

function TZ80State.GetFlagC: Boolean;
begin
  Result := F and CMask <> 0;
end;

function TZ80State.GetFlagH: Boolean;
begin
  Result := F and HMask <> 0;
end;

function TZ80State.GetFlagN: Boolean;
begin
  Result := F and NMask <> 0;
end;

function TZ80State.GetFlagPV: Boolean;
begin
  Result := F and PVMask <> 0;
end;

function TZ80State.GetFlagS: Boolean;
begin
  Result := F and SMask <> 0;
end;

function TZ80State.GetFlagX: Boolean;
begin
  Result := F and XMask <> 0;
end;

function TZ80State.GetFlagY: Boolean;
begin
  Result := F and YMask <> 0;
end;

function TZ80State.GetFlagZ: Boolean;
begin
  Result := F and ZMask <> 0;
end;

function TZ80State.GetReg8(r: TReg8;IndexModifier: TIndexModifier): byte;
begin
  Result := 0;
  case r of
  0: Result := B;
  1: Result := C;
  2: Result := D;
  3: Result := E;
  4:
    case IndexModifier of
      imMain: Result := H;
      imIX: Result := IXH;
      imIY: Result := IYH;
    end;
  5:
    case IndexModifier of
      imMain: Result := L;
      imIX: Result := IXL;
      imIY: Result := IYL;
    end;

  //6 = (HL) - not valid here
  7: Result := A;
  else
    raise EZ80Exception.Create('Invalid 8 bit register encoding');
  end;
end;

function TZ80State.GetRegRAF(r: TReg16; IndexModifier: TIndexmodifier): Word;
begin
  Result := 0;
  case r of
    0: Result := BC;
    1: Result := DE;
    2:
    case IndexModifier of
      imMain: Result := HL;
      imIX: Result := IX;
      imIY: Result := IY;
    end;
    3: Result := AF;
  else
    raise EZ80Exception.Create('Invalid register encoding');
  end;
end;

function TZ80State.GetRegRSP(r: TReg16; IndexModifier: TIndexmodifier): Word;
begin
  Result := 0;
  case r of
    0: Result := BC;
    1: Result := DE;
    2:
    case IndexModifier of
      imMain: Result := HL;
      imIX: Result := IX;
      imIY: Result := IY;
    end;
    3: Result := SP;
  else
    raise EZ80Exception.Create('Invalid register encoding');
  end;
end;

function TZ80State.GetRPByIndex(RP: Integer): Word;
begin
  Result := FRegs.RP[RP];
end;

procedure TZ80State.InitRegPointers;
begin
  Reg8Base[0] := @FRegs.B;
  Reg8Base[1] := @FRegs.C;
  Reg8Base[2] := @FRegs.D;
  Reg8Base[3] := @FRegs.E;
  Reg8Base[4] := @FRegs.H;
  Reg8Base[5] := @FRegs.L;
  Reg8Base[6] := nil;
  Reg8Base[7] := @FRegs.A;

  Reg8IX := Reg8Base;
  Reg8IX[4] := @FRegs.IXH;
  Reg8IX[5] := @FRegs.IXL;

  Reg8IY := Reg8Base;
  Reg8IY[4] := @FRegs.IYH;
  Reg8IY[5] := @FRegs.IYL;

  RegRSPBase[0] := @FRegs.BC;
  RegRSPBase[1] := @FRegs.DE;
  RegRSPBase[2] := @FRegs.HL;
  RegRSPBase[3] := @FRegs.SP;

  RegRSPIX[0] := @FRegs.BC;
  RegRSPIX[1] := @FRegs.DE;
  RegRSPIX[2] := @FRegs.IX;
  RegRSPIX[3] := @FRegs.SP;

  RegRSPIY[0] := @FRegs.BC;
  RegRSPIY[1] := @FRegs.DE;
  RegRSPIY[2] := @FRegs.IY;
  RegRSPIY[3] := @FRegs.SP;

  RegRAFBase[0] := @FRegs.BC;
  RegRAFBase[1] := @FRegs.DE;
  RegRAFBase[2] := @FRegs.HL;
  RegRAFBase[3] := @FRegs.AF;

  RegRAFIX[0] := @FRegs.BC;
  RegRAFIX[1] := @FRegs.DE;
  RegRAFIX[2] := @FRegs.IX;
  RegRAFIX[3] := @FRegs.AF;

  RegRAFIY[0] := @FRegs.BC;
  RegRAFIY[1] := @FRegs.DE;
  RegRAFIY[2] := @FRegs.IY;
  RegRAFIY[3] := @FRegs.AF;
end;

function TZ80State.PCInc: Word;
begin
  Result := PC;
  inc(FRegs.PC);
end;

function TZ80State.RInc: byte;
begin
  Result := R;
  R := ((R + 1) and $7f) or (R and $80); //R is only 7 bits so zero bit 7
end;

procedure TZ80State.SetFlag(Mask: Byte; Value: Boolean);
begin
  if Value then
    F := F or Mask
  else
    F := F and not Mask;
end;

procedure TZ80State.SetFlagC(const Value: Boolean);
begin
  SetFlag(CMask, Value);
end;

procedure TZ80State.SetFlagH(const Value: Boolean);
begin
  SetFlag(HMask, Value);
end;

procedure TZ80State.SetFlagN(const Value: Boolean);
begin
  SetFlag(NMask, Value);
end;

procedure TZ80State.SetFlagPV(const Value: Boolean);
begin
  SetFlag(PVMask, Value);
end;

procedure TZ80State.SetFlagS(const Value: Boolean);
begin
  SetFlag(SMask, Value);
end;

procedure TZ80State.SetFlagX(const Value: Boolean);
begin
  SetFlag(XMask, Value);
end;

procedure TZ80State.SetFlagY(const Value: Boolean);
begin
  SetFlag(YMask, Value);
end;

procedure TZ80State.SetFlagZ(const Value: Boolean);
begin
  SetFlag(ZMask, Value);
end;

procedure TZ80State.SetReg8(r: TReg8;Value: byte;IndexModifier: TIndexModifier);
begin
  case r of
  0: B := Value;
  1: C := Value;
  2: D := Value;
  3: E := Value;
  4:
    case IndexModifier of
    imMain: H := Value;
    imIX: IXH := Value;
    imIY: IYH := Value;
    end;
  5:
    case IndexModifier of
    imMain: L := Value;
    imIX: IXL := Value;
    imIY: IYL := Value;
    end;

  //6: = (HL) - Invalid here
  7: A := Value;
  else
    raise EZ80Exception.Create('Invalid 8 bit register encoding');
  end;
end;

procedure TZ80State.SetRegRAF(r: TReg16; Value: Word;
  IndexModifier: TIndexModifier);
begin
  case r of
    0: BC := Value;
    1: DE := Value;
    2:
    case IndexModifier of
      imMain: HL := Value;
      imIX: IX := Value;
      imIY: IY := Value;
    end;
    3: AF := Value;
  end;
end;

procedure TZ80State.SetRegRSP(r: TReg16; Value: Word;
  IndexModifier: TIndexModifier);
begin
  case r of
    0: BC := Value;
    1: DE := Value;
    2:
    case IndexModifier of
      imMain: HL := Value;
      imIX: IX := Value;
      imIY: IY := Value;
    end;
    3: SP := Value;
  end;
end;

function TZ80State.TestFlag(r: TReg8): Boolean;
begin
  case r shr 1 of
    0: Result := FlagZ;
    1: Result := FlagC;
    2: Result := FlagPV;
    3: Result := FlagS;
  else
    raise EZ80Exception.Create('Invalid flag encoding');
  end;
  if (r and 1) = 0 then
    Result := not Result;
end;

function TZ80State.ToString: String;
begin
  Result := 'A F  B C  D E  H L  IX   IY   PC   SP   IR    A''F'' B''C'' D''E'' H''L'''#13#10;
  Result := Result + IntToHex(AF,4);
  if EXAFAFState then
    Result := Result + '^'
  else
    Result := Result + ' ';
  Result := Result + IntToHex(BC,4);
  if EXXState then
    Result := Result + '^'
  else
    Result := Result + ' ';
  Result := Result + IntToHex(DE,4);
  if EXDEHLState then
    Result := Result + '~'
  else
    Result := Result + ' ';
  Result := Result + IntToHex(HL,4) + ' ';
  Result := Result + IntToHex(IX,4) + ' ';
  Result := Result + IntToHex(IY,4) + ' ';
  Result := Result + IntToHex(PC,4) + ' ';
  Result := Result + IntToHex(SP,4) + ' ';
  Result := Result + IntToHex(IR,4) + ' ';
  Result := Result + IntToHex(AFa,4) + ' ';
  Result := Result + IntToHex(BCa,4) + ' ';
  Result := Result + IntToHex(DEa,4) + ' ';
  Result := Result + IntToHex(HLa,4)+#13#10;

  if FlagS then
    Result := Result + 'S'
  else
    Result := Result + 's';
  if FlagZ then
    Result := Result + 'Z'
  else
    Result := Result + 'z';
  if FlagY then
    Result := Result + 'Y'
  else
    Result := Result + 'y';
  if FlagH then
    Result := Result + 'H'
  else
    Result := Result + 'h';
  if FlagX then
    Result := Result + 'X'
  else
    Result := Result + 'x';
  if FlagPV then
    Result := Result + 'P'
  else
    Result := Result + 'p';
  if FlagN then
    Result := Result + 'N'
  else
    Result := Result + 'n';
  if FlagC then
    Result := Result + 'C'
  else
    Result := Result + 'c';
end;

procedure TZ80State.WarmReset;
begin
  PC := 0;
end;

end.

