//Z80 Test suites: zexall/zexdoc/z80test

//Speed test (time to run 1 million instructions).
//With Application.ProcessMessages: 3.5s/0.7s
//Baseline: 0.25/0.11s.  Forced only LD B,B: 0.07s
//
//First time is after program load. Second is second million
unit mZ80;

interface
uses mZ80State, SysUtils{$ifndef fpc}, FMX.Dialogs{$endif};

type TDir = -1..1;

type TOpCodeProc = procedure(Opcode: Byte) of object;
  POpcodePage = ^TOpcodePage;
  TOpcodePage = array[0..255] of TOpCodeProc;

type
{$ifdef fpc}
  THookProc = procedure(Addr: Word);
{$else}
  THookProc = TProc<Word>;
{$endif}

type TZ80Executor = class
  private
    FZ80: TZ80State;

    //Opcode LUTs
    FPageBase: TOpcodePage;
    FPageXY: TOpcodePage;
    FPageED: TOpcodePage;
    FPageCB: TOpcodePage;

    FCurPage: POpcodePage;

    FCurR8: PReg8Pointers;
    FCurRSP: PReg16Pointers;
    FCurRAF: PReg16Pointers;

    FIndexModifier: TIndexModifier;

    FIndirectAddr: Word;
    FHaveIndirectAddr: Boolean;
    FHookM1: THookProc;

    procedure InitBaseLUT;
    procedure InitIXIYLUTs;
    procedure InitEDLUT;
    procedure InitCBLUT;
    procedure InitPageLUTs;

    //Do an M1/Refresh cycle
    procedure DoM1Refresh;
    //Clears any values specific to current opcode
    procedure EndOpcode;

    //Add an 8-bit offset to an 16-bit value (address)
    function AddRelative(Addr:word;Offset: byte): word;
    function CalcParity(Value: Byte): Boolean;

    //Returns either HL, IX+d or IY+d depending on the instruction set
    function IndirectAddr(IndexModifier: TIndexModifier): Word;
    //Sets FIndirectionAddr to HL and FHaveIndirectionAddr to True.
    //If UseInstructionSet then sets FIndirectionAddr to (IX+d) or (IY+d) as appropriate
    procedure SetIndirectAddr;
    //Gets the byte at PC and increments PC
    function GetImmediate8: Byte;
    //Get a word at PC and increments PC by 2
    function GetImmediate16: Word;
    //Get the value of an 8 bit register. Handles Indirect (HL) values and,
    //also (IX+d)/(IY+d) if UseInstructionSet
    //Stores indirect address for re-use by SetReg8 so we don't read offset twice.
    function GetReg8(r: TReg8;UseInstructionSet: Boolean): byte;
    //Set the value of an 8 bit register. Handles Indirect (HL) values and,
    //also (IX+d)/(IY+d) if UseInstructionSet.
    //Uses idirect address stored by GetReg8 if available to save reading offset twice.
    procedure SetReg8(r: TReg8;UseInstructionSet: Boolean;Value: Byte);
    //Get one of the registers BC, DE, HL/IX/IY, SP
//    function GetRegRSP(r: TReg16): Word;
    //Set one of the registers BC, DE, HL/IX/IY, SP
//    procedure SetRegRSP(r: TReg16;Value: Word);
    //Get one of the registers BC, DE, HL/IX/IY, AF
//    function GetRegRAF(r: TReg16): Word;
    //Set one of the registers BC, DE, HL/IX/IY, AF
//    procedure SetRegRAF(r: TReg16;Value: Word);

    //Calculate (as needed) and set flags
    //CalcMask is a mask of flags to be calculated
    //FixMask is a mask of flags to be given fixed values,
    //Fixed values are give in FixValues

    //The only flags which can be calculated are sign (Negative),
    //Zero and Parity (not oVerflow), plus undocumented YF and XF.
    //YF and XF values used are the respective bits of Value

    //oVerflow and other values will have to be passed in via FixMask and FixValues
    procedure SetFlags(Value, CalcMask, FixMask, FixValues: Byte);

    //Pop from stack and update SP
    function StackPop: Word;
    //Push onto stack an update SP
    procedure StackPush(Value: Word);

    //System opcodes
    procedure OpHALT(Opcode: Byte);
{}    procedure OpPUSH(Opcode: Byte);
    procedure OpNOP(Opcode: Byte);
{}    procedure OpPOP(Opcode: Byte);
    procedure OpDI(Opcode: Byte);
    procedure OpEI(Opcode: Byte);
    procedure OpCBOpset(Opcode: Byte);
    procedure OpCBXYOpset(Opcode: Byte);
    procedure OpIXOpset(Opcode: Byte);
    procedure OpEDOpset(Opcode: Byte);
    procedure OpIYOpset(Opcode: Byte);

    //8-bit LDs
{}    procedure OpLDrr(Opcode: Byte);
{}    procedure OpLDriHL(Opcode: Byte);
    procedure OpLDrixyd(Opcode: Byte);
{}    procedure OpLDiHLr(Opcode: Byte);
    procedure OpLDixydr(Opcode: Byte);

{}    procedure OpLDrn(Opcode: Byte);
    procedure OpLDiHLn(Opcode: Byte);
    procedure OpLDixydn(Opcode: Byte);

{.}    procedure OpLDirspA(Opcode: Byte);
{.}    procedure OpLDAirsp(Opcode: Byte);
{.}    procedure OpLDinnA(Opcode: Byte);
{.}    procedure OpLDAinn(Opcode: Byte);

    //16-bit LDs
{}    procedure OpLDrspnn(Opcode: Byte);
{}    procedure OpLDinnHL(Opcode: Byte);
{}    procedure OpLDHLinn(Opcode: Byte);
{}    procedure OpLDSPHL(Opcode: Byte);

    //Exchanges
    procedure OpEXAFAF(Opcode: Byte);
{}    procedure OpEXiSPHL(Opcode: Byte);
    procedure OpEXX(Opcode: Byte);
    procedure OpEXHLDE(Opcode: Byte);

    //8-bit ALU
    function OpADD8Do(Value1, Value2: Byte): Byte;
{}    procedure OpADDAr(Opcode: Byte);
    procedure OpADDAiHL(Opcode: Byte);
//    procedure OpADDAixyd(Opcode: Byte);
    procedure OpADDAn(Opcode: Byte);
    function OpADC8Do(Value1, Value2: Byte): Byte;
{}    procedure OpADCAr(Opcode: Byte);
    procedure OpADCAiHL(Opcode: Byte);
//    procedure OpADCAixyd(Opcode: Byte);
    procedure OpADCAn(Opcode: Byte);
    function OpSUB8Do(Value1, Value2: Byte): Byte;
{}    procedure OpSUBAr(Opcode: Byte);
    procedure OpSUBAiHL(Opcode: Byte);
//    procedure OpSUBAixyd(Opcode: Byte);
    procedure OpSUBAn(Opcode: Byte);
    function OpSBC8Do(Value1, Value2: Byte): Byte;
{}    procedure OpSBCAr(Opcode: Byte);
    procedure OpSBCAiHL(Opcode: Byte);
//    procedure OpSBCAixyd(Opcode: Byte);
    procedure OpSBCAn(Opcode: Byte);
    function OpAND8Do(Value1, Value2: Byte): Byte;
{}    procedure OpANDr(Opcode: Byte);
    procedure OpANDiHL(Opcode: Byte);
//    procedure OpANDixyd(Opcode: Byte);
    procedure OpANDn(Opcode: Byte);
    function OpXOR8Do(Value1, Value2: Byte): Byte;
{}    procedure OpXORr(Opcode: Byte);
    procedure OpXORiHL(Opcode: Byte);
//    procedure OpXORixyd(Opcode: Byte);
    procedure OpXORn(Opcode: Byte);
    function OpOR8Do(Value1, Value2: Byte): Byte;
{}    procedure OpORr(Opcode: Byte);
    procedure OpORiHL(Opcode: Byte);
//    procedure OpORixyd(Opcode: Byte);
    procedure OpORn(Opcode: Byte);
{}    procedure OpCPr(Opcode: Byte);
    procedure OpCPiHL(Opcode: Byte);
//    procedure OpCPixyd(Opcode: Byte);
    procedure OpCPn(Opcode: Byte);

    //16-bit ALU
{}    procedure OpADDHLrsp(Opcode: Byte);

    //INCs and DECs
    function OpINC8Do(Value: Byte): Byte;
{}    procedure OpINCr(Opcode: Byte);
    procedure OpINCiHL(Opcode: Byte);
    procedure OpINCixyd(Opcode: Byte);
    function OpDEC8Do(Value: Byte): Byte;
{}    procedure OpDECr(Opcode: Byte);
    procedure OpDECiHL(Opcode: Byte);
//    procedure OpDECixyd(Opcode: Byte);
{}    procedure OpINCrsp(Opcode: Byte);
{}    procedure OpDECrsp(Opcode: Byte);

    //Shifts and rotates
    procedure OpRLCA(Opcode: Byte);
    procedure OpRLA(Opcode: Byte);
    procedure OpRRCA(Opcode: Byte);
    procedure OpRRA(Opcode: Byte);

    //Other ALU
    procedure OpDAA(Opcode: Byte);
    procedure OpCPL(Opcode: Byte);
    procedure OpSCF(Opcode: Byte);
    procedure OpCCF(Opcode: Byte);

    //Flow of execution
    procedure OpDJNZ(Opcode: Byte);
    procedure OpJR(Opcode: Byte);
    procedure OpJRc(Opcode: Byte);
    procedure OpRETcc(Opcode: Byte);
    procedure OpRET(Opcode: Byte);
    procedure OpJPcc(Opcode: Byte);
    procedure OpJP(Opcode: Byte);
{}    procedure OpJPHL(Opcode: Byte);
    procedure OpCALLcc(Opcode: Byte);
    procedure OpCALLnn(Opcode: Byte);
    procedure OpRST(Opcode: Byte);


    //Input/Output
    procedure OpOUTnA(Opcode: Byte);
    procedure OpINAn(Opcode: Byte);

    //Old stuff

    function OpALU16(Operation: Byte;Operand1, Operand2: Word): Word;
    function OpALU8(Operation, Operand1, Operand2: byte): byte;
    //FlagMask is a mask of flag values to be calculated by SetFlags
    procedure OpShiftRotate(Operation: TReg8;Reg: TReg8;FlagMask: Byte);


    procedure OpINrC(r: TReg8);

    procedure OpRLD;
    procedure OpRRD;
    procedure OpBit(Bit, Reg: TReg8);
    procedure OpSETRES(DoSet: Boolean;Bit, Reg: TReg8);
    function OpLDID(Dir: TDir): Boolean;
    function OpCPID(Dir: TDir): Boolean;
    function OpINID(Dir: TDir): Boolean;
    function OpOUTID(Dir: TDir): Boolean;
    //LD A,I/LD A,R
    procedure OpLDAIR(Value: Byte);

    //No Q1 or Q3 opcodes for ED opcodes
    procedure ExecEDOpcode;
    procedure ExecEDQ1Opcode(Opcode: byte);
    procedure ExecEDQ2Opcode(Opcode: byte);
    procedure ExecCBOpcode;

//    procedure OpLDrspinn(Opcode: Byte);
  public
    constructor Create;
    destructor Destroy;override;

    procedure ExecOpcode;

    procedure SignalNMI;
    procedure SignalINT;
    property Z80: TZ80State read FZ80;

    //Hook when an M1/Refresh state occurs. Word = address bus (R register)
    //When triggered this happens /before the opcode is read from memory
    property HookM1: THookProc read FHookM1 write FHookM1;
  end;


implementation
uses mHardware;

const
  OpADD = 0;
  OpADC = 1;
  OpSUB = 2;
  OpSBC = 3;
  OpAND = 4;
  OpXOR = 5;
  OpOR = 6;
  OpCP = 7;

  OpINC = 8;
  OpDEC = 9;
  OpNEG = 10;

const
  BinToBit : array[TReg8] of Byte = ($01,$02,$04,$08,$10,$20,$40,$80);

  { TZ80Executor }

function TZ80Executor.AddRelative(Addr: word; Offset: byte): word;
var
  Offset16: word;
begin
  Offset16 := Offset;
  if Offset >= 128 then
    Offset16 := Offset16 or $ff00;
  Result := Addr + Offset16;
end;

function TZ80Executor.CalcParity(Value: Byte): Boolean;
begin
  Result := True;
  while Value <> 0 do
  begin
    if Value and 1 <> 0 then
      Result := not Result;
    Value := Value shr 1;
  end;
end;

constructor TZ80Executor.Create;
begin
  inherited Create;
  InitPageLUTs;
  FCurPage := @FPageBase;

  FZ80 := TZ80State.Create;
  FCurR8 := @Z80.Reg8Base;
  FCurRSP := @Z80.RegRSPBase;
  FCurRAF := @Z80.RegRAFBase;

  EndOpcode;
end;

destructor TZ80Executor.Destroy;
begin
  Z80.Free;
  inherited Destroy;
end;

procedure TZ80Executor.DoM1Refresh;
begin
  if Assigned(HookM1) then
    HookM1(Z80.R);
  Z80.RInc;
end;

procedure TZ80Executor.EndOpcode;
begin
  FIndexModifier := imMain;

  FHaveIndirectAddr := False;
end;

procedure TZ80Executor.ExecCBOpcode;
var
  Opcode: Byte;
  B543: TReg8;
  B210: TReg8;
begin
  //If we're have an IX/IY modifier then we need to read the offset before we
  //read the final opcoide byte.
  SetIndirectAddr;

  Opcode := Hardware.ReadMemoryByte(Z80.PCInc);
  B543 := (OpCode shr 3) and 7;
  B210 := OpCode and 7;
  case Opcode shr 6 of
  0: //Shift/Rotate
    OpShiftRotate(B543, B210, SMask or ZMask or PVMask);
  1: //BIT
    OpBIT(B543, B210);
  2: //RES
    OpSETRES(False, B543, B210);
  3: //SET
    OpSETRES(True, B543, B210);
  else
    raise EZ80Exception.Create('Invalid base opcode quarter');
  end;
end;

procedure TZ80Executor.ExecEDOpcode;
var Opcode: Byte;
begin
  DoM1Refresh; //M1
  Opcode := Hardware.ReadMemoryByte(Z80.PCInc);
  case Opcode shr 6 of
  1: ExecEDQ1opcode(Opcode);
  2: ExecEDQ2Opcode(Opcode);
  else
    //NOP
  end;
end;

procedure TZ80Executor.ExecEDQ1Opcode(Opcode: byte);
var
  B543: TReg8;
  B54: TReg16;
begin
  B54 := (Opcode shr 4) and 3;
  B543 := (Opcode shr 3) and 7;
  case Opcode and $f of
    0,8: //IN r,(C)
      OpINRC(B543);
    1,9: //OUT (C),r
      if B543 <> Reg8HL then
        Hardware.WriteIO(Z80.BC, GetReg8(B543, False))
      else
        Hardware.WriteIO(Z80.BC, 0);
    2: //SBC HL,rsp - No IX/IY
      Z80.HL := opALU16(opSBC, Z80.HL, Z80.GetRegRSP(B54, imMain));
    3: //LD (nn),rsp - No IX/IY
      Hardware.WriteMemoryWord(GetImmediate16, Z80.GetRegRSP(B54, imMain));
    4,$c: //NEG
      OpALU8(OpNEG, 0, Z80.A);
    5,$d: //RETN/RETI - Officially these differ. Unofficially they don't
      begin
        Z80.PC := StackPop;
        Z80.IFF1 := False;//Officially should be Z80.IFF1 := Z80.IFF2;
      end;
    6: //IM 0/IM 1
      Z80.IM := (OpCode shr 3) and 1;
    7: //LD I,A/LD A,I/RRD
      case OpCode and $30 of
        $00: Z80.I := Z80.A;
        $10: OpLDAIR(Z80.I);
        $20: OpRRD;
        else
          //NOP
      end;
    $a: //ADC HL,rsp - No IX/IY
      Z80.HL := opALU16(opADC, Z80.HL, Z80.GetRegRSP(B54, imMain));
    $b: //LD rsp,(nn) - No IX/IY
      Z80.SetRegRSP(B54, Hardware.ReadMemoryWord(GetImmediate16), imMain);
    $e: //IM 0/1???/IM 2
      Z80.IM := (OpCode shr 2) and 2;
    $f: //LD R,A/LD A,R/RLD
      case OpCode and $30 of
        $00: Z80.R := Z80.A;
        $10: OpLDAIR(Z80.R);
        $20: OpRLD;
        else
          //NOP
      end;
  end;
end;

procedure TZ80Executor.ExecEDQ2Opcode(Opcode: byte);
begin
  case Opcode of
    $A0: //LDI
      OpLDID(1);
    $A1: //CPI
      OpCPID(1);
    $A2: //INI
      OpINID(1);
    $A3: //OUTI
      OpOUTID(1);
    $A8: //LDD
      OpLDID(-1);
    $A9: //CPD
      OpCPID(-1);
    $AA: //IND
      OpINID(-1);
    $AB: //OUTD
      OpOUTID(-1);
    $B0: //LDIR
      if OpLDID(1) then
        Z80.PC := Z80.PC - 2;
    $B1: //CPIR
      if OpCPID(1) then
        Z80.PC := Z80.PC - 2;
    $B2: //INIR
      if OpINID(1) then
        Z80.PC := Z80.PC - 2;
    $B3: //OTIR
      if OpOUTID(1) then
        Z80.PC := Z80.PC - 2;
    $B8: //LDDR
      if OpLDID(-1) then
        Z80.PC := Z80.PC - 2;
    $B9: //CPDR
      if OpCPID(-1) then
        Z80.PC := Z80.PC - 2;
    $BA: //INDR
      if OpINID(-1) then
        Z80.PC := Z80.PC - 2;
    $BB: //OTDR
      if OpOUTID(-1) then
        Z80.PC := Z80.PC - 2;
  else
    //NOP
  end;

end;

procedure TZ80Executor.ExecOpcode;
var Opcode: Byte;
begin
  try
    DoM1Refresh;  //M1
  Opcode := Hardware.ReadMemoryByte(Z80.PCInc);
  if Assigned(FCurPage[Opcode]) then
    FCurPage[Opcode](Opcode)
  else
    FPageBase[Opcode](Opcode);

  EndOpcode;
  except
{$ifndef fpc}
    on E:Exception do
      ShowMessage('Exception: ' + E.Message + #13#10 + Z80.ToString);
{$endif}
  end;
end;

function TZ80Executor.GetImmediate16: Word;
begin
  Result := Hardware.ReadMemoryByte(Z80.PCInc) or (Hardware.ReadMemoryByte(Z80.PCInc) shl 8);
end;

function TZ80Executor.GetImmediate8: Byte;
begin
  Result := Hardware.ReadMemoryByte(Z80.PCInc);
end;

function TZ80Executor.GetReg8(r: TReg8; UseInstructionSet: Boolean): byte;
begin
  if r = Reg8HL then
  begin
    if not FHaveIndirectAddr then
      SetIndirectAddr;

    Result := Hardware.ReadMemoryByte(FIndirectAddr);
  end
  else if UseInstructionSet then
    Result := FCurR8[r]^
  else
    Result := Z80.Reg8Base[r]^
end;

{function TZ80Executor.GetRegRAF(r: TReg16): Word;
begin
  Result := Z80.GetRegRAF(r, FIndexModifier);
end;
}
{function TZ80Executor.GetRegRSP(r: TReg16): Word;
begin
  Result := Z80.GetRegRSP(r, FIndexModifier);
end;
}
function TZ80Executor.IndirectAddr(IndexModifier: TIndexModifier): Word;
begin
  case IndexModifier of
    imMain: Result := Z80.HL;
    imIX: Result := AddRelative(Z80.IX, Hardware.ReadMemoryByte(Z80.PCInc));
    imIY: Result := AddRelative(Z80.IY, Hardware.ReadMemoryByte(Z80.PCInc));
  else
    raise EZ80Exception.Create('Invalid modifier');
  end;
end;

procedure TZ80Executor.InitBaseLUT;
var I: Integer;
begin
  for I := 0 to 255 do
    FPageBase[I] := nil;

  //Quadrant 0

  //Column 0
  FPageBase[$00] := OpNOP;
  FPageBase[$10] := OpDJNZ;

  //Column 2
  FPageBase[$02] := OpLDirspa;  //LD (BC),A
  FPageBase[$12] := OpLDirspa;  //LD (DE),A
  FPageBase[$22] := OpLDinnHL;  //LD (nn),HL
  FPageBase[$32] := OpLDinnA;   //LD (nn),A

  //Column 7
  FPageBase[$07] := OpRLCA;
  FPageBase[$17] := OpRLA;
  FPageBase[$27] := OpDAA;
  FPageBase[$37] := OpSCF;

  //Column 8
  FPageBase[$08] := OpEXAFAF;
  FPageBase[$18] := OpJR;

  //Column A
  FPageBase[$0a] := OpLDAirsp;  //LD A,(BC)
  FPageBase[$1a] := OpLDAirsp;  //LD A,(DE)
  FPageBase[$2a] := OpLDHLinn;  //LD HL,(nn)
  FPageBase[$3a] := OpLDAinn;   //LD A,(nn)

  //Column f
  FPageBase[$0f] := OpRRCA;
  FPageBase[$1f] := OpRRA;
  FPageBase[$2f] := OpCPL;
  FPageBase[$3f] := OpCCF;

  //Other columns
  for I := 0 to $3f do
    case I and $0f of
      $00,$08:
        if I and $20 <> 0 then
          FPageBase[I] := OpJRc;
      $01: FPageBase[I] := OpLDrspnn;
      $03: FPageBase[I] := OpINCrsp;
      $04, $0c:
        if (I shr 3) and $07 = $06 then
          FPageBase[I] := OpINCiHL
        else
          FPageBase[I] := OpINCr;
      $05, $0d:
        if (I shr 3) and $07 = $06 then
          FPageBase[I] := OpDECiHL
        else
          FPageBase[I] := OpDECr;
      $06, $0e:
        if (I shr 3) and $07 = $06 then
          FPageBase[I] := OpLDiHLn
        else
          FPageBase[I] := OpLDrn;
      $09: FPageBase[I] := OpADDHLrsp;
      $0b: FPageBAse[I] := OpDECrsp;
    end;

  //Quadrant 1
  //LD r,r/LR r,(HL)/LD(HL),r/HALT
  for I := $40 to $7f do
    if (I and $07 = $06) then
      if ((I shr 3) and $07 = $06) then
        FPageBase[I] := OpHALT
      else  //LD r,(HL)
        FPageBase[I] := OpLDriHL
    else if ((I shr 3) and $07 = $06) then
      //LD (HL),r
      FPageBase[I] := OpLDiHLr
    else  //LD r,r
      FPageBase[I] := OpLDrr;

  //Quadrant 2
  for I := $80 to $87 do
    FPageBase[I] := OpADDAr;
  FPageBase[$86] := OpADDAiHL;
  for I := $88 to $8f do
    FPageBase[I] := OpADCAr;
  FPageBase[$8e] := OpADCAiHL;
  for I := $90 to $97 do
    FPageBase[I] := OpSUBAr;
  FPageBase[$96] := OpSUBAiHL;
  for I := $98 to $9f do
    FPageBase[I] := OpSBCAr;
  FPageBase[$9e] := OpSBCAiHL;
  for I := $a0 to $a7 do
    FPageBase[I] := OpANDr;
  FPageBase[$a6] := OpANDiHL;
  for I := $a8 to $af do
    FPageBase[I] := OpXORr;
  FPageBase[$ae] := OpXORiHL;
  for I := $b0 to $b7 do
    FPageBase[I] := OpORr;
  FPageBase[$b6] := OpORiHL;
  for I := $b8 to $bf do
    FPageBase[I] := OpCPr;
  FPageBase[$be] := OpCPiHL;

  //Quadrant 3

  //Column 3
  FPageBase[$c3] := OpJP;
  FPageBase[$d3] := OpOUTnA;
  FPageBase[$e3] := OpEXiSPHL;
  FPageBase[$f3] := OpDI;

  //Columns 6 and e
  FPageBase[$c6] := OpADDAn;
  FPageBase[$ce] := OpADCAn;
  FPageBase[$d6] := OpSUBAn;
  FPageBase[$de] := OpSBCAn;
  FPageBase[$e6] := OpANDn;
  FPageBase[$ee] := OpXORn;
  FPageBase[$f6] := OpORn;
  FPageBase[$fe] := OpCPn;

  //Column 9
  FPageBase[$c9] := OpRET;
  FPageBase[$d9] := OpEXX;
  FPageBase[$e9] := OpJPHL;
  FPageBase[$f9] := OpLDSPHL;

  //Column b
  FPageBase[$cb] := OpCBOpset;
  FPageBase[$db] := OpINAn;
  FPageBase[$eb] := OpEXHLDE;
  FPageBase[$fb] := OpEI;

  //Column d
  FPageBase[$cd] := OpCALLnn;
  FPageBase[$dd] := OpIXOpset;
  FPageBase[$ed] := OpEDOpset;
  FPageBase[$fd] := OpIYOpset;

  for I := $c0 to $ff do
    case I and $0f of
      $00, $08: FPageBase[I] := OpRETcc;
      $01:      FPAgeBase[I] := OpPOP;
      $02, $0a: FPageBase[I] := OpJPcc;
      $04, $0c: FPageBase[I] := OpCALLcc;
      $05:      FPageBase[I] := OpPUSH;
      $07, $0f: FPageBase[I] := OpRST;
    end;
end;

procedure TZ80Executor.InitCBLUT;
var I: Integer;
begin
  for I := 0 to 255 do
    FPageCB[I] := nil;
end;

procedure TZ80Executor.InitEDLUT;
var I: Integer;
begin
  for I := 0 to 255 do
    FPageED[I] := nil;
end;

procedure TZ80Executor.InitIXIYLUTs;
var I: Integer;
begin
  //Note: Where the instruction simply substitutes HL with IX or IY the base
  //instruction will do the substitution due to the opcode pointer tables
  //having been updated. The only instructions we need to explicitly add/create
  //are where functionality differs from that of the base instruction, for example
  //where an (xy+d) replaces a (HL). This also includes the LD r,r' instructions
  //where, for example, LD H,(HL) becomes LD H,(xy+d) instead of LD IXH,(xy+d)

  //Quadrant 0
  FPageXY[$34] := OpINCixyd;
  FPageXY[$35] := OpINCixyd;
  FPageXY[$36] := OpLDixydn;

  //Quadrant 1
  for I := $40 to $7f do
    if I and $07 = $06 then
      if I shr 3 and $07 = $06 then
        //Ignore (HALT)
      else
        FPageXY[I] := OpLDrixyd
    else if I shr 3 and $07 = $06 then
      FPageXY[I] := OpLDixydr;

  //Quadrant 2
  FPageXY[$86] := OpADDAiHL;
  FPageXY[$8e] := OpADCAiHL;
  FPageXY[$96] := OpSUBAiHL;
  FPageXY[$9e] := OpSBCAiHL;
  FPageXY[$a6] := OpANDiHL;
  FPageXY[$ae] := OpXORiHL;
  FPageXY[$b6] := OpORiHL;
  FPageXY[$be] := OpCPiHL;

  //Quadrant 3
  FPageXY[$cb] := OpCBXYOpset;

{  for I := 0 to 255 do
  begin
    if not Assigned(FPageXY[I]) then
      FPageXY[I] := FPageBase[I];
  end;
}end;

procedure TZ80Executor.InitPageLUTs;
begin
  InitBaseLUT;
  InitIXIYLUTs;
  InitEDLUT;
  InitCBLUT;
end;

function TZ80Executor.OpADC8Do(Value1, Value2: Byte): Byte;
var
  Carry: Byte;
  ResultInt: Integer;
  Flags: Byte;
begin
  Carry := Z80.F and CMask;
  ResultInt := Value1 + Value2 + Carry;
  Result := ResultInt and $ff;

  //Flags
  Flags := Result and SMask;
  if Result = 0 then
    Flags := Flags or ZMask;
  if (Value1 and $07) + (Value2 and $07) + Carry > $07 then
    Flags := Flags or HMask;

  //PV
  if ((Value1 and $80) = (Value2 and $80)) and ((Value2 and $80) <> (Result and $80)) then
    Flags := Flags or PVMask;

  if ResultInt > $ff then
    Flags := Flags or CMask;

  //Undocumented
  Flags := Flags or (Result and (YMask or XMask));

  Z80.F := Flags;
end;

procedure TZ80Executor.OpADCAiHL(Opcode: Byte);
begin
  Z80.A := OpADC8Do(Z80.A, Hardware.ReadMemoryByte(Z80.HL));
end;

{procedure TZ80Executor.OpADCAixyd(Opcode: Byte);
var Addr: Word;
begin
  Addr := AddRelative(FCurRSP[2]^, GetImmediate8);
  Z80.A := OpADC8Do(Z80.A, Hardware.ReadMemoryByte(Addr));
end;
}
procedure TZ80Executor.OpADCAn(Opcode: Byte);
begin
  Z80.A := OpADC8Do(Z80.A, GetImmediate8);
end;

procedure TZ80Executor.OpADCAr(Opcode: Byte);
begin
  Z80.A := OpADC8Do(Z80.A, FCurR8[Opcode and $07]^);
end;

function TZ80Executor.OpADD8Do(Value1, Value2: Byte): Byte;
var
  ResultInt: Integer;
  Flags: Byte;
begin
  ResultInt := Value1 + Value2;
  Result := ResultInt and $ff;

  //Flags
  Flags := Result and SMask;
  if Result = 0 then
    Flags := Flags or ZMask;
  if (Value1 and $07) + (Value2 and $07) > $07 then
    Flags := Flags or HMask;

  //PV
  if ((Value1 and $80) = (Value2 and $80)) and ((Value2 and $80) <> (Result and $80)) then
    Flags := Flags or PVMask;

  if ResultInt > $ff then
    Flags := Flags or CMask;

  //Undocumented
  Flags := Flags or (Result and (YMask or XMask));

  Z80.F := Flags;
end;

procedure TZ80Executor.OpADDAiHL(Opcode: Byte);
begin
  Z80.A := OpADD8Do(Z80.A, Hardware.ReadMemoryByte(Z80.HL));
end;

{procedure TZ80Executor.OpADDAixyd(Opcode: Byte);
var Addr: Word;
begin
  Addr := AddRelative(FCurRSP[2]^, GetImmediate8);
  Z80.A := OpADD8Do(Z80.A, Hardware.ReadMemoryByte(Addr));
end;
}
procedure TZ80Executor.OpADDAn(Opcode: Byte);
begin
  Z80.A := OpADD8Do(Z80.A, GetImmediate8);
end;

procedure TZ80Executor.OpADDAr(Opcode: Byte);
begin
  Z80.A := OpADD8Do(Z80.A, FCurR8[Opcode and $07]^);
end;

procedure TZ80Executor.OpADDHLrsp(Opcode: Byte);
const Mask = not (YMask + HMask + XMask + NMask + CMask);
var
  Operand1: Word;
  Operand2: Word;
  ResultInt: Longint;
  Flags: Byte;
begin
  Operand1 := FCurRSP[2]^;
  Operand2 := FCurRSP[(Opcode shr 4) and $03]^;
  ResultInt := Operand1 + Operand2;
  FCurRSP[2]^ := ResultInt and $ffff;

  //Flags
  Flags := Z80.F and Mask;

  //Carry
  if ResultInt >= $10000 then
    Flags := Flags or CMask;

  //Half Carry - from bit 11 to bit 12
  if (Operand1 and $fff) + (Operand2 and $fff) > $fff then
    Flags := Flags or HMask;

  //Undocumented
  Flags := Flags or ((ResultInt shr 8) and (YMask or XMask));

  Z80.F := Flags;
end;

function TZ80Executor.OpALU16(Operation: Byte;Operand1, Operand2: Word): Word;
var
  ResultInt: Longint;
  Carry: Integer;
  Flags: Byte;
begin
  if Z80.FlagC then
    Carry := 1
  else
    Carry := 0;

  case Operation of
    opADD: ResultInt := Operand1 + Operand2;
    opADC: ResultInt := Operand1 + Operand2 + Carry;
    opSBC: ResultInt := Operand1 - Operand2 - Carry;
  else
    raise EZ80Exception.Create('Invalid operation');
  end;

  Result := ResultInt and $ffff;

  //Flags
  Flags := 0;

  //Carry
  if (ResultInt and $10000) <> 0 then
    Flags := Flags or CMask;

  //Sign
  if Operation = OpADD then
  begin
    //Half Carry - from bit 11 to bit 12
    if (Operand1 and $fff) + (Operand2 and $fff) + Carry > $fff then
      Flags := Flags or HMask;

    SetFlags(Result shr 8,YMask or XMask,HMask or NMask or CMask, Flags);
  end
  else
  begin
    if (ResultInt and $8000) <> 0 then
      Flags := Flags or SMask;

    //Zero
    if Result = 0 then
      Flags := Flags or ZMask;

    //Undocumented

    //Half carry - from bit 11 to bit 12
    case Operation of
      opADC:     if (Operand1 and $fff) + (Operand2 and $fff) + Carry > $fff then
        Flags := Flags or HMask;
//      opSBC:     if (Operand1 and $fff) + ((not Operand2) and $fff) + Carry > $fff then
      opSBC:     if ((Operand1 and $fff) - (Operand2 and $fff) - Carry) and $1000 <> 0 then
        Flags := Flags or HMask;
    end;

    //Undocumented

    //Overflow - Signs of inputs are the same as each other and different to the result
    case Operation of
      opADC:
        if ((Operand1 and $8000) = (Operand2 and $8000)) and
          ((Operand1 and $8000) <> (Result and $8000)) then
            Flags := Flags or PVMask;
      opSBC:
        if ((Operand1 and $8000) <> (Operand2 and $8000)) and
          ((Operand2 and $8000) = (Result and $8000)) then
            Flags := Flags or PVMask;
    end;

    //Subtract
    if Operation = opSBC then
      Flags := Flags or NMask;

    SetFlags(Result shr 8, YMask or XMask, SMask or ZMask or HMask or PVMask or NMask or CMask, Flags)
  end;
end;

function TZ80Executor.OpALU8(Operation, Operand1, Operand2: byte): byte;
var
  Op1Int: Integer;
  Op2Int: Integer;
  ResultInt: Integer;
  CarryInt: Integer;
  CalcMask: Byte;
  FixMask: Byte;
  FixValues: Byte;
  //Used for half carry
  Op1H: Integer;
  Op2H: Integer;
  ResultH: Integer;
begin
  Op1Int := Operand1;
  Op2Int := Operand2;
  if Z80.FlagC then
    CarryInt := 1
  else
    CarryInt := 0;

  //Operation itself
  case Operation of
  OpADD: //ADD
    ResultInt := Operand1 + Operand2;
  OpADC: //ADC
    ResultInt := Operand1 + Operand2 + CarryInt;
  OpSUB, OpCP, OpNEG: //SUB, CP
    ResultInt := Op1Int - Op2Int;
  OpSBC: //SBC
    ResultInt := Op1Int - Op2Int - CarryInt;
  OpAND: //AND
    ResultInt := Operand1 and Operand2;
  OpXOR: //XOR
    ResultInt := Operand1 xor Operand2;
  OpOR: //OR
    ResultInt := Operand1 or Operand2;
  OpINC:
    ResultInt := Operand1 + 1;
  OpDEC:
    ResultInt := Operand1 - 1;
  else
    raise EZ80Exception.Create('Invalid operation');
  end;

  Result := ResultInt and $ff;
  if Operation in [OpADD, OpADC, OpSUB, OpSBC, OpAND, OpXOR, OpOR, OpNEG] then
    Z80.A := Result;


  //Flags
  CalcMask := SMask or ZMask;
  FixMask := HMask or PVMask or NMask or CMask;
  FixValues := 0;

  //TK - Undocumented flag

  //Half carry
  Op1H := Operand1 and $f;
  Op2H := Operand2 and $f;
  ResultH := 0;

  //Half Carry
  case Operation of
  OpADD, OpINC: ResultH := Op1H + Op2H;
  OpADC:  ResultH := Op1H + Op2H + CarryInt;
  OpSUB, OpCP, OpDEC, OpNEG: ResultH := (Op1H - Op2H) and $ff;
  OpSBC: ResultH := (Op1H - Op2H - CarryInt) and $ff;
  OpAND: FixValues := FixValues or HMask;
  OpXOR, OpOR:; //H = 0
  end;
  if ResultH > $f then
    FixValues := FixValues or HMask;

  //TK - Undocumented flag

  //Parity/Overflow
  case Operation of
  OpADD, OpADC, OpINC:
  begin
    FixMask := FixMask or PVMask;
    //Overflow: Signs of both inputs are the same, sign of the output is different
    if ((Operand1 and $80) = (Operand2 and $80)) and ((Operand1 and $80) <> (Result and $80)) then
      FixValues := FixValues or PVMask;
  end;
  OpSUB, OpSBC, OpCP, OpDEC, OpNEG:
  begin
    FixMask := FixMask or PVMask;
    //Overflow: Signs of both inputs are different and sign of the output differs from that of the subtrahend
    if ((Operand1 and $80) <> (Operand2 and $80)) and ((Operand2 and $80) = (Result and $80)) then
      FixValues := FixValues or PVMask;
  end;
  OpAND, OpXOR, OpOR: CalcMask := CalcMask or PVMask;
  end;

  //Subtraction
  case Operation of
  OpADD, OpADC, OpINC:; //FlagN := False;
  OpSUB, OpSBC, OpCP, OpDEC, OpNEG: FixValues := FixValues or NMask;
  OpAND, OpXOR, OpOR:; //FlagN := False;
  end;

  //Carry
  case Operation of
  OpADD, OpADC, OpSUB, OpSBC, OpCP, OpNEG:
    if ResultInt and $100 <> 0 then
      FixValues := FixValues or CMask;
  OpAND, OpXOR, OpOR:; //C = 0
  OpINC, OpDEC: FixMask := FixMask and not CMask;  //Unchanged
  end;

  case Operation of
  OpCP:
  begin
    //Undocumented flags: CP uses values from second operand instead of those from result.
    FixMask := FixMask or YMask or XMask;
    FixValues := FixValues or (Operand2 and (YMask or XMask));
    SetFlags(Result, CalcMask, FixMask, FixValues);
  end;
  else
    SetFlags(Result, CalcMask or YMask or XMask, FixMask, FixValues);
  end;
end;

function TZ80Executor.OpAND8Do(Value1, Value2: Byte): Byte;
var
  Flags: Byte;
begin
  Result := Value1 and Value2;

  //Flags
  Flags := HMask or (Result and SMask);
  if Result = 0 then
    Flags := Flags or ZMask;

  //PV
  if CalcParity(Result) then
    Flags := Flags or PVMask;

  //Undocumented
  Flags := Flags or (Result and (YMask or XMask));

  Z80.F := Flags;
end;

procedure TZ80Executor.OpANDiHL(Opcode: Byte);
begin
  Z80.A := OpAND8Do(Z80.A, Hardware.ReadMemoryByte(Z80.HL));
end;

{procedure TZ80Executor.OpANDixyd(Opcode: Byte);
var Addr: Word;
begin
  Addr := AddRelative(FCurRSP[2]^, GetImmediate8);
  Z80.A := OpAND8Do(Z80.A, Hardware.ReadMemoryByte(Addr));
end;
}
procedure TZ80Executor.OpANDn(Opcode: Byte);
begin
  Z80.A := OpAND8Do(Z80.A, GetImmediate8);
end;

procedure TZ80Executor.OpANDr(Opcode: Byte);
begin
  Z80.A := OpAND8Do(Z80.A, FCurR8[Opcode and $07]^);
end;

procedure TZ80Executor.OpBit(Bit, Reg: TReg8);
var
  UseReg: TReg8;
  RegValue: Byte;
  Value: Byte;
  Flags: Byte;
begin
  //If IX/IY then test (IX/IY+d) - no version for IXH/IYL/IYH/IYL
  if FIndexModifier <> imMain then
    UseReg := Reg8HL
  else
    UseReg := Reg;

  Flags := HMask;

  RegValue := GetReg8(UseReg, True);
  Value := RegValue and BinToBit[Bit];
  if Value = 0 then
    Flags := Flags or ZMask or PVMask;

  if RegValue and YMask <> 0 then
    Flags := Flags or YMask;
  if RegValue and XMask <> 0 then
    Flags := Flags or XMask;

  SetFlags(Value, SMask, ZMask or YMask or HMask or XMask or PVMask or NMask, Flags);
end;

procedure TZ80Executor.OpCALLnn(Opcode: Byte);
var Addr: Word;
begin
  Addr := GetImmediate16;
  StackPush(Z80.PC);
  Z80.PC := Addr;
end;

procedure TZ80Executor.OpCBOpset(Opcode: Byte);
begin
  //TODO - Temp
  ExecCBOpcode;
end;

procedure TZ80Executor.OpCBXYOpset(Opcode: Byte);
begin
  //TODO - Temp
  ExecCBOpcode;
end;

procedure TZ80Executor.OpCALLcc(Opcode: Byte);
var FlagMask: Byte;
  Match: Boolean;
  Addr: Word;
begin
  Addr := GetImmediate16;
  FlagMask := FlagMasks[Opcode shr 4 and 3];
  Match := (Opcode shr 3 and 1) <> 0;
  if ((Z80.F and FlagMask) <> 0) = Match then
  begin
    StackPush(Z80.PC);
    Z80.PC := Addr;
  end;
end;

procedure TZ80Executor.OpCCF(Opcode: Byte);
const Mask = not (YMask + XMask + HMask + NMask);
var
  Flags: Byte;
begin
  Flags := Z80.F and Mask;  //Clear H and N
  Flags := Flags xor CMask; //Carry
  Flags := Flags or (Z80.A and (YMask or XMask)); //Y and X
  Z80.F := Flags;
end;

procedure TZ80Executor.OpCPiHL(Opcode: Byte);
begin
  OpSUB8Do(Z80.A, Hardware.ReadMemoryByte(Z80.HL));
end;

{procedure TZ80Executor.OpCPixyd(Opcode: Byte);
var Addr: Word;
begin
  Addr := AddRelative(FCurRSP[2]^, GetImmediate8);
  OpSUB8Do(Z80.A, Hardware.ReadMemoryByte(Addr));
end;
}
procedure TZ80Executor.OpCPn(Opcode: Byte);
begin
  OpSUB8Do(Z80.A, GetImmediate8);
end;

procedure TZ80Executor.OpCPr(Opcode: Byte);
begin
  OpSUB8Do(Z80.A, FCurR8[Opcode and $07]^);
end;

function TZ80Executor.OpCPID(Dir: TDir): Boolean;
var
  Comp: Byte;
  Flags: Byte;
begin
  if Dir = 0 then
    raise EZ80Exception.Create('Invalid Dir (0)');

  Comp := Z80.A - Hardware.ReadMemoryByte(Z80.HL);
  Z80.HL := Z80.HL + Dir;
  Z80.BC := Z80.BC - 1;

  Result := (Z80.BC <> 0) and (Comp <> 0);

  Flags := NMask;

  //Half carry
//if  Flags := Flags or HMask;

  if Z80.BC <> 0 then
    Flags := Flags or PVMask;

  SetFlags(Comp, SMask or ZMask or YMask or XMask, HMask or PVMask or NMask, Flags);
end;

procedure TZ80Executor.OpCPL(Opcode: Byte);
var Flags: Byte;
begin
  Z80.A := not Z80.A;
  Flags := Z80.F and not (YMask or XMask);
  Flags := Flags or HMask or NMask;
  Flags := Flags or (Z80.A and (YMAsk or XMask));
  Z80.F := Flags;
end;

procedure TZ80Executor.OpDAA(Opcode: Byte);
var
  t: Integer;
  Offset: Byte;
  Flags: Byte;
begin
  //Translated from code at https://stackoverflow.com/questions/8119577/z80-daa-instruction/8119836
  t := 0;
  Flags := 0;

  if Z80.FlagH or ((Z80.A and $0f) > 9) then
    inc(t);

  if Z80.FlagC or (Z80.A > $99) then
  begin
    inc(t, 2);
    Flags := Flags or CMask;
  end;


  if Z80.FlagN and not Z80.FlagH then
    Z80.FlagH := False
  else
  begin
    if Z80.FlagN and Z80.FlagH then
      Z80.FlagH := (Z80.A and $0f) < 6
    else
      Z80.FlagH := (Z80.A and $0f) >= $0a;
  end;

  case t of
  1: Offset := $06;
  2: Offset := $60;
  3: Offset := $66;
  else
    Offset := 0;
  end;

  if Z80.FlagN then
    Z80.A := Z80.A - Offset
  else
    Z80.A := Z80.A + Offset;

  SetFlags(Z80.A, SMask or ZMask or YMask or XMask or PVMask, CMask, Flags);
end;

function TZ80Executor.OpDEC8Do(Value: Byte): Byte;
const FlagMask = not (SMask + ZMask + YMask + HMask + XMask + PVMask);
var Flags: Byte;
begin
  Result := (Value - 1) and $ff;
  //Flags
  Flags := (Z80.F and FlagMask) or NMask; //Sign
  Flags := Flags or (Result and $80);
  if Result = 0 then
    Flags := Flags or ZMask;
  if (Value and $0f) = $00 then
    Flags := Flags or HMask;
//  if ((Operand1 and $80) <> (Operand2 and $80)) and ((Operand2 and $80) = (Result and $80)) then

  if ((Value and $80) {<> (Operand2 and $80)}) and ({(Operand2 and $80) xor }$80 xor (Result and $80)) <> 0 then
//  if (Value and $80) <> (Result and $80) then
    Flags := Flags or PVMask;
  Flags := Flags or (Result and (YMask or XMask));
  Z80.F := Flags;
end;

procedure TZ80Executor.OpDECiHL(Opcode: Byte);
begin
  Hardware.WriteMemoryByte(Z80.HL, OpDEC8Do(Hardware.ReadMemoryByte(Z80.HL)));
end;

{procedure TZ80Executor.OpDECixyd(Opcode: Byte);
var Addr: Word;
begin
  Addr := AddRelative(FCurRSP[2]^, GetImmediate8);
  Hardware.WriteMemoryByte(Addr, OpDEC8Do(Hardware.ReadMemoryByte(Addr)));
end;
}
procedure TZ80Executor.OpDECr(Opcode: Byte);
var Reg: PByte;
begin
  Reg := FCurR8[Opcode shr 3 and $07];
  Reg^ := OpDEC8Do(Reg^);
end;

procedure TZ80Executor.OpDECrsp(Opcode: Byte);
var Reg: PWord;
begin
  Reg := FCurRSP[(Opcode shr 4) and $03];
  Reg^ := (Reg^ - 1) and $ffff;
end;

procedure TZ80Executor.OpDI(Opcode: Byte);
begin
  Z80.IFF1 := False;
  Z80.IFF2 := False;
end;

procedure TZ80Executor.OpDJNZ(Opcode: Byte);
begin
  Z80.B := (Z80.B - 1) and $ff;
  if Z80.B <> 0 then
    Z80.PC := AddRelative(Z80.PC, GetImmediate8)
  else //Step over offset
    GetImmediate8;
end;

procedure TZ80Executor.OpEDOpset(Opcode: Byte);
begin
  ExecEDOpcode;
end;

procedure TZ80Executor.OpEI(Opcode: Byte);
begin
  Z80.IFF1 := True;
  Z80.IFF2 := True;
end;

procedure TZ80Executor.OpEXAFAF(Opcode: Byte);
begin
  Z80.EXAFAF;
end;

procedure TZ80Executor.OpEXHLDE(Opcode: Byte);
begin
  Z80.EXHLDE;
end;

procedure TZ80Executor.OpEXiSPHL(Opcode: Byte);
var Temp: Word;
  Reg: PWord;
begin
  Temp := StackPop;
  Reg := FCurRAF[2];
  StackPush(Reg^);
  Reg^ := Temp;
end;

procedure TZ80Executor.OpEXX(Opcode: Byte);
begin
  Z80.EXX;
end;

procedure TZ80Executor.OpHALT(Opcode: Byte);
begin
  Z80.PC := Z80.PC-1
end;

procedure TZ80Executor.OpINAn(Opcode: Byte);
begin
  Z80.A := Hardware.ReadIO((Z80.A shl 8) or GetImmediate8);
end;

function TZ80Executor.OpINC8Do(Value: Byte): Byte;
const FlagMask = not (SMask + ZMask + YMask + HMask + XMask + PVMask + NMask);
var Flags: Byte;
begin
  Result := (Value + 1) and $ff;
  //Flags
  Flags := Z80.F and FlagMask;
  Flags := Flags or (Result and $80);  //Sign
  if Result = 0 then
    Flags := Flags or ZMask;
  if (Value and $0f) = $0f then
    Flags := Flags or HMask;
  if ((Value and $80) xor $80{= (Operand2 and $80)}) and ((Value and $80) xor (Result and $80)) <> 0 then
    Flags := Flags or PVMask;
  Flags := Flags or (Result and (YMask or XMask));
  Z80.F := Flags;
end;

procedure TZ80Executor.OpINCiHL(Opcode: Byte);
begin
  Hardware.WriteMemoryByte(Z80.HL, OpINC8Do(Hardware.ReadMemoryByte(Z80.HL)));
end;

procedure TZ80Executor.OpINCixyd(Opcode: Byte);
var Addr: Word;
begin
  Addr := AddRelative(FCurRSP[2]^, GetImmediate8);
  Hardware.WriteMemoryByte(Addr, OpINC8Do(Hardware.ReadMemoryByte(Addr)));
end;

procedure TZ80Executor.OpINCr(Opcode: Byte);
var Reg: PByte;
begin
  Reg := FCurR8[Opcode shr 3 and $07];
  Reg^ := OpINC8Do(Reg^);
end;

procedure TZ80Executor.OpINCrsp(Opcode: Byte);
var Reg: PWord;
begin
  Reg := FCurRSP[(Opcode shr 4) and $03];
  Reg^ := (Reg^ + 1) and $ffff;
end;

function TZ80Executor.OpINID(Dir: TDir): Boolean;
begin
  if Dir = 0 then
    raise EZ80Exception.Create('Invalid Dir (0)');

  Hardware.WriteMemoryByte(Z80.HL, Hardware.ReadIO(Z80.BC));
  Z80.HL := Z80.HL + Dir;
  Z80.B := Z80.B - 1;

  Result := Z80.B <> 0;

  SetFlags(Z80.B, ZMask or YMask or XMask, SMask or HMask or PVMask or NMask, NMask);
end;

procedure TZ80Executor.OpINrC(r: TReg8);
var
  Value: Byte;
begin
  Value := Hardware.ReadIO(Z80.BC);
  if r <> Reg8HL then
    SetReg8(r, False, Value);
  SetFlags(Value, SMAsk or ZMask or PVMask or YMask or XMask, HMask or NMask, 0);
end;

procedure TZ80Executor.OpIXOpset(Opcode: Byte);
begin
  try
  FCurR8 := @Z80.Reg8IX;
  FCurRSP := @Z80.RegRSPIX;
  FCurRAF := @Z80.RegRAFIX;

  FCurPage := @FPageXY;
  FIndexModifier := imIX;
  ExecOpCode;
  FCurPage := @FPageBase;

  finally
  FCurR8 := @Z80.Reg8Base;
  FCurRSP := @Z80.RegRSPBase;
  FCurRAF := @Z80.RegRAFBase;
  end;
end;

procedure TZ80Executor.OpIYOpset(Opcode: Byte);
begin
  try
  FCurR8 := @Z80.Reg8IY;
  FCurRSP := @Z80.RegRSPIY;
  FCurRAF := @Z80.RegRAFIY;

  FCurPage := @FPageXY;
  FIndexModifier := imIY;
  ExecOpCode;
  FCurPage := @FPageBase;

  finally
  FCurR8 := @Z80.Reg8Base;
  FCurRSP := @Z80.RegRSPBase;
  FCurRAF := @Z80.RegRAFBase;
  end;
end;

procedure TZ80Executor.OpJP(Opcode: Byte);
begin
    Z80.PC := GetImmediate16;
end;

procedure TZ80Executor.OpJPcc(Opcode: Byte);
var FlagMask: Byte;
  Match: Boolean;
begin
  FlagMask := FlagMasks[Opcode shr 4 and 3];
  Match := (Opcode shr 3 and 1) <> 0;
  if ((Z80.F and FlagMask) <> 0) = Match then
    Z80.PC := GetImmediate16
  else //Step over offset
    GetImmediate16;
end;

procedure TZ80Executor.OpJPHL(Opcode: Byte);
begin
  Z80.PC := FCurRAF[2]^;
end;

procedure TZ80Executor.OpJR(Opcode: Byte);
begin
  Z80.PC := AddRelative(Z80.PC, GetImmediate8);
end;

procedure TZ80Executor.OpJRc(Opcode: Byte);
var FlagMask: Byte;
  Match: Boolean;
begin
  FlagMask := FlagMasks[Opcode shr 4 and 1];
  Match := (Opcode shr 3 and 1) <> 0;
  if ((Z80.F and FlagMask) <> 0) = Match then
    Z80.PC := AddRelative(Z80.PC, GetImmediate8)
  else //Step over offset
    GetImmediate8;
end;

procedure TZ80Executor.OpLDAinn(Opcode: Byte);
begin
  Z80.A := HArdware.ReadMemoryByte(GetImmediate16);
end;

procedure TZ80Executor.OpLDAIR(Value: Byte);
var
  Flags: Byte;
begin
  Z80.A := Value;

  Flags := 0;
  if Z80.IFF2 then
    Flags := Flags or PVMask;

  SetFlags(Value, SMask or ZMask or YMask or XMask, HMask or PVMask or NMask, Flags);
end;

procedure TZ80Executor.OpLDAirsp(Opcode: Byte);
begin //Note: Only applies to BC and DE
  Z80.A := Hardware.ReadMemoryByte(Z80.RegRSPBase[(Opcode shr 4) and $03]^);
end;

procedure TZ80Executor.OpLDHLinn(Opcode: Byte);
begin
  FCurRSP[2]^ := Hardware.ReadMemoryWord(GetImmediate16);
end;

function TZ80Executor.OpLDID(Dir: TDir): Boolean;
var
  Flags: Byte;
  Data: Byte;
begin
  if Dir = 0 then
    raise EZ80Exception.Create('Invalid Dir (0)');

  Data := Hardware.ReadMemoryByte(Z80.HL);
  Hardware.WriteMemoryByte(Z80.DE, Data);
  Z80.DE := Z80.DE + Dir;
  Z80.HL := Z80.HL + Dir;
  Z80.BC := Z80.BC - 1;
  Result := Z80.BC <> 0;

  if Result then
    Flags := PVMask
  else
    Flags := 0;

  if Data and 2 <> 0 then
    Flags := Flags or YMask;

  if Data and 8 <> 0 then
    Flags := Flags or XMask;

  //HF - always reset

  //N - Subtract
  if Dir = -1 then
    Flags := Flags or NMask;

  SetFlags(0, 0, YMask or HMask or XMask or PVMask or NMask, Flags);
end;

procedure TZ80Executor.OpLDiHLn(Opcode: Byte);
begin
  Hardware.WriteMemoryByte(Z80.HL, GetImmediate8);
end;

procedure TZ80Executor.OpLDiHLr(Opcode: Byte);
begin
  Hardware.WriteMemoryByte(Z80.HL, FCurR8[Opcode and $07]^);
end;

procedure TZ80Executor.OpLDinnA(Opcode: Byte);
begin
  Hardware.WriteMemoryByte(GetImmediate16, Z80.A);
end;

procedure TZ80Executor.OpLDinnHL(Opcode: Byte);
begin
  Hardware.WriteMemoryWord(GetImmediate16, FCurRSP[2]^);
end;

procedure TZ80Executor.OpLDirspA(Opcode: Byte);
begin //Note: Only applies to BC and DE
  Hardware.WriteMemoryByte(Z80.RegRSPBase[(Opcode shr 4) and $03]^, Z80.A);
end;

procedure TZ80Executor.OpLDixydn(Opcode: Byte);
var Data: Byte;
  Addr: Word;
begin
  Addr := AddRelative(FCurRSP[2]^, GetImmediate8);
  Data := GetImmediate8;
  Hardware.WriteMemoryByte(Addr, Data);
end;

procedure TZ80Executor.OpLDixydr(Opcode: Byte);
var Addr: Word;
begin
  Addr := AddRelative(FCurRSP[2]^, GetImmediate8);
  //Note: Always read from base registers - don't use IXL, IXH etc.
  Hardware.WriteMemoryByte(Addr, Z80.Reg8Base[Opcode and 7]^);
end;

procedure TZ80Executor.OpLDriHL(Opcode: Byte);
begin
  FCurR8[(Opcode shr 3) and $07]^ := Hardware.ReadMemoryByte(Z80.HL);
end;

procedure TZ80Executor.OpLDrixyd(Opcode: Byte);
var Addr: Word;
begin
  Addr := AddRelative(FCurRSP[2]^, GetImmediate8);
  //Note: Always read from base registers - don't use IXL, IXH etc.
  Z80.Reg8Base[Opcode shr 3 and 7]^ := Hardware.ReadMemoryByte(Addr);
end;

procedure TZ80Executor.OpLDrn(Opcode: Byte);
begin
  FCurR8[(Opcode shr 3) and $07]^ := GetImmediate8;
end;

procedure TZ80Executor.OpLDrr(Opcode: Byte);
begin
  FCurR8[(Opcode shr 3) and $07]^ := FCurR8[Opcode and $07]^;
end;

procedure TZ80Executor.OpLDrspnn(Opcode: Byte);
begin
  FCurRSP[(Opcode shr 4) and $03]^ := GetImmediate16;
end;

procedure TZ80Executor.OpLDSPHL(Opcode: Byte);
begin
  Z80.SP := FCurRAF[2]^;
end;

procedure TZ80Executor.OpNOP(Opcode: Byte);
begin
  //Do nothing
end;

function TZ80Executor.OpOR8Do(Value1, Value2: Byte): Byte;
var
  Flags: Byte;
begin
  Result := Value1 or Value2;

  //Flags
  Flags := Result and SMask;
  if Result = 0 then
    Flags := Flags or ZMask;

  //PV
  if CalcParity(Result) then
    Flags := Flags or PVMask;

  //Undocumented
  Flags := Flags or (Result and (YMask or XMask));

  Z80.F := Flags;
end;

procedure TZ80Executor.OpORiHL(Opcode: Byte);
begin
  Z80.A := OpOR8Do(Z80.A, Hardware.ReadMemoryByte(Z80.HL));
end;

{procedure TZ80Executor.OpORixyd(Opcode: Byte);
var Addr: Word;
begin
  Addr := AddRelative(FCurRSP[2]^, GetImmediate8);
  Z80.A := OpOR8Do(Z80.A, Hardware.ReadMemoryByte(Addr));
end;
}
procedure TZ80Executor.OpORn(Opcode: Byte);
begin
  Z80.A := OpOR8Do(Z80.A, GetImmediate8);
end;

procedure TZ80Executor.OpORr(Opcode: Byte);
begin
  Z80.A := OpOR8Do(Z80.A, FCurR8[Opcode and $07]^);
end;

function TZ80Executor.OpOUTID(Dir: TDir): Boolean;
begin
  if Dir = 0 then
    raise EZ80Exception.Create('Invalid Dir (0)');

  Z80.B := Z80.B - 1;
  Hardware.WriteIO(Z80.BC, Hardware.ReadMemoryByte(Z80.HL));
  Z80.HL := Z80.HL + Dir;

  Result := Z80.B <> 0;

  SetFlags(Z80.B, ZMask or YMask or XMask, SMask or HMask or PVMask or NMask, NMask);
end;

procedure TZ80Executor.OpOUTnA(Opcode: Byte);
begin
  Hardware.WriteIO((Z80.A shl 8) or GetImmediate8, Z80.A);
end;

procedure TZ80Executor.OpPOP(Opcode: Byte);
begin
  FCurRAF[(Opcode shr 4) and 3]^ := StackPop;
end;

procedure TZ80Executor.OpPUSH(Opcode: Byte);
begin
  StackPush(FCurRAF[Opcode shr 4 and 3]^);
end;

procedure TZ80Executor.OpRET(Opcode: Byte);
begin
  Z80.PC := StackPop;
end;

procedure TZ80Executor.OpRETcc(Opcode: Byte);
var FlagMask: Byte;
  Match: Boolean;
begin
  FlagMask := FlagMasks[Opcode shr 4 and 3];
  Match := (Opcode shr 3 and 1) <> 0;
  if ((Z80.F and FlagMask) <> 0) = Match then
    Z80.PC := StackPop;
end;

procedure TZ80Executor.OpRLA(Opcode: Byte);
var NewC: Byte;
  NewFlags: Byte;
  NewA: Byte;
begin
  NewC := Z80.A shr 7;
  NewFlags := (Z80.F and not CMask) or NewC;
  NewA := (Z80.A shl 1) or (Z80.F and CMask);
  Z80.A := NewA;
  Z80.F := NewFlags;
end;

procedure TZ80Executor.OpRLCA(Opcode: Byte);
var NewC: Byte;
  NewFlags: Byte;
  NewA: Byte;
begin
  NewC := Z80.A shr 7;
  NewFlags := (Z80.F and not CMask) or NewC;
  NewA := (Z80.A shl 1) or NewC;
  Z80.A := NewA;
  Z80.F := NewFlags;
end;

procedure TZ80Executor.OpRLD;
var
  Addr: Word;
  OldA: Byte;
  Mem: Byte;
  NewA: Byte;
begin
  Addr := Z80.HL;
  Mem := Hardware.ReadMemoryByte(Addr);
  OldA := Z80.A;
  //High nybble of (HL) to low nybble of A
  NewA := (OldA and $f0) or (Mem shr 4);
  Z80.A := NewA;
  //Rotate low nybble of (HL) to high nybble and move low nybble of A to low nybble of (HL)
  Hardware.WriteMemoryByte(Addr, (Mem shl 4) or (OldA and $f));

  SetFlags(NewA, SMask or ZMask or PVMask or YMask or XMask, HMask or NMask, 0);
end;

procedure TZ80Executor.OpRRA(Opcode: Byte);
var NewC: Byte;
  NewFlags: Byte;
  NewA: Byte;
begin
  NewC := Z80.A and 1;
  NewFlags := (Z80.F and not CMask) or NewC;
  NewA := (Z80.A shr 1) or ((Z80.F and CMask) shl 7);
  Z80.A := NewA;
  Z80.F := NewFlags;
end;

procedure TZ80Executor.OpRRCA(Opcode: Byte);
var NewC: Byte;
  NewFlags: Byte;
  NewA: Byte;
begin
  NewC := Z80.A and 1;
  NewFlags := (Z80.F and not CMask) or NewC;
  NewA := (Z80.A shr 1) or (NewC shl 7);
  Z80.A := NewA;
  Z80.F := NewFlags;
end;

procedure TZ80Executor.OpRRD;
var
  Addr: Word;
  OldA: Byte;
  Mem: Byte;
  NewA: Byte;
begin
  Addr := Z80.HL;
  Mem := Hardware.ReadMemoryByte(Addr);
  OldA := Z80.A;
  //Low nybble of (HL) to low nybble of A
  NewA := (OldA and $f0) or (Mem and $0f);
  Z80.A := NewA;
  //Rotate high nybble of (HL) to low nybble and move low nybble of A to high nybble of (HL)
  Hardware.WriteMemoryByte(Addr, (Mem shr 4) or (OldA shl 4));

  SetFlags(NewA, SMask or ZMask or PVMask or YMask or XMask, HMask or NMask, 0);
end;
procedure TZ80Executor.OpRST(Opcode: Byte);
begin
  StackPush(Z80.PC);
  Z80.PC := Opcode and $38;
end;

function TZ80Executor.OpSBC8Do(Value1, Value2: Byte): Byte;
var
  Carry: Integer;
  Op1Int: Integer;
  Op2Int: Integer;
  ResultInt: Integer;
  Flags: Byte;
begin
  Carry := Z80.F and 1;
  Op1Int := Value1;
  Op2Int := Value2;

  ResultInt := Op1Int - Op2Int - Carry;
  Result := ResultInt and $ff;

  //Flags
  Flags := (Result and SMask) or NMask;

  if Result = 0 then
    Flags := Flags or ZMask;

  if ((Op1Int and $0f) - (Op2Int and $0f) - Carry) and $ff > $0f then
    Flags := Flags or HMask;

  if ((Value1 and $80) <> (Value2 and $80)) and ((Value2 and $80) = (Result and $80)) then
    Flags := Flags or PVMask;

  if ResultInt < 0 then
    Flags := Flags or CMask;

  Flags := Flags or (Result and (YMask or XMask));

  Z80.F := Flags;
end;

procedure TZ80Executor.OpSBCAiHL(Opcode: Byte);
begin
  Z80.A := OpSBC8Do(Z80.A, Hardware.ReadMemoryByte(Z80.HL));
end;

{procedure TZ80Executor.OpSBCAixyd(Opcode: Byte);
var Addr: Word;
begin
  Addr := AddRelative(FCurRSP[2]^, GetImmediate8);
  Z80.A := OpSBC8Do(Z80.A, Hardware.ReadMemoryByte(Addr));
end;
}
procedure TZ80Executor.OpSBCAn(Opcode: Byte);
begin
  Z80.A := OpSBC8Do(Z80.A, GetImmediate8);
end;

procedure TZ80Executor.OpSBCAr(Opcode: Byte);
begin
  Z80.A := OpSBC8Do(Z80.A, FCurR8[Opcode and $07]^);
end;

procedure TZ80Executor.OpSCF(Opcode: Byte);
begin
  Z80.F := Z80.F or CMask;
end;

procedure TZ80Executor.OpSETRES(DoSet: Boolean; Bit, Reg: TReg8);
var
  Value: Byte;
begin
  //If IX/IY use (IX/IY+d)
  if FIndexModifier <> imMain then
    Value := GetReg8(Reg8HL, True)
  else
    Value := GetReg8(Reg, True);
  if DoSet then
    Value := Value or BinToBit[Bit]
  else
    Value := Value and not BinToBit[Bit];

  //If IX/IY then set (IX/IY+d) AND Reg
  if FIndexModifier <> imMain then
    SetReg8(Reg8HL, True, Value);
  SetReg8(Reg, True, Value);
end;

procedure TZ80Executor.OpShiftRotate(Operation, Reg: TReg8;FlagMask: Byte);
const
  OpRLC = 0;
  OpRRC = 1;
  OpRL = 2;
  OpRR = 3;
  OpSLA = 4;
  OpSRA = 5;
  OpSLL = 6;
  OpSRL = 7;

var
  Start: Byte;
  Result: Byte;
  Carry: Byte;
begin
  //If IX or IY then we modify the byte at (IX/IY+d)
  if FIndexModifier <> imMain then
    Start := GetReg8(Reg8HL, True)
  else
    Start := GetReg8(Reg, True);

  //Rotate
  if Operation and 1 = 0 then
    Result := Start shl 1
  else
    Result := Start shr 1;

  //Set the new bit
  case Operation of
    OpRLC: Result := Result or (Start shr 7);
    OpRRC: Result := Result or (Start shl 7);
    OpRL:
      if Z80.FlagC then
        Result := Result or 1;
    OpRR:
      if Z80.FlagC then
        Result := Result or $80;
    OpSLA:; //Nothing
    OpSRA: Result := Result or (Start and $80);
    OpSLL: Result := Result or 1;
    OpSRL:; //Nothing
  end;

  //If IX/IY then we return result to (IX/IY+d) AND the register
  if FIndexModifier <> imMain then
    SetReg8(Reg8HL, True, Result);

  SetReg8(Reg, True, Result);

  //Carry out
  case Operation of
    OpRLC, OpRL, OpSLA, OpSLL: Carry := Start shr 7;
    OpRRC, OpRR, OpSRA, OpSRL: Carry := Start and 1;
  else
    raise EZ80Exception.Create('Invalid operation');
  end;

  SetFlags(Result, FlagMask or YMask or XMask, HMask or NMask or CMask, Carry and 1);
end;

function TZ80Executor.OpSUB8Do(Value1, Value2: Byte): Byte;
var
  Op1Int: Integer;
  Op2Int: Integer;
  ResultInt: Integer;
  Flags: Byte;
begin
  Op1Int := Value1;
  Op2Int := Value2;

  ResultInt := Op1Int - Op2Int;
  Result := ResultInt and $ff;

  //Flags
  Flags := (Result and SMask) or NMask;

  if Result = 0 then
    Flags := Flags or ZMask;

  if ((Op1Int and $0f) - (Op2Int and $0f)) and $ff > $0f then
    Flags := Flags or HMask;

  if ((Value1 and $80) <> (Value2 and $80)) and ((Value2 and $80) = (Result and $80)) then
    Flags := Flags or PVMask;

  if ResultInt < 0 then
    Flags := Flags or CMask;

  Flags := Flags or (Result and (YMask or XMask));

  Z80.F := Flags;
end;

procedure TZ80Executor.OpSUBAiHL(Opcode: Byte);
begin
  Z80.A := OpSUB8Do(Z80.A, Hardware.ReadMemoryByte(Z80.HL));
end;

{procedure TZ80Executor.OpSUBAixyd(Opcode: Byte);
var Addr: Word;
begin
  Addr := AddRelative(FCurRSP[2]^, GetImmediate8);
  Z80.A := OpSUB8Do(Z80.A, Hardware.ReadMemoryByte(Addr));
end;
}
procedure TZ80Executor.OpSUBAn(Opcode: Byte);
begin
  Z80.A := OpSUB8Do(Z80.A, GetImmediate8);
end;

procedure TZ80Executor.OpSUBAr(Opcode: Byte);
begin
  Z80.A := OpSUB8Do(Z80.A, FCurR8[Opcode and $07]^);
end;

function TZ80Executor.OpXOR8Do(Value1, Value2: Byte): Byte;
var
  Flags: Byte;
begin
  Result := Value1 xor Value2;

  //Flags
  Flags := Result and SMask;
  if Result = 0 then
    Flags := Flags or ZMask;

  //PV
  if CalcParity(Result) then
    Flags := Flags or PVMask;

  //Undocumented
  Flags := Flags or (Result and (YMask or XMask));

  Z80.F := Flags;
end;

procedure TZ80Executor.OpXORiHL(Opcode: Byte);
begin
  Z80.A := OpOR8Do(Z80.A, Hardware.ReadMemoryByte(Z80.HL));
end;

{procedure TZ80Executor.OpXORixyd(Opcode: Byte);
var Addr: Word;
begin
  Addr := AddRelative(FCurRSP[2]^, GetImmediate8);
  Z80.A := OpXOR8Do(Z80.A, Hardware.ReadMemoryByte(Addr));
end;
}
procedure TZ80Executor.OpXORn(Opcode: Byte);
begin
  Z80.A := OpXOR8Do(Z80.A, GetImmediate8);
end;

procedure TZ80Executor.OpXORr(Opcode: Byte);
begin
  Z80.A := OpXOR8Do(Z80.A, FCurR8[Opcode and $07]^);
end;


procedure TZ80Executor.SetFlags(Value, CalcMask, FixMask, FixValues: Byte);
var
  Flags: Byte;
begin
  if CalcMask and not (SMask or ZMask or YMask or XMask or PVMask) <> 0 then
    raise EZ80Exception.Create('SetFlags can only calculate N, Z and P(arity) flags');

  Flags := (Z80.F and not FixMask and not CalcMask) or FixValues;

  //Sign
  if (CalcMask and SMask) <> 0 then
    if Value and $80 = $80 then
      Flags := Flags or SMask;

  //Zero
  if (CalcMask and ZMask) <> 0 then
    if Value and $ff = 0 then
      Flags := Flags or ZMask;

  //YF - Undocumented flag
  if (CalcMask and YMask) <> 0 then
    if Value and YMask <> 0 then
      Flags := Flags or YMask;

  //Half carry - Can't calculate here

  //XF - Undocumented flag
  if (CalcMask and XMask) <> 0 then
    if Value and XMask <> 0 then
      Flags := Flags or XMask;

  //Parity (We can't do oVerflow here)
  if (CalcMask and PVMask) <> 0 then
    if CalcParity(Value) then
      Flags := Flags or PVMask;

  //Subtraction - Can't calculate here

  //Carry - Can't Calculate here

  Z80.F := Flags;
end;

procedure TZ80Executor.SetIndirectAddr;
begin
  FIndirectAddr := IndirectAddr(FIndexModifier);
  FHaveIndirectAddr := True;
end;

procedure TZ80Executor.SetReg8(r: TReg8; UseInstructionSet: Boolean;
  Value: Byte);
begin
  if r = Reg8HL then
  begin
    if not FHaveIndirectAddr then
      SetIndirectAddr;

    Hardware.WriteMemoryByte(FIndirectAddr, Value);
  end
  else if UseInstructionSet then
    Z80.SetReg8(r, Value, FIndexModifier)
  else
    Z80.SetReg8(r, Value);
end;

{procedure TZ80Executor.SetRegRAF(r: TReg16; Value: Word);
begin
  Z80.SetRegRAF(r, Value, FIndexModifier);
end;
}
{procedure TZ80Executor.SetRegRSP(r: TReg16; Value: Word);
begin
  Z80.SetRegRSP(r, Value, FIndexModifier);
end;
}
procedure TZ80Executor.SignalINT;
//var Vector: Byte; TODO
begin
  if Z80.IFF1 then
    case Z80.IM of
    0:; //Read Opcode from bush
    1:
    begin
      StackPush(Z80.PC);
      Z80.PC := $0038;
    end;
    2:
    begin
      //Read vector from bus
      //Vector := ??BusRead
      StackPush(Z80.PC);
      Z80.PC := Z80.R shl 8;// or Vector;
    end;
    end;
end;

procedure TZ80Executor.SignalNMI;
begin
  Z80.IFF2 := Z80.IFF1;
//  Z80.IFF1 := False;
  StackPush(Z80.PC);
  Z80.PC := $0066;
end;

function TZ80Executor.StackPop: Word;
begin
  Result := Hardware.ReadMemoryWord(Z80.SP);
  Z80.SP := Z80.SP + 2;
end;

procedure TZ80Executor.StackPush(Value: Word);
begin
  Hardware.WriteMemoryByte(Z80.SP-1, Value shr 8);
  Hardware.WriteMemoryByte(Z80.SP-2, Value and $ff);
  Z80.SP := Z80.SP - 2;
end;

end.

