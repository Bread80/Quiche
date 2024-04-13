unit Z80.CPUState;

interface
uses SysUtils, Variables, ILData, Z80.CPU;

//Sets all registers to rskUnknown
procedure RegStateInitialise;

//Does NOT affect the state of any other register, including the given register as
//part of, or separate from, a pair
procedure SetRegStateUnknown(Reg: TCPUReg);

//Also sets the state of the related register pair, or separate register
//Does NOT affect the other half of a register pair
procedure SetRegStateLiteral(Reg: TCPUReg;AValue: Integer);

//Also sets the state of the related register pair, or separate register
//Does NOT affect the other alf of a register pair
procedure SetRegStateVariable(Reg: TCPUReg;AVariable: PVariable;AVersion: Integer);

//If the variable (value) is currently stored in a register return the register
function RegStateFindVariable(AVariable: PVariable;AVersion: Integer): TCPUReg;

//Find and return a suitable 8-bit register for use.
//Regs specifies a set of registers which MUST NOT be returned.
function CPUStateAllocReg8(Regs: TCPURegSet): TCPUReg;

function CPUStateToString: String;

implementation

type TRegStateKind =
  (rskUnknown, rskLiteral, rskVariable);

type TRegState = record
  function ToString: String;

  case Kind: TRegStateKind of
    rskUnknown: ();
    rskLiteral: (
      Literal: Integer;
      );
    rskVariable: (
      Variable: PVariable;
      Version: Integer;
      );
  end;

type TCPUStateReg = (
  crNone,
  crA, crB, crC, crD, crE, crH, crL,
  crBC, crDE, crHL,
  crCarry);

type TCPUState = array[low(TCPUStateReg)..high(TCPUStateReg)] of TRegState;

var CPUState: TCPUState;

const RegToReg: array[low(TCPUReg)..high(TCPUReg)] of TCPUStateReg =
  (crNone, crNone, crNone, crNone, crNone,
  crA, crB, crC, crD, crE, crH, crL,
  crNone,
  crHL, crDE, crBC, crNone, crNone,
  crNone, crNone, crNone, crNone,
  crCarry, crNone, crNone, crNone);
const RegToPair: array[low(TCPUReg)..high(TCPUReg)] of TCPUStateReg =
  (crNone, crNone, crNone, crNone, crNone,
  crNone, crBC, crBC, crDE, crDE, crHL, crHL,
  crNone,
  crHL, crDE, crBC, crNone, crNone,
  crNone, crNone, crNone, crNone,
  crNone, crNone, crNone, crNone);
const RegToHigh: array[low(TCPUReg)..high(TCPUReg)] of TCPUStateReg =
  (crNone, crNone, crNone, crNone, crNone,
  crNone, crB, crNone, crD, crNone, crH, crNone,
  crNone,
  crH, crD, crB, crNone, crNone,
  crNone, crNone, crNone, crNone,
  crNone, crNone, crNone, crNone);
const RegToLow: array[low(TCPUReg)..high(TCPUReg)] of TCPUStateReg =
  (crNone, crNone, crNone, crNone, crNone,
  crNone, crNone, crC, crNone, crE, crNone, crL,
  crNone,
  crL, crE, crC, crNone, crNone,
  crNone, crNone, crNone, crNone,
  crNone, crNone, crNone, crNone);
const RegToSingle: array[low(TCPUReg)..high(TCPUReg)] of TCPUStateReg =
  (crNone, crNone, crNone, crNone, crNone,
  crA, crB, crC, crD, crE, crH, crL,
  crNone,
  crNone, crNone, crNone, crNone, crNone,
  crNone, crNone, crNone, crNone,
  crNone, crNone, crNone, crNone);

const StateRegToReg: array[low(TCPUStateReg)..high(TCPUStateReg)] of TCPUReg =
  (rNone, rA, rB, rC, rD, rE, rH, rL, rBC, rDE, rHL, rCF);

procedure RegStateInitialise;
var R: TCPUStateReg;
begin
  for R := low(TCPUStateReg) to high(TCPUStateReg) do
    CPUState[R].Kind := rskUnknown;
end;

procedure SetRegStateUnknown(Reg: TCPUReg);
begin
  CPUState[RegToReg[Reg]].Kind := rskUnknown;
end;

procedure SetRegStateLiteral(Reg: TCPUReg;AValue: Integer);
begin
  case Reg of
    rHL, rDE, rBC:
    begin //Pairs
      CPUState[RegToPair[Reg]].Kind := rskLiteral;
      CPUState[RegToPair[Reg]].Literal := AValue;
      CPUState[RegToHigh[Reg]].Kind := rskLiteral;
      CPUState[RegToHigh[Reg]].Literal := (AValue shr 8) and $ff;
      CPUState[RegToLow[Reg]].Kind := rskLiteral;
      CPUState[RegToLow[Reg]].Literal := AValue and $ff;
    end;
    rB, rD, rH:
    begin //High bytes
      if CPUState[RegToPair[Reg]].Kind = rskLiteral then
        CPUState[RegToPair[Reg]].Literal :=
          CPUState[RegToPair[Reg]].Literal and $ff or ((AValue and $ff) shl 8)
      else
        CPUState[RegToPair[Reg]].Kind := rskUnknown;

      CPUState[RegToSingle[Reg]].Kind := rskLiteral;
      CPUState[RegToSingle[Reg]].Literal := AValue;
    end;
    rC, rE, rL:
    begin //Low bytes
      if CPUState[RegToPair[Reg]].Kind = rskLiteral then
        CPUState[RegToPair[Reg]].Literal :=
          CPUState[RegToPair[Reg]].Literal and $ff00 or (AValue and $ff)
      else
        CPUState[RegToPair[Reg]].Kind := rskUnknown;

      CPUState[RegToSingle[Reg]].Kind := rskLiteral;
      CPUState[RegToSingle[Reg]].Literal := AValue;
    end;
    rA, rCF:
    begin //Not part of a pair
      CPUState[RegToSingle[Reg]].Kind := rskLiteral;
      CPUState[RegToSingle[Reg]].Literal := AValue;
    end;
  else
    Assert(False);
  end;
end;

procedure SetRegStateVariable(Reg: TCPUReg;AVariable: PVariable;AVersion: Integer);
begin
  case Reg of
    rHL, rDE, rBC:
    begin //Pairs
      CPUState[RegToPair[Reg]].Kind := rskVariable;
      CPUState[RegToPair[Reg]].Variable := AVariable;
      CPUState[RegToPair[Reg]].Version := AVersion;

      CPUState[RegToHigh[Reg]].Kind := rskUnknown;
      CPUState[RegToLow[Reg]].Kind := rskUnknown;
    end;
    rA, rB, rC, rD, rE, rH, rL, rCF:
    begin //Singles anf flags
      CPUState[RegToPair[Reg]].Kind := rskUnknown;

      CPUState[RegToReg[Reg]].Kind := rskVariable;
      CPUState[RegToReg[Reg]].Variable := AVariable;
      CPUState[RegToReg[Reg]].Version := AVersion;
    end;
  else
    Assert(False);
  end;
end;

function RegStateFindVariable(AVariable: PVariable;AVersion: Integer): TCPUReg;
var R: TCPUStateReg;
  State: TRegState;
begin
  for R := low(TCPUStateReg) to high(TCPUStateReg) do
  begin
    State := CPUState[R];
    if (State.Kind = rskVariable) and (State.Variable = AVariable) and (State.Version = AVersion) then
      EXIT(StateRegToReg[R]);
  end;

  Result := rNone;
end;

function CPUStateAllocReg8(Regs: TCPURegSet): TCPUReg;
var R: TCPUStateReg;
begin
  //Tries to return a unused register (rskUnknown), if not returns the
  //first register not in Regs
  Result := rNone;
  for R := crA to crL do
    if not (StateRegToReg[R] in Regs) then
      if CPUState[R].Kind = rskUnknown then
        EXIT(StateRegToReg[R])
      else if Result = rNone then
        Result := StateRegToReg[R];

  Assert(False, 'Unable to allocate a register');
end;


{ TRegState }

function TRegState.ToString: String;
const KindStrings: array[low(TRegStateKind)..high(TRegStateKind)] of String =
  ('?', '', '%');
begin
  Result := KindStrings[Kind];
  case Kind of
    rskUnknown: ;
    rskLiteral:
      Result := Result + '$' + IntToHex(Literal);
    rskVariable: Result := Result + Variable.Name + '_' + Version.ToString;
  else
    Assert(False);
  end;
end;

function CPUStateToString: String;
var R: TCPUStateReg;
const RegStrings: array[low(TCPUStateReg)..high(TCPUStateReg)] of String = (
  'NONE','A','B','C','D','E','H','L','BC','DE','HL','Carry');
begin
  Result := '';
  for R := crA to high(TCPUStateReg) do
    if CPUState[R].Kind <> rskUnknown then
      Result := Result + RegStrings[R] + '=' + CPUState[R].ToString + ' ';
end;

end.
