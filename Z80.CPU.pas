(*
Low level CPU data and code generators
*)
unit Z80.CPU;

interface
uses Def.QTypes, Def.Consts, Def.Variables;

type
  //This list details the CPU-level places where primitives can find input data
  //and place result data.
  //For a primitive this specifies the options available for register selection.
  //For ILData this specifies where a primitive must find or place data. I.e.
  //after primitive selection and register allocation has taken place.
  //Not all values are valid in all situtions (inputs, results, primitives, ILData)
  TCPUReg = (
    rNone,      //No parameter or unassigned
    rA, rB, rC, rD, rE, rH, rL, //8 bit registers
    rAF,        //Only for stack operations
    rHL, rDE, rBC, rIX, rIY,    //16 bit register pairs
    rZF, rZFA,  //Zero flag Set. The second variant also sets A as a boolean
    rNZF, rNZFA,//Zero flag Clear. The second variant also sets A as a boolean
    rCF,        //Carry flag set
    rNCF,       //Carry flag clear
    rCPLA,      //Result is complement of A (for boolean results only)
    rFlags      //Flags (for corrupt only)
    );
  TCPURegSet = set of TCPUReg;

//All 'registers' which are truly 'registers'
const
  CPURegsAll = [rA..rL,rIX,rIY,rZF,rCF,rFlags]; //Used by Perserves attribute & values
  CPURegRegs = [rA..rL,rHL..rIY,rZFA,rNZFA,rCPLA];
  CPUReg8Bit = [rA..rL];
  CPUReg16Bit = [rHL..rIY]; //AF does NOT count as a true 16-bit reg...
  CPURegPairs = [rAF, rBC, rDE, rHL]; //...but it is a Pair. (Whereas IX, and IY aren't!)
                                      //(Well they could be using undocumented opcodes,
                                      //but we don't use them that way)
  CPURegFlags = [rZF,rNZF,rCF,rNCF,rCPLA];
  //Registers and flags which are meaningful for data about which registers are
  //corruped or preserved
//  CPURegCorruptable = [rA,rB,rC,rD,rE,rH,rL,rIX,rIY,rCF,rZF,rFlags];

const
  CPURegPairToLow: array[low(TCPUReg)..High(TCPUReg)] of TCPUReg = (
  rNone,
  rNone, rNone, rNone, rNone, rNone, rNone, rNone,
  rNone,
  rL, rE, rC, rNone, rNone,
  rNone, rNone, rNone, rNone,
  rNone, rNone, rNone, rNone);
  CPURegPairToHigh: array[low(TCPUReg)..High(TCPUReg)] of TCPUReg = (
  rNone,
  rNone, rNone, rNone, rNone, rNone, rNone, rNone,
  rNone,
  rH, rD, rB, rNone, rNone,
  rNone, rNone, rNone, rNone,
  rNone, rNone, rNone, rNone);
  CPUReg8ToPair: array[low(TCPUReg)..High(TCPUReg)] of TCPUReg = (
  rNone,
  rAF, rBC, rBC, rDE, rDE, rHL, rHL,
  rNone,
  rNone, rNone, rNone, rNone, rNone,
  rNone, rNone, rNone, rNone,
  rNone, rNone, rNone, rNone);
  CPURegLowToHigh: array[low(TCPUReg)..High(TCPUReg)] of TCPUReg = (
  rNone,
  rNone, rNone, rB, rNone, rD, rNone, rH,
  rNone,
  rNone, rNone, rNone, rNone, rNone,
  rNone, rNone, rNone, rNone,
  rNone, rNone, rNone, rNone);
  CPURegHighToLow: array[low(TCPUReg)..High(TCPUReg)] of TCPUReg = (
  rNone,
  rNone, rC, rNone, rE, rNone, rL, rNone,
  rNone,
  rNone, rNone, rNone, rNone, rNone,
  rNone, rNone, rNone, rNone,
  rNone, rNone, rNone, rNone);
const
  //Mappings between register and register name
  CPUReg8ToChar: array[low(TCPUReg)..High(TCPUReg)] of Char = (
  #0,
  'a','b','c','d','e','h','l',
  #0,#0,#0,#0,#0,#0,
  #0,#0,#0,#0,#0,#0,#0,#0);
  CPURegPairToString: array[low(TCPUReg)..High(TCPUReg)] of String = (
  #0,
  #0,#0,#0,#0,#0,#0,#0,
  'af','hl','de','bc','ix','iy',
  #0,#0,#0,#0,#0,#0,#0,#0);
  //Low reg of a pair
  CPURegLowToChar: array[low(TCPUReg)..High(TCPUReg)] of Char = (
  #0,
  #0,#0,#0,#0,#0,#0,#0,
  #0,'l','e','c',#0,#0,
  #0,#0,#0,#0,#0,#0,#0,#0);
  //High reg of a pair
  CPURegHighToChar: array[low(TCPUReg)..High(TCPUReg)] of Char = (
  #0,
  #0,#0,#0,#0,#0,#0,#0,
  #0,'h','d','b',#0,#0,
  #0,#0,#0,#0,#0,#0,#0,#0);

const CPURegStrings: array[low(TCPUReg)..high(TCPUReg)] of String = (
  '',
  'a','b','c','d','e','h','l',
  'af',
  'hl','de','bc','ix','iy',
  'ZF','','','',
  'CF','','','');

const CPURegAllStrings: array[low(TCPUReg)..high(TCPUReg)] of String = (
  'none',
  'a','b','c','d','e','h','l',
  'af',
  'hl','de','bc','ix','iy',
  'ZF','ZFA','NZF','NZFA',
  'CF','NCF','CPLA','Flags');

const CPURegToVarType: array[low(TCPUReg)..high(TCPUReg)] of TVarType = (
  vtUnknown,
  vtByte, vtByte, vtByte, vtByte, vtByte, vtByte, vtByte,
  vtUnknown,
  vtWord, vtWord, vtWord, vtWord, vtWord,
  vtBoolean, vtUnknown, vtUnknown, vtUnknown,
  vtBoolean, vtUnknown, vtUnknown, vtUnknown);

function CharToCPUReg(C: Char;ForCorrupts: Boolean): TCPUReg;
function StrToCPURegAll(S: String;ForCorrupts: Boolean): TCPUReg;

function IdentToCPUReg(Ident: String): TCPUReg;

//============Code generation 'helpers'

//If ByteIndex is zero,
//Generates (IX+<full-varname>)
//otherwise,
//Generates (IX+<full-varname>+<ByteOffset>)
function OffsetToStr(Reg: TCPUReg;Variable: PVariable;ByteIndex: Integer = 0): String;
function OffsetHiToStr(Reg: TCPUReg;Variable: PVariable): String;


//Convenience routines to generate specific opcodes
procedure OpLD(Dest, Source: TCPUReg);overload;
//Converts Source to 8 or 16-bit string depending on the Dest register size.
//If source is a Char, might also be converted to characters literal
procedure OpLD(Dest: TCPUReg; Source: TImmValue);overload;
procedure OpLD(Dest: TCPUReg;Value: Integer);overload;
//To/from index register - can move both 8 and 16 bit values
procedure OpLD(DestXY: TCPUReg;DestV: PVariable;Source: TCPUReg;ByteIndex: Integer = 0);overload;
procedure OpLD(Dest, SourceXY: TCPUReg;SourceV: PVariable;ByteIndex: Integer = 0);overload;
//To/from variable
procedure OpLD(DestV: PVariable;Source: TCPUReg);overload;
procedure OpLD(Dest: TCPUReg;SourceV: PVariable);overload;
//ByteIndex allows to specify high or low byte. Reg must be 8-bit
procedure OpLD(Dest: TCPUReg;SourceV: PVariable;ByteIndex: Integer);overload;
procedure OpLD(DestV: PVariable;ByteIndex: Integer;Source: TCPUReg);overload;
procedure OpLD(Dest: TCPUReg;Source: String);overload;

procedure OpPUSH(Reg: TCPUReg);
procedure OpPOP(Reg: TCPUReg);

procedure OpEXHLDE;

procedure OpADD(RAcc, RAdd: TCPUReg);
procedure OpINC(Reg: TCPUReg);
procedure OpDEC(Reg: TCPUReg);

procedure OpcodeOR(Reg: TCPUReg);

procedure OpANDA;

//Used to specify limitations when loading, storing, converting, validating, etc. parameters etc.
type TMoveOptions = (
  moPreserveA,    //Preserve the A register
  moPreserveHL,   //Preserve the HL register
  moPreserveHLDE, //Preserve either HL or DE. Instructs the codegen to NOT use EX HL,DE
  moPreserveCF,   //Preserve the carry flag
  moPreserveOtherFlags //The (testable) flags other than the Carry flag (ie. Sign, PV, Zero)
  );
  TMoveOptionSet = set of TMoveOptions;


//Sign extends an 8-bit value in RIn to a 16-bit value in ROutRIn
//Updates RegState as required
procedure GenSignExtend(RIn, ROut: TCPUReg;Options: TMoveOptionSet);

//Generate code to perform an optimised load of a literal value into a register.
//Various optimisations are available including copying a value from another register
//and 'modifying' the existing value, eg. by INCrementing it.
//The available optimisations depend on both register, the current CPPU state and
//whether CPU flags need to be preserved.
procedure GenLoadRegLiteral(Reg: TCPUReg;const Value: TImmValue;Options: TMoveOptionSet);

//Generate code to move FromReg to ToReg
//Currently only allows 'main' registers (ABCDEHL)
//Can sign extend a value moving from an 8-bit to a 16-bit register. When doing so
//  the A register will be trashed (and the Flags register cannot be preserved)
procedure GenRegMove(FromReg, ToReg: TCPUReg;Signed: Boolean;Options: TMoveOptionSet);

implementation
uses SysUtils, Codegen,
  Z80.CPUState;


const lutCharToCPUReg: array['a'..'z'] of TCPUReg = (
  rA, rB, rC, rD, rE, rFlags, rNone,  //A..G
  rH, rNone, rNone, rNone, rL, rNone,  //L..M
  rNone, rNone, rNone, rNone, rNone, rNone, rNone, rNone, //N..U
  rNone, rNone, rNone, rNone, rNone);   //V..Z

function CharToCPUReg(C: Char;ForCorrupts: Boolean): TCPUReg;
begin
  if (C = 'f') and not ForCorrupts then
    raise Exception.Create('Invalid register: ' + C);

  if CharInSet(C, ['a'..'z']) then
    Result := lutCharToCPUReg[C]
  else
    raise Exception.Create('Invalid register: ' + C);
  if Result = rNone then
    raise Exception.Create('Invalid register: ' + C);
end;

function IdentToCPUReg(Ident: String): TCPUReg;
begin
  for Result := low(TCPUReg) to high(TCPUReg) do
    if CompareText(CPURegStrings[Result], Ident) = 0 then
      EXIT;

  Result := rNone;
end;

function StrToCPURegAll(S: String;ForCorrupts: Boolean): TCPUReg;
begin
  if Length(S) = 0 then
    Result := rNone
  else if Length(S) = 1 then
    Result := CharToCPUReg(S.Chars[0], ForCorrupts)
  else
  begin
    for Result := low(TCPUReg) to high(TCPUReg) do
      if CompareText(CPURegAllStrings[Result], S) = 0 then
        EXIT;
    Result := rNone;
  end;
end;


function OffsetToStr(Reg: TCPUReg;Variable: PVariable;ByteIndex: Integer = 0): String;
begin
  Assert(Variable.Storage = vsStack);

  Result := '('+CPURegStrings[Reg];
  if Variable.Offset < 0 then
    Result := Result + '-'
  else
    Result := Result + '+';
  Result := Result + Variable.GetAsmName;

  if ByteIndex <> 0 then
    Result := Result + ' +' + ByteIndex.ToString;
  Result := Result + ')';
end;

function OffsetHiToStr(Reg: TCPUReg;Variable: PVariable): String;
begin
  Result := OffsetToStr(Reg, Variable, 1);
end;

procedure OpLD(Dest, Source: TCPUReg);overload;
begin
  Opcode('ld',CPURegStrings[Dest],CPURegStrings[Source]);
end;

procedure OpLD(Dest: TCPUReg; Source: TImmValue);overload;
begin
  if Dest in CPUReg8Bit then
    Opcode('ld',CPURegStrings[Dest], Source.ToStringByte)
  else if Dest in CPUReg16Bit then
    Opcode('ld',CPURegStrings[Dest], Source.ToStringWord)
  else
    Assert(False);
end;

procedure OpLD(Dest: TCPUReg;Value: Integer);overload;
begin
  if Dest in CPUReg8Bit then
    Opcode('ld',CPURegStrings[Dest],ByteToStr(Value))
  else
    Opcode('ld',CPURegStrings[Dest],WordToStr(Value));
end;

procedure OpLD(DestXY: TCPUReg;DestV: PVariable;Source: TCPUReg;ByteIndex: Integer = 0);overload;
begin
  Assert(DestXY in [rIX, rIY]);
  Assert(DestV.Storage = vsStack);

  if Source in CPURegPairs then
  begin
    Opcode('ld',OffsetToStr(DestXY, DestV, ByteIndex), CPURegStrings[CPURegPairToLow[Source]]);
    Opcode('ld',OffsetToStr(DestXY, DestV, ByteIndex +1), CPURegStrings[CPURegPairToHigh[Source]]);
  end
  else
    Opcode('ld',OffsetToStr(DestXY, DestV, ByteIndex), CPURegStrings[Source]);
end;

procedure OpLD(Dest, SourceXY: TCPUReg;SourceV: PVariable;ByteIndex: Integer = 0);overload;
begin
  Assert(SourceXY in [rIX, rIY]);
  Assert(SourceV.Storage = vsStack);

  if Dest in CPURegPairs then
  begin
    Opcode('ld', CPURegStrings[CPURegPairToLow[Dest]], OffsetToStr(SourceXY, SourceV, ByteIndex));
    Opcode('ld', CPURegStrings[CPURegPairToHigh[Dest]], OffsetToStr(SourceXY, SourceV, ByteIndex +1));
  end
  else
    Opcode('ld', CPURegStrings[Dest], OffsetToStr(SourceXY, SourceV, ByteIndex));
end;

procedure OpLD(DestV: PVariable;Source: TCPUReg);overload;
begin
  Assert(DestV.Storage = vsStatic);
  Opcode('ld', '('+DestV.GetAsmName+')', CPURegStrings[Source]);
end;

procedure OpLD(Dest: TCPUReg;SourceV: PVariable);overload;
begin
  Assert(SourceV.Storage = vsStatic);
  Opcode('ld', CPURegStrings[Dest], '('+SourceV.GetAsmName+')');
end;

procedure OpLD(Dest: TCPUReg;SourceV: PVariable;ByteIndex: Integer);overload;
var S: String;
begin
  Assert(SourceV.Storage = vsStatic);
  S := '('+SourceV.GetAsmName;
  if ByteIndex <> 0 then
    S := S +' +' +ByteIndex.ToString;
  S := S + ')';
  Opcode('ld', CPURegStrings[Dest], S);
end;

procedure OpLD(DestV: PVariable;ByteIndex: Integer;Source: TCPUReg);overload;
var S: String;
begin
  Assert(DestV.Storage = vsStatic);

  S := '('+DestV.GetAsmName;
  if ByteIndex <> 0 then
    S := S +' +' +ByteIndex.ToString;
  S := S + ')';
  Opcode('ld', S, CPURegStrings[Source]);
end;

procedure OpLD(Dest: TCPUReg;Source: String);overload;
begin
  Opcode('ld', CPURegStrings[Dest], Source);
end;

procedure OpPUSH(Reg: TCPUReg);
begin
  Assert(Reg in (CPUReg16Bit + [rAF]));
  Opcode('push',CPURegStrings[Reg]);
end;

procedure OpPOP(Reg: TCPUReg);
begin
  Assert(Reg in (CPUReg16Bit + [rAF]));
  Opcode('pop',CPURegStrings[Reg]);
end;

procedure OpEXHLDE;
begin
  Instr('ex hl,de');
end;

procedure OpADD(RAcc, RAdd: TCPUReg);
begin
  Opcode('add',CPURegStrings[RAcc],CPURegStrings[RAdd]);
end;

procedure OpINC(Reg: TCPUReg);
begin
  Opcode('inc',CPURegStrings[Reg]);
end;

procedure OpDEC(Reg: TCPUReg);
begin
  Opcode('dec',CPURegStrings[Reg]);
end;

procedure OpcodeOR(Reg: TCPUReg);
begin
  Opcode('or',CPURegStrings[Reg]);
end;

procedure OpANDA;
begin
  Opcode('and','a');
end;

//Sign extends an 8-bit value in RIn to a 16-bit value in ROutRIn
//Updates RegState as required
procedure GenSignExtend(RIn, ROut: TCPUReg;Options: TMoveOptionSet);
begin
  System.Assert(([moPreserveA, moPreserveCF, moPreserveOtherFlags] * Options) = [],
    'Unable to extend whilst preserving flags');

  Assert(RIn in CPUReg8Bit);
  Assert(ROut in CPUReg8Bit);
  if RIn <> rA then
    OpLD(rA, RIn);  //Move value to A
  Opcode('rla');       //Move sign bit to Carry
  Instr('sbc a,a');   //If carry we get -1, otherwise 0
  if ROut <> rA then
    OpLD(ROut, rA);  //Move result to register
  RegStateSetUnknowns([rA, rOut, rFlags, rCF]);
end;


//====================================SCAVENGING LITERALS

//Sub of LoadRegLiteral
//Attempt to find an optimised way to load the given value into the given register
function TryLoadReg8Optimised(Reg: TCPUReg;Value: Integer;Options: TMoveOptionSet): Boolean;

  //Does a left rotation match the desired results
  //Value is the original register value
  //Bit0 is the new bit 0 (0 or <> 0)
  //NewValue is the required result
  //NewCF is the required new carry flag zero (0 or <> 0) (ir PreserveCarry)
  //if PreserveCarry is True the new value of Carry must match NewCF
  function LeftRotMatch(Value, Bit0, NewValue, NewCF: Integer;PreserveCarry: Boolean): Boolean;
  var Calc: Integer;
  begin
    Calc := (Value and $fe) shl 1;
    if Bit0 <> 0 then
      Calc := Calc or 1;
    Result := ((Calc and $ff) = NewValue);
    if PreserveCarry then
      Result := Result and ((NewCF <> 0) = ((Value and $80) <> 0));
  end;

  //As above but Bit7 is the new bit 7
  function RightRotMatch(Value, Bit7, NewValue, NewCF: Integer;PreserveCarry: Boolean): Boolean;
  var Calc: Integer;
  begin
    Calc := (Value and $fe) shr 1;
    if Bit7 <> 0 then
      Calc := Calc or $80;
    Result := ((Calc and $ff) = NewValue);
    if PreserveCarry then
      Result := Result and ((NewCF <> 0) = ((Value and 1) <> 0));
  end;

var R: TCPUReg;
  CalcValue: Integer;
  LitA: Integer;  //The current value of the register
  LitCF: Integer; //The current value of Carry Flag
  PreserveCF: Boolean;
  PreserveOtherFlags: Boolean;
begin //We only want optimisations which are faster/smaller than loading a literal (1B/1M/4T)
  System.Assert(Reg in CPUReg8Bit);

  PreserveCF := moPreserveCF in Options;
  PreserveOtherFlags := moPreserveOtherFlags in Options;

  Result := False;
  //If the register already contains a literal value - we may be able to transform
  //it into the value we want
  if RegStateIsLiteral(Reg) then
    //Optimisation for any register
    if not PreserveOtherFlags then
    begin
      //These trash all flags except CF
      if Value = (RegStateGetLiteral(Reg) + 1) and $ff then
      begin
        Opcode('inc',CPURegStrings[Reg]); //1/1/4
        Result := True;
      end
      else if Value = (RegStateGetLiteral(Reg) - 1) and $ff then
      begin
        Opcode('dec',CPURegStrings[Reg]); //1/1/4
        Result := True;
      end;
      if Result then
      begin
        //Trash flags except CF
        RegStateSetUnknown(rFlags); //Other than CF
        EXIT;
      end;
    end;


  //A register specific optimisations
  if Reg = rA then
  begin
    if RegStateIsLiteral(rA) then
    begin
      LitA := RegStateGetLiteral(rA);

      if LitA = Value xor $ff then
      begin
        Opcode('cpl');  //1/1/4. Only changes H and N flags
        EXIT(True);
      end;

      //If both A and Carry are literals we can look for rotations
      if RegStateIsLiteral(rCF) then
        LitCF := RegStateGetLiteral(rCF)
      else
        LitCF := -1;

      if RegStateIsLiteral(rCF) then
      begin
        Result := True;
        if LeftRotMatch(LitA, LitCF, Value, LitCF, PreserveCF) then
        begin
          Opcode('rla'); //1/1/4
          RegStateSetLiteral(rCF, LitA and $80);
        end
        else if RightRotMatch(LitA, LitCF, Value, LitCF, PreserveCF) then
        begin
          Opcode('rra');
          RegStateSetLiteral(rCF, LitA and 1);
        end
        else
          Result := False;
        if Result then
          EXIT;
      end;
      if RegStateIsLiteral(rCF) or not PreserveCF then
      begin
        Result := True;

        if LeftRotMatch(LitA, LitA and $80, Value, LitCF, PreserveCF) then
        begin
          Opcode('rlca');  //1/1/4
          RegStateSetLiteral(rCF, LitA and $80);
        end
        else if RightRotMatch(LitA, LitA and 1, Value, LitCF, PreserveCF) then
        begin
          Opcode('rrca');
          RegStateSetLiteral(rCF, LitA and 1);
        end
        else
          Result := False;
        if Result then
          EXIT;
      end;

      //A register optimisations which can trash flags
      if not PreserveCF and not PreserveOtherFlags then
      begin //Can we add or subtract another register?
        for R := rA to rL do
          if RegStateIsLiteral(R) then
          begin
            CalcValue := RegStateGetLiteral(rA) + RegStateGetLiteral(R);
            if Value = CalcValue and $ff then
            begin
              Opcode('add','a',CPURegStrings[R]); //1B/1M/4T
              RegStateSetUnknown(rFlags);
              RegStateSetLiteral(rCF, CalcValue and $100);
              EXIT(True);
            end;

            CalcValue := RegStateGetLiteral(rA) - RegStateGetLiteral(R);
            if Value = CalcValue and $ff then
            begin
              Opcode('sub',CPURegStrings[R]);
              RegStateSetUnknown(rFlags);
              RegStateSetLiteral(rCF, CalcValue and $100);
              EXIT(True);
            end;

            //Carry flag is set - we can try ADC and SBC
            if RegStateIsLiteral(rCF) and (RegStateGetLiteral(rCF) <> 0) then
            begin
              CalcValue := RegStateGetLiteral(rA) + RegStateGetLiteral(R) + 1;
              if Value = CalcValue and $ff then
              begin
                Opcode('adc','a',CPURegStrings[R]); //1B/1M/4T
                RegStateSetUnknown(rFlags);
                RegStateSetLiteral(rCF, CalcValue and $100);
                EXIT(True);
              end;

              CalcValue := RegStateGetLiteral(rA) - RegStateGetLiteral(R) - 1;
              if Value = CalcValue and $ff then
              begin
                Opcode('sbc','a',CPURegStrings[R]);
                RegStateSetUnknown(rFlags);
                RegStateSetLiteral(rCF, CalcValue and $100);
                EXIT(True);
              end;
            end;
          end;  ///Loop
      end;
    end
    //Set A to zero
    else if not PreserveOtherFlags then
      if (Value = 0) and not PreserveCF then
        begin
          Opcode('xor','a');
          RegStateSetUnknown(rFlags);
          RegStateSetLiteral(rCF, 0);
          EXIT(True);
        end;
  end;
end;

//Sub of LoadRegLiteral
//Attempt to find an optimisaed way to load the given value into the given register pair
function TryLoadRegPairOptimised(Reg: TCPUReg;Value: Integer;Options: TMoveOptionSet): Boolean;
var
  Test: Integer;
  LowIncDec: Boolean;
  HighIncDec: Boolean;
  RLow: TCPUReg;
  RHigh: TCPUReg;
begin
  //LD dd,nn is 3/3/10. We only want optimisations which are faster or smaller
  //LD r,n is 2/2/7 so it's not effective to only optimise one half and LD immediate the other
  //If we need to update both then BOTH must be optimised.
  //If we only need to update one then that can be treated as a single byte load
  //(and optimised or not as such)
  System.Assert(Reg in CPURegPairs);

  Result := False;

  if not (moPreserveHLDE in Options) then
    if ((Reg = rHL) and RegStateEqualsLiteral(rDE, Value)) or
      ((Reg = rDE) and RegStateEqualsLiteral(rHL, Value)) then
    begin
      Opcode('ex','hl','de');
      RegStateEXHLDE;
      EXIT(True);
    end;


  //Are high and/or low bytes already in registers?
  RLow := RegStateFindLiteral8(Value and $ff);
  RHigh := RegStateFindLiteral8((Value shr 8) and $ff);

  //Can we do a 16-bit INC or DEC? (we've already established that both bytes
  //need to change)
  if RegStateIsLiteral(Reg) then
    if Value = (RegStateGetLiteral(Reg) + 1) and $ffff then
    begin
      //This is the best option if:
      //  We need to preserve flags (can't use 8-bit INC) and
      //    We can't copy low byte from another register
      //  -or -
      //  We need to INC both bytes
      if ((RLow = rNone) and (moPreserveOtherFlags in Options)) or ((Value and $ff00) <> (RegStateGetLiteral(Reg) and $ff00)) then
      begin
        Opcode('inc',CPURegStrings[Reg]); //1/1/6
        EXIT(True);
      end;
    end
    else if Value = (RegStateGetLiteral(Reg) - 1) and $ffff then
      if ((RLow = rNone) and (moPreserveOtherFlags in Options)) or ((Value and $ff00) <> (RegStateGetLiteral(Reg) and $ff00)) then
      begin
        Opcode('dec',CPURegStrings[Reg]); //1/1/6
        EXIT(True);
      end;


  //Is the low byte value already in place?
  if RegStateEqualsLiteral(CPURegPairToLow[Reg], Value and $ff) then
  begin //If so optimised load the high byte
    GenLoadRegLiteral(CPURegPairToHigh[Reg], TImmValue.CreateInteger((Value shr 8) and $ff), Options);
    EXIT(True);
  end
    //Is the high byte value already in place?
  else if RegStateEqualsLiteral(CPURegPairToHigh[Reg], (Value shr 8) and $ff) then
  begin //If so optimised load the low byte
    GenLoadRegLiteral(CPURegPairToLow[Reg], TImmValue.CreateInteger(Value and $ff), Options);
    EXIT(True);
  end
  else  //Can both be optimised??
  begin
    LowIncDec := False; //Can we INC or DEC either byte?
    HighIncDec := False;
    if not (moPreserveOtherFlags in Options) then
    begin
      if RegStateIsLiteral(CPURegPairToLow[Reg]) then
      begin
        Test := RegStateGetLiteral(CPURegPairToLow[Reg]);
        LowIncDec := (lo(Value) = (Test + 1) and $ff) or (lo(Value) = (Test - 1) and $ff);
      end;
      if RegStateIsLiteral(CPURegPairToHigh[Reg]) then
      begin
        Test := RegStateGetLiteral(CPURegPairToHigh[Reg]);
        HighIncDec := (hi(Value)  = (Test + 1) and $ff) or (hi(Value) = (Test - 1) and $ff);
      end;
    end;

    //Edge case: we need to swap both halves!
    if (RHigh = CPURegPairToLow[Reg]) and (RLow = CPURegPairToHigh[Reg]) then
      EXIT(False);

    //Can we optimised both bytes?
    if ((RLow <> rNone) or LowIncDec) and ((RHigh <> rNone) or HighIncDec) then
    begin
      //If the new high byte is the old low byte we need to swap the load order
      if RHigh <> CPURegPairToLow[Reg] then
      begin
        if RLow <> rNone then
        begin
          OpLD(CPURegPairToLow[Reg], RLow);
          Result := True;
        end
        else
          Result := TryLoadReg8Optimised(CPURegPairToLow[Reg], Value and $ff, Options);

        if RHigh <> rNone then
        begin
          OpLD(CPURegPairToHigh[Reg], RHigh);
          Result := Result and True;
        end
        else
          Result := Result and TryLoadReg8Optimised(CPURegPairToHigh[Reg], (Value shr 8) and $ff, Options);
      end
      else  //Swapped order
      begin
        if RHigh <> rNone then
        begin
          OpLD(CPURegPairToHigh[Reg],RHigh);
          Result := True;
        end
        else
          Result := TryLoadReg8Optimised(CPURegPairToHigh[Reg], (Value shr 8) and $ff, Options);

        if RLow <> rNone then
        begin
          OpLD(CPURegPairToLow[Reg],RLow);
          Result := Result and True;
        end
        else
          Result := Result and TryLoadReg8Optimised(CPURegPairToLow[Reg], Value and $ff, Options);
      end;
    end;
  end;
end;

//Loads a literal value into any register, register pair or the Carry Flag
//Includes IX and IY
//If not true the operation failed (ie. one or more of the conditions specified
//in Options was not met. Ie. the load couldn't be completed without trashing
//something which needed to be kept)
procedure GenLoadRegLiteral(Reg: TCPUReg;const Value: TImmValue;Options: TMoveOptionSet);
var R: TCPUReg;
begin
  //If Reg aleady holds target value then EXIT
  if RegStateEqualsLiteral(Reg, Value.ToInteger) then
    EXIT;

  case Reg of
    rA,rB,rC,rD,rE,rH,rL:
    begin
      //If the literal value is already in a register...
      R := RegStateFindLiteral8(Value.ToInteger and $ff);
      if R <> rNone then
        //...then copy it
        OpLD(Reg,R) //1/1/4 (Bytes/M Cycles/T States)
      else
      begin
        //Look for an optimised way to load the value
        if not TryLoadReg8Optimised(Reg, Value.ToInteger, Options) then
          //Otherwise load the literal
          OpLD(Reg, Value) //2B/2M/7T
      end;
    end;
    rHL, rDE, rBC:
    begin
      //Can we do an optimised load?
      if not TryLoadRegPairOptimised(Reg, Value.Tointeger, Options) then
        //We've gotten here without loading either half, so load the pair together
        OpLD(Reg, Value)
    end;
    rIX,rIY:
      OpLD(Reg, Value);
    rCF:
    begin
      if Value.ToInteger <> 0 then
        Opcode('scf')
      else  //Set to 0
        if RegStateIsLiteral(rCF) then
        begin
          System.Assert(RegStateGetLiteral(rCF) <> 0, 'Setting to current value. Should have been filtered out already');
          Opcode('ccf');  //1/1/4
        end
        else if moPreserveOtherFlags in Options then
        begin
          Opcode('scf');  //1/1/4
          Opcode('ccf');  //1/1/4
        end
        else  //Can trash flags
        begin
          Opcode('and','a');  //1/1/4
          RegStateSetUnknown(rFlags);
        end;
    end
  else
    System.Assert(False, 'Unable to handle given register');
  end;

  RegStateSetLiteral(Reg, Value.ToInteger);
end;

//===========================REGISTER MOVING

//Generate code to move FromReg to ToReg
//Currently only allows 'main' registers (ABCDEHL)
//Can sign extend a value moving from an 8-bit to a 16-bit register. When doing so
//  the A register will be trashed (and the Flags register cannot be preserved)
procedure GenRegMove(FromReg, ToReg: TCPUReg;Signed: Boolean;Options: TMoveOptionSet);
begin
  System.Assert(FromReg <> ToReg);

  if FromReg in CPUReg8Bit then
  begin //From 8 bit
    if ToReg in CPUReg8Bit then
    begin //8-bit to 8-bit
      OpLD(ToReg, FromReg);
      RegStateCopy(ToReg, FromReg);
    end

    else if ToReg in [rBC, rHL, rDE] then
    begin //8-bit to 16-bit
      //Low byte. Ignore if we're just extending (FromReg is low of ToReg)
      if FromReg <> CPURegPairToLow[ToReg] then
      begin
        OpLD(CPURegPairToLow[ToReg], FromReg);
        RegStateCopy(CPURegPairToLow[ToReg], FromReg);
      end;

      //Sign extend?
      if Signed then  //Sign extend (if needed)
      begin
        if FromReg = rA then
          GenSignExtend(FromReg, CPURegPairToHigh[ToReg], Options)
        else  //TODO: Preserve A?
          GenSignExtend(CPURegPairToLow[ToReg], CPURegPairToHigh[ToReg], Options)
      end
      else
        //otherwise zero extend
        GenLoadRegLiteral(CPURegPairToHigh[ToReg],TImmValue.CreateInteger(0),Options);
    end
    else
      System.Assert(False);

  end
  else if (FromReg in [rHL, rDE]) and (ToReg in [rHL, rDE]) and not (moPreserveHLDE in Options) then
  begin //EX HL,DE - TODO: Do we need to preserve the other value?
        //If so we'll need to use move option below
    System.Assert(FromReg <> ToReg);
    Opcode('ex','hl','de');
    RegStateEXHLDE;
  end
  else if (FromReg in [rHL, rDE, rBC]) and (ToReg in [rHL, rDE, rBC]) then
  begin //16-bit load
    //TODO: From 16 bit to 8 bit
    //TODO: Validate (optional) if down-sizing the value
    System.Assert(ToReg in [rHL, rDE, rBC]);
    OpLD(CPURegPairToLow[ToReg],CPURegPairToLow[FromReg]);
    OpLD(CPURegPairToHigh[ToReg],CPURegPairToHigh[FromReg]);

    RegStateCopy(ToReg, FromReg);
  end
  else if (FromReg in (CPUReg16Bit + CPURegPairs)) and (ToReg in (CPUReg16Bit + CPURegPairs)) then
  begin
    OpPUSH(FromReg);
    OpPOP(ToReg);

    RegStateCopy(ToReg, FromReg);
  end
  else
    System.Assert(False, 'Sorry, unable to handle that register move');
end;

end.
