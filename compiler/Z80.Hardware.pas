(*
Data defining the Z80 hardware (registers, flags etc)
*)
unit Z80.Hardware;

interface
uses Def.QTypes;

type
  //Where is the parameter value stored (Location)
  //(Used by Lib.Fragments and CleverPuppy)
  TCodeProcLoc = (
    plNone,       //No such paramater (or no return value)
    plRegister, //Value will be consumed from/be output in a register
    //The remaining types are used where a primitive can directly consume/write to
    //a location other than a CPU register
    plImmediate,  //The primitive can directly consume an immediate/literal, eg. LD r,n. Not valid as a result.
    plStaticVar,  //The primitive can read from/write to a static variable, eg. LD a,(nn)
    plStackVar);  //The primitive can read from/write to a stack variable , eg. INC (IX+d)

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

function CPURegSetToString(Regs: TCPURegSet): String;

//Returns the number of elements in the set
function RegSetCount(Regs: TCPURegSet): Integer;

implementation
uses SysUtils;

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

function CPURegSetToString(Regs: TCPURegSet): String;
var Reg: TCPUReg;
begin
  Result := '';
  for Reg in Regs do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + CPURegStrings[Reg];
  end;
  Result := '[' + Result.ToUpper + ']'
end;

function RegSetCount(Regs: TCPURegSet): Integer;
var Reg: TCPUReg;
begin
  Result := 0;
  for Reg in Regs do
    inc(Result);
end;

end.
