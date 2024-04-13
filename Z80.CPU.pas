unit Z80.CPU;

interface

type
  //This list details the CPU-level places where primitives can find input data
  //and place result data.
  //For a primitive this specifies the options available for register selection.
  //For ILData this specifies where a primitive must find or place data. I.e.
  //after primitive selection and register allocation has taken place.
  //Not all values are valid in all situtions (inputs, results, primitives, ILData)
  TCPUReg = (
    rNone,      //No parameter or unassigned
    rImm,       //Immediate (constant) value
    rIndirect,  //Indirect data, i.e. (HL)
    rOffset,    //Offset data, i.e. (IX+d)
    rP1,        //Data is output in the same register as param1
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
const CPURegRegs = [rA..rL,rHL..rIY,rZFA,rNZFA,rCPLA];
  CPURegFlags = [rZF,rNZF,rCF,rNCF,rCPLA];

const CPURegPairToLow: array[low(TCPUReg)..High(TCPUReg)] of TCPUReg = (
  rNone, rNone, rNone, rNone, rNone,
  rNone, rNone, rNone, rNone, rNone, rNone, rNone,
  rNone,
  rL, rE, rC, rNone, rNone,
  rNone, rNone, rNone, rNone,
  rNone, rNone, rNone, rNone);
  CPURegPairToHigh: array[low(TCPUReg)..High(TCPUReg)] of TCPUReg = (
  rNone, rNone, rNone, rNone, rNone,
  rNone, rNone, rNone, rNone, rNone, rNone, rNone,
  rNone,
  rH, rD, rB, rNone, rNone,
  rNone, rNone, rNone, rNone,
  rNone, rNone, rNone, rNone);
const
  //Mappings between register and register name
  CPUReg8ToChar: array[low(TCPUReg)..High(TCPUReg)] of Char = (
  #0,#0,#0,#0,#0,
  'a','b','c','d','e','h','l',
  #0,#0,#0,#0,#0,#0,
  #0,#0,#0,#0,#0,#0,#0,#0);
  CPURegPairToString: array[low(TCPUReg)..High(TCPUReg)] of String = (
  #0,#0,#0,#0,#0,
  #0,#0,#0,#0,#0,#0,#0,
  'af','hl','de','bc','ix','iy',
  #0,#0,#0,#0,#0,#0,#0,#0);
  //Low reg of a pair
  CPURegLowToChar: array[low(TCPUReg)..High(TCPUReg)] of Char = (
  #0,#0,#0,#0,#0,
  #0,#0,#0,#0,#0,#0,#0,
  #0,'l','e','c',#0,#0,
  #0,#0,#0,#0,#0,#0,#0,#0);
  //High reg of a pair
  CPURegHighToChar: array[low(TCPUReg)..High(TCPUReg)] of Char = (
  #0,#0,#0,#0,#0,
  #0,#0,#0,#0,#0,#0,#0,
  #0,'h','d','b',#0,#0,
  #0,#0,#0,#0,#0,#0,#0,#0);

implementation

end.
