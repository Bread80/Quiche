(*
Algos represent small pieces of code which handle, for example, loading, storing,
or moving data between memory and registers, extending values, range checking,
and overflow checking values.

Enumerating the available library of Algos allows the code generator to determine
the exact size and execution time of the code which will be generated, and allows
it to account for and mitigate for any register corruption or other side-effects
which may happen as a result of a piece of code.

Essentially, Primitives enumerate operations which have meaning to the programmer
whereas Algo enumerated operations which the compiler needs to be able generate
alongside Primitives to enable the final code to function and be efficient.
*)
unit Z80.Algos;

interface

type TAlgo = (
  agUnspecified,  //Error if this gets to the end...
  agNone,     //Do nothing

//NOTE: Items preceded with {SP} have had a StateProc added.
//This annotation will be removed once all routines which require an SP have one

  //Algos for sourcing data from register or loading from memory
//(8 bit only!)  saImmediate,//Immediate data encoded into primitive (instruction)
  //Load register from immediate data
  laImm8,       //8-bit
  agImm16,      //16-bit (register pair)
  laImmXY,      //IX or IY
  laSCF,        //Set the carry flag
  laClearCF,    //Clear carry flag
  //TODO: Multiple optimisation strategies!
//  saCCF,

  //Load register from variable
{SP}  laStaticHL,   //LD HL,(nnnn) - faster/smaller than saStatic16
{SP}  laStatic16,   //LD rp,(nnnn)  - from static variable
{SP}  laStack16,    //LD rpl(IX+dl):LD rph,(IX+dh) - From stack variable
//  saRegister, //From registeers. See TMoveAlgo
//  saPop,      //POP rp        - from stack

  //Algos for moving data between registers
  maPreserve,   //Retain in existing register
//  maFrom8Bit,   //LD r,r'
{SP}  maFromPair,   //LD r1low,r2low:LD r1high,r2high
//  maPushXYPop,  //PUSH [IX/IY]:POP <pair>
//  maPushPopXY,  //PUSH <pair>:POP [IX/IY]
//  maPushXYPopXY,//PUSH [IX/IY]:POP [IX/IY]

  //Exchanges (mutual swaps between registers)
{SP}  agExchangedElsewhere, //We don't want to generate exchange code twice, so one
                        //side will be set to this which generates nothing
{SP}  agEXHLDE,     //EX HL,DE

  //Algos for storing data to memory (variable)
  saStaticHL,   //LD (nnnn),HL  - faster/smaller than daStatic16
  saStatic16,   //LD (nnnn),rp  - to static variable
  saStack16,  //LD (IX+dl),rpl:LD (IX+dh),rph - To stack variable
//  daPush      //PUSH rp       - to stack

  //Overflow routines
  agCOverflow,  //If Carry set
  agNCOverflow, //If Carry clear
  agNZOverflow, //In Zero clear
  agPOverflow,  //If Positive
  agMOverflow,  //If Megative
  agPEOverflow//, //If Parity Even
//agHRegNZOverflow, //If H Register is non-zero (requires StateProc)

  );
  TAlgoSet = set of TAlgo;

implementation

end.
