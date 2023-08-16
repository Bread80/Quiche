//Import file for the Quiche compiler
//This file is the Low Kernal Jumpblock for the Amstrad CPC firmware

//Doesn't return
[PreservesAll]
procedure LOW_RESET_ENTRY; extern $0000;

//RST 1
//Takes inline target address
//Flags and registers in and out as per the target routine
[PreservesAll]
procedure LOW_LOW_JUMP; extern $0008;

//Flags and registers in and out as per the target routine
[PreservesAll]
procedure LOW_KL_LOW_PCHL(LowAddress: HL as Pointer); extern $000b;

//Flags and registers in and out as per the target routine
[PreservesAll]
procedure LOW_PCBC_INSTRUCTION(Address: BC as Pointer); extern $000e;

//RST 2
//Takes inline target address
//Flags and registers in and out as per the target routine,
//except IY which is preserved
//[Preserves IY]
procedure LOW_SIDE_CALL; extern $0010;

//RST 2
//Takes inline target address
//Flags and registers in and out as per the target routine,
//except IY which is preserved
//[Preserves IY]
procedure LOW_KL_SIDE_PCHL(SideAddress: HL as Pointer); extern $0013;

//Flags and registers in and out as per the target routine
[PreservesAll]
procedure LOW_PCDE_INSTRUCTION(Address: DE as Pointer); extern $0016;

//RST 3
//Takes inline far address
//Flags and registers in and out as per the target routine,
//except IY which is preserved
//[Preserves IY]
procedure LOW_FAR_CALL; extern $0018;

//Flags and registers in and out as per the target routine,
//except IY which is preserved
//[Preserves IY]
procedure LOW_KL_FAR_PCHL(RoutineToCall: HL as Pointer;ROMSelect: C); extern $001b;

//Flags and registers in and out as per the target routine.
[PreservesAll]
procedure LOW_PCHL_INSTRUCTION(Address: HL as Pointer); extern $001e;

//RST 4
[PreservesAll]
function LOW_RAM_LAM(Address: HL as Pointer): A; extern $0020;

//Flags and registers in and out as per the target routine,
//except IY which is preserved
//[Preserves IY]
procedure LOW_KL_FAR_ICALL(AddressToCall: HL as Pointer); extern $0023;

//RST 5
//Takes inline target address
//Flags and registers in and out as per the target routine.
[PreservesAll]
procedure LOW_FIRM_JUMP; extern $0028;

//RST 6
//Unknown
procedure LOW_USER_RESTART; extern $0030;

//RST 7
[PreservesAll]
procedure LOW_INTERRUPT_ENTRY; extern $0038;

[Corrupts AF, BC, DE, HL]
procedure LOW_EXT_INTERRUPT; extern $003b;
