//Import file for the Quiche compiler
//This file is the Low Kernal Jumpblock for the Amstrad CPC firmware
//Note: Many of these routines require inline data or use the IY register
//and are therefore ususable from Quiche

//For documentation see https://www.cpcwiki.eu/index.php/BIOS_Functions

//Doesn't return
[PreservesAll]
procedure LOW_RESET_ENTRY; rst 0;

//RST 1
//Takes inline target address
//Flags and registers in and out as per the target routine
[PreservesAll]
procedure LOW_LOW_JUMP; rst 1;

//Flags and registers in and out as per the target routine
[PreservesAll]
procedure LOW_KL_LOW_PCHL(LowAddress: HL as Pointer); call $000b;

//Flags and registers in and out as per the target routine
[PreservesAll]
procedure LOW_PCBC_INSTRUCTION(Address: BC as Pointer); call $000e;

//RST 2
//Takes inline target address
//Flags and registers in and out as per the target routine,
//except IY which is preserved
//[Preserves IY]
procedure LOW_SIDE_CALL; rst 2;

//Takes inline target address
//Flags and registers in and out as per the target routine,
//except IY which is preserved
//[Preserves IY]
procedure LOW_KL_SIDE_PCHL(SideAddress: HL as Pointer); call $0013;

//Flags and registers in and out as per the target routine
[PreservesAll]
procedure LOW_PCDE_INSTRUCTION(Address: DE as Pointer); call $0016;

//RST 3
//Takes inline far address
//Flags and registers in and out as per the target routine,
//except IY which is preserved
//[Preserves IY]
procedure LOW_FAR_CALL; rst 3;

//Flags and registers in and out as per the target routine,
//except IY which is preserved
//[Preserves IY]
procedure LOW_KL_FAR_PCHL(RoutineToCall: HL as Pointer;ROMSelect: C); call $001b;

//Flags and registers in and out as per the target routine.
[PreservesAll]
procedure LOW_PCHL_INSTRUCTION(Address: HL as Pointer); call $001e;

//RST 4
[PreservesAll]
function LOW_RAM_LAM(Address: HL as Pointer): A; rst 4;

//Flags and registers in and out as per the target routine,
//except IY which is preserved
//[Preserves IY]
procedure LOW_KL_FAR_ICALL(AddressToCall: HL as Pointer); call $0023;

//RST 5
//Takes inline target address
//Flags and registers in and out as per the target routine.
[PreservesAll]
procedure LOW_FIRM_JUMP; rst 5;

//RST 6
//Unknown
procedure LOW_USER_RESTART; rst 6;

//RST 7
[PreservesAll]
procedure LOW_INTERRUPT_ENTRY; rst 7;

[Corrupts AF, BC, DE, HL]
procedure LOW_EXT_INTERRUPT; call $003b;
