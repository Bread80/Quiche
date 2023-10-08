//Quiche import file for the Amstrad CPC firmware
//This file imports the Machine Pack routines

//Never returns!
[PreservesAll]
procedure MC_BOOT_PROGRAM(ProgramLoader: HL as Pointer); extern $bd13;

//Never returns!
[PreservesAll]
procedure MC_START_PROGRAM(EntryPoint: HL as Pointer;ROMSelect: C); extern $bd16;

[PreservesAll]
procedure MC_WAIT_FLYBACK; extern $bd19;

[Corrupts AF]
procedure MC_SET_MODE(Mode: A); extern $bd1c;

[Corrupts AF]
procedure MC_SCREEN_OFFSET(ScreenBase: A;ScreenOffset: HL); extern $bd1f;

[Corrupts AF]
procedure MC_CLEAR_INKS(InkVector: DE as Pointer); extern $bd22;

[Corrupts AF]
procedure MC_SET_INKS(InkVector: DE as Pointer); extern $bd25;

[Corrupts AF, BC, DE, HL]
procedure MC_RESET_PRINTER; extern $bd28;

//Returns True if the character was sent okay
[Corrupts A, F]
function MC_PRINT_CHAR(Character: A as Char): CF; extern $bd2b;

//Returns True if the port is busy
[Corrupts F]
function MC_BUSY_PRINTER: CF; extern $bd2e;

[Corrupts A, F]
procedure MC_SEND_PRINTER(Character: A as Char); extern $bd31;

[Corrupts AF, BC]
procedure MC_SOUND_REGISTER(Register: A;Data: C); extern $bd34;

//Returns True if the table is okay
[Corrupts A, BC, DE, HL, F]
function MC_PRINT_TRANSLATION(Table: HL as Pointer): CF; extern $bd58;
