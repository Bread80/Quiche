//Quiche import file for the Amstrad CPC firmware
//This file imports the Machine Pack routines

//For documentation see https://www.cpcwiki.eu/index.php/BIOS_Functions

//Never returns!
[PreservesAll]
procedure MC_BOOT_PROGRAM(ProgramLoader: HL as Pointer); call $bd13;

//Never returns!
[PreservesAll]
procedure MC_START_PROGRAM(EntryPoint: HL as Pointer;ROMSelect: C); call $bd16;

[PreservesAll]
procedure MC_WAIT_FLYBACK; call $bd19;

[Corrupts AF]
procedure MC_SET_MODE(Mode: A); call $bd1c;

[Corrupts AF]
procedure MC_SCREEN_OFFSET(ScreenBase: A;ScreenOffset: HL); call $bd1f;

[Corrupts AF]
procedure MC_CLEAR_INKS(InkVector: DE as Pointer); call $bd22;

[Corrupts AF]
procedure MC_SET_INKS(InkVector: DE as Pointer); call $bd25;

[Corrupts AF, BC, DE, HL]
procedure MC_RESET_PRINTER; call $bd28;

//Returns True if the character was sent okay
[Corrupts A, F]
function MC_PRINT_CHAR(Character: A as Char): CF; call $bd2b;

//Returns True if the port is busy
[Corrupts F]
function MC_BUSY_PRINTER: CF; call $bd2e;

[Corrupts A, F]
procedure MC_SEND_PRINTER(Character: A as Char); call $bd31;

[Corrupts AF, BC]
procedure MC_SOUND_REGISTER(Register: A;Data: C); call $bd34;

//Returns True if the table is okay
[Corrupts A, BC, DE, HL, F]
function MC_PRINT_TRANSLATION(Table: HL as Pointer): CF; call $bd58;
