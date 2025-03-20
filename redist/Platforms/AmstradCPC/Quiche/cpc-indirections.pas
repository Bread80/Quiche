//Import file for the Quiche compiler
//Imports the indirections for the Amstrad CPC firmware

//For documentation see https://www.cpcwiki.eu/index.php/BIOS_Functions

[Corrupts AF]
procedure IND_TXT_DRAW_CURSOR; call $bdcd;

[Corrupts AF]
procedure IND_TXT_UNDRAW_CURSOR; call $bdd0;

[Corrupts AF, BC, DE, HL]
procedure IND_TXT_WRITE_CHAR(Character: A as Char;PhysicalColumn: H;PhysicalRow: L); call $bdd3;

//Returns True if a character was found
[Corrupts BC, DE, HL, F]
function IND_TXT_UNWRITE(PhysicalColumn: H;PhysicalRow: L;out CharacterRead: A): CF; call $bdd6;

[Corrupts AF, BC, DE, HL]
procedure IND_TXT_OUT_ACTION(CharacterOrCode: A); call $bdd9;

[Corrupts AF, BC, DE, HL]
procedure IND_GRA_PLOT(UserXCoordinate: DE;UserYCoordinate: HL); call $bddc;

[Corrupts BC, DE, HL, F]
function IND_GRA_TEST(UserXCoordinate: DE;UserYCoordinate: HL): A; call $bddf;

[Corrupts AF, BC, DE, HL]
procedure IND_GRA_LINE(UserXCoordinate: DE;UserYCoordinate: HL); call $bde2;

[Corrupts F]
function IND_SCR_READ(PixelAddress: HL as Pointer;PixelMask: C): A; call $bde5;

[Corrupts AF]
procedure IND_SCR_WRITE(PixelAddress: HL as Pointer;PixelMask: C;EncodedInk: B); call $bde8;

[Corrupts AF, BC, DE, HL]
procedure IND_SCR_MODE_CLEAR; call $bdeb;

[Corrupts AF, HL]
procedure IND_KM_TEST_BREAK(ShiftAndControlKeyStates: C); call $bdee;

//Returns True if the character was sent okay
[Corrupts A, BC]
function IND_MC_WAIT_PRINTER(Character: A as Char): CF; call $bdf1;
