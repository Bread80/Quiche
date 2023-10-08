//Import file for the Quiche compiler
//Imports the indirections for the Amstrad CPC firmware

[Corrupts AF]
procedure IND_TXT_DRAW_CURSOR; extern $bdcd;

[Corrupts AF]
procedure IND_TXT_UNDRAW_CURSOR; extern $bdd0;

[Corrupts AF, BC, DE, HL]
procedure IND_TXT_WRITE_CHAR(Character: A as Char;PhysicalColumn: H;PhysicalRow: L); extern $bdd3;

//Returns True if a character was found
[Corrupts BC, DE, HL, F]
function IND_TXT_UNWRITE(PhysicalColumn: H;PhysicalRow: L;out CharacterRead: A): CF; extern $bdd6;

[Corrupts AF, BC, DE, HL]
procedure IND_TXT_OUT_ACTION(CharacterOrCode: A); extern $bdd9;

[Corrupts AF, BC, DE, HL]
procedure IND_GRA_PLOT(UserXCoordinate: DE;UserYCoordinate: HL); extern $bddc;

[Corrupts BC, DE, HL, F]
function IND_GRA_TEST(UserXCoordinate: DE;UserYCoordinate: HL): A; extern $bddf;

[Corrupts AF, BC, DE, HL]
procedure IND_GRA_LINE(UserXCoordinate: DE;UserYCoordinate: HL); extern $bde2;

[Corrupts F]
function IND_SCR_READ(PixelAddress: HL as Pointer;PixelMask: C): A; extern $bde5;

[Corrupts AF]
procedure IND_SCR_WRITE(PixelAddress: HL as Pointer;PixelMask: C;EncodedInk: B); extern $bde8;

[Corrupts AF, BC, DE, HL]
procedure IND_SCR_MODE_CLEAR; extern $bdeb;

[Corrupts AF, HL]
procedure IND_KM_TEST_BREAK(ShiftAndControlKeyStates: C); extern $bdee;

//Returns True if the character was sent okay
[Corrupts A, BC]
function IND_MC_WAIT_PRINTER(Character: A as Char): CF; extern $bdf1;
