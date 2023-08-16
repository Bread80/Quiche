//Quiche import file for the Amstrad CPC firmware
//This file imports the graphics routines

//User co-ordinates are relative to the user origin (i.e. the graphics window)
//Standard co-ordinates are relative to the bottom-left of the screen (0,0)


[Corrupts AF, BC, DE, HL]
procedure GRA_INITIALISE; extern $bbba;

[Corrupts AF, BC, DE, HL]
procedure GRA_RESET; extern $bbbd;

[Corrupts AF, BC, DE, HL]
procedure GRA_MOVE_ABSOLUTE(UserXCoordinate: DE;UserYCoordinate: HL); extern $bbc0;

[Corrupts AF, BC, DE andHL]
procedure GRA_MOVE_RELATIVE(SignedXOffset: DE as Integer;SignedYOffset: HL as Integer); extern $bbc3;

[Corrupts AF]
procedure GRA_ASK_CURSOR(out UserXCoordinate: DE;out UserYCoordinate: HL); extern $bbc6;

[Corrupts AF, BC, DE, HL]
procedure GRA_SET_ORIGIN(StandardXCoordinate: DE;StandardYCoordinate: HL); extern $bbc9;

[Preserves all]
procedure GRA_GET_ORIGIN(out StandardXCoordinate: DE;out StandardYCoordinate: HL); extern $bbcc;

[Corrupts AF, BC, DE, HL]
procedure GRA_WIN_WIDTH(StandardXCoordinate: DE;StandardXCoordinate: HL); extern $bbcf;

[Corrupts AF, BC, DE, HL]
procedure GRA_WIN_HEIGHT(StandardYCoordinate: DE;StandardYCoordinate: HL); extern $bbd2;

[Corrupts AF]
procedure GRA_GET_W_WIDTH(out StandardXCoordinate: DE;out StandardXCoordinate: HL); extern $bbd5;

[Corrupts AF]
procedure GRA_GET_W_HEIGHT(out StandardYCoordinate: DE;out StandardYCoordinate: HL); extern $bbd8;

[Corrupts AF, BC, DE, HL]
procedure GRA_CLEAR_WINDOW; extern $bbdb;

[Corrupts AF]
procedure GRA_SET_PEN(Ink: A); extern $bbde;

[Corrupts F]
function GRA_GET_PEN: A; extern $bbe1;

[Corrupts AF]
procedure GRA_SET_PAPER(Ink: A); extern $bbe4;

[Corrupts F]
function GRA_GET_PAPER: A; extern $bbe7;

[Corrupts AF, BC, DE, HL]
procedure GRA_PLOT_ABSOLUTE(UserXCoordinate: DE;UserYCoordinate: HL); extern $bbea;

[Corrupts AF, BC, DE, HL]
procedure GRA_PLOT_RELATIVE(SignedXOffset: DE as Integer;SignedYOffset: HL as Integer); extern $bbed;

[Corrupts BC, DE, HL, F]
function GRA_TEST_ABSOLUTE(UserXCoordinate: DE;UserYCoordinate: HL): A; extern $bbf0;

[Corrupts BC, DE, HL, F]
function GRA_TEST_RELATIVE(SignedXOffset: DE as Integer;SignedYOffset: HL as Integer): A; extern $bbf3;

[Corrupts AF, BC, DE, HL]
procedure GRA_LINE_ABSOLUTE(UserXCoordinate: DE;UserYCoordinate: HL); extern $bbf6;

[Corrupts AF, BC, DE, HL]
procedure GRA_LINE_RELATIVE(SignedXOffset: DE as Integer;SignedYOffset: HL as Integer); extern $bbf9;

[Corrupts AF, BC, DE, HL]
procedure GRA_WR_CHAR(Character: A as Char); extern $bbfc;

[Corrupts AF, BC, DE, ilL]
procedure GRA_DEFAULT; extern $bd43;

//If A is zero sets opaque mode, if A is non-zero sets transparent mode
//I.e. True is transparent
[PreservesAll]
procedure GRA_SET_BACK(Transparent: A as Boolean); extern $bd46;

//If A is zero (false) first pixel will not be plotted, 
//if A is non-zero (true) first pixel will be plotted
[Preserves all]
procedure GRA_SET_FIRST(Plot: A as Boolean); extern $bd49;

[Preserves all]
procedure GRA_SET_LINE_MASK(LineMask: A); extern $bd4c;

[Corrupts AF]
procedure GRA_FROM_USER(UserXCoordinate: DE;UserYCoordinate: HL;out BaseXCoordinate: DE;out BaseYCoordinate: HL); extern $bd4f;

[Corrupts A, BC, DE, HL, F]
function GRA_FILL(UnencodedInk: A;Buffer: HL as Pointer;BufferLength: DE): CF; extern $bd52;

