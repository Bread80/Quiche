//Quiche import file for the Amstrad CPC firmware
//This file imports the graphics routines

//For documentation see https://www.cpcwiki.eu/index.php/BIOS_Functions

//User co-ordinates are relative to the user origin (i.e. the graphics window)
//Standard co-ordinates are relative to the bottom-left of the screen (0,0)


[Corrupts AF, BC, DE, HL]
procedure GRA_INITIALISE; call $bbba;

[Corrupts AF, BC, DE, HL]
procedure GRA_RESET; call $bbbd;

[Corrupts AF, BC, DE, HL]
procedure GRA_MOVE_ABSOLUTE(UserXCoordinate: DE;UserYCoordinate: HL); call $bbc0;

[Corrupts AF, BC, DE, HL]
procedure GRA_MOVE_RELATIVE(SignedXOffset: DE as Integer;SignedYOffset: HL as Integer); call $bbc3;

[Corrupts AF]
procedure GRA_ASK_CURSOR(out UserXCoordinate: DE;out UserYCoordinate: HL); call $bbc6;

[Corrupts AF, BC, DE, HL]
procedure GRA_SET_ORIGIN(StandardXCoordinate: DE;StandardYCoordinate: HL); call $bbc9;

[PreservesAll]
procedure GRA_GET_ORIGIN(out StandardXCoordinate: DE;out StandardYCoordinate: HL); call $bbcc;

[Corrupts AF, BC, DE, HL]
procedure GRA_WIN_WIDTH(StandardXCoordinate1: DE;StandardXCoordinate2: HL); call $bbcf;

[Corrupts AF, BC, DE, HL]
procedure GRA_WIN_HEIGHT(StandardYCoordinate1: DE;StandardYCoordinate2: HL); call $bbd2;

[Corrupts AF]
procedure GRA_GET_W_WIDTH(out StandardXCoordinate1: DE;out StandardXCoordinate2: HL); call $bbd5;

[Corrupts AF]
procedure GRA_GET_W_HEIGHT(out StandardYCoordinate1: DE;out StandardYCoordinate2: HL); call $bbd8;

[Corrupts AF, BC, DE, HL]
procedure GRA_CLEAR_WINDOW; call $bbdb;

[Corrupts AF]
procedure GRA_SET_PEN(Ink: A); call $bbde;

[Corrupts F]
function GRA_GET_PEN: A; call $bbe1;

[Corrupts AF]
procedure GRA_SET_PAPER(Ink: A); call $bbe4;

[Corrupts F]
function GRA_GET_PAPER: A; call $bbe7;

[Corrupts AF, BC, DE, HL]
procedure GRA_PLOT_ABSOLUTE(UserXCoordinate: DE;UserYCoordinate: HL); call $bbea;

[Corrupts AF, BC, DE, HL]
procedure GRA_PLOT_RELATIVE(SignedXOffset: DE as Integer;SignedYOffset: HL as Integer); call $bbed;

[Corrupts BC, DE, HL, F]
function GRA_TEST_ABSOLUTE(UserXCoordinate: DE;UserYCoordinate: HL): A; call $bbf0;

[Corrupts BC, DE, HL, F]
function GRA_TEST_RELATIVE(SignedXOffset: DE as Integer;SignedYOffset: HL as Integer): A; call $bbf3;

[Corrupts AF, BC, DE, HL]
procedure GRA_LINE_ABSOLUTE(UserXCoordinate: DE;UserYCoordinate: HL); call $bbf6;

[Corrupts AF, BC, DE, HL]
procedure GRA_LINE_RELATIVE(SignedXOffset: DE as Integer;SignedYOffset: HL as Integer); call $bbf9;

[Corrupts AF, BC, DE, HL]
procedure GRA_WR_CHAR(Character: A as Char); call $bbfc;

[Corrupts AF, BC, DE, HL]
procedure GRA_DEFAULT; call $bd43;

//If A is zero sets opaque mode, if A is non-zero sets transparent mode
//I.e. True is transparent
[PreservesAll]
procedure GRA_SET_BACK(Transparent: A as Boolean); call $bd46;

//If A is zero (false) first pixel will not be plotted, 
//if A is non-zero (true) first pixel will be plotted
[PreservesAll]
procedure GRA_SET_FIRST(Plot: A as Boolean); call $bd49;

[PreservesAll]
procedure GRA_SET_LINE_MASK(LineMask: A); call $bd4c;

[Corrupts AF]
procedure GRA_FROM_USER(UserXCoordinate: DE;UserYCoordinate: HL;out BaseXCoordinate: DE;out BaseYCoordinate: HL); call $bd4f;

[Corrupts A, BC, DE, HL, F]
function GRA_FILL(UnencodedInk: A;Buffer: HL as Pointer;BufferLength: DE): CF; call $bd52;

