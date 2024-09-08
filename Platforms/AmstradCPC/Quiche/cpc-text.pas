//Import file for Quiche compiler
//for the Amstrad CPC text VDU firmware routines

//For documentation see https://www.cpcwiki.eu/index.php/BIOS_Functions

//Logical co-rodinates are 1-based, Physical co-ordinates are zero based
[Corrupts AF, BC, DE, HL]
procedure TXT_INITIALISE; call $bb4e;

[Corrupts AF, BC, DE, HL]
procedure TXT_RESET; call $bb51;

[Corrupts AF]
procedure TXT_VDU_ENABLE; call $bb54;

[Corrupts AF]
procedure TXT_VDU_DISABLE; call $bb57;

[PreservesAll]
procedure TXT_OUTPUT(Character: A as Char); call $bb5a;

[Corrupts AF, BC, DE, HL]
procedure TXT_WR_CHAR(Character: A as Char); call $bb5d;

//Retrusn true if character found
[Corrupts F]
function TXT_RD_CHAR(out CharacterRead: A): CF; call $bb60;

//A is non-zero to turn on, zero to turn off
[Corrupts AF]
procedure TXT_SET_GRAPHIC(SetOn: A as Boolean); call $bb63;

[Corrupts AF, BC, DE, HL]
procedure TXT_WIN_ENABLE(PhysicalColumn1: H;PhysicalColumn2: D;PhysicalRow1: L;PhysicalRow2: E); call $bb66;

//Returns true if window covers less than the whole screen
[Corrupts A]
function TXT_GET_WINDOW(out LeftmostColumn: H;out RightmostColumn: D;out TopmostRow: L;out BottommostRow: E): CF; call $bb69;

[Corrupts AF, BC, DE, HL]
procedure TXT_CLEAR_WINDOW; call $bb6c;

[Corrupts AF, HL]
procedure TXT_SET_COLUMN(LogicalColumn: A); call $bb6f;

[Corrupts AF, HL]
procedure TXT_SET_ROW(LogicalRow: A); call $bb72;

[Corrupts AF, HL]
procedure TXT_SET_CURSOR(LogicalColumn: H;LogicalRow: L); call $bb75;

[Corrupts F]
procedure TXT_GET_CURSOR(out LogicalCursorColumn: H;out LogicalCursorRow: L;out RollCount: A); call $bb78;

[Corrupts AF]
procedure TXT_CUR_ENABLE; call $bb7b;

[Corrupts AF]
procedure TXT_CUR_DISABLE; call $bb7e;

[PreservesAll]
procedure TXT_CUR_ON; call $bb81;

[PreservesAll]
procedure TXT_CUR_OFF; call $bb84;

//Returns True if printing at the position would *not* cause the window to roll
//B returns #FF for roll up, #00 for roll down (corrupt if no roll needed)
[Corrupts A, F]
function TXT_VALIDATE(LogicalColumn: H;LogicalRow: L;out RollUp: B as FFBoolean;out LogicalColumnOut: H;out LogicalRowOut: L): CF; call $bb87;

[Corrupts AF]
procedure TXT_PLACE_CURSOR; call $bb8a;

[Corrupts AF]
procedure TXT_REMOVE_CURSOR; call $bb8d;

[Corrupts AF, HL]
procedure TXT_SET_PEN(Ink: A); call $bb90;

[Corrupts F]
function TXT_GET_PEN: A; call $bb93;

[Corrupts AF, HL]
procedure TXT_SET_PAPER(Ink: A); call $bb96;

[Corrupts F]
function TXT_GET_PAPER: A; call $bb99;

[Corrupts AF, HL]
procedure TXT_INVERSE; call $bb9c;

//A is zero for opaque mode, non-zero for transparent mode
[Corrupts AF, HL]
procedure TXT_SET_BACK(Transparent: A as Boolean); call $bb9f;

//TODO: NZBoolean not yet defined
//Returns A as zero for oqaque mode, non-zero for transparent mode
//I.e returns true for transparent mode
//[Corrupts DE, HL, F]
//function TXT_GET_BACK: A as NZBoolean; call $bba2;

//Returns true if the matrix is in the user defined matrix table
[Corrupts A, F]
function TXT_GET_MATRIX(Character: A as Char;out Matrix: HL as Pointer): CF; call $bba5;

//Returns true if the character is user definable
[Corrupts A, BC, DE, HL, F]
function TXT_SET_MATRIX(Character: A as Char;Matrix: HL as Pointer): CF; call $bba8;

//Returns true if there was a user defined matrix table beforee
[Corrupts BC, DE, F]
function TXT_SET_M_TABLE(FirstCharacter: DE;NewTable: HL as Pointer;out OldFirstCharacter: A;out OldTable: HL as Pointer): CF; call $bbab;

//Returns true if there is a user defined matrix table
[Corrupts F]
function TXT_GET_M_TABLE(out FirstCharacter: A;out Table: HL as Pointer): CF; call $bbae;

//Returns address of control code table
[PreservesAll]
function TXT_GET_CONTROLS: HL as Pointer; call $bbb1;

[Corrupts HL, F]
function TXT_STR_SELECT(Stream: A): A; call $bbb4;

[Corrupts AF, BC, DE, HL]
procedure TXT_SWAP_STREAMS(StreamNumber: B;AnotherStreamNumber: C); call $bbb7;

//Returns text VDU state (bitwise value)
[Corrupts F]
function TXT_ASK_STATE: A; call $bd40;
