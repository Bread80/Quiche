//Quiche import for Amstrad CPC firmware routiones
//Imports the Screen Pack

//For documentation see https://www.cpcwiki.eu/index.php/BIOS_Functions

[Corrupts AF, BC, DE, HL]
procedure SCR_INITIALISE; call $bbff;

[Corrupts AF, BC, DE, HL]
procedure SCR_RESET; call $bc02;

[Corrupts AF, HL]
procedure SCR_SET_OFFSET(Offset: HL); call $bc05;

[Corrupts AF, HL]
procedure SCR_SET_BASE(MoreSignificantByte: A); call $bc08;

[Corrupts F]
procedure SCR_GET_LOCATION(out MoreSignificantByte: A;out Offset: HL); call $bc0b;

[Corrupts AF, BC, DE, HL]
procedure SCR_SET_MODE(Mode: A); call $bc0e;

//Returns true if in mode 0
//Returns Mode1 true if mode 1
//Returns false and Mode1 false if mode 2
[Corrupts F]
function SCR_GET_MODE(out Mode1: ZF;out Mode: A): CF; call $bc11;

[Corrupts AF, BC, DE, HL]
procedure SCR_CLEAR; call $bc14;

[Corrupts AF]
procedure SCR_CHAR_LIMITS(out PhysicalLastColumn: B;out PhysicalLastRow: C); call $bc17;

[Corrupts AF]
procedure SCR_CHAR_POSITION(PhysicalCharacterColumn: H;PhysicalCharacterRow: L;out TopLeftCornerOfTheCharacter: HL as Pointer;out ByteWidth: B); call $bc1a;

[Corrupts AF, DE]
procedure SCR_DOT_POSITION(BaseXCoordinate: DE;BaseYCoordinate: HL;out PixelAddress: HL as Pointer;out PixelMask: C;out PixelsPerByteLessOne: B); call $bc1d;

[Corrupts AF]
function SCR_NEXT_BYTE(ScreenAddress: HL as Pointer): HL as Pointer; call $bc20;

[Corrupts AF]
function SCR_PREV_BYTE(ScreenAddress: HL as Pointer): HL as Pointer; call $bc23;

[Corrupts AF]
function SCR_NEXT_LINE(ScreenAddress: HL as Pointer): HL as Pointer; call $bc26;

[Corrupts AF]
function SCR_PREV_LINE(ScreenAddress: HL as Pointer): HL as Pointer; call $bc29;

[Corrupts F]
function SCR_INK_ENCODE(InkNumber: A): A; call $bc2c;

[Corrupts F]
function SCR_INK_DECODE(EncodedInk: A): A; call $bc2f;

[Corrupts AF, BC, DE, HL]
procedure SCR_SET_INK(InkNumber: A;FirstColour: B;SecondColour: C); call $bc32;

[Corrupts AF, DE, HL]
procedure SCR_GET_INK(InkNumber: A;out FirstColour: B;out SecondColour: C); call $bc35;

[Corrupts AF, BC, DE, HL]
procedure SCR_SET_BORDER(FirstColour: B;SecondColour: C); call $bc38;

[Corrupts AF, DE, HL]
procedure SCR_GET_BORDER(out FirstColour: B;out SecondColour: C); call $bc3b;

[Corrupts AF, HL]
procedure SCR_SET_FLASHING(Period1: H;Period2: L); call $bc3e;

[Corrupts AF]
procedure SCR_GET_FLASHING(out Period1: H;out Period2: L); call $bc41;

[Corrupts AF, BC, DE, HL]
procedure SCR_FILL_BOX(EncodedInk: A;PhysicalLeftColumn: H;PhysicalRightColumn: D;PhysicalTopRow: L;PhysicalBottomRow: E); call $bc44;

[Corrupts AF, BC, DE, HL]
procedure SCR_FLOOD_BOX(EncodedInk: C;TopLeftAddress: HL as Pointer;Width: D;Height: E); call $bc47;

[Corrupts AF, BC, DE, HL]
procedure SCR_CHAR_INVERT(EncodedInk: B;AnotherEncodedInk: C;PhysicalCharacterColumn: H;PhysicalCharacterRow: L); call $bc4a;

//B is non-zero to roll up, zero to roll down
//i.e. True to roll up, False to roll down
[Corrupts AF, BC, DE, HL]
procedure SCR_HW_ROLL(RollUp: B as Boolean;EncodedInk: A); call $bc4d;

//B is non-zero to roll up, zero to roll down
//i.e. True to roll up, False to roll down
[Corrupts AF, BC, DE, HL]
procedure SCR_SW_ROLL(RollUp: B as Boolean;EncodedInk: A;PhysicalLeftColumn: H;PhysicalRightColumn: D;PhysicalTopRow: L;PhysicalBottomRow: E); call $bc50;

[Corrupts AF, BC, DE, HL]
procedure SCR_UNPACK(Matrix: HL as Pointer;AreaToUnpackInto: DE as Pointer); call $bc53;

[Corrupts AF, BC, DE, HL]
procedure SCR_REPACK(EncodedInk: A;PhysicalCharacterColumn: H;PhysicalCharacterRow: L;AreaToConstructTheMatrixIn: DE as Pointer); call $bc56;

//0=Overwrite, 1=XOR, 2=AND, 3=OR
[Corrupts AF, BC, DE, HL]
procedure SCR_ACCESS(WriteMode: A); call $bc59;

[Corrupts AF]
procedure SCR_PIXELS(EncodedInk: B;PixelMask: C;PixelAddress: HL as Pointer); call $bc5c;

[Corrupts AF, BC, DE, HL]
procedure SCR_HORIZONTAL(EncodedInk: A;BaseXStart: DE;BaseXEnd: BC;BaseYCoordinate: HL); call $bc5f;

[Corrupts AF, BC, DE, HL]
procedure SCR_VERTICAL(EncodedInk: A;BaseXCoordinate: DE;BaseYStart: HL;BaseYEnd: BC); call $bc62;

[Corrupts F]
procedure SCR_SET_POSITION(ScreenBase: A;ScreenOffset: HL;out ScreenBaseMaskedAsRequired: A;out ScreenOffsetMaskedAsRequired: HL); call $bd55;
