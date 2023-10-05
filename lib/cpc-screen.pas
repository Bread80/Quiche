//Quiche import for Amstrad CPC firmware routiones
//Imports the Screen Pack

[Corrupts AF, BC, DE, HL]
procedure SCR_INITIALISE; extern $bbff;

[Corrupts AF, BC, DE, HL]
procedure SCR_RESET; extern $bc02;

[Corrupts AF, HL]
procedure SCR_SET_OFFSET(Offset: HL); extern $bc05;

[Corrupts AF, HL]
procedure SCR_SET_BASE(MoreSignificantByte: A); extern $bc08;

[Corrupts F]
procedure SCR_GET_LOCATION(out MoreSignificantByte: A;out Offset: HL); extern $bc0b;

[Corrupts AF, BC, DE, HL]
procedure SCR_SET_MODE(Mode: A); extern $bc0e;

//Returns true if in mode 0
//Returns Mode1 true if mode 1
//Returns false and Mode1 false if mode 2
[Corrupts F]
function SCR_GET_MODE(out Mode1: ZF;out Mode: A): CF; extern $bc11;

[Corrupts AF, BC, DE, HL]
procedure SCR_CLEAR; extern $bc14;

[Corrupts AF]
procedure SCR_CHAR_LIMITS(out PhysicalLastColumn: B;out PhysicalLastRow: C); extern $bc17;

[Corrupts AF]
procedure SCR_CHAR_POSITION(PhysicalCharacterColumn: H;PhysicalCharacterRow: L;out TopLeftCornerOfTheCharacter: HL as Pointer;out ByteWidth: B); extern $bc1a;

[Corrupts AF, DE]
procedure SCR_DOT_POSITION(BaseXCoordinate: DE;BaseYCoordinate: HL;out PixelAddress: HL as Pointer;out PixelMask: C;out PixelsPerByteLessOne: B); extern $bc1d;

[Corrupts AF]
function SCR_NEXT_BYTE(ScreenAddress: HL as Pointer): HL as Pointer; extern $bc20;

[Corrupts AF]
function SCR_PREV_BYTE(ScreenAddress: HL as Pointer): HL as Pointer; extern $bc23;

[Corrupts AF]
function SCR_NEXT_LINE(ScreenAddress: HL as Pointer): HL as Pointer; extern $bc26;

[Corrupts AF]
function SCR_PREV_LINE(ScreenAddress: HL as Pointer): HL as Pointer; extern $bc29;

[Corrupts F]
function SCR_INK_ENCODE(InkNumber: A): A; extern $bc2c;

[Corrupts F]
function SCR_INK_DECODE(EncodedInk: A): A; extern $bc2f;

[Corrupts AF, BC, DE, HL]
procedure SCR_SET_INK(InkNumber: A;FirstColour: B;SecondColour: C); extern $bc32;

[Corrupts AF, DE, HL]
procedure SCR_GET_INK(InkNumber: A;out FirstColour: B;out SecondColour: C); extern $bc35;

[Corrupts AF, BC, DE, HL]
procedure SCR_SET_BORDER(FirstColour: B;SecondColour: C); extern $bc38;

[Corrupts AF, DE, HL]
procedure SCR_GET_BORDER(out FirstColour: B;out SecondColour: C); extern $bc3b;

[Corrupts AF, HL]
procedure SCR_SET_FLASHING(Period1: H;Period2: L); extern $bc3e;

[Corrupts AF]
procedure SCR_GET_FLASHING(out Period1: H;out Period2: L); extern $bc41;

[Corrupts AF, BC, DE, HL]
procedure SCR_FILL_BOX(EncodedInk: A;PhysicalLeftColumn: H;PhysicalRightColumn: D;PhysicalTopRow: L;PhysicalBottomRow: E); extern $bc44;

[Corrupts AF, BC, DE, HL]
procedure SCR_FLOOD_BOX(EncodedInk: C;TopLeftAddress: HL as Pointer;Width: D;Height: E); extern $bc47;

[Corrupts AF, BC, DE, HL]
procedure SCR_CHAR_INVERT(EncodedInk: B;AnotherEncodedInk: C;PhysicalCharacterColumn: H;PhysicalCharacterRow: L); extern $bc4a;

//B is non-zero to roll up, zero to roll down
//i.e. True to roll up, False to roll down
[Corrupts AF, BC, DE, HL]
procedure SCR_HW_ROLL(RollUp: B as Boolean;EncodedInk: A); extern $bc4d;

//B is non-zero to roll up, zero to roll down
//i.e. True to roll up, False to roll down
[Corrupts AF, BC, DE, HL]
procedure SCR_SW_ROLL(RollUp: B as Boolean;EncodedInk: A;PhysicalLeftColumn: H;PhysicalRightColumn: D;PhysicalTopRow: L;PhysicalBottomRow: E); extern $bc50;

[Corrupts AF, BC, DE, HL]
procedure SCR_UNPACK(Matrix: HL as Pointer;AreaToUnpackInto: DE as Pointer); extern $bc53;

[Corrupts AF, BC, DE, HL]
procedure SCR_REPACK(EncodedInk: A;PhysicalCharacterColumn: H;PhysicalCharacterRow: L;AreaToConstructTheMatrixIn: DE as Pointer); extern $bc56;

//0=Overwrite, 1=XOR, 2=AND, 3=OR
[Corrupts AF, BC, DE, HL]
procedure SCR_ACCESS(WriteMode: A); extern $bc59;

[Corrupts AF]
procedure SCR_PIXELS(EncodedInk: B;PixelMask: C;PixelAddress: HL as Pointer); extern $bc5c;

[Corrupts AF, BC, DE, HL]
procedure SCR_HORIZONTAL(EncodedInk: A;BaseXStart: DE;BaseXEnd: BC;BaseYCoordinate: HL); extern $bc5f;

[Corrupts AF, BC, DE, HL]
procedure SCR_VERTICAL(EncodedInk: A;BaseXCoordinate: DE;BaseYStart: HL;BaseYEnd: BC); extern $bc62;

[Corrupts F]
procedure SCR_SET_POSITION(ScreenBase: A;ScreenOffset: HL;out ScreenBaseMaskedAsRequired: A;out ScreenOffsetMaskedAsRequired: HL); extern $bd55;
