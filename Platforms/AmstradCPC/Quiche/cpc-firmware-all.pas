[Corrupts AF, BC, DE, HL]
procedure KM_INITIALISE; extern $bb00;

[Corrupts AF, BC, DE, HL]
procedure KM_RESET; extern $bb03;

[Corrupts F]
function KM_WAIT_CHAR: A as Char; extern $bb06;

[Corrupts F]
function KM_READ_CHAR(out Character: A as Char): CF; extern $bb09;

[Preserves all]
procedure KM_CHAR_RETURN(Character: A as Char); extern $bb0c;

[Corrupts A, BC, DE, HL, F]
function KM_SET_EXPAND(ExpansionToken: B;Length: C;String: HL as Pointer): CF; extern $bb0f;

[Corrupts DE, F]
function KM_GET_EXPAND(ExpansionToken: A;CharacterNumber: L;out Character: A as Char): CF; extern $bb12;

[Corrupts A, BC, DE, HL, F]
function KM_EXP_BUFFER(Buffer: DE as Pointer;Length: HL): CF; extern $bb15;

[Corrupts F]
function KM_WAIT_KEY: A; extern $bb18;

[Corrupts F]
function KM_READ_KEY(out CharacterOrExpansionToken: A): CF; extern $bb1b;

[Corrupts A, HL, F]
function KM_TEST_KEY(KeyNumber: A;out ShiftAndControl: C): ZF; extern $bb1e;

[Corrupts AF]
procedure KM_GET_STATE(out ShiftLock: L;out CapsLock: H); extern $bb21;

[Corrupts F]
procedure KM_GET_JOYSTICK(out Joystick0: H;out Joystick1: L;out Joystick0: A); extern $bb24;

[Corrupts AF, HL]
procedure KM_SET_TRANSLATE(KeyNumber: A;Translation: B); extern $bb27;

[Corrupts HL, F]
function KM_GET_TRANSLATE(KeyNumber: A): A; extern $bb2a;

[Corrupts AF, HL]
procedure KM_SET_SHIFT(KeyNumber: A;Translation: B); extern $bb2d;

[Corrupts HL, F]
function KM_GET_SHIFT(KeyNumber: A): A; extern $bb30;

[Corrupts AF, HL]
procedure KM_SET_CONTROL(KeyNumber: A;Translation: B); extern $bb33;

[Corrupts HL, F]
function KM_GET_CONTROL(KeyNumber: A): A; extern $bb36;

[Corrupts AF, BC, HL]
procedure KM_SET_REPEAT(Repeat: B;KeyNumber: A); extern $bb39;

[Corrupts A, HL, F]
function KM_GET_REPEAT(KeyNumber: A): ZF; extern $bb3c;

[Corrupts AF]
procedure KM_SET_DELAY(StartUpDelay: H;RepeatSpeed: L); extern $bb3f;

[Corrupts AF]
procedure KM_GET_DELAY(out StartUpDelay: H;out RepeatSpeed: L); extern $bb42;

[Corrupts AF, BC, DE, HL]
procedure KM_ARM_BREAKS(BreakEventRoutine: DE as Pointer;ROMSelectAddress: C); extern $bb45;

[Corrupts AF, HL]
procedure KM_DISARM_BREAK; extern $bb48;

[Corrupts AF, HL]
procedure KM_BREAK_EVENT; extern $bb4b;

[Corrupts AF, BC, DE, HL]
procedure TXT_INITIALISE; extern $bb4e;

[Corrupts AF, BC, DE, HL]
procedure TXT_RESET; extern $bb51;

[Corrupts AF]
procedure TXT_VDU_ENABLE; extern $bb54;

[Corrupts AF]
procedure TXT_VDU_DISABLE; extern $bb57;

[Preserves all]
procedure TXT_OUTPUT(Character: A as Char); extern $bb5a;

[Corrupts AF, BC, DE, HL]
procedure TXT_WR_CHAR(Character: A as Char); extern $bb5d;

[Corrupts F]
function TXT_RD_CHAR(out CharacterRead: A): CF; extern $bb60;

[Corrupts AF]
procedure TXT_SET_GRAPHIC(On: A); extern $bb63;

[Corrupts AF, BC, DE, HL]
procedure TXT_WIN_ENABLE(PhysicalColumn: H;PhysicalColumn: D;PhysicalRow: L;PhysicalRow: E); extern $bb66;

[Corrupts A]
function TXT_GET_WINDOW(out LeftmostColumn: H;out RightmostColumn: D;out TopmostRow: L;out BottommostRow: E): CF; extern $bb69;

[Corrupts AF, BC, DE, HL]
procedure TXT_CLEAR_WINDOW; extern $bb6c;

[Corrupts AF, HL]
procedure TXT_SET_COLUMN(LogicalColumn: A); extern $bb6f;

[Corrupts AF, HL]
procedure TXT_SET_ROW(LogicalRow: A); extern $bb72;

[Corrupts AF, HL]
procedure TXT_SET_CURSOR(LogicalColumn: H;LogicalRow: L); extern $bb75;

[Corrupts F]
procedure TXT_GET_CURSOR(out LogicalCursorColumn: H;out LogicalCursorRow: L;out RollCount: A); extern $bb78;

[Corrupts AF]
procedure TXT_CUR_ENABLE; extern $bb7b;

[Corrupts AF]
procedure TXT_CUR_DISABLE; extern $bb7e;

[Preserves all]
procedure TXT_CUR_ON; extern $bb81;

[Preserves all]
procedure TXT_CUR_OFF; extern $bb84;

[Corrupts A, F]
function TXT_VALIDATE(LogicalColumn: H;LogicalRow: L;out Roll: B;out LogicalColumn: H;out LogicalRow: L): CF; extern $bb87;

[Corrupts AF]
procedure TXT_PLACE_CURSOR; extern $bb8a;

[Corrupts AF]
procedure TXT_REMOVE_CURSOR; extern $bb8d;

[Corrupts AF, HL]
procedure TXT_SET_PEN(Ink: A); extern $bb90;

[Corrupts F]
function TXT_GET_PEN: A; extern $bb93;

[Corrupts AF, HL]
procedure TXT_SET_PAPER(Ink: A); extern $bb96;

[Corrupts F]
function TXT_GET_PAPER: A; extern $bb99;

[Corrupts AF, HL]
procedure TXT_INVERSE; extern $bb9c;

[Corrupts AF, HL]
procedure TXT_SET_BACK(Mode: A); extern $bb9f;

[Corrupts DE, HL, F]
function TXT_GET_BACK: A; extern $bba2;

[Corrupts A, F]
function TXT_GET_MATRIX(Character: A as Char;out Matrix: HL as Pointer): CF; extern $bba5;

[Corrupts A, BC, DE, HL, F]
function TXT_SET_MATRIX(Character: A as Char;MatrixToSet: HL as Pointer): CF; extern $bba8;

[Corrupts BC, DE, F]
function TXT_SET_M_TABLE(FirstCharacter: DE;StartOfTheNewTable: HL as Pointer;out FirstCharacter: A;out OldTable: HL as Pointer): CF; extern $bbab;

[Corrupts F corrupt]
function TXT_GET_M_TABLE(out FirstCharacter: A;out StartOfTheTable: HL as Pointer): CF; extern $bbae;

[Preserves all]
function TXT_GET_CONTROLS: HL as Pointer; extern $bbb1;

[Corrupts HL, F]
function TXT_STR_SELECT(Stream: A): A; extern $bbb4;

[Corrupts AF, BC, DE, HL]
procedure TXT_SWAP_STREAMS(StreamNumber: B;AnotherStreamNumber: C); extern $bbb7;

[Corrupts AF, BC, DE, HL]
procedure GRA_INITIALISE; extern $bbba;

[Corrupts AF, BC, DE, HL]
procedure GRA_RESET; extern $bbbd;

[Corrupts AF, BC, DE, HL]
procedure GRA_MOVE_ABSOLUTE(UserXCoordinate: DE;UserYCoordinate: HL); extern $bbc0;

[Corrupts AF, BC, DE andHL]
procedure GRA_MOVE_RELATIVE(SignedXOffset: DE;SignedYOffset: HL); extern $bbc3;

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
procedure GRA_PLOT_RELATIVE(SignedXOffset: DE;SignedYOffset: HL); extern $bbed;

[Corrupts BC, DE, HL, F]
function GRA_TEST_ABSOLUTE(UserXCoordinate: DE;UserYCoordinate: HL): A; extern $bbf0;

[Corrupts BC, DE, HL, F]
function GRA_TEST_RELATIVE(SignedXOffset: DE;SignedYOffset: HL): A; extern $bbf3;

[Corrupts AF, BC, DE, HL]
procedure GRA_LINE_ABSOLUTE(UserXCoordinate: DE;UserYCoordinate: HL); extern $bbf6;

[Corrupts AF, BC, DE, HL]
procedure GRA_LINE_RELATIVE(SignedXOffset: DE;SignedYOffset: HL); extern $bbf9;

[Corrupts AF, BC, DE, HL]
procedure GRA_WR_CHAR(Character: A as Char); extern $bbfc;

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

[Corrupts F]
function SCR_GET_MODE(out ZeroFalse: ZF;out 0: A): CF; extern $bc11;

[Corrupts AF, BC, DE, HL]
procedure SCR_CLEAR; extern $bc14;

[Corrupts AF]
procedure SCR_CHAR_LIMITS(out PhysicalLastColumn: B;out PhysicalLastRow: C); extern $bc17;

[Corrupts AF]
procedure SCR_CHAR_POSITION(PhysicalCharacterColumn: H;PhysicalCharacterRow: L;out TopLeftCornerOfTheCharacter: HL as Pointer;out Width: B); extern $bc1a;

[Corrupts AF, DE]
procedure SCR_DOT_POSITION(BaseXCoordinate: DE;BaseYCoordinate: HL;out Pixel: HL as Pointer;out Mask: C;out OneLessThanTheNumber: B); extern $bc1d;

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
procedure SCR_SET_FLASHING(Period: H;Period: L); extern $bc3e;

[Corrupts AF]
procedure SCR_GET_FLASHING(out Period: H;out Period: L); extern $bc41;

[Corrupts AF, BC, DE, HL]
procedure SCR_FILL_BOX(EncodedInk: A;PhysicalLeftColumn: H;PhysicalRightColumn: D;PhysicalTopRow: L;PhysicalBottomRow: E); extern $bc44;

[Corrupts AF, BC, DE, HL]
procedure SCR_FLOOD_BOX(EncodedInk: C;TopLeftCornerOfTheAreaToFill: HL as Pointer;UnsignedWidth: D;UnsignedHeight: E); extern $bc47;

[Corrupts AF, BC, DE, HL]
procedure SCR_CHAR_INVERT(EncodedInk: B;AnotherEncodedInk: C;PhysicalCharacterColumn: H;PhysicalCharacterRow: L); extern $bc4a;

[Corrupts AF, BC, DE, HL]
procedure SCR_HW_ROLL(Down: B;EncodedInk: A); extern $bc4d;

[Corrupts AF, BC, DE, HL]
procedure SCR_SW_ROLL(Down: B;EncodedInk: A;PhysicalLeftColumn: H;PhysicalRightColumn: D;PhysicalTopRow: L;PhysicalBottomRow: E); extern $bc50;

[Corrupts AF, BC, DE, HL]
procedure SCR_UNPACK(Matrix: HL as Pointer;AreaToUnpackInto: DE as Pointer); extern $bc53;

[Corrupts AF, BC, DE, HL]
procedure SCR_REPACK(EncodedInk: A;PhysicalCharacterColumn: H;PhysicalCharacterRow: L;AreaToConstructTheMatrixIn: DE as Pointer); extern $bc56;

[Corrupts AF, BC, DE, HL]
procedure SCR_ACCESS(WriteMode: A); extern $bc59;

[Corrupts AF]
procedure SCR_PIXELS(EncodedInk: B;Mask: C;PixelS: HL as Pointer); extern $bc5c;

[Corrupts AF, BC, DE, HL]
procedure SCR_HORIZONTAL(EncodedInk: A;BaseXCoordinate: DE;BaseXCoordinate: BC;BaseYCoordinate: HL); extern $bc5f;

[Corrupts AF, BC, DE, HL]
procedure SCR_VERTICAL(EncodedInk: A;BaseXCoordinate: DE;BaseYCoordinate: HL;BaseYCoordinate: BC); extern $bc62;

[Corrupts AF, BC, DE, HL]
procedure CAS_INITIALISE; extern $bc65;

[Corrupts AF, HL]
procedure CAS_SET_SPEED(Length: HL;Precompensation: A); extern $bc68;

[Corrupts AF]
procedure CAS_NOISY(Enabled: A); extern $bc6b;

[Corrupts F]
function CAS_START_MOTOR(out PreviousMotor: A): CF; extern $bc6e;

[Corrupts F]
function CAS_STOP_MOTOR(out PreviousMotor: A): CF; extern $bc71;

[Corrupts A, F]
function CAS_RESTORE_MOTOR(PreviousMotor: A): CF; extern $bc74;

ERROR in line: In V1.1: A contains an error number (#0E)
ERROR in line: In V1.1: A contains an error number(#00)
[Corrupts IX, F All other registers preserved]
function CAS_IN_OPEN(Length: B;Filename: HL as Pointer;2KBufferToUse: DE as Pointer;out ZeroFalse: ZF;out BufferContainingTheFileHeader: HL as Pointer;out DataLocationFromTheHeader: DE;out LogicalFileLengthFromTheHeader: BC;out FileTypeFromTheHeader: A): CF; extern $bc77;

ERROR in line: In V1.1: A containsaerrornumber(#0E)
[Corrupts BC, DE, HL, F]
function CAS_IN_CLOSE: CF; extern $bc7a;

[Corrupts AF, BC, DE, HL]
procedure CAS_IN_ABANDON; extern $bc7d;

ERROR in line: In V1.1: A contains an error number(#0E or #0F)
ERROR in line: In V1.1: A contains an error number(#00)
ERROR in line: In V1.0: Acorrupt
[Corrupts IX, F]
function CAS_IN_CHAR(out ZeroFalse: ZF;out CharacterReadFromTheFile: A): CF; extern $bc8o;

ERROR in line: In V1.1: A contains an errornumber(#OE)
ERROR in line: In V1.0: Acorrupt
ERROR in line: In V1.1: A contains an error number(#00)
[Corrupts BC, DE, IX, F]
function CAS_IN_DIRECT(Address: HL as Pointer;out ZeroFalse: ZF;out EntryAddressFromTheHeader: HL as Pointer): CF; extern $bc83;

[Preserves all]
procedure CAS_RETURN; extern $bc86;

ERROR in line: In V1.1: A contains an error number(#0E or #0F)
ERROR in line: In V1.1: A contains a error number(#00)
[Corrupts IX, F]
function CAS_TEST_EOF(out ZeroFalse: ZF): CF; extern $bc89;

ERROR in line: In V1.1: A contains an error number(#00)
ERROR in line: In V1.0: Acorrupt
ERROR in line: In V1.i: A contains an error number(#OE),
ERROR in line: InVi.0: Acorrupt
ERROR in line: written to each file block
[Corrupts BC, DE, IX, F]
function CAS_OUT_OPEN(Length: B;Filename: HL as Pointer;2KBufferToUse: DE as Pointer;out ZeroTrue: ZF;out BufferContainingTheHeaderThatWillBe: HL as Pointer): CF; extern $bc8c;

ERROR in line: In V1.1: A contains an error number(#OE)
ERROR in line: In Vl.0: Acorrupt. If the user hit escape:
ERROR in line: In V1.1: A contains an errornumber(#00)
ERROR in line: In V1.0: Acorrupt
[Corrupts BC, DE, HL, IX, F]
function CAS_OUT_CLOSE(out ZeroFalse: ZF): CF; extern $bc8f;

[Corrupts AF, BC, DE, HL]
procedure CAS_OUT_ABANDON; extern $bc92;

ERROR in line: In V1.1: A contains an error number(#OE)
ERROR in line: In V1.0: Acorrupt. If the user hit escape:
ERROR in line: In V1.1: A contains an error number(#00)
ERROR in line: In V1.0: Acorrupt
[Corrupts A, IX, F]
function CAS_OUT_CHAR(Character: A as Char;out ZeroFalse: ZF): CF; extern $bc95;

ERROR in line: In V1.1: A contains in error number(#OE)
ERROR in line: In V1.0: Acorrupt. If the user hit escape:
ERROR in line: In V1.1: A contains a error number(#00)
ERROR in line: In V1.0: Acorrupt
[Corrupts BC, DE, HL, IX, F]
function CAS_OUT_DIRECT(DataToWrite: HL as Pointer;Length: DE;EntryAddressToGoIntoTheHeader: BC as Pointer;FileTypeToGoIntoTheHeader: A;out ZeroFalse: ZF): CF; extern $bc98;

ERROR in line: In V1.1: A contains an error number (#0E)
[Corrupts BC ,DE, HL, IX, F]
function CAS_CATALOG(2KBufferToUse: DE as Pointer;out ZeroFalse: ZF): CF; extern $bc9b;

[Corrupts BC, DE, HL, IX]
function CAS_WRITE(DataToWrite: HL as Pointer;Length: DE;SyncCharacter: A;out ErrorCode: A): CF; extern $bc9e;

[Corrupts BC, DE, HL, IX, F]
function CAS_READ(Address: HL as Pointer;Length: DE;SyncCharacterExpected: A;out ErrorCode: A): CF; extern $bca1;

[Corrupts BC, DE, HL, IX, F]
function CAS_CHECK(DataToCheck: HL as Pointer;Length: DE;SyncCharacterExpected: A;out ErrorCode: A): CF; extern $bca4;

[Corrupts AF, BC, DE, HL]
procedure SOUND_RESET; extern $bca7;

[Corrupts A, BC, DE, IX, F]
function SOUND_QUEUE(SoundProgramWhichMustLieInTheCentral32KOfRAM: HL as Pointer): CF; extern $bcaa;

[Corrupts BC, DE, HL, F]
function SOUND_CHECK(Bit: A): A; extern $bcad;

[Corrupts AF, BC, DE, HL]
procedure SOUND_ARM_EVENT(Bit: A;EventBlock: HL as Pointer); extern $bcb0;

[Corrupts AF, BC, DE, HL, IX]
procedure SOUND_RELEASE(Bits: A); extern $bcb3;

[Corrupts A, BC, HL, F]
function SOUND_HOLD: CF; extern $bcb6;

[Corrupts AF, BC, DE, IX]
procedure SOUND_CONTINUE; extern $bcb9;

[Corrupts DE, F]
function SOUND_AMPL_ENVELOPE(EnvelopeNumber: A;AmplitudeDataBlock: HL as Pointer;out DataBlock16: HL as Pointer): CF; extern $bcbc;

[Corrupts DE, F]
function SOUND_TONE_ENVELOPE(EnvelopeNumber: A;ToneDataBlock: HL as Pointer;out DataBlock16: HL as Pointer): CF; extern $bcbf;

[Corrupts A, F]
function SOUND_A_ADDRESS(EnvelopeNumber: A;out AmplitudeEnvelope: HL as Pointer;out Length: BC): CF; extern $bcc2;

[Corrupts A, F]
function SOUND_T_ADDRESS(EnvelopeNumber: A;out ToneEnvelope: HL as Pointer;out Length: BC): CF; extern $bcc5;

[Corrupts AF, HL]
procedure KL_CHOKE_OFF(out ForegroundROMIfAny: B;out Address: DE as Pointer;out ROMSelectAddress: C); extern $bcc8;

[Corrupts AF, BC]
procedure KL_ROM_WALK(FirstUsableByteOfMemoryLowestAddress: DE as Pointer;LastUsableByteOfMemoryHighestAddress: HL as Pointer;out FirstUsableByteOfMemory: DE as Pointer;out LastUsableByteOfMemory: HL as Pointer); extern $bccb;

[Corrupts AF, B]
procedure KL_INIT_BACK(ROMToInitialize: C;FirstUsableByteOfMemoryLowestAddress: DE as Pointer;LastUsableByteOfMemoryHighestAddress: HL as Pointer;out FirstUsableByteOfMemory: DE as Pointer;out LastUsableByteOfMemory: HL as Pointer); extern $bcce;

[Corrupts DE]
procedure KL_LOG_EXT(RSXSCommandTable: BC as Pointer;4ByteAreaOfRAMForTheKernelSUse: HL as Pointer); extern $bcd1;

[Corrupts A, B, DE]
function KL_FIND_COMMAND(CommandNameToSearchFor: HL as Pointer;out ROMSelectAddress: C;out Routine: HL as Pointer): CF; extern $bcd4;

[Corrupts AF, DE, HL]
procedure KL_NEW_FRAME_FLY(FrameFlybackBlock: HL as Pointer;EventClass: B;EventRoutine: C;EventRoutine: DE as Pointer); extern $bcd7;

[Corrupts AF, DE, HL]
procedure KL_ADD_FRAME_FLY(FrameFlybackBlock: HL as Pointer); extern $bcda;

[Corrupts AF, DE, HL]
procedure KL_DEL_FRAME_FLY(FrameFlybackBlock: HL as Pointer); extern $bcdd;

[Corrupts AF, DE, HL]
procedure KL_NEW_FAST_TICKER(FastTickerBlock: HL as Pointer;EventClass: B;EventRoutine: C;EventRoutine: DE as Pointer); extern $bceo;

[Corrupts AF, DE, HL]
procedure KL_ADD_FAST_TICKER(FastTickerBlock: HL as Pointer); extern $bce3;

[Corrupts AF, DE, ilL]
procedure KL_DEL_FAST_TICKER(FastTickerBlock: HL as Pointer); extern $bce6;

[Corrupts AF, BC, DE, HL]
procedure KL_ADD_TICKER(TickBlock: HL as Pointer;InitialValue: DE;Value: BC); extern $bce9;

[Corrupts A, HL, F]
function KL_DEL_TICKER(TickBlock: HL as Pointer;out CountRemainingBeforeTheNextEvent: DE): CF; extern $bcec;

[Preserves all]
function KL_INIT_EVENT(EventBlock: HL as Pointer;EventClass: B;EventRoutine: C;EventRoutine: DE as Pointer): HL as Pointer; extern $bcef;

[Corrupts AF, BC, DE, HL]
procedure KL_EVENT(EventBlock: HL as Pointer); extern $bcf2;

[Corrupts AF, HL]
procedure KL_SYNC_RESET; extern $bcf5;

[Corrupts AF, BC, DE, HL]
procedure KL_DEL_SYNCHRONOUS(EventBlock: HL as Pointer); extern $bcf8;

[Corrupts DE]
function KL_NEXT_SYNC(out EventBlock: HL as Pointer;out PreviousEventPriorityIfAny: A): CF; extern $bcfb;

[Corrupts AF, BC, DE, HL]
procedure KL_DO_SYNC(EventBlock: HL as Pointer); extern $bcfe;

[Corrupts AF, BC, DE, HL corrupt]
procedure KL_DONE_SYNC(PreviousEventPriority: A;EventBlock: HL as Pointer); extern $bd01;

[Corrupts HL]
procedure KL_EVENT_DISABLE; extern $bd04;

[Corrupts HL]
procedure KL_EVENT_ENABLE; extern $bd07;

[Corrupts AF]
procedure KL_DISARM_EVENT(EventBlock: HL as Pointer); extern $bd0a;

ERROR in line: DEHL contains the four byte count (D contains the most significant byte and L
ERROR in line: the least significant byte)
[Preserves all]
procedure KL_TIME_PLEASE; extern $bd0d;

ERROR in line: DEHL contains the four byte count to set (D contains the most significant byte
ERROR in line: and L the least significant byte)
[Corrupts AF]
procedure KL_TIME_SET; extern $bd10;

ERROR in line: Does not exit!
[Preserves all]
procedure MC_BOOT_PROGRAM(RoutineToCallToLoadTheProgram: HL as Pointer); extern $bd13;

ERROR in line: Never exits!
[Preserves all]
procedure MC_START_PROGRAM(EntryPointAddress: HL as Pointer;ROMSelection: C); extern $bd16;

[Preserves all]
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

[Corrupts A, F]
function MC_PRINT_CHAR(Character: A as Char): CF; extern $bd2b;

[Corrupts F]
function MC_BUSY_PRINTER: CF; extern $bd2e;

[Corrupts A, F]
procedure MC_SEND_PRINTER(Character: A as Char); extern $bd31;

[Corrupts AF, BC]
procedure MC_SOUND_REGISTER(SoundChipRegisterNumber: A;Data: C); extern $bd34;

[Corrupts AF, BC. DE, HL]
procedure JUMP_RESTORE; extern $bd37;

[Corrupts AF]
procedure KM_SET_LOCKS(CapsLock: H;ShiftLock: L); extern $bd3a;

[Corrupts AF]
procedure KM_FLUSH; extern $bd3d;

[Corrupts F]
function TXT_ASK_STATE: A; extern $bd40;

[Corrupts AF, BC, DE, ilL]
procedure GRA_DEFAULT; extern $bd43;

[Preserves all]
procedure GRA_SET_BACK(Mode: A); extern $bd46;

[Preserves all]
procedure GRA_SET_FIRST(Zero: A); extern $bd49;

[Preserves all]
procedure GRA_SET_LINE_MASK(LineMask: A); extern $bd4c;

[Corrupts AF]
procedure GRA_FROM_USER(UserXCoordinate: DE;UserYCoordinate: HL;out BaseXCoordinate: DE;out BaseYCoordinate: HL); extern $bd4f;

[Corrupts A, BC, DE, HL, F]
function GRA_FILL(UnencodedInk: A;Buffer: HL as Pointer;Length: DE): CF; extern $bd52;

[Corrupts F]
procedure SCR_SET_POSITION(ScreenBase: A;ScreenOffset: HL;out ScreenBaseMaskedAsRequired: A;out ScreenOffsetMaskedAsRequired: HL); extern $bd55;

[Corrupts A, BC, DE, HL, F]
function MC_PRINT_TRANSLATION(Table: HL as Pointer): CF; extern $bd58;

[Corrupts F]
function KL_BANK_SWITCH(Organization: A): A; extern $bd5b;

[Corrupts AF]
procedure IND_TXT_DRAW_CURSOR; extern $bdcd;

[Corrupts AF]
procedure IND_TXT_UNDRAW_CURSOR; extern $bdd0;

[Corrupts AF, BC, DE, HL]
procedure IND_TXT_WRITE_CHAR(Character: A as Char;PhysicalColumn: H;PhysicalRow: L); extern $bdd3;

[Corrupts BC, DE, HL, F]
function IND_TXT_UNWRITE(PhysicalColumn: H;PhysicalRow: L;out CharacterRead: A): CF; extern $bdd6;

[Corrupts AF, BC, DE, HL]
procedure IND_TXT_OUT_ACTION(CharacterOrCode: A); extern $bdd9;

[Corrupts AF, BC, DE, HL]
procedure IND_GRA_PLOT(UserXCoordinate: DE;UserYCoordinate: HL); extern $bddc;

[Corrupts BC, DE, HL, F]
function IND_GRA_TEST(UserXCoordinate: DE;UserYCoordinate: HL): A; extern $bddf;

[Corrupts AF, BC. DE, HL]
procedure IND_GRA_LINE(UserXCoordinate: DE;UserYCoordinate: HL); extern $bde2;

[Corrupts F]
function IND_SCR_READ(Pixel: HL as Pointer;Mask: C): A; extern $bde5;

[Corrupts AF]
procedure IND_SCR_WRITE(PixelS: HL as Pointer;Mask: C;EncodedInk: B); extern $bde8;

[Corrupts AF, BC, DE, HL]
procedure IND_SCR_MODE_CLEAR; extern $bdeb;

ERROR in line: Interrupts disabled
[Corrupts AF, HL]
procedure IND_KM_TEST_BREAK(ShiftAndControlKeyStates: C); extern $bdee;

[Corrupts A, BC]
function IND_MC_WAIT_PRINTER(Character: A as Char): CF; extern $bdf1;

[Corrupts F]
function HI_KL_U_ROM_ENABLE: A; extern $b900;

[Corrupts F]
function HI_KL_U_ROM_DISABLE: A; extern $b903;

[Corrupts F]
function HI_KL_L_ROM_ENABLE: A; extern $b906;

[Corrupts F]
function HI_KL_L_ROM_DISABLE: A; extern $b909;

[Corrupts AF]
procedure HI_KL_ROM_RESTORE(PreviousROM: A); extern $b9oc;

[Corrupts AF]
procedure HI_KL_ROM_SELECT(ROM: C;out PreviouslySelectedROM: C;out PreviousROM: B); extern $b90f;

[Preserves all]
function HI_KL_CURR_SELECTION: A; extern $b912;

[Corrupts B, F]
procedure HI_KL_PROBE_ROM(ROMToProbe: C;out ROMSClass: A;out ROMSMarkNumber: L;out ROMSVersionNumber: H); extern $b915;

[Corrupts B]
function HI_KL_ROM_DESELECT(PreviouslySelectedROM: C;PreviousROM: B): C; extern $b918;

ERROR in line: BC, DE, HL as required by the LDIR instruction
ERROR in line: F, BC, DE, HL as set by the LDIR instruction
[Preserves all]
procedure HI_KL_LDIR; extern $b91b;

ERROR in line: BC, DE, HL as required by LDDR instruction
ERROR in line: F, BC, DE, HL as set by LDDR instruction
[Preserves all]
procedure HI_KL_LDDR; extern $b91e;

[Corrupts A, F]
function HI_KL_POLL_SYNCHRONOUS: CF; extern $b921;

[Corrupts AF, HL]
procedure HI_KL_SCAN_NEEDED; extern $b92a;

ERROR in line: Does not return!
[Preserves all]
procedure LOW_RESET_ENTRY; extern $0000;

ERROR in line: All registers and flags are passed to the target routine untouched
ERROR in line: All registers and flags are as set by the target routine
[Preserves all]
procedure LOW_LOW_JUMP; extern $0008;

ERROR in line: All registers and flags are passed to the target routine untouched
ERROR in line: All registers and flags are as set by the target routine
[Preserves all]
procedure LOW_KL_LOW_PCHL(LowAddress: HL as Pointer); extern $000b;

ERROR in line: All registers and flags are passed to the target routine untouched
ERROR in line: All registers and flags are as set by the target routine
[Preserves all]
procedure LOW_PCBC_INSTRUCTION(Address: BC as Pointer); extern $000e;

ERROR in line: All registers and flags are passed to the target routine untouched except for
ERROR in line: All other registers and flags are as set by the target routine
[Corrupts IY]
procedure LOW_SIDE_CALL(WhichIsSet: IY); extern $0010;

ERROR in line: All registers and flags are passed to the target routine untouched except for
ERROR in line: All other registers and flags are as set by the target routine
[Corrupts IY corrupt:]
procedure LOW_KL_SIDE_PCHL(SideAddress: HL as Pointer;WhichIsSet: IY); extern $0013;

ERROR in line: All registers and flags are passed to the target routine untouched
ERROR in line: All registers and flags are as set by the target routine
[Preserves all]
procedure LOW_PCDE_INSTRUCTION(Address: DE as Pointer); extern $0016;

ERROR in line: All registers and flags are passed to the target routine untouched except for IY
ERROR in line: (which is set to point at a background ROMâ€™s upper data area)
ERROR in line: All other registers and flags are as set by the target routine
[Preserves all]
procedure LOW_FAR_CALL; extern $0018;

ERROR in line: All registers and flags are passed to the target routine untouched except for IY (which is set to point at a background ROMâ€™s upper data area)
ERROR in line: All other registers and flags are as set by the target routine
[Preserves all]
procedure LOW_KL_FAR_PCHL(RoutineToCall: HL as Pointer;ROMSelectByte: C); extern $001b;

ERROR in line: All registers and flags are passed to the target routine untouched
ERROR in line: All registers and flags are as set by the target routine
[Preserves all]
procedure LOW_PCHL_INSTRUCTION(Address: HL as Pointer); extern $001e;

[Preserves all]
function LOW_RAM_LAM(LocationToRead: HL as Pointer): A; extern $0020;

ERROR in line: All registers and flags are passed to the target routine untouched except for
ERROR in line: All other registers and flags are as set by the target routine
[Preserves all]
procedure LOW_KL_FAR_ICALL(FArAddressToCall: HL as Pointer;WhichIsSet: IY); extern $0023;

ERROR in line: All registers and flags are passed to the target routine untouched
ERROR in line: All registers and flags are as set by the target routine
[Preserves all]
procedure LOW_FIRM_JUMP; extern $0028;

ERROR in line: Unknown
ERROR in line: Unknown
[Preserves all]
procedure LOW_USER_RESTART; extern $0030;

[Preserves all]
procedure LOW_INTERRUPT_ENTRY; extern $0038;

[Corrupts AF, BC, DE, HL]
procedure LOW_EXT_INTERRUPT; extern $003b;

[Corrupts HL, F]
function BIOS_SET_MESSAGE(00: A): A;

ERROR in line: Format of the parameter block:
ERROR in line: bytes 0, 1 motor on timeout in 20 millisecond units
ERROR in line: bytes 2, 3 motor off timeout in 20 millisecond units
ERROR in line: byte 4 write current off time in 10 microsecond units
ERROR in line: byte 5 head settle time in 1 millisecond units
ERROR in line: byte 6 step rate time in 1 millisecond units
ERROR in line: byte 7 head unload delay (as per ÂµPD765A SPECIFY
ERROR in line: command)
ERROR in line: byte 8 bits 7â€¦1: head load delay, bit 0: non-DMA mode (as
ERROR in line: per ÂµPD765A SPECIFY command)
[Corrupts AF, BC, DE, HL]
procedure BIOS_SETUP_DISC(Address: HL as Pointer);

ERROR in line: #41 => system format
ERROR in line: #C1 => data only format
ERROR in line: #01 => IBM format
ERROR in line: #00 => A:
ERROR in line: #01 => B:
[Corrupts AF, BC, DE, HL]
procedure BIOS_SELECT_FORMAT(FirstSectorNumber: A;DriveNumber: E);

ERROR in line: #00 => A:
ERROR in line: #01 => B:
[Corrupts F]
function BIOS_READ_SECTOR(Address: HL as Pointer;DriveNumber: E;TrackNumber: D;SectorNumber: C;out OK: A;out Address: HL as Pointer): CF;

ERROR in line: #00 => A:
ERROR in line: #01 => B:
[Corrupts F]
function BIOS_WRITE_SECTOR(Address: HL as Pointer;DriveNumber: E;TrackNumber: D;SectorNumber: C;out OK: A;out Address: HL as Pointer): CF;

ERROR in line: #00 => A:
ERROR in line: #0l => B:
ERROR in line: Format of header information:
ERROR in line: sector entry for first sector
ERROR in line: sector entry for second sector
ERROR in line: â€¦
ERROR in line: sector entry for last sector
ERROR in line: sector entry format:
ERROR in line: byte 0: track number
ERROR in line: byte 1: head number
ERROR in line: byte 2 : sector number
ERROR in line: byte 3 : log 2 (sector size) - 7
[Corrupts F]
function BIOS_FORMAT_TRACK(Address: HL as Pointer;DriveNumber: E;TrackNumber: D;out OK: A;out Address: HL as Pointer): CF;

ERROR in line: #00 => A:
ERROR in line: #01 => B:
ERROR in line: Always
[Corrupts F]
function BIOS_MOVE_TRACK(DriveNumber: E;TrackNumber: D;out OK: A;out Address: HL as Pointer): CF;

ERROR in line: #00 => A:
ERROR in line: #01 => B:
ERROR in line: If carry true
ERROR in line: If carry false
ERROR in line: as defined above
ERROR in line: Always
ERROR in line: Notes
ERROR in line: This routine returns carry to indicate which set of exit conditions have occurred. No other meaning should be attached to the state of carry
ERROR in line: Related entries:
ERROR in line: SELECT FORMAT
ERROR in line: READ SECTOR
ERROR in line: WRITE SECTOR
ERROR in line: FORMAT TRACK
ERROR in line: MOVE TRACK
ERROR in line: BIOS: SET RETRY COUNT (CTRL/I)
[Corrupts F corrupt]
procedure BIOS_GET_DR_STATUS(DriveNumber: A;out DriveStatusByteAsDefinedAbove: A;out Address: HL as Pointer);

[Corrupts HL, F]
function BIOS_SET_RETRY_COUNT(Value: A): A;
