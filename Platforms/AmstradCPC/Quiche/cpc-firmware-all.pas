//Import file for the Amstrad CPC firmware
//This file imports standard routines. See other files to import on a 
//pack-by-pack basis

//For documentation see https://www.cpcwiki.eu/index.php/BIOS_Functions

[Corrupts AF, BC, DE, HL]
procedure KM_INITIALISE; call $bb00;

[Corrupts AF, BC, DE, HL]
procedure KM_RESET; call $bb03;

[Corrupts F]
function KM_WAIT_CHAR: A as Char; call $bb06;

[Corrupts F]
function KM_READ_CHAR(out Character: A as Char): CF; call $bb09;

[Preserves all]
procedure KM_CHAR_RETURN(Character: A as Char); call $bb0c;

[Corrupts A, BC, DE, HL, F]
function KM_SET_EXPAND(ExpansionToken: B;Length: C;String: HL as Pointer): CF; call $bb0f;

[Corrupts DE, F]
function KM_GET_EXPAND(ExpansionToken: A;CharacterNumber: L;out Character: A as Char): CF; call $bb12;

[Corrupts A, BC, DE, HL, F]
function KM_EXP_BUFFER(Buffer: DE as Pointer;Length: HL): CF; call $bb15;

[Corrupts F]
function KM_WAIT_KEY: A; call $bb18;

[Corrupts F]
function KM_READ_KEY(out CharacterOrExpansionToken: A): CF; call $bb1b;

[Corrupts A, HL, F]
function KM_TEST_KEY(KeyNumber: A;out ShiftAndControl: C): ZF; call $bb1e;

[Corrupts AF]
procedure KM_GET_STATE(out ShiftLock: L;out CapsLock: H); call $bb21;

[Corrupts F]
procedure KM_GET_JOYSTICK(out Joystick0: H;out Joystick1: L;out Joystick0: A); call $bb24;

[Corrupts AF, HL]
procedure KM_SET_TRANSLATE(KeyNumber: A;Translation: B); call $bb27;

[Corrupts HL, F]
function KM_GET_TRANSLATE(KeyNumber: A): A; call $bb2a;

[Corrupts AF, HL]
procedure KM_SET_SHIFT(KeyNumber: A;Translation: B); call $bb2d;

[Corrupts HL, F]
function KM_GET_SHIFT(KeyNumber: A): A; call $bb30;

[Corrupts AF, HL]
procedure KM_SET_CONTROL(KeyNumber: A;Translation: B); call $bb33;

[Corrupts HL, F]
function KM_GET_CONTROL(KeyNumber: A): A; call $bb36;

[Corrupts AF, BC, HL]
procedure KM_SET_REPEAT(Repeat: B;KeyNumber: A); call $bb39;

[Corrupts A, HL, F]
function KM_GET_REPEAT(KeyNumber: A): ZF; call $bb3c;

[Corrupts AF]
procedure KM_SET_DELAY(StartUpDelay: H;RepeatSpeed: L); call $bb3f;

[Corrupts AF]
procedure KM_GET_DELAY(out StartUpDelay: H;out RepeatSpeed: L); call $bb42;

[Corrupts AF, BC, DE, HL]
procedure KM_ARM_BREAKS(BreakEventRoutine: DE as Pointer;ROMSelectAddress: C); call $bb45;

[Corrupts AF, HL]
procedure KM_DISARM_BREAK; call $bb48;

[Corrupts AF, HL]
procedure KM_BREAK_EVENT; call $bb4b;

[Corrupts AF, BC, DE, HL]
procedure TXT_INITIALISE; call $bb4e;

[Corrupts AF, BC, DE, HL]
procedure TXT_RESET; call $bb51;

[Corrupts AF]
procedure TXT_VDU_ENABLE; call $bb54;

[Corrupts AF]
procedure TXT_VDU_DISABLE; call $bb57;

[Preserves all]
procedure TXT_OUTPUT(Character: A as Char); call $bb5a;

[Corrupts AF, BC, DE, HL]
procedure TXT_WR_CHAR(Character: A as Char); call $bb5d;

[Corrupts F]
function TXT_RD_CHAR(out CharacterRead: A): CF; call $bb60;

[Corrupts AF]
procedure TXT_SET_GRAPHIC(On: A); call $bb63;

[Corrupts AF, BC, DE, HL]
procedure TXT_WIN_ENABLE(PhysicalColumn: H;PhysicalColumn: D;PhysicalRow: L;PhysicalRow: E); call $bb66;

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

[Preserves all]
procedure TXT_CUR_ON; call $bb81;

[Preserves all]
procedure TXT_CUR_OFF; call $bb84;

[Corrupts A, F]
function TXT_VALIDATE(LogicalColumn: H;LogicalRow: L;out Roll: B;out LogicalColumn: H;out LogicalRow: L): CF; call $bb87;

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

[Corrupts AF, HL]
procedure TXT_SET_BACK(Mode: A); call $bb9f;

[Corrupts DE, HL, F]
function TXT_GET_BACK: A; call $bba2;

[Corrupts A, F]
function TXT_GET_MATRIX(Character: A as Char;out Matrix: HL as Pointer): CF; call $bba5;

[Corrupts A, BC, DE, HL, F]
function TXT_SET_MATRIX(Character: A as Char;MatrixToSet: HL as Pointer): CF; call $bba8;

[Corrupts BC, DE, F]
function TXT_SET_M_TABLE(FirstCharacter: DE;StartOfTheNewTable: HL as Pointer;out FirstCharacter: A;out OldTable: HL as Pointer): CF; call $bbab;

[Corrupts F corrupt]
function TXT_GET_M_TABLE(out FirstCharacter: A;out StartOfTheTable: HL as Pointer): CF; call $bbae;

[Preserves all]
function TXT_GET_CONTROLS: HL as Pointer; call $bbb1;

[Corrupts HL, F]
function TXT_STR_SELECT(Stream: A): A; call $bbb4;

[Corrupts AF, BC, DE, HL]
procedure TXT_SWAP_STREAMS(StreamNumber: B;AnotherStreamNumber: C); call $bbb7;

[Corrupts AF, BC, DE, HL]
procedure GRA_INITIALISE; call $bbba;

[Corrupts AF, BC, DE, HL]
procedure GRA_RESET; call $bbbd;

[Corrupts AF, BC, DE, HL]
procedure GRA_MOVE_ABSOLUTE(UserXCoordinate: DE;UserYCoordinate: HL); call $bbc0;

[Corrupts AF, BC, DE andHL]
procedure GRA_MOVE_RELATIVE(SignedXOffset: DE;SignedYOffset: HL); call $bbc3;

[Corrupts AF]
procedure GRA_ASK_CURSOR(out UserXCoordinate: DE;out UserYCoordinate: HL); call $bbc6;

[Corrupts AF, BC, DE, HL]
procedure GRA_SET_ORIGIN(StandardXCoordinate: DE;StandardYCoordinate: HL); call $bbc9;

[Preserves all]
procedure GRA_GET_ORIGIN(out StandardXCoordinate: DE;out StandardYCoordinate: HL); call $bbcc;

[Corrupts AF, BC, DE, HL]
procedure GRA_WIN_WIDTH(StandardXCoordinate: DE;StandardXCoordinate: HL); call $bbcf;

[Corrupts AF, BC, DE, HL]
procedure GRA_WIN_HEIGHT(StandardYCoordinate: DE;StandardYCoordinate: HL); call $bbd2;

[Corrupts AF]
procedure GRA_GET_W_WIDTH(out StandardXCoordinate: DE;out StandardXCoordinate: HL); call $bbd5;

[Corrupts AF]
procedure GRA_GET_W_HEIGHT(out StandardYCoordinate: DE;out StandardYCoordinate: HL); call $bbd8;

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
procedure GRA_PLOT_RELATIVE(SignedXOffset: DE;SignedYOffset: HL); call $bbed;

[Corrupts BC, DE, HL, F]
function GRA_TEST_ABSOLUTE(UserXCoordinate: DE;UserYCoordinate: HL): A; call $bbf0;

[Corrupts BC, DE, HL, F]
function GRA_TEST_RELATIVE(SignedXOffset: DE;SignedYOffset: HL): A; call $bbf3;

[Corrupts AF, BC, DE, HL]
procedure GRA_LINE_ABSOLUTE(UserXCoordinate: DE;UserYCoordinate: HL); call $bbf6;

[Corrupts AF, BC, DE, HL]
procedure GRA_LINE_RELATIVE(SignedXOffset: DE;SignedYOffset: HL); call $bbf9;

[Corrupts AF, BC, DE, HL]
procedure GRA_WR_CHAR(Character: A as Char); call $bbfc;

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

[Corrupts F]
function SCR_GET_MODE(out ZeroFalse: ZF;out 0: A): CF; call $bc11;

[Corrupts AF, BC, DE, HL]
procedure SCR_CLEAR; call $bc14;

[Corrupts AF]
procedure SCR_CHAR_LIMITS(out PhysicalLastColumn: B;out PhysicalLastRow: C); call $bc17;

[Corrupts AF]
procedure SCR_CHAR_POSITION(PhysicalCharacterColumn: H;PhysicalCharacterRow: L;out TopLeftCornerOfTheCharacter: HL as Pointer;out Width: B); call $bc1a;

[Corrupts AF, DE]
procedure SCR_DOT_POSITION(BaseXCoordinate: DE;BaseYCoordinate: HL;out Pixel: HL as Pointer;out Mask: C;out OneLessThanTheNumber: B); call $bc1d;

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
procedure SCR_SET_FLASHING(Period: H;Period: L); call $bc3e;

[Corrupts AF]
procedure SCR_GET_FLASHING(out Period: H;out Period: L); call $bc41;

[Corrupts AF, BC, DE, HL]
procedure SCR_FILL_BOX(EncodedInk: A;PhysicalLeftColumn: H;PhysicalRightColumn: D;PhysicalTopRow: L;PhysicalBottomRow: E); call $bc44;

[Corrupts AF, BC, DE, HL]
procedure SCR_FLOOD_BOX(EncodedInk: C;TopLeftCornerOfTheAreaToFill: HL as Pointer;UnsignedWidth: D;UnsignedHeight: E); call $bc47;

[Corrupts AF, BC, DE, HL]
procedure SCR_CHAR_INVERT(EncodedInk: B;AnotherEncodedInk: C;PhysicalCharacterColumn: H;PhysicalCharacterRow: L); call $bc4a;

[Corrupts AF, BC, DE, HL]
procedure SCR_HW_ROLL(Down: B;EncodedInk: A); call $bc4d;

[Corrupts AF, BC, DE, HL]
procedure SCR_SW_ROLL(Down: B;EncodedInk: A;PhysicalLeftColumn: H;PhysicalRightColumn: D;PhysicalTopRow: L;PhysicalBottomRow: E); call $bc50;

[Corrupts AF, BC, DE, HL]
procedure SCR_UNPACK(Matrix: HL as Pointer;AreaToUnpackInto: DE as Pointer); call $bc53;

[Corrupts AF, BC, DE, HL]
procedure SCR_REPACK(EncodedInk: A;PhysicalCharacterColumn: H;PhysicalCharacterRow: L;AreaToConstructTheMatrixIn: DE as Pointer); call $bc56;

[Corrupts AF, BC, DE, HL]
procedure SCR_ACCESS(WriteMode: A); call $bc59;

[Corrupts AF]
procedure SCR_PIXELS(EncodedInk: B;Mask: C;PixelS: HL as Pointer); call $bc5c;

[Corrupts AF, BC, DE, HL]
procedure SCR_HORIZONTAL(EncodedInk: A;BaseXCoordinate: DE;BaseXCoordinate: BC;BaseYCoordinate: HL); call $bc5f;

[Corrupts AF, BC, DE, HL]
procedure SCR_VERTICAL(EncodedInk: A;BaseXCoordinate: DE;BaseYCoordinate: HL;BaseYCoordinate: BC); call $bc62;

[Corrupts AF, BC, DE, HL]
procedure CAS_INITIALISE; call $bc65;

[Corrupts AF, HL]
procedure CAS_SET_SPEED(Length: HL;Precompensation: A); call $bc68;

[Corrupts AF]
procedure CAS_NOISY(Enabled: A); call $bc6b;

[Corrupts F]
function CAS_START_MOTOR(out PreviousMotor: A): CF; call $bc6e;

[Corrupts F]
function CAS_STOP_MOTOR(out PreviousMotor: A): CF; call $bc71;

[Corrupts A, F]
function CAS_RESTORE_MOTOR(PreviousMotor: A): CF; call $bc74;

ERROR in line: In V1.1: A contains an error number (#0E)
ERROR in line: In V1.1: A contains an error number(#00)
[Corrupts IX, F All other registers preserved]
function CAS_IN_OPEN(Length: B;Filename: HL as Pointer;2KBufferToUse: DE as Pointer;out ZeroFalse: ZF;out BufferContainingTheFileHeader: HL as Pointer;out DataLocationFromTheHeader: DE;out LogicalFileLengthFromTheHeader: BC;out FileTypeFromTheHeader: A): CF; call $bc77;

ERROR in line: In V1.1: A containsaerrornumber(#0E)
[Corrupts BC, DE, HL, F]
function CAS_IN_CLOSE: CF; call $bc7a;

[Corrupts AF, BC, DE, HL]
procedure CAS_IN_ABANDON; call $bc7d;

ERROR in line: In V1.1: A contains an error number(#0E or #0F)
ERROR in line: In V1.1: A contains an error number(#00)
ERROR in line: In V1.0: Acorrupt
[Corrupts IX, F]
function CAS_IN_CHAR(out ZeroFalse: ZF;out CharacterReadFromTheFile: A): CF; call $bc8o;

ERROR in line: In V1.1: A contains an errornumber(#OE)
ERROR in line: In V1.0: Acorrupt
ERROR in line: In V1.1: A contains an error number(#00)
[Corrupts BC, DE, IX, F]
function CAS_IN_DIRECT(Address: HL as Pointer;out ZeroFalse: ZF;out EntryAddressFromTheHeader: HL as Pointer): CF; call $bc83;

[Preserves all]
procedure CAS_RETURN; call $bc86;

ERROR in line: In V1.1: A contains an error number(#0E or #0F)
ERROR in line: In V1.1: A contains a error number(#00)
[Corrupts IX, F]
function CAS_TEST_EOF(out ZeroFalse: ZF): CF; call $bc89;

ERROR in line: In V1.1: A contains an error number(#00)
ERROR in line: In V1.0: Acorrupt
ERROR in line: In V1.i: A contains an error number(#OE),
ERROR in line: InVi.0: Acorrupt
ERROR in line: written to each file block
[Corrupts BC, DE, IX, F]
function CAS_OUT_OPEN(Length: B;Filename: HL as Pointer;2KBufferToUse: DE as Pointer;out ZeroTrue: ZF;out BufferContainingTheHeaderThatWillBe: HL as Pointer): CF; call $bc8c;

ERROR in line: In V1.1: A contains an error number(#OE)
ERROR in line: In Vl.0: Acorrupt. If the user hit escape:
ERROR in line: In V1.1: A contains an errornumber(#00)
ERROR in line: In V1.0: Acorrupt
[Corrupts BC, DE, HL, IX, F]
function CAS_OUT_CLOSE(out ZeroFalse: ZF): CF; call $bc8f;

[Corrupts AF, BC, DE, HL]
procedure CAS_OUT_ABANDON; call $bc92;

ERROR in line: In V1.1: A contains an error number(#OE)
ERROR in line: In V1.0: Acorrupt. If the user hit escape:
ERROR in line: In V1.1: A contains an error number(#00)
ERROR in line: In V1.0: Acorrupt
[Corrupts A, IX, F]
function CAS_OUT_CHAR(Character: A as Char;out ZeroFalse: ZF): CF; call $bc95;

ERROR in line: In V1.1: A contains in error number(#OE)
ERROR in line: In V1.0: Acorrupt. If the user hit escape:
ERROR in line: In V1.1: A contains a error number(#00)
ERROR in line: In V1.0: Acorrupt
[Corrupts BC, DE, HL, IX, F]
function CAS_OUT_DIRECT(DataToWrite: HL as Pointer;Length: DE;EntryAddressToGoIntoTheHeader: BC as Pointer;FileTypeToGoIntoTheHeader: A;out ZeroFalse: ZF): CF; call $bc98;

ERROR in line: In V1.1: A contains an error number (#0E)
[Corrupts BC ,DE, HL, IX, F]
function CAS_CATALOG(2KBufferToUse: DE as Pointer;out ZeroFalse: ZF): CF; call $bc9b;

[Corrupts BC, DE, HL, IX]
function CAS_WRITE(DataToWrite: HL as Pointer;Length: DE;SyncCharacter: A;out ErrorCode: A): CF; call $bc9e;

[Corrupts BC, DE, HL, IX, F]
function CAS_READ(Address: HL as Pointer;Length: DE;SyncCharacterExpected: A;out ErrorCode: A): CF; call $bca1;

[Corrupts BC, DE, HL, IX, F]
function CAS_CHECK(DataToCheck: HL as Pointer;Length: DE;SyncCharacterExpected: A;out ErrorCode: A): CF; call $bca4;

[Corrupts AF, BC, DE, HL]
procedure SOUND_RESET; call $bca7;

[Corrupts A, BC, DE, IX, F]
function SOUND_QUEUE(SoundProgramWhichMustLieInTheCentral32KOfRAM: HL as Pointer): CF; call $bcaa;

[Corrupts BC, DE, HL, F]
function SOUND_CHECK(Bit: A): A; call $bcad;

[Corrupts AF, BC, DE, HL]
procedure SOUND_ARM_EVENT(Bit: A;EventBlock: HL as Pointer); call $bcb0;

[Corrupts AF, BC, DE, HL, IX]
procedure SOUND_RELEASE(Bits: A); call $bcb3;

[Corrupts A, BC, HL, F]
function SOUND_HOLD: CF; call $bcb6;

[Corrupts AF, BC, DE, IX]
procedure SOUND_CONTINUE; call $bcb9;

[Corrupts DE, F]
function SOUND_AMPL_ENVELOPE(EnvelopeNumber: A;AmplitudeDataBlock: HL as Pointer;out DataBlock16: HL as Pointer): CF; call $bcbc;

[Corrupts DE, F]
function SOUND_TONE_ENVELOPE(EnvelopeNumber: A;ToneDataBlock: HL as Pointer;out DataBlock16: HL as Pointer): CF; call $bcbf;

[Corrupts A, F]
function SOUND_A_ADDRESS(EnvelopeNumber: A;out AmplitudeEnvelope: HL as Pointer;out Length: BC): CF; call $bcc2;

[Corrupts A, F]
function SOUND_T_ADDRESS(EnvelopeNumber: A;out ToneEnvelope: HL as Pointer;out Length: BC): CF; call $bcc5;

[Corrupts AF, HL]
procedure KL_CHOKE_OFF(out ForegroundROMIfAny: B;out Address: DE as Pointer;out ROMSelectAddress: C); call $bcc8;

[Corrupts AF, BC]
procedure KL_ROM_WALK(FirstUsableByteOfMemoryLowestAddress: DE as Pointer;LastUsableByteOfMemoryHighestAddress: HL as Pointer;out FirstUsableByteOfMemory: DE as Pointer;out LastUsableByteOfMemory: HL as Pointer); call $bccb;

[Corrupts AF, B]
procedure KL_INIT_BACK(ROMToInitialize: C;FirstUsableByteOfMemoryLowestAddress: DE as Pointer;LastUsableByteOfMemoryHighestAddress: HL as Pointer;out FirstUsableByteOfMemory: DE as Pointer;out LastUsableByteOfMemory: HL as Pointer); call $bcce;

[Corrupts DE]
procedure KL_LOG_EXT(RSXSCommandTable: BC as Pointer;4ByteAreaOfRAMForTheKernelSUse: HL as Pointer); call $bcd1;

[Corrupts A, B, DE]
function KL_FIND_COMMAND(CommandNameToSearchFor: HL as Pointer;out ROMSelectAddress: C;out Routine: HL as Pointer): CF; call $bcd4;

[Corrupts AF, DE, HL]
procedure KL_NEW_FRAME_FLY(FrameFlybackBlock: HL as Pointer;EventClass: B;EventRoutine: C;EventRoutine: DE as Pointer); call $bcd7;

[Corrupts AF, DE, HL]
procedure KL_ADD_FRAME_FLY(FrameFlybackBlock: HL as Pointer); call $bcda;

[Corrupts AF, DE, HL]
procedure KL_DEL_FRAME_FLY(FrameFlybackBlock: HL as Pointer); call $bcdd;

[Corrupts AF, DE, HL]
procedure KL_NEW_FAST_TICKER(FastTickerBlock: HL as Pointer;EventClass: B;EventRoutine: C;EventRoutine: DE as Pointer); call $bceo;

[Corrupts AF, DE, HL]
procedure KL_ADD_FAST_TICKER(FastTickerBlock: HL as Pointer); call $bce3;

[Corrupts AF, DE, ilL]
procedure KL_DEL_FAST_TICKER(FastTickerBlock: HL as Pointer); call $bce6;

[Corrupts AF, BC, DE, HL]
procedure KL_ADD_TICKER(TickBlock: HL as Pointer;InitialValue: DE;Value: BC); call $bce9;

[Corrupts A, HL, F]
function KL_DEL_TICKER(TickBlock: HL as Pointer;out CountRemainingBeforeTheNextEvent: DE): CF; call $bcec;

[Preserves all]
function KL_INIT_EVENT(EventBlock: HL as Pointer;EventClass: B;EventRoutine: C;EventRoutine: DE as Pointer): HL as Pointer; call $bcef;

[Corrupts AF, BC, DE, HL]
procedure KL_EVENT(EventBlock: HL as Pointer); call $bcf2;

[Corrupts AF, HL]
procedure KL_SYNC_RESET; call $bcf5;

[Corrupts AF, BC, DE, HL]
procedure KL_DEL_SYNCHRONOUS(EventBlock: HL as Pointer); call $bcf8;

[Corrupts DE]
function KL_NEXT_SYNC(out EventBlock: HL as Pointer;out PreviousEventPriorityIfAny: A): CF; call $bcfb;

[Corrupts AF, BC, DE, HL]
procedure KL_DO_SYNC(EventBlock: HL as Pointer); call $bcfe;

[Corrupts AF, BC, DE, HL corrupt]
procedure KL_DONE_SYNC(PreviousEventPriority: A;EventBlock: HL as Pointer); call $bd01;

[Corrupts HL]
procedure KL_EVENT_DISABLE; call $bd04;

[Corrupts HL]
procedure KL_EVENT_ENABLE; call $bd07;

[Corrupts AF]
procedure KL_DISARM_EVENT(EventBlock: HL as Pointer); call $bd0a;

ERROR in line: DEHL contains the four byte count (D contains the most significant byte and L
ERROR in line: the least significant byte)
[Preserves all]
procedure KL_TIME_PLEASE; call $bd0d;

ERROR in line: DEHL contains the four byte count to set (D contains the most significant byte
ERROR in line: and L the least significant byte)
[Corrupts AF]
procedure KL_TIME_SET; call $bd10;

ERROR in line: Does not exit!
[Preserves all]
procedure MC_BOOT_PROGRAM(RoutineToCallToLoadTheProgram: HL as Pointer); call $bd13;

ERROR in line: Never exits!
[Preserves all]
procedure MC_START_PROGRAM(EntryPointAddress: HL as Pointer;ROMSelection: C); call $bd16;

[Preserves all]
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

[Corrupts A, F]
function MC_PRINT_CHAR(Character: A as Char): CF; call $bd2b;

[Corrupts F]
function MC_BUSY_PRINTER: CF; call $bd2e;

[Corrupts A, F]
procedure MC_SEND_PRINTER(Character: A as Char); call $bd31;

[Corrupts AF, BC]
procedure MC_SOUND_REGISTER(SoundChipRegisterNumber: A;Data: C); call $bd34;

[Corrupts AF, BC. DE, HL]
procedure JUMP_RESTORE; call $bd37;

[Corrupts AF]
procedure KM_SET_LOCKS(CapsLock: H;ShiftLock: L); call $bd3a;

[Corrupts AF]
procedure KM_FLUSH; call $bd3d;

[Corrupts F]
function TXT_ASK_STATE: A; call $bd40;

[Corrupts AF, BC, DE, ilL]
procedure GRA_DEFAULT; call $bd43;

[Preserves all]
procedure GRA_SET_BACK(Mode: A); call $bd46;

[Preserves all]
procedure GRA_SET_FIRST(Zero: A); call $bd49;

[Preserves all]
procedure GRA_SET_LINE_MASK(LineMask: A); call $bd4c;

[Corrupts AF]
procedure GRA_FROM_USER(UserXCoordinate: DE;UserYCoordinate: HL;out BaseXCoordinate: DE;out BaseYCoordinate: HL); call $bd4f;

[Corrupts A, BC, DE, HL, F]
function GRA_FILL(UnencodedInk: A;Buffer: HL as Pointer;Length: DE): CF; call $bd52;

[Corrupts F]
procedure SCR_SET_POSITION(ScreenBase: A;ScreenOffset: HL;out ScreenBaseMaskedAsRequired: A;out ScreenOffsetMaskedAsRequired: HL); call $bd55;

[Corrupts A, BC, DE, HL, F]
function MC_PRINT_TRANSLATION(Table: HL as Pointer): CF; call $bd58;

[Corrupts F]
function KL_BANK_SWITCH(Organization: A): A; call $bd5b;

[Corrupts AF]
procedure IND_TXT_DRAW_CURSOR; call $bdcd;

[Corrupts AF]
procedure IND_TXT_UNDRAW_CURSOR; call $bdd0;

[Corrupts AF, BC, DE, HL]
procedure IND_TXT_WRITE_CHAR(Character: A as Char;PhysicalColumn: H;PhysicalRow: L); call $bdd3;

[Corrupts BC, DE, HL, F]
function IND_TXT_UNWRITE(PhysicalColumn: H;PhysicalRow: L;out CharacterRead: A): CF; call $bdd6;

[Corrupts AF, BC, DE, HL]
procedure IND_TXT_OUT_ACTION(CharacterOrCode: A); call $bdd9;

[Corrupts AF, BC, DE, HL]
procedure IND_GRA_PLOT(UserXCoordinate: DE;UserYCoordinate: HL); call $bddc;

[Corrupts BC, DE, HL, F]
function IND_GRA_TEST(UserXCoordinate: DE;UserYCoordinate: HL): A; call $bddf;

[Corrupts AF, BC. DE, HL]
procedure IND_GRA_LINE(UserXCoordinate: DE;UserYCoordinate: HL); call $bde2;

[Corrupts F]
function IND_SCR_READ(Pixel: HL as Pointer;Mask: C): A; call $bde5;

[Corrupts AF]
procedure IND_SCR_WRITE(PixelS: HL as Pointer;Mask: C;EncodedInk: B); call $bde8;

[Corrupts AF, BC, DE, HL]
procedure IND_SCR_MODE_CLEAR; call $bdeb;

ERROR in line: Interrupts disabled
[Corrupts AF, HL]
procedure IND_KM_TEST_BREAK(ShiftAndControlKeyStates: C); call $bdee;

[Corrupts A, BC]
function IND_MC_WAIT_PRINTER(Character: A as Char): CF; call $bdf1;

[Corrupts F]
function HI_KL_U_ROM_ENABLE: A; call $b900;

[Corrupts F]
function HI_KL_U_ROM_DISABLE: A; call $b903;

[Corrupts F]
function HI_KL_L_ROM_ENABLE: A; call $b906;

[Corrupts F]
function HI_KL_L_ROM_DISABLE: A; call $b909;

[Corrupts AF]
procedure HI_KL_ROM_RESTORE(PreviousROM: A); call $b9oc;

[Corrupts AF]
procedure HI_KL_ROM_SELECT(ROM: C;out PreviouslySelectedROM: C;out PreviousROM: B); call $b90f;

[Preserves all]
function HI_KL_CURR_SELECTION: A; call $b912;

[Corrupts B, F]
procedure HI_KL_PROBE_ROM(ROMToProbe: C;out ROMSClass: A;out ROMSMarkNumber: L;out ROMSVersionNumber: H); call $b915;

[Corrupts B]
function HI_KL_ROM_DESELECT(PreviouslySelectedROM: C;PreviousROM: B): C; call $b918;

ERROR in line: BC, DE, HL as required by the LDIR instruction
ERROR in line: F, BC, DE, HL as set by the LDIR instruction
[Preserves all]
procedure HI_KL_LDIR; call $b91b;

ERROR in line: BC, DE, HL as required by LDDR instruction
ERROR in line: F, BC, DE, HL as set by LDDR instruction
[Preserves all]
procedure HI_KL_LDDR; call $b91e;

[Corrupts A, F]
function HI_KL_POLL_SYNCHRONOUS: CF; call $b921;

[Corrupts AF, HL]
procedure HI_KL_SCAN_NEEDED; call $b92a;

ERROR in line: Does not return!
[Preserves all]
procedure LOW_RESET_ENTRY; call $0000;

ERROR in line: All registers and flags are passed to the target routine untouched
ERROR in line: All registers and flags are as set by the target routine
[Preserves all]
procedure LOW_LOW_JUMP; call $0008;

ERROR in line: All registers and flags are passed to the target routine untouched
ERROR in line: All registers and flags are as set by the target routine
[Preserves all]
procedure LOW_KL_LOW_PCHL(LowAddress: HL as Pointer); call $000b;

ERROR in line: All registers and flags are passed to the target routine untouched
ERROR in line: All registers and flags are as set by the target routine
[Preserves all]
procedure LOW_PCBC_INSTRUCTION(Address: BC as Pointer); call $000e;

ERROR in line: All registers and flags are passed to the target routine untouched except for
ERROR in line: All other registers and flags are as set by the target routine
[Corrupts IY]
procedure LOW_SIDE_CALL(WhichIsSet: IY); call $0010;

ERROR in line: All registers and flags are passed to the target routine untouched except for
ERROR in line: All other registers and flags are as set by the target routine
[Corrupts IY corrupt:]
procedure LOW_KL_SIDE_PCHL(SideAddress: HL as Pointer;WhichIsSet: IY); call $0013;

ERROR in line: All registers and flags are passed to the target routine untouched
ERROR in line: All registers and flags are as set by the target routine
[Preserves all]
procedure LOW_PCDE_INSTRUCTION(Address: DE as Pointer); call $0016;

ERROR in line: All registers and flags are passed to the target routine untouched except for IY
ERROR in line: (which is set to point at a background ROMâ€™s upper data area)
ERROR in line: All other registers and flags are as set by the target routine
[Preserves all]
procedure LOW_FAR_CALL; call $0018;

ERROR in line: All registers and flags are passed to the target routine untouched except for IY (which is set to point at a background ROMâ€™s upper data area)
ERROR in line: All other registers and flags are as set by the target routine
[Preserves all]
procedure LOW_KL_FAR_PCHL(RoutineToCall: HL as Pointer;ROMSelectByte: C); call $001b;

ERROR in line: All registers and flags are passed to the target routine untouched
ERROR in line: All registers and flags are as set by the target routine
[Preserves all]
procedure LOW_PCHL_INSTRUCTION(Address: HL as Pointer); call $001e;

[Preserves all]
function LOW_RAM_LAM(LocationToRead: HL as Pointer): A; call $0020;

ERROR in line: All registers and flags are passed to the target routine untouched except for
ERROR in line: All other registers and flags are as set by the target routine
[Preserves all]
procedure LOW_KL_FAR_ICALL(FArAddressToCall: HL as Pointer;WhichIsSet: IY); call $0023;

ERROR in line: All registers and flags are passed to the target routine untouched
ERROR in line: All registers and flags are as set by the target routine
[Preserves all]
procedure LOW_FIRM_JUMP; call $0028;

ERROR in line: Unknown
ERROR in line: Unknown
[Preserves all]
procedure LOW_USER_RESTART; call $0030;

[Preserves all]
procedure LOW_INTERRUPT_ENTRY; call $0038;

[Corrupts AF, BC, DE, HL]
procedure LOW_EXT_INTERRUPT; call $003b;

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
