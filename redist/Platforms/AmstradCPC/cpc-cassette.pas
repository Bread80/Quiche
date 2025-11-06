//Quiche import file for the Amstrad CPC
//These are the casette (and by patching, disc) routines in the firmware

//For documentation see https://www.cpcwiki.eu/index.php/BIOS_Functions

[Corrupts AF, BC, DE, HL]
procedure CAS_INITIALISE; call $bc65;

[Corrupts AF, HL]
procedure CAS_SET_SPEED(ZeroBitLength: HL;Precompensation: A); call $bc68;

//A is zero (False) to disable message, non-zero (True) to enable them
[Corrupts AF]
procedure CAS_NOISY(Disable: A); call $bc6b;

//Returns True if the motor started okay, False if the user hit escape
[Corrupts F]
function CAS_START_MOTOR(out PreviousMotor: A): CF; call $bc6e;

//Returns True if the motor was stopped successfully, False if the user hit escape
[Corrupts F]
function CAS_STOP_MOTOR(out PreviousMotor: A): CF; call $bc71;

//Returns True if the motor was turned on or off succesfully, False if the user pressed escape
[Corrupts A, F]
function CAS_RESTORE_MOTOR(PreviousMotor: A): CF; call $bc74;

//Returns True if the file was opened okay
//Returns Zero Flag set if the user pressed escape (cassette)
//                         failed for any other reason (disc)
//Returns False and Zero clear if an error occured (cassette)
//                                stream already open (disc)
[Corrupts IX, F]
function CAS_IN_OPEN(NameLength: B;Filename: HL as Pointer;Buffer: DE as Pointer;out Escape: ZF;out Header: HL as Pointer;out DataAddr: DE;out LogicalFileLength: BC;out FileTypeOrError: A): CF; call $bc77;

//Returns True if the stream was closed OK
//Disc systems only: 
//  If function returns False then:
//    OtherFail returns False if the file was not open
//    OtherFail returns True if the operation failed for any other reason
[Corrupts BC, DE, HL, F]
function CAS_IN_CLOSE(out OtherFail: ZF;out Error: A): CF; call $bc7a;

[Corrupts AF, BC, DE, HL]
procedure CAS_IN_ABANDON; call $bc7d;

//Returns True if the character was read okay
//Returns Zero Flag set if the user pressed escape (cassette)
//                      if failed for any other reason (disc)
//Returns False and Zero clear if an error occured (cassette)
//                             if end of file found or stream not open (disc)
[Corrupts IX, F]
function CAS_IN_CHAR(out Escape: ZF;out CharacterOrError: A as Char): CF; call $bc80;

//Returns True if the file was read okay
//Returns Zero Flag set if the user pressed escape (cassette)
//                      if failed for any other reason (disc)
//Returns False and Zero clear if the file was not open (both)
[Corrupts BC, DE, IX, F]
function CAS_IN_DIRECT(Address: HL as Pointer;out Escape: ZF;out EntryAddress: HL as Pointer;out Error: A): CF; call $bc83;

[PreservesAll]
procedure CAS_RETURN; call $bc86;

//Returns True if end of file was not found
//Returns Zero Flag set if the user pressed escape (cassette)
//                      if failed for any other reason (disc)
//Returns False and Zero clear if end of file was found (cassette)
//                             if end of file found or stream not open (disc)
[Corrupts IX, F]
function CAS_TEST_EOF(out Escape: ZF;out Error: A): CF; call $bc89;

//Returns True if the file was opened okay
//Returns Zero Flag set if the user pressed escape (cassette)
//                         failed for any other reason (disc)
//Returns False and Zero clear if the stream is already in use (cassette)
//                                stream already open (disc)
[Corrupts BC, DE, IX, F]
function CAS_OUT_OPEN(NameLength: B;Filename: HL as Pointer;Buffer: DE as Pointer;out Escape: ZF;out BufferOut: HL as Pointer;out Error: A): CF; call $bc8c;

//Returns True if the file was closed okay
//Returns Zero Flag set if the user pressed escape (cassette)
//                         failed for any other reason (disc)
//Returns False and Zero clear if the stream was not open (cassette)
//                                stream already open (disc)
[Corrupts BC, DE, HL, IX, F]
function CAS_OUT_CLOSE(out Escape: ZF;out Error: A): CF; call $bc8f;

[Corrupts AF, BC, DE, HL]
procedure CAS_OUT_ABANDON; call $bc92;

//Returns True if the character was written okay
//Returns Zero Flag set if the user pressed escape (cassette)
//                      if failed for any other reason (disc)
//Returns False and Zero clear if the file was not open (both)
[Corrupts A, IX, F]
function CAS_OUT_CHAR(Character: A as Char;out Escape: ZF;out Error: A): CF; call $bc95;

//Returns True if the file was written okay
//Returns Zero Flag set if the user pressed escape (cassette)
//                      if failed for any other reason (disc)
//Returns False and Zero clear if the file was not open (both)
[Corrupts BC, DE, HL, IX, F]
function CAS_OUT_DIRECT(DataToWrite: HL as Pointer;Length: DE;EntryAddress: BC as Pointer;FileType: A;out Escape: ZF;out Error: A): CF; call $bc98;

//Returns True if the file was succeeded
//Returns Zero Flag set if failed for any reason (disc only)
//Returns False and Zero clear if the stream was in use (cassette only)
[Corrupts BC ,DE, HL, IX, F]
function CAS_CATALOG(Buffer: DE as Pointer;out Escape: ZF;out Error: A): CF; call $bc9b;

//Returns true if the record was written okay
[Corrupts BC, DE, HL, IX]
function CAS_WRITE(Address: HL as Pointer;Length: DE;SyncCharacter: A;out ErrorCode: A): CF; call $bc9e;

//Returns true if the record was read okay
[Corrupts BC, DE, HL, IX, F]
function CAS_READ(Address: HL as Pointer;Length: DE;SyncCharacterExpected: A;out ErrorCode: A): CF; call $bca1;

//Returns true if the data checked ok
[Corrupts BC, DE, HL, IX, F]
function CAS_CHECK(Address: HL as Pointer;Length: DE;SyncCharacterExpected: A;out ErrorCode: A): CF; call $bca4;
