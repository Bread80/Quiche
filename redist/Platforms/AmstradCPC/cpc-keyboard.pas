//Import file for Quiche compiler
//Imports the Amstrad CPC firmware keyboard routines

//For documentation see https://www.cpcwiki.eu/index.php/BIOS_Functions

[Corrupts AF, BC, DE, HL]
procedure KM_INITIALISE; call $bb00;

[Corrupts AF, BC, DE, HL]
procedure KM_RESET; call $bb03;

[Corrupts F]
function KM_WAIT_CHAR: A as Char; call $bb06;

//Returns true if char available
[Corrupts F]
function KM_READ_CHAR(out Character: A as Char): CF; call $bb09;

[PreservesAll]
procedure KM_CHAR_RETURN(Character: A as Char); call $bb0c;

//Returns true if expansion okay
[Corrupts A, BC, DE, HL, F]
function KM_SET_EXPAND(ExpansionToken: B;Length: C;Str: HL as Pointer): CF; call $bb0f;

//Returns true if character found
[Corrupts DE, F]
function KM_GET_EXPAND(ExpansionToken: A;CharacterNumber: L;out Character: A as Char): CF; call $bb12;

//Returns true if buffer okay
[Corrupts A, BC, DE, HL, F]
function KM_EXP_BUFFER(Buffer: DE as Pointer;Length: HL): CF; call $bb15;

[Corrupts F]
function KM_WAIT_KEY: A; call $bb18;

//Returns true if key available
[Corrupts F]
function KM_READ_KEY(out CharacterOrExpansionToken: A): CF; call $bb1b;

//Returns *false* if key pressed! (i.e. zero flag after a CP
[Corrupts A, HL, F]
function KM_TEST_KEY(KeyNumber: A;out ShiftAndControl: C): ZF; call $bb1e;

[Corrupts AF]
procedure KM_GET_STATE(out ShiftLock: L;out CapsLock: H); call $bb21;

[Corrupts F]
procedure KM_GET_JOYSTICK(out Joystick0H: H;out Joystick1: L;out Joystick0A: A); call $bb24;

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

//FFBoolean: $FF = true, $00 = false
[Corrupts AF, BC, HL]
procedure KM_SET_REPEAT(DoRepeat: B as FFBoolean;KeyNumber: A); call $bb39;

//Returns false if allowed to repeat
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

[Corrupts AF]
procedure KM_SET_LOCKS(CapsLock: H;ShiftLock: L); call $bd3a;

[Corrupts AF]
procedure KM_FLUSH; call $bd3d;
