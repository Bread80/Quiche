//Import file for Quiche compiler
//Imports the Amstrad CPC firmware keyboard routines

[Corrupts AF, BC, DE, HL]
procedure KM_INITIALISE; extern $bb00;

[Corrupts AF, BC, DE, HL]
procedure KM_RESET; extern $bb03;

[Corrupts F]
function KM_WAIT_CHAR: A as Char; extern $bb06;

//Returns true if char available
[Corrupts F]
function KM_READ_CHAR(out Character: A as Char): CF; extern $bb09;

[PreservesAll]
procedure KM_CHAR_RETURN(Character: A as Char); extern $bb0c;

//Returns true if expansion okay
[Corrupts A, BC, DE, HL, F]
function KM_SET_EXPAND(ExpansionToken: B;Length: C;Str: HL as Pointer): CF; extern $bb0f;

//Returns true if character found
[Corrupts DE, F]
function KM_GET_EXPAND(ExpansionToken: A;CharacterNumber: L;out Character: A as Char): CF; extern $bb12;

//Returns true if buffer okay
[Corrupts A, BC, DE, HL, F]
function KM_EXP_BUFFER(Buffer: DE as Pointer;Length: HL): CF; extern $bb15;

[Corrupts F]
function KM_WAIT_KEY: A; extern $bb18;

//Returns true if key available
[Corrupts F]
function KM_READ_KEY(out CharacterOrExpansionToken: A): CF; extern $bb1b;

//Returns *false* if key pressed! (i.e. zero flag after a CP
[Corrupts A, HL, F]
function KM_TEST_KEY(KeyNumber: A;out ShiftAndControl: C): ZF; extern $bb1e;

[Corrupts AF]
procedure KM_GET_STATE(out ShiftLock: L;out CapsLock: H); extern $bb21;

[Corrupts F]
procedure KM_GET_JOYSTICK(out Joystick0H: H;out Joystick1: L;out Joystick0A: A); extern $bb24;

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

//FFBoolean: $FF = true, $00 = false
[Corrupts AF, BC, HL]
procedure KM_SET_REPEAT(DoRepeat: B as FFBoolean;KeyNumber: A); extern $bb39;

//Returns false if allowed to repeat
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

[Corrupts AF]
procedure KM_SET_LOCKS(CapsLock: H;ShiftLock: L); extern $bd3a;

[Corrupts AF]
procedure KM_FLUSH; extern $bd3d;
