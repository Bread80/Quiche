//Quiche import file for the Amstrad CPC firmware
//This file imports the kernel routines

//For documentation see https://www.cpcwiki.eu/index.php/BIOS_Functions

[Corrupts AF, HL]
procedure KL_CHOKE_OFF(out ForegroundROM: B;out ROMEntry: DE as Pointer;out RAMSelect: C); call $bcc8;

[Corrupts AF, BC]
procedure KL_ROM_WALK(FirstUsableByte: DE as Pointer;LastUsableByte: HL as Pointer;out NewFirstUsableByte: DE as Pointer;out NewLastUsableByte: HL as Pointer); call $bccb;

[Corrupts AF, B]
procedure KL_INIT_BACK(ROMSelect: C;FirstUsableByte: DE as Pointer;LastUsableByte: HL as Pointer;out NewFirstUsableByte: DE as Pointer;out NewLastUsableByte: HL as Pointer); call $bcce;

[Corrupts DE]
procedure KL_LOG_EXT(RSXCommandTable: BC as Pointer;DataArea: HL as Pointer); call $bcd1;

//Returns True if the command was found
[Corrupts A, B, DE]
function KL_FIND_COMMAND(CommandName: HL as Pointer;out ROMSelect: C;out RoutineAddress: HL as Pointer): CF; call $bcd4;

[Corrupts AF, DE, HL]
procedure KL_NEW_FRAME_FLY(FrameFlybackBlock: HL as Pointer;EventClass: B;ROMSelect: C;EventRoutine: DE as Pointer); call $bcd7;

[Corrupts AF, DE, HL]
procedure KL_ADD_FRAME_FLY(FrameFlybackBlock: HL as Pointer); call $bcda;

[Corrupts AF, DE, HL]
procedure KL_DEL_FRAME_FLY(FrameFlybackBlock: HL as Pointer); call $bcdd;

[Corrupts AF, DE, HL]
procedure KL_NEW_FAST_TICKER(FastTickerBlock: HL as Pointer;EventClass: B;ROMSelect: C;EventRoutine: DE as Pointer); call $bce0;

[Corrupts AF, DE, HL]
procedure KL_ADD_FAST_TICKER(FastTickerBlock: HL as Pointer); call $bce3;

[Corrupts AF, DE, HL]
procedure KL_DEL_FAST_TICKER(FastTickerBlock: HL as Pointer); call $bce6;

[Corrupts AF, BC, DE, HL]
procedure KL_ADD_TICKER(TickBlock: HL as Pointer;InitialCount: DE;Recharge: BC); call $bce9;

//Returns True if the block was found on the tick list
[Corrupts A, HL, F]
function KL_DEL_TICKER(TickBlock: HL as Pointer;out Count: DE): CF; call $bcec;

//Returns event block address + 7
[PreservesAll]
function KL_INIT_EVENT(EventBlock: HL as Pointer;EventClass: B;ROMSelect: C;EventRoutine: DE as Pointer): HL as Pointer; call $bcef;

[Corrupts AF, BC, DE, HL]
procedure KL_EVENT(EventBlock: HL as Pointer); call $bcf2;

[Corrupts AF, HL]
procedure KL_SYNC_RESET; call $bcf5;

[Corrupts AF, BC, DE, HL]
procedure KL_DEL_SYNCHRONOUS(EventBlock: HL as Pointer); call $bcf8;

//Returns True if there is an event to be processed
[Corrupts DE]
function KL_NEXT_SYNC(out EventBlock: HL as Pointer;out PreviousEventPriority: A): CF; call $bcfb;

[Corrupts AF, BC, DE, HL]
procedure KL_DO_SYNC(EventBlock: HL as Pointer); call $bcfe;

[Corrupts AF, BC, DE, HL]
procedure KL_DONE_SYNC(PreviousEventPriority: A;EventBlock: HL as Pointer); call $bd01;

[Corrupts HL]
procedure KL_EVENT_DISABLE; call $bd04;

[Corrupts HL]
procedure KL_EVENT_ENABLE; call $bd07;

[Corrupts AF]
procedure KL_DISARM_EVENT(EventBlock: HL as Pointer); call $bd0a;

//Returns separate high word and low word of the 32-bit time count
[PreservesAll]
procedure KL_TIME_PLEASE(out HighWord: DE;out LowWord: HL); call $bd0d;

//Set the high word and low word of the 32-bit time count
[Corrupts AF]
procedure KL_TIME_SET(HighWord: DE;LowWord: HL); call $bd10;

//Returns the previous organization
[Corrupts F]
function KL_BANK_SWITCH(Organization: A): A; call $bd5b;
