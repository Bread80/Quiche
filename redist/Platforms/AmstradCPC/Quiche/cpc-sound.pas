//Quiche import file for the Amstrad CPC firmware
//Data for the sound manager routines

//For documentation see https://www.cpcwiki.eu/index.php/BIOS_Functions

[Corrupts AF, BC, DE, HL]
procedure SOUND_RESET; call $bca7;

//Returns true if the sound was added to the queue
[Corrupts A, BC, DE, IX, F]
function SOUND_QUEUE(SoundProgram: HL as Pointer): CF; call $bcaa;

[Corrupts BC, DE, HL, F]
function SOUND_CHECK(ChannelBit: A): A; call $bcad;

[Corrupts AF, BC, DE, HL]
procedure SOUND_ARM_EVENT(ChannelBit: A;EventBlock: HL as Pointer); call $bcb0;

[Corrupts AF, BC, DE, HL, IX]
procedure SOUND_RELEASE(ChannelBits: A); call $bcb3;

//Returns True if a sound was active
[Corrupts A, BC, HL, F]
function SOUND_HOLD: CF; call $bcb6;

[Corrupts AF, BC, DE, IX]
procedure SOUND_CONTINUE; call $bcb9;

//Returns True if the envelope was set up OK (i.e. the envelope number was valid)
[Corrupts DE, F]
function SOUND_AMPL_ENVELOPE(EnvelopeNumber: A;DataBlock: HL as Pointer;out DataBlockPlus16: HL as Pointer): CF; call $bcbc;

//Returns True if the envelope was set up OK (i.e. the envelope number was valid)
[Corrupts DE, F]
function SOUND_TONE_ENVELOPE(EnvelopeNumber: A;DataBlock: HL as Pointer;out DataBlockPlus16: HL as Pointer): CF; call $bcbf;

//Returns True if the envelope was found OK (i.e. the envelope number was valid)
[Corrupts A, F]
function SOUND_A_ADDRESS(EnvelopeNumber: A;out DataBlock: HL as Pointer;out Length: BC): CF; call $bcc2;

//Returns True if the envelope was found OK (i.e. the envelope number was valid)
[Corrupts A, F]
function SOUND_T_ADDRESS(EnvelopeNumber: A;out DataBlock: HL as Pointer;out Length: BC): CF; call $bcc5;
