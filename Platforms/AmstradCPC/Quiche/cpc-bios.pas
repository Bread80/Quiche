//Import file for the Quiche compiler
//These are the BIOS routines for the Amstrad CPC computers
//These calls are to RSX commands an not currently callable via Quiche

//CTRL/A (#01)
//A=#00 to enable, A=#FF to disable
[Corrupts HL, F]
function BIOS_SET_MESSAGE(Disable: A as Boolean): A as FFBoolean;

//CTRL/B (#02)

[Corrupts AF, BC, DE, HL]
procedure BIOS_SETUP_DISC(ParameterBlock: HL as Pointer);

//CTRL/C (#03)
[Corrupts AF, BC, DE, HL]
procedure BIOS_SELECT_FORMAT(FirstSectorNumber: A;DriveNumber: E);

//CTRL/D (#04)
//Returns True if the sector was read okay
[Corrupts F]
function BIOS_READ_SECTOR(Buffer: HL as Pointer;DriveNumber: E;TrackNumber: D;SectorNumber: C;out Error: A;out ErrorBuffer: HL as Pointer): CF;

//CTRL/E (#05)
//Returns True if the sector was written okay
[Corrupts F]
function BIOS_WRITE_SECTOR(Buffer: HL as Pointer;DriveNumber: E;TrackNumber: D;SectorNumber: C;out Error: A;out ErrorBuffer: HL as Pointer): CF;

//CTRL/F (#06)
//Returns True if the track formatted okay
[Corrupts F]
function BIOS_FORMAT_TRACK(HeaderBuffer: HL as Pointer;DriveNumber: E;TrackNumber: D;out Error: A;out ErrorBuffer: HL as Pointer): CF;

//CTRL/G (#07)
//Returns True if moved to track ok
[Corrupts F]
function BIOS_MOVE_TRACK(DriveNumber: E;TrackNumber: D;out Error: A;out ErrorBuffer: HL as Pointer): CF;

//CTRL/H (#08)
//Returns True if error status byte returned successfully
[Corrupts F corrupt]
procedure BIOS_GET_DR_STATUS(DriveNumber: A;out DriveStatusByte: A;out ErrorBuffer: HL as Pointer): CF;

//CTRL/I (#09)
[Corrupts HL, F]
function BIOS_SET_RETRY_COUNT(RetryCount: A): A;
