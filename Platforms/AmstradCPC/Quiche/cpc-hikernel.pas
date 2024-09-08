//Quiche import file for Amstrad CPC firmware
//This file imports the Hi Kernel Jumpblock

//For documentation see https://www.cpcwiki.eu/index.php/BIOS_Functions

[Corrupts F]
function HI_KL_U_ROM_ENABLE: A; call $b900;

[Corrupts F]
function HI_KL_U_ROM_DISABLE: A; call $b903;

[Corrupts F]
function HI_KL_L_ROM_ENABLE: A; call $b906;

[Corrupts F]
function HI_KL_L_ROM_DISABLE: A; call $b909;

[Corrupts AF]
procedure HI_KL_ROM_RESTORE(PreviousROM: A); call $b90c;

[Corrupts AF]
procedure HI_KL_ROM_SELECT(ROMSelect: C;out PreviousROMSelect: C;out PreviousROMState: B); call $b90f;

[PreservesAll]
function HI_KL_CURR_SELECTION: A; call $b912;

[Corrupts B, F]
procedure HI_KL_PROBE_ROM(ROMSelect: C;out ROMClass: A;out ROMMark: L;out ROMVersion: H); call $b915;

[Corrupts B]
function HI_KL_ROM_DESELECT(PreviouslyROMSelect: C;PreviousROMState: B): C; call $b918;

[Corrupts F,BC,DE,HL]
procedure HI_KL_LDIR(Source: HL as Pointer;Dest: DE as Pointer;Length: BC); call $b91b;

[Corrupts F,BC,DE,HL]
procedure HI_KL_LDDR(Source: HL as Pointer;Dest: DE as Pointer;Length: BC); call $b91e;

//Returns True if there is a higher priority event pending
[Corrupts A, F]
function HI_KL_POLL_SYNCHRONOUS: CF; call $b921;

[Corrupts AF, HL]
procedure HI_KL_SCAN_NEEDED; call $b92a;
