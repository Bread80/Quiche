
[Corrupts F]
function HI_KL_U_ROM_ENABLE: A; extern $b900;

[Corrupts F]
function HI_KL_U_ROM_DISABLE: A; extern $b903;

[Corrupts F]
function HI_KL_L_ROM_ENABLE: A; extern $b906;

[Corrupts F]
function HI_KL_L_ROM_DISABLE: A; extern $b909;

[Corrupts AF]
procedure HI_KL_ROM_RESTORE(PreviousROM: A); extern $b90c;

[Corrupts AF]
procedure HI_KL_ROM_SELECT(ROMSelect: C;out PreviousROMSelect: C;out PreviousROMState: B); extern $b90f;

[PreservesAll]
function HI_KL_CURR_SELECTION: A; extern $b912;

[Corrupts B, F]
procedure HI_KL_PROBE_ROM(ROMSelect: C;out ROMClass: A;out ROMMark: L;out ROMVersion: H); extern $b915;

[Corrupts B]
function HI_KL_ROM_DESELECT(PreviouslyROMSelect: C;PreviousROMState: B): C; extern $b918;

[Corrupts F,BC,DE,HL]
procedure HI_KL_LDIR(Source: HL as Pointer;Dest: DE as Pointer;Length: BC); extern $b91b;

[Corrupts F,BC,DE,HL]
procedure HI_KL_LDDR(Source: HL as Pointer;Dest: DE as Pointer;Length: BC); extern $b91e;

//Returns True if there is a higher priority event pending
[Corrupts A, F]
function HI_KL_POLL_SYNCHRONOUS: CF; extern $b921;

[Corrupts AF, HL]
procedure HI_KL_SCAN_NEEDED; extern $b92a;
