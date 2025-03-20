//Import file for the Quiche compiler
//Import for Jump Restore routine for the Amstrad CPC firmware

//For documentation see https://www.cpcwiki.eu/index.php/BIOS_Functions

[Corrupts AF, BC, DE, HL]
procedure JUMP_RESTORE; call $bd37;
