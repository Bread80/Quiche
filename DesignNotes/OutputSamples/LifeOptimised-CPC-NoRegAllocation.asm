;Quiche object code
;Auto-created. Will be overwritten!
;Designed for RASM assembler

;Insert platform specific code
include "C:\Dropbox\Delphi\Quiche\Platforms\AmstradCPC\Assembler/AmstradCPC.asm"

;Insert Quiche libraries
include "C:\Dropbox\Delphi\Quiche\Assembler/QuicheCore.asm"

;Generated code starts here
quiche:
;==========InitBuffer

_initbuffer:
                     ;Fragment: stacklocal_enter
  ld hl,-$0004
  call stacklocal_enter
;72: for Buffer := Buffer to Buffer + BufSize - 1 do
;IL-0: %Buffer_1:U16/None=move:U16 %Buffer_0:Pointer 
_initbuffer1:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Buffer  
  ld h,(ix+$05)
                     ;Fragment: store_rel16_r16
  ld (ix+$04),l	;Buffer
  ld (ix+$05),h
;IL-1: %_0:U16/None=add:U16 %Buffer_1:Pointer %BufSize_0:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Buffer  
  ld h,(ix+$05)
                     ;Fragment: load_p2r16_abs
  ld de,(v__Global_BufSize)	;BufSize
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$02),l	;$$
  ld (ix-$01),h
;IL-2: %_1:U16/None=subtract:U16 %_0:Pointer $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$02)	;$$  
  ld h,(ix-$01)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: sub16
  and a
  sbc hl,de	
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$04),l	;$$
  ld (ix-$03),h
;IL-3: Branch {2} 
;IL-4: %Buffer_2/None=phi [%Buffer_1 {1}]  [%Buffer_0 {4}] 
_initbuffer2:
;IL-5: CondBranch {3,5} :Boolean lessequal:U16 %Buffer_2:Pointer
                     ;Fragment: load_p1r16_rel
  ld e,(ix+$04)	;Buffer  
  ld d,(ix+$05)
                     ;Fragment: load_p2r16_rel
  ld l,(ix-$04)	;$$  
  ld h,(ix-$03)
  call compare_u16 ;Call
                     ;Prim: empty
  jp c,_initbuffer5
;73: poke(Buffer, 0)
;IL-6: _ poke %Buffer_2:Pointer
_initbuffer3:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Buffer  
  ld h,(ix+$05)
                     ;Fragment: poke_hl_imm8
  ld (hl),$00
;74: end
;IL-7: Branch {4} 
;IL-8: %Buffer_3:U16/None=add:U16 %Buffer_2:Pointer $01:Byte
_initbuffer4:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Buffer  
  ld h,(ix+$05)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix+$04),l	;Buffer
  ld (ix+$05),h
;IL-9: Branch {2} 
  jp _initbuffer2
_initbuffer5:
                     ;Fragment: stacklocal_exit
  ld hl,$0002 + $0004	;+ Stack frame size (return addr, prev IX)
  jp stacklocal_exit

;----------InitBuffer

;==========InitScreen

_initscreen:
                     ;Fragment: stacklocal_enter
  ld hl,-$0000
  call stacklocal_enter
;80: SCR_SET_MODE(1)
;IL-0: _ funccall $01:Byteprocedure SCR_SET_MODE(Mode: A as Byte); Register; extern $BC0E;
_initscreen5:
                     ;Fragment: load_p1r8_imm
  ld a,$01
  call $BC0E
;82: TXT_CUR_OFF
;IL-1: _ funccall procedure TXT_CUR_OFF; Register; extern $BB84;
  call $BB84
_initscreen6:
                     ;Fragment: stacklocal_exit
  ld hl,$0000 + $0004	;+ Stack frame size (return addr, prev IX)
  jp stacklocal_exit

;----------InitScreen

;==========CoordToOffset

_coordtooffset:
                     ;Fragment: stacklocal_enter
  ld hl,-$0006
  call stacklocal_enter
;90: Result := Y * (ColCount+2) + X
;IL-0: %_0:U16/None=add:U16 %ColCount_0:Word $0002:Word
_coordtooffset6:
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_ColCount)	;ColCount
                     ;Fragment: load_p2r16_imm
  ld de,$0002
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$04),l	;$$
  ld (ix-$03),h
;IL-1: %_0:U16/None=multiply:U16 %Y_0:Word %_0:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Y  
  ld h,(ix+$05)
                     ;Fragment: load_p2r16_rel
  ld e,(ix-$04)	;$$  
  ld d,(ix-$03)
  call mult16_u_u__u ;Call
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$06),l	;$$
  ld (ix-$05),h
;IL-2: %CoordToOffset_1:U16/None=add:U16 %_0:Word %X_0:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$06)	;$$  
  ld h,(ix-$05)
                     ;Fragment: load_p2r16_rel
  ld e,(ix+$06)	;X  
  ld d,(ix+$07)
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$02),l	;CoordToOffset
  ld (ix-$01),h
;91: end
;IL-3: DATALOAD: DE:=%CoordToOffset_1:Worddataload %CoordToOffset_1:Word
                     ;Fragment: load_p1r16_rel
  ld e,(ix-$02)	;CoordToOffset  
  ld d,(ix-$01)
_coordtooffset7:
                     ;Fragment: stacklocal_exit
  ld hl,$0004 + $0004	;+ Stack frame size (return addr, prev IX)
  jp stacklocal_exit

;----------CoordToOffset

;==========ReadCell

_readcell:
                     ;Fragment: stacklocal_enter
  ld hl,-$0005
  call stacklocal_enter
;95: Result := Peek(Buffer + CoordToOffset(X, Y), Boolean)
;IL-0: PUSH:U16/None=move:U16 %X_0:Word 
_readcell7:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;X  
  ld h,(ix+$07)
                     ;Fragment: push_word
  push hl
;IL-1: PUSH:U16/None=move:U16 %Y_0:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Y  
  ld h,(ix+$05)
                     ;Fragment: push_word
  push hl
;IL-2: %_0:U16/None=funccall:U16  function CoordToOffset(X: Word; Y: Word): Word; StackLocal;
  call _CoordToOffset
                     ;Fragment: store_rel16_r16
  ld (ix-$03),l	;$$
  ld (ix-$02),h
;IL-3: %_0:U16/None=add:U16 %Buffer_0:Pointer %_0:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$08)	;Buffer  
  ld h,(ix+$09)
                     ;Fragment: load_p2r16_rel
  ld e,(ix-$03)	;$$  
  ld d,(ix-$02)
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$05),l	;$$
  ld (ix-$04),h
;IL-4: %ReadCell_1:Boolean/None=peek:Boolean %_0:Pointer True:Boolean
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$05)	;$$  
  ld h,(ix-$04)
                     ;Fragment: peek8_reg16
  ld a,(hl)
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$01),a	;ReadCell
;96: end
;IL-5: DATALOAD: A:=%ReadCell_1:Booleandataload %ReadCell_1:Boolean
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$01)	;ReadCell
_readcell8:
                     ;Fragment: stacklocal_exit
  ld hl,$0006 + $0004	;+ Stack frame size (return addr, prev IX)
  jp stacklocal_exit

;----------ReadCell

;==========WriteCell

_writecell:
                     ;Fragment: stacklocal_enter
  ld hl,-$0004
  call stacklocal_enter
;102: poke(Buffer + CoordToOffset(X, Y), Value)
;IL-0: PUSH:U16/None=move:U16 %X_0:Word 
_writecell8:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$07)	;X  
  ld h,(ix+$08)
                     ;Fragment: push_word
  push hl
;IL-1: PUSH:U16/None=move:U16 %Y_0:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$05)	;Y  
  ld h,(ix+$06)
                     ;Fragment: push_word
  push hl
;IL-2: %_0:U16/None=funccall:U16  function CoordToOffset(X: Word; Y: Word): Word; StackLocal;
  call _CoordToOffset
                     ;Fragment: store_rel16_r16
  ld (ix-$02),l	;$$
  ld (ix-$01),h
;IL-3: %_0:U16/None=add:U16 %Buffer_0:Pointer %_0:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$09)	;Buffer  
  ld h,(ix+$0a)
                     ;Fragment: load_p2r16_rel
  ld e,(ix-$02)	;$$  
  ld d,(ix-$01)
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$04),l	;$$
  ld (ix-$03),h
;IL-4: _ poke %_0:Pointer
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$04)	;$$  
  ld h,(ix-$03)
                     ;Fragment: load_p2r8_rel
  ld a,(ix+$04)	;Value
                     ;Fragment: poke_hl_reg8
  ld (hl),a
_writecell9:
                     ;Fragment: stacklocal_exit
  ld hl,$0007 + $0004	;+ Stack frame size (return addr, prev IX)
  jp stacklocal_exit

;----------WriteCell

;==========SetInitialState

_setinitialstate:
                     ;Fragment: stacklocal_enter
  ld hl,-$0000
  call stacklocal_enter
;109: WriteCell(Buffer, 2,3, 1)
;IL-0: PUSH:U16/None=move:U16 %Buffer_0:Pointer 
_setinitialstate9:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Buffer  
  ld h,(ix+$05)
                     ;Fragment: push_word
  push hl
;IL-1: PUSH:U16/None=move:U16 $02:Byte 
                     ;Fragment: load_p1r16_imm
  ld hl,$0002
                     ;Fragment: push_word
  push hl
;IL-2: PUSH:U16/None=move:U16 $03:Byte 
                     ;Fragment: load_p1r16_imm
  ld hl,$0003
                     ;Fragment: push_word
  push hl
;IL-3: PUSHBYTE:U8/None=move:U8 $01:Byte 
                     ;Fragment: load_p1r8_imm
  ld a,$01
                     ;Fragment: push_byte_a
  push af
  inc sp
;IL-4: _ funccall procedure WriteCell(Buffer: Pointer; X: Word = 1; Y: Word; Value: Byte); StackLocal;
  call _WriteCell
;110: WriteCell(Buffer, 3,4, 1)
;IL-5: PUSH:U16/None=move:U16 %Buffer_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Buffer  
  ld h,(ix+$05)
                     ;Fragment: push_word
  push hl
;IL-6: PUSH:U16/None=move:U16 $03:Byte 
                     ;Fragment: load_p1r16_imm
  ld hl,$0003
                     ;Fragment: push_word
  push hl
;IL-7: PUSH:U16/None=move:U16 $04:Byte 
                     ;Fragment: load_p1r16_imm
  ld hl,$0004
                     ;Fragment: push_word
  push hl
;IL-8: PUSHBYTE:U8/None=move:U8 $01:Byte 
                     ;Fragment: load_p1r8_imm
  ld a,$01
                     ;Fragment: push_byte_a
  push af
  inc sp
;IL-9: _ funccall procedure WriteCell(Buffer: Pointer; X: Word = 1; Y: Word; Value: Byte); StackLocal;
  call _WriteCell
;111: WriteCell(Buffer, 4,2, 1)
;IL-10: PUSH:U16/None=move:U16 %Buffer_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Buffer  
  ld h,(ix+$05)
                     ;Fragment: push_word
  push hl
;IL-11: PUSH:U16/None=move:U16 $04:Byte 
                     ;Fragment: load_p1r16_imm
  ld hl,$0004
                     ;Fragment: push_word
  push hl
;IL-12: PUSH:U16/None=move:U16 $02:Byte 
                     ;Fragment: load_p1r16_imm
  ld hl,$0002
                     ;Fragment: push_word
  push hl
;IL-13: PUSHBYTE:U8/None=move:U8 $01:Byte 
                     ;Fragment: load_p1r8_imm
  ld a,$01
                     ;Fragment: push_byte_a
  push af
  inc sp
;IL-14: _ funccall procedure WriteCell(Buffer: Pointer; X: Word = 1; Y: Word; Value: Byte); StackLocal;
  call _WriteCell
;112: WriteCell(Buffer, 4,3, 1)
;IL-15: PUSH:U16/None=move:U16 %Buffer_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Buffer  
  ld h,(ix+$05)
                     ;Fragment: push_word
  push hl
;IL-16: PUSH:U16/None=move:U16 $04:Byte 
                     ;Fragment: load_p1r16_imm
  ld hl,$0004
                     ;Fragment: push_word
  push hl
;IL-17: PUSH:U16/None=move:U16 $03:Byte 
                     ;Fragment: load_p1r16_imm
  ld hl,$0003
                     ;Fragment: push_word
  push hl
;IL-18: PUSHBYTE:U8/None=move:U8 $01:Byte 
                     ;Fragment: load_p1r8_imm
  ld a,$01
                     ;Fragment: push_byte_a
  push af
  inc sp
;IL-19: _ funccall procedure WriteCell(Buffer: Pointer; X: Word = 1; Y: Word; Value: Byte); StackLocal;
  call _WriteCell
;113: WriteCell(Buffer, 4,4, 1)
;IL-20: PUSH:U16/None=move:U16 %Buffer_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Buffer  
  ld h,(ix+$05)
                     ;Fragment: push_word
  push hl
;IL-21: PUSH:U16/None=move:U16 $04:Byte 
                     ;Fragment: load_p1r16_imm
  ld hl,$0004
                     ;Fragment: push_word
  push hl
;IL-22: PUSH:U16/None=move:U16 $04:Byte 
                     ;Fragment: load_p1r16_imm
  ld hl,$0004
                     ;Fragment: push_word
  push hl
;IL-23: PUSHBYTE:U8/None=move:U8 $01:Byte 
                     ;Fragment: load_p1r8_imm
  ld a,$01
                     ;Fragment: push_byte_a
  push af
  inc sp
;IL-24: _ funccall procedure WriteCell(Buffer: Pointer; X: Word = 1; Y: Word; Value: Byte); StackLocal;
  call _WriteCell
_setinitialstate10:
                     ;Fragment: stacklocal_exit
  ld hl,$0002 + $0004	;+ Stack frame size (return addr, prev IX)
  jp stacklocal_exit

;----------SetInitialState

;==========UpdateCell

_updatecell:
                     ;Fragment: stacklocal_enter
  ld hl,-$0039
  call stacklocal_enter
;124: var Base: Pointer = OldBuf + Offset
;IL-0: %Base_1:U16/None=add:U16 %OldBuf_0:Pointer %Offset_0:Word
_updatecell10:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$08)	;OldBuf  
  ld h,(ix+$09)
                     ;Fragment: load_p2r16_rel
  ld e,(ix+$04)	;Offset  
  ld d,(ix+$05)
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$04),l	;Base
  ld (ix-$03),h
;127: Alive := peek(Base - ColCount-3) +
;IL-1: %_0:U16/None=subtract:U16 %Base_1:Pointer %ColCount_0:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$04)	;Base  
  ld h,(ix-$03)
                     ;Fragment: load_p2r16_abs
  ld de,(v__Global_ColCount)	;ColCount
                     ;Fragment: sub16
  and a
  sbc hl,de	
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$06),l	;$$
  ld (ix-$05),h
;IL-2: %_0:U16/None=subtract:U16 %_0:Pointer $0003:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$06)	;$$  
  ld h,(ix-$05)
                     ;Fragment: load_p2r16_imm
  ld de,$0003
                     ;Fragment: sub16
  and a
  sbc hl,de	
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$08),l	;$$
  ld (ix-$07),h
;IL-3: %_0:U8/None=peek:U8 %_0:Pointer $FF:Byte
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$08)	;$$  
  ld h,(ix-$07)
                     ;Fragment: peek8_reg16
  ld a,(hl)
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$09),a	;$$
;128: peek(Base - ColCount-2) + peek(Base - ColCount-1) +
;IL-4: %_0:U16/None=subtract:U16 %Base_1:Pointer %ColCount_0:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$04)	;Base  
  ld h,(ix-$03)
                     ;Fragment: load_p2r16_abs
  ld de,(v__Global_ColCount)	;ColCount
                     ;Fragment: sub16
  and a
  sbc hl,de	
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$0b),l	;$$
  ld (ix-$0a),h
;IL-5: %_0:U16/None=subtract:U16 %_0:Pointer $0002:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$0b)	;$$  
  ld h,(ix-$0a)
                     ;Fragment: load_p2r16_imm
  ld de,$0002
                     ;Fragment: sub16
  and a
  sbc hl,de	
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$0d),l	;$$
  ld (ix-$0c),h
;IL-6: %_0:U8/None=peek:U8 %_0:Pointer $FF:Byte
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$0d)	;$$  
  ld h,(ix-$0c)
                     ;Fragment: peek8_reg16
  ld a,(hl)
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$0e),a	;$$
;IL-7: %_0:U8/None=add:U8 %_0:Byte %_0:Byte
                     ;Fragment: load_p2r8_rel
  ld b,(ix-$0e)	;$$
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$09)	;$$
                     ;Fragment: add8_reg_reg
  add a,b
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel8_r8
  ld (ix-$0f),a	;$$
;IL-8: %_0:U16/None=subtract:U16 %Base_1:Pointer %ColCount_0:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$04)	;Base  
  ld h,(ix-$03)
                     ;Fragment: load_p2r16_abs
  ld de,(v__Global_ColCount)	;ColCount
                     ;Fragment: sub16
  and a
  sbc hl,de	
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$11),l	;$$
  ld (ix-$10),h
;IL-9: %_0:U16/None=subtract:U16 %_0:Pointer $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$11)	;$$  
  ld h,(ix-$10)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: sub16
  and a
  sbc hl,de	
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$13),l	;$$
  ld (ix-$12),h
;IL-10: %_0:U8/None=peek:U8 %_0:Pointer $FF:Byte
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$13)	;$$  
  ld h,(ix-$12)
                     ;Fragment: peek8_reg16
  ld a,(hl)
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$14),a	;$$
;IL-11: %_0:U8/None=add:U8 %_0:Byte %_0:Byte
                     ;Fragment: load_p2r8_rel
  ld b,(ix-$14)	;$$
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$0f)	;$$
                     ;Fragment: add8_reg_reg
  add a,b
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel8_r8
  ld (ix-$15),a	;$$
;131: peek(Base - 1) + peek(Base + 1) +
;IL-12: %_0:U16/None=subtract:U16 %Base_1:Pointer $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$04)	;Base  
  ld h,(ix-$03)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: sub16
  and a
  sbc hl,de	
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$17),l	;$$
  ld (ix-$16),h
;IL-13: %_0:U8/None=peek:U8 %_0:Pointer $FF:Byte
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$17)	;$$  
  ld h,(ix-$16)
                     ;Fragment: peek8_reg16
  ld a,(hl)
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$18),a	;$$
;IL-14: %_0:U8/None=add:U8 %_0:Byte %_0:Byte
                     ;Fragment: load_p2r8_rel
  ld b,(ix-$18)	;$$
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$15)	;$$
                     ;Fragment: add8_reg_reg
  add a,b
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel8_r8
  ld (ix-$19),a	;$$
;IL-15: %_0:U16/None=add:U16 %Base_1:Pointer $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$04)	;Base  
  ld h,(ix-$03)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$1b),l	;$$
  ld (ix-$1a),h
;IL-16: %_0:U8/None=peek:U8 %_0:Pointer $FF:Byte
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$1b)	;$$  
  ld h,(ix-$1a)
                     ;Fragment: peek8_reg16
  ld a,(hl)
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$1c),a	;$$
;IL-17: %_0:U8/None=add:U8 %_0:Byte %_0:Byte
                     ;Fragment: load_p2r8_rel
  ld b,(ix-$1c)	;$$
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$19)	;$$
                     ;Fragment: add8_reg_reg
  add a,b
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel8_r8
  ld (ix-$1d),a	;$$
;134: peek(Base + ColCount+1) + peek(Base + ColCount+2) +
;IL-18: %_0:U16/None=add:U16 %Base_1:Pointer %ColCount_0:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$04)	;Base  
  ld h,(ix-$03)
                     ;Fragment: load_p2r16_abs
  ld de,(v__Global_ColCount)	;ColCount
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$1f),l	;$$
  ld (ix-$1e),h
;IL-19: %_0:U16/None=add:U16 %_0:Pointer $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$1f)	;$$  
  ld h,(ix-$1e)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$21),l	;$$
  ld (ix-$20),h
;IL-20: %_0:U8/None=peek:U8 %_0:Pointer $FF:Byte
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$21)	;$$  
  ld h,(ix-$20)
                     ;Fragment: peek8_reg16
  ld a,(hl)
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$22),a	;$$
;IL-21: %_0:U8/None=add:U8 %_0:Byte %_0:Byte
                     ;Fragment: load_p2r8_rel
  ld b,(ix-$22)	;$$
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$1d)	;$$
                     ;Fragment: add8_reg_reg
  add a,b
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel8_r8
  ld (ix-$23),a	;$$
;IL-22: %_0:U16/None=add:U16 %Base_1:Pointer %ColCount_0:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$04)	;Base  
  ld h,(ix-$03)
                     ;Fragment: load_p2r16_abs
  ld de,(v__Global_ColCount)	;ColCount
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$25),l	;$$
  ld (ix-$24),h
;IL-23: %_0:U16/None=add:U16 %_0:Pointer $0002:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$25)	;$$  
  ld h,(ix-$24)
                     ;Fragment: load_p2r16_imm
  ld de,$0002
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$27),l	;$$
  ld (ix-$26),h
;IL-24: %_0:U8/None=peek:U8 %_0:Pointer $FF:Byte
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$27)	;$$  
  ld h,(ix-$26)
                     ;Fragment: peek8_reg16
  ld a,(hl)
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$28),a	;$$
;IL-25: %_0:U8/None=add:U8 %_0:Byte %_0:Byte
                     ;Fragment: load_p2r8_rel
  ld b,(ix-$28)	;$$
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$23)	;$$
                     ;Fragment: add8_reg_reg
  add a,b
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel8_r8
  ld (ix-$29),a	;$$
;135: peek(Base + ColCount+3)
;IL-26: %_0:U16/None=add:U16 %Base_1:Pointer %ColCount_0:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$04)	;Base  
  ld h,(ix-$03)
                     ;Fragment: load_p2r16_abs
  ld de,(v__Global_ColCount)	;ColCount
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$2b),l	;$$
  ld (ix-$2a),h
;IL-27: %_0:U16/None=add:U16 %_0:Pointer $0003:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$2b)	;$$  
  ld h,(ix-$2a)
                     ;Fragment: load_p2r16_imm
  ld de,$0003
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$2d),l	;$$
  ld (ix-$2c),h
;IL-28: %_0:U8/None=peek:U8 %_0:Pointer $FF:Byte
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$2d)	;$$  
  ld h,(ix-$2c)
                     ;Fragment: peek8_reg16
  ld a,(hl)
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$2e),a	;$$
;IL-29: %Alive_1:U8/None=add:U8 %_0:Byte %_0:Byte
                     ;Fragment: load_p2r8_rel
  ld b,(ix-$2e)	;$$
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$29)	;$$
                     ;Fragment: add8_reg_reg
  add a,b
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel8_r8
  ld (ix-$02),a	;Alive
;137: if peek(Base) <> 0 then
;IL-30: %_0:U8/None=peek:U8 %Base_1:Pointer $FF:Byte
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$04)	;Base  
  ld h,(ix-$03)
                     ;Fragment: peek8_reg16
  ld a,(hl)
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$2f),a	;$$
;IL-31: CondBranch {11,15} :Boolean notequal:Boolean %_0:Byte
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$2f)	;$$
                     ;Fragment: compare8_reg_imm
  cp $00
                     ;Prim: empty
  jp z,_updatecell15
;139: if (Alive = 2) or (Alive = 3) then
;IL-32: %_0:Boolean/None=equal:Boolean %Alive_1:Byte $02:Byte
_updatecell11:
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$02)	;Alive
                     ;Fragment: compare8_reg_imm
  cp $02
                     ;Prim: empty
                     ;Fragment: zftoboolean
  ld a,0
  jr nz,$+3
  dec a
                     ;Fragment: store_rel8_a
  ld (ix-$30),a        ;$$  
;IL-33: %_0:Boolean/None=equal:Boolean %Alive_1:Byte $03:Byte
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$02)	;Alive
                     ;Fragment: compare8_reg_imm
  cp $03
                     ;Prim: empty
                     ;Fragment: zftoboolean
  ld a,0
  jr nz,$+3
  dec a
                     ;Fragment: store_rel8_a
  ld (ix-$31),a        ;$$  
;IL-34: CondBranch {12,13} :Boolean or:Boolean %_0:Boolean %_0:Boolean
                     ;Fragment: load_p2r8_rel
  ld b,(ix-$31)	;$$
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$30)	;$$
                     ;Fragment: or8_reg_reg
  or b
                     ;Prim: empty
                     ;Fragment: atozf
  and a
  jp z,_updatecell13
;141: Poke(NewBuf + Offset, 1)
;IL-35: %_0:U16/None=add:U16 %NewBuf_0:Pointer %Offset_0:Word
_updatecell12:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;NewBuf  
  ld h,(ix+$07)
                     ;Fragment: load_p2r16_rel
  ld e,(ix+$04)	;Offset  
  ld d,(ix+$05)
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$33),l	;$$
  ld (ix-$32),h
;IL-36: _ poke %_0:Pointer $01:Byte
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$33)	;$$  
  ld h,(ix-$32)
                     ;Fragment: poke_hl_imm8
  ld (hl),$01
;142: Result := 2
;IL-37: %UpdateCell_1:U8/None=storeimm:U8 $02:Byte 
                     ;Fragment: assign_rel8_imm8
  ld (ix-$01),$02	;UpdateCell
;143: end
;IL-38: Branch {14} 
  jp _updatecell14
;146: poke(NewBuf + Offset, 0)
;IL-39: %_0:U16/None=add:U16 %NewBuf_0:Pointer %Offset_0:Word
_updatecell13:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;NewBuf  
  ld h,(ix+$07)
                     ;Fragment: load_p2r16_rel
  ld e,(ix+$04)	;Offset  
  ld d,(ix+$05)
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$35),l	;$$
  ld (ix-$34),h
;IL-40: _ poke %_0:Pointer $00:Byte
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$35)	;$$  
  ld h,(ix-$34)
                     ;Fragment: poke_hl_imm8
  ld (hl),$00
;147: Result := 0
;IL-41: %UpdateCell_2:U8/None=storeimm:U8 $00:Byte 
                     ;Fragment: assign_rel8_imm8
  ld (ix-$01),$00	;UpdateCell
;148: end
;IL-42: Branch {14} 
;IL-43: %UpdateCell_3/None=phi [%UpdateCell_1 {12}]  [%UpdateCell_2 {13}] 
_updatecell14:
;IL-44: %_1/None=phi [%_0 {12}]  [%_0 {13}] 
;IL-45: %_1/None=phi [%_0 {12}]  [%_0 {13}] 
;149: end
;IL-46: Branch {19} 
  jp _updatecell19
;152: if Alive = 3 then
;IL-47: CondBranch {16,17} :Boolean equal:Boolean %Alive_1:Byte $03:Byte
_updatecell15:
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$02)	;Alive
                     ;Fragment: compare8_reg_imm
  cp $03
                     ;Prim: empty
  jp nz,_updatecell17
;154: poke(NewBuf + Offset, 1)
;IL-48: %_0:U16/None=add:U16 %NewBuf_0:Pointer %Offset_0:Word
_updatecell16:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;NewBuf  
  ld h,(ix+$07)
                     ;Fragment: load_p2r16_rel
  ld e,(ix+$04)	;Offset  
  ld d,(ix+$05)
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$37),l	;$$
  ld (ix-$36),h
;IL-49: _ poke %_0:Pointer $01:Byte
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$37)	;$$  
  ld h,(ix-$36)
                     ;Fragment: poke_hl_imm8
  ld (hl),$01
;155: Result := 1
;IL-50: %UpdateCell_4:U8/None=storeimm:U8 $01:Byte 
                     ;Fragment: assign_rel8_imm8
  ld (ix-$01),$01	;UpdateCell
;156: end
;IL-51: Branch {18} 
  jp _updatecell18
;159: Poke(NewBuf + Offset, 0)
;IL-52: %_0:U16/None=add:U16 %NewBuf_0:Pointer %Offset_0:Word
_updatecell17:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;NewBuf  
  ld h,(ix+$07)
                     ;Fragment: load_p2r16_rel
  ld e,(ix+$04)	;Offset  
  ld d,(ix+$05)
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$39),l	;$$
  ld (ix-$38),h
;IL-53: _ poke %_0:Pointer $00:Byte
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$39)	;$$  
  ld h,(ix-$38)
                     ;Fragment: poke_hl_imm8
  ld (hl),$00
;160: Result := 2
;IL-54: %UpdateCell_5:U8/None=storeimm:U8 $02:Byte 
                     ;Fragment: assign_rel8_imm8
  ld (ix-$01),$02	;UpdateCell
;161: end
;IL-55: Branch {18} 
;IL-56: %UpdateCell_6/None=phi [%UpdateCell_4 {16}]  [%UpdateCell_5 {17}] 
_updatecell18:
;IL-57: %_1/None=phi [%_0 {16}]  [%_0 {17}] 
;IL-58: %_1/None=phi [%_0 {16}]  [%_0 {17}] 
;162: end
;IL-59: Branch {19} 
;IL-60: %UpdateCell_7/None=phi [%UpdateCell_2 {14}]  [%UpdateCell_5 {18}] 
_updatecell19:
;IL-61: %_2/None=phi [%_0 {14}]  [%_0 {18}] 
;IL-62: %_2/None=phi [%_0 {14}]  [%_0 {18}] 
;IL-63: %_1/None=phi [%_0 {14}]  [%_0 {18}] 
;IL-64: %_1/None=phi [%_0 {14}]  [%_0 {18}] 
;IL-65: %_2/None=phi [%_0 {14}]  [%_0 {18}] 
;IL-66: %_2/None=phi [%_0 {14}]  [%_0 {18}] 
;163: end
;IL-67: DATALOAD: A:=%UpdateCell_7:Bytedataload %UpdateCell_7:Byte
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$01)	;UpdateCell
_updatecell20:
                     ;Fragment: stacklocal_exit
  ld hl,$0006 + $0004	;+ Stack frame size (return addr, prev IX)
  jp stacklocal_exit

;----------UpdateCell

;==========Update

_update:
                     ;Fragment: stacklocal_enter
  ld hl,-$000b
  call stacklocal_enter
;173: Offset := ColCount+3
;IL-0: %Offset_1:U16/None=add:U16 %ColCount_0:Word $0003:Word
_update20:
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_ColCount)	;ColCount
                     ;Fragment: load_p2r16_imm
  ld de,$0003
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$02),l	;Offset
  ld (ix-$01),h
;174: for Y := 1 to RowCount do
;IL-1: %Y_1:U16/None=storeimm:U16 $01:Byte 
                     ;Fragment: assign_rel16_imm16
  ld (ix-$06),$01	;Y
  ld (ix-$05),$00
;IL-2: %_1:U16/None=move:U16 %RowCount_0:Word 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_RowCount)	;RowCount
                     ;Fragment: store_rel16_r16
  ld (ix-$08),l	;$$
  ld (ix-$07),h
;IL-3: Branch {21} 
;IL-4: %Y_2/None=phi [%Y_1 {20}]  [%Y_0 {32}] 
_update21:
;IL-5: %_2/None=phi [%_1 {32}]  [%_0 {20}] 
;IL-6: %State_3/None=phi [%State_1 {32}]  [%State_0 {20}] 
;IL-7: %X_4/None=phi [%X_3 {32}]  [%X_0 {20}] 
;IL-8: %Offset_5/None=phi [%Offset_4 {32}]  [%Offset_1 {20}] 
;IL-9: CondBranch {22,33} :Boolean lessequal:U16 %Y_2:Word
                     ;Fragment: load_p1r16_rel
  ld e,(ix-$06)	;Y  
  ld d,(ix-$05)
                     ;Fragment: load_p2r16_rel
  ld l,(ix-$08)	;$$  
  ld h,(ix-$07)
  call compare_u16 ;Call
                     ;Prim: empty
  jp c,_update33
;176: for X := 1 to ColCount do
;IL-10: %X_1:U16/None=storeimm:U16 $01:Byte 
_update22:
                     ;Fragment: assign_rel16_imm16
  ld (ix-$04),$01	;X
  ld (ix-$03),$00
;IL-11: %_1:U16/None=move:U16 %ColCount_0:Word 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_ColCount)	;ColCount
                     ;Fragment: store_rel16_r16
  ld (ix-$0a),l	;$$
  ld (ix-$09),h
;IL-12: Branch {23} 
;IL-13: %X_2/None=phi [%X_1 {22}]  [%X_0 {30}] 
_update23:
;IL-14: %State_2/None=phi [%State_1 {30}]  [%State_0 {22}] 
;IL-15: %Offset_3/None=phi [%Offset_2 {30}]  [%Offset_1 {22}] 
;IL-16: CondBranch {24,31} :Boolean lessequal:U16 %X_2:Word %_1:Word
                     ;Fragment: load_p1r16_rel
  ld e,(ix-$04)	;X  
  ld d,(ix-$03)
                     ;Fragment: load_p2r16_rel
  ld l,(ix-$0a)	;$$  
  ld h,(ix-$09)
  call compare_u16 ;Call
                     ;Prim: empty
  jp c,_update31
;178: var State := UpdateCell(OldBuf, NewBuf, Offset)
;IL-17: PUSH:U16/None=move:U16 %OldBuf_0:Pointer 
_update24:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;OldBuf  
  ld h,(ix+$07)
                     ;Fragment: push_word
  push hl
;IL-18: PUSH:U16/None=move:U16 %NewBuf_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;NewBuf  
  ld h,(ix+$05)
                     ;Fragment: push_word
  push hl
;IL-19: PUSH:U16/None=move:U16 %Offset_3:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$02)	;Offset  
  ld h,(ix-$01)
                     ;Fragment: push_word
  push hl
;IL-20: %State_1:U8/None=funccall:U8  function UpdateCell(OldBuf: Pointer; NewBuf: Pointer; Offset: Word): Byte; StackLocal;
  call _UpdateCell
                     ;Fragment: store_rel8_r8
  ld (ix-$0b),a	;State
;179: if State < 2 then
;IL-21: CondBranch {25,29} :Boolean less:Boolean %State_1:Byte $02:Byte
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$0b)	;State
                     ;Fragment: compare8_reg_imm
  cp $02
                     ;Prim: empty
  jp nc,_update29
;181: TXT_SET_CURSOR(X, Y)
;IL-22: _ funccall %X_2:Word %Y_2:Wordprocedure TXT_SET_CURSOR(LogicalColumn: H as Byte; LogicalRow: L as Byte); Register; extern $BB75;
_update25:
                     ;Fragment: load_p1r8_rel
  ld h,(ix-$04)	;X
                     ;Fragment: load_p2r8_rel
  ld l,(ix-$06)	;Y
  call $BB75
;182: if State = 0 then
;IL-23: CondBranch {26,27} :Boolean equal:Boolean %State_1:Byte
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$0b)	;State
                     ;Fragment: compare8_reg_imm
  cp $00
                     ;Prim: empty
  jp nz,_update27
;183: TXT_OUTPUT(' ')
;IL-24: _ funccall  :Charprocedure TXT_OUTPUT(Character: A as Char); Register; extern $BB5A;
_update26:
                     ;Fragment: load_p1r8_imm
  ld a,$20
  call $BB5A
;184: else //State = 1
;IL-25: Branch {28} 
  jp _update28
;185: TXT_OUTPUT(#$e9)
;IL-26: _ funccall é:Charprocedure TXT_OUTPUT(Character: A as Char); Register; extern $BB5A;
_update27:
                     ;Fragment: load_p1r8_imm
  ld a,$e9
  call $BB5A
;186: end
;IL-27: Branch {28} 
;IL-28: Branch {29} 
_update28:
;188: inc(Offset)
;IL-29: %Offset_2:U16/None=inc:U16 %Offset_3:Word 1:Integer
_update29:
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$02)	;Offset  
  ld h,(ix-$01)
                     ;Fragment: inc16_reg
  inc hl
                     ;Prim: empty
                     ;Fragment: store_rel16_r16
  ld (ix-$02),l	;Offset
  ld (ix-$01),h
;189: end
;IL-30: Branch {30} 
;IL-31: %X_3:U16/None=add:U16 %X_2:Word $01:Byte
_update30:
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$04)	;X  
  ld h,(ix-$03)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$04),l	;X
  ld (ix-$03),h
;IL-32: Branch {23} 
  jp _update23
;191: inc(Offset,2)
;IL-33: %Offset_4:U16/None=inc:U16 %Offset_3:Word $02:Byte
_update31:
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$02)	;Offset  
  ld h,(ix-$01)
                     ;Fragment: inc16_reg
  inc hl
                     ;Fragment: inc16_reg
  inc hl
                     ;Prim: empty
                     ;Fragment: store_rel16_r16
  ld (ix-$02),l	;Offset
  ld (ix-$01),h
;192: end
;IL-34: Branch {32} 
;IL-35: %Y_3:U16/None=add:U16 %Y_2:Word $01:Byte
_update32:
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$06)	;Y  
  ld h,(ix-$05)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$06),l	;Y
  ld (ix-$05),h
;IL-36: Branch {21} 
  jp _update21
_update33:
                     ;Fragment: stacklocal_exit
  ld hl,$0004 + $0004	;+ Stack frame size (return addr, prev IX)
  jp stacklocal_exit

;----------Update

;==========UpdateScreen

_updatescreen:
                     ;Fragment: stacklocal_enter
  ld hl,-$0008
  call stacklocal_enter
;200: for Y := 1 to RowCount do
;IL-0: %Y_1:U16/None=storeimm:U16 $01:Byte 
_updatescreen33:
                     ;Fragment: assign_rel16_imm16
  ld (ix-$04),$01	;Y
  ld (ix-$03),$00
;IL-1: %_1:U16/None=move:U16 %RowCount_0:Word 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_RowCount)	;RowCount
                     ;Fragment: store_rel16_r16
  ld (ix-$06),l	;$$
  ld (ix-$05),h
;IL-2: Branch {34} 
;IL-3: %Y_2/None=phi [%Y_1 {33}]  [%Y_3997796 {43}] 
_updatescreen34:
;IL-4: %_2/None=phi [%_1 {43}]  [%_0 {33}] 
;IL-5: %X_4/None=phi [%X_3 {43}]  [%X_0 {33}] 
;IL-6: CondBranch {35,44} :Boolean lessequal:U16 %Y_2:Word
                     ;Fragment: load_p1r16_rel
  ld e,(ix-$04)	;Y  
  ld d,(ix-$03)
                     ;Fragment: load_p2r16_rel
  ld l,(ix-$06)	;$$  
  ld h,(ix-$05)
  call compare_u16 ;Call
                     ;Prim: empty
  jp c,_updatescreen44
;203: TXT_SET_CURSOR(1, Y)
;IL-7: _ funccall $01:Byte %Y_2:Wordprocedure TXT_SET_CURSOR(LogicalColumn: H as Byte; LogicalRow: L as Byte); Register; extern $BB75;
_updatescreen35:
                     ;Fragment: load_p1r8_imm
  ld h,$01
                     ;Fragment: load_p2r8_rel
  ld l,(ix-$04)	;Y
  call $BB75
;205: for X := 1 to ColCount do
;IL-8: %X_1:U16/None=storeimm:U16 $01:Byte 
                     ;Fragment: assign_rel16_imm16
  ld (ix-$02),$01	;X
  ld (ix-$01),$00
;IL-9: %_1:U16/None=move:U16 %ColCount_0:Word 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_ColCount)	;ColCount
                     ;Fragment: store_rel16_r16
  ld (ix-$08),l	;$$
  ld (ix-$07),h
;IL-10: Branch {36} 
;IL-11: %X_2/None=phi [%X_1 {35}]  [%X_6684783 {41}] 
_updatescreen36:
;IL-12: CondBranch {37,42} :Boolean lessequal:U16 %X_2:Word %_1:Word
                     ;Fragment: load_p1r16_rel
  ld e,(ix-$02)	;X  
  ld d,(ix-$01)
                     ;Fragment: load_p2r16_rel
  ld l,(ix-$08)	;$$  
  ld h,(ix-$07)
  call compare_u16 ;Call
                     ;Prim: empty
  jp c,_updatescreen42
;206: if ReadCell(Buffer, X, Y) then
;IL-13: PUSH:U16/None=move:U16 %Buffer_0:Pointer 
_updatescreen37:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Buffer  
  ld h,(ix+$05)
                     ;Fragment: push_word
  push hl
;IL-14: PUSH:U16/None=move:U16 %X_2:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$02)	;X  
  ld h,(ix-$01)
                     ;Fragment: push_word
  push hl
;IL-15: PUSH:U16/None=move:U16 %Y_2:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$04)	;Y  
  ld h,(ix-$03)
                     ;Fragment: push_word
  push hl
;IL-16: CondBranch {38,39} :Boolean funccall:Boolean function ReadCell(Buffer: Pointer; X: Word; Y: Word): Boolean; StackLocal;
  call _ReadCell
                     ;Fragment: atozf
  and a
  jp z,_updatescreen39
;210: TXT_OUTPUT(#$e9)
;IL-17: _ funccall é:Charprocedure TXT_OUTPUT(Character: A as Char); Register; extern $BB5A;
_updatescreen38:
                     ;Fragment: load_p1r8_imm
  ld a,$e9
  call $BB5A
;211: else
;IL-18: Branch {40} 
  jp _updatescreen40
;212: TXT_OUTPUT(' ')
;IL-19: _ funccall  :Charprocedure TXT_OUTPUT(Character: A as Char); Register; extern $BB5A;
_updatescreen39:
                     ;Fragment: load_p1r8_imm
  ld a,$20
  call $BB5A
;213: end
;IL-20: Branch {40} 
;IL-21: Branch {41} 
_updatescreen40:
;IL-22: %X_3:U16/None=add:U16 %X_2:Word $01:Byte
_updatescreen41:
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$02)	;X  
  ld h,(ix-$01)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$02),l	;X
  ld (ix-$01),h
;IL-23: Branch {36} 
  jp _updatescreen36
;IL-24: Branch {43} 
_updatescreen42:
;IL-25: %Y_3:U16/None=add:U16 %Y_2:Word $01:Byte
_updatescreen43:
                     ;Fragment: load_p1r16_rel
  ld l,(ix-$04)	;Y  
  ld h,(ix-$03)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_rel16_r16
  ld (ix-$04),l	;Y
  ld (ix-$03),h
;IL-26: Branch {34} 
  jp _updatescreen34
_updatescreen44:
                     ;Fragment: stacklocal_exit
  ld hl,$0002 + $0004	;+ Stack frame size (return addr, prev IX)
  jp stacklocal_exit

;----------UpdateScreen

;==========_Global

__global:
;224: ColCount := 20
;IL-0: %ColCount_1:U16/None=storeimm:U16 $14:Byte 
__global44:
                     ;Fragment: assign_abs16_imm16
  ld a,$14
  ld (v__Global_ColCount),a		;ColCount
  ld a,$00
  ld (v__Global_ColCount + 1),a
;225: RowCount := 20
;IL-1: %RowCount_1:U16/None=storeimm:U16 $14:Byte 
                     ;Fragment: assign_abs16_imm16
  ld a,$14
  ld (v__Global_RowCount),a		;RowCount
  ld a,$00
  ld (v__Global_RowCount + 1),a
;226: BufSize := (ColCount + 2) * (RowCount + 2)
;IL-2: %_0:U16/None=add:U16 %ColCount_1:Word $0002:Word
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_ColCount)	;ColCount
                     ;Fragment: load_p2r16_imm
  ld de,$0002
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_abs16_r16
  ld (v__Global__temp_5),hl	;$$
;IL-3: %_0:U16/None=add:U16 %RowCount_1:Word $0002:Word
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_RowCount)	;RowCount
                     ;Fragment: load_p2r16_imm
  ld de,$0002
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_abs16_r16
  ld (v__Global__temp_6),hl	;$$
;IL-4: %BufSize_1:U16/None=multiply:U16 %_0:Word %_0:Word
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global__temp_5)	;$$
                     ;Fragment: load_p2r16_abs
  ld de,(v__Global__temp_6)	;$$
  call mult16_u_u__u ;Call
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_abs16_r16
  ld (v__Global_BufSize),hl	;BufSize
;229: Buffer1 := $4000
;IL-5: %Buffer1_1:U16/None=storeimm:U16 $4000:Pointer 
                     ;Fragment: assign_abs16_imm16
  ld a,$00
  ld (v__Global_Buffer1),a		;Buffer1
  ld a,$40
  ld (v__Global_Buffer1 + 1),a
;230: Buffer2 := Buffer1 + BufSize
;IL-6: %Buffer2_1:U16/None=add:U16 %Buffer1_1:Pointer %BufSize_1:Word
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer1)	;Buffer1
                     ;Fragment: load_p2r16_abs
  ld de,(v__Global_BufSize)	;BufSize
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: store_abs16_r16
  ld (v__Global_Buffer2),hl	;Buffer2
;233: InitBuffer(Buffer1)
;IL-7: PUSH:U16/None=move:U16 %Buffer1_1:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer1)	;Buffer1
                     ;Fragment: push_word
  push hl
;IL-8: _ funccall procedure InitBuffer(Buffer: Pointer); StackLocal;
  call _InitBuffer
;234: InitBuffer(Buffer2)
;IL-9: PUSH:U16/None=move:U16 %Buffer2_1:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer2)	;Buffer2
                     ;Fragment: push_word
  push hl
;IL-10: _ funccall procedure InitBuffer(Buffer: Pointer); StackLocal;
  call _InitBuffer
;236: InitScreen
;IL-11: _ funccall procedure InitScreen; StackLocal;
  call _InitScreen
;239: SetInitialState(Buffer1)
;IL-12: PUSH:U16/None=move:U16 %Buffer1_1:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer1)	;Buffer1
                     ;Fragment: push_word
  push hl
;IL-13: _ funccall procedure SetInitialState(Buffer: Pointer); StackLocal;
  call _SetInitialState
;240: UpdateScreen(Buffer1)
;IL-14: PUSH:U16/None=move:U16 %Buffer1_1:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer1)	;Buffer1
                     ;Fragment: push_word
  push hl
;IL-15: _ funccall procedure UpdateScreen(Buffer: Pointer); StackLocal;
  call _UpdateScreen
;243: for var gen:=1 to 50 do
;IL-16: %gen_1:S16/None=storeimm:X8 $01:Byte 
                     ;Fragment: assign_abs16_imm8
  ld a,$01
  ld (v__Global_gen),a		;gen
  xor a
  ld (v__Global_gen + 1),a
;IL-17: %_1:S16/None=storeimm:S16 $32:Byte 
                     ;Fragment: assign_abs16_imm16
  ld a,$32
  ld (v__Global__temp_8),a		;$$
  ld a,$00
  ld (v__Global__temp_8 + 1),a
;IL-18: Branch {45} 
;IL-19: %gen_2/None=phi [%gen_1 {44}]  [%gen_2097193 {47}] 
__global45:
;IL-20: %Temp_2/None=phi [%Temp_1 {47}]  [%Temp_0 {44}] 
;IL-21: %Buffer1_3/None=phi [%Buffer1_2 {47}]  [%Buffer1_1 {44}] 
;IL-22: %Buffer2_3/None=phi [%Buffer2_2 {47}]  [%Buffer2_1 {44}] 
;IL-23: CondBranch {46,48} :Boolean lessequal:S16 %gen_2:Integer
                     ;Fragment: load_p1r16_abs
  ld de,(v__Global_gen)	;gen
                     ;Fragment: load_p2r16_abs
  ld hl,(v__Global__temp_8)	;$$
  call compare_s16 ;Call
                     ;Prim: empty
  jp c,__global48
;246: Update(Buffer1, Buffer2)
;IL-24: PUSH:U16/None=move:U16 %Buffer1_3:Pointer 
__global46:
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer1)	;Buffer1
                     ;Fragment: push_word
  push hl
;IL-25: PUSH:U16/None=move:U16 %Buffer2_3:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer2)	;Buffer2
                     ;Fragment: push_word
  push hl
;IL-26: _ funccall procedure Update(OldBuf: Pointer; NewBuf: Pointer); StackLocal;
  call _Update
;249: var Temp := Buffer1
;IL-27: %Temp_1:U16/None=move:U16 %Buffer1_3:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer1)	;Buffer1
                     ;Fragment: store_abs16_r16
  ld (v__Global_Temp),hl	;Temp
;250: Buffer1 := Buffer2
;IL-28: %Buffer1_2:U16/None=move:U16 %Buffer2_3:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer2)	;Buffer2
                     ;Fragment: store_abs16_r16
  ld (v__Global_Buffer1),hl	;Buffer1
;251: Buffer2 := Temp
;IL-29: %Buffer2_2:U16/None=move:U16 %Temp_1:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Temp)	;Temp
                     ;Fragment: store_abs16_r16
  ld (v__Global_Buffer2),hl	;Buffer2
;252: end
;IL-30: Branch {47} 
;IL-31: %gen_3:S16/None=add:S16 %gen_2:Integer $01:Byte
__global47:
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_gen)	;gen
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: pe_overflow
  jp pe,raise_overflow
                     ;Fragment: store_abs16_r16
  ld (v__Global_gen),hl	;gen
;IL-32: Branch {45} 
  jp __global45
__global48:
  ret

;----------_Global

v__Global_ColCount:   dw 0
v__Global_RowCount:   dw 0
v__Global_BufSize:   dw 0
v__Global_Buffer1:   dw 0
v__Global_Buffer2:   dw 0
v__Global__temp_5:   dw 0
v__Global__temp_6:   dw 0
v__Global_gen:   dw 0
v__Global__temp_8:   dw 0
v__Global_Temp:   dw 0

__quiche_end:
