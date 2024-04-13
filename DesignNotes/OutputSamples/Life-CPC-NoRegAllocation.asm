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
;73: for Buffer := Buffer to Buffer + BufSize - 1 do
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
;IL-4: %Buffer_2/None=phi [%Buffer_1 {1}]  [%Buffer_3997796 {4}] 
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
;74: poke(Buffer, False)
;IL-6: _ poke %Buffer_2:Pointer
_initbuffer3:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Buffer  
  ld h,(ix+$05)
                     ;Fragment: poke_hl_imm8
  ld (hl),$00
;75: end
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
;81: SCR_SET_MODE(1)
;IL-0: _ funccall $01:Byteprocedure SCR_SET_MODE(Mode: A as Byte); Register; extern $BC0E;
_initscreen5:
                     ;Fragment: load_p1r8_imm
  ld a,$01
  call $BC0E
;83: TXT_CUR_OFF
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
;91: Result := Y * (ColCount+2) + X
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
;92: end
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
;96: Result := Peek(Buffer + CoordToOffset(X, Y), Boolean)
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
;97: end
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
;103: poke(Buffer + CoordToOffset(X, Y), Value)
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
;110: WriteCell(Buffer, 2,3, True)
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
;IL-3: PUSHBYTE:Boolean/None=move:Boolean True:Boolean 
                     ;Fragment: load_p1r8_imm
  ld a,$ff
                     ;Fragment: push_byte_a
  push af
  inc sp
;IL-4: _ funccall procedure WriteCell(Buffer: Pointer; X: Word; Y: Word; Value: Boolean); StackLocal;
  call _WriteCell
;111: WriteCell(Buffer, 3,4, True)
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
;IL-8: PUSHBYTE:Boolean/None=move:Boolean True:Boolean 
                     ;Fragment: load_p1r8_imm
  ld a,$ff
                     ;Fragment: push_byte_a
  push af
  inc sp
;IL-9: _ funccall procedure WriteCell(Buffer: Pointer; X: Word; Y: Word; Value: Boolean); StackLocal;
  call _WriteCell
;112: WriteCell(Buffer, 4,2, True)
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
;IL-13: PUSHBYTE:Boolean/None=move:Boolean True:Boolean 
                     ;Fragment: load_p1r8_imm
  ld a,$ff
                     ;Fragment: push_byte_a
  push af
  inc sp
;IL-14: _ funccall procedure WriteCell(Buffer: Pointer; X: Word; Y: Word; Value: Boolean); StackLocal;
  call _WriteCell
;113: WriteCell(Buffer, 4,3, True)
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
;IL-18: PUSHBYTE:Boolean/None=move:Boolean True:Boolean 
                     ;Fragment: load_p1r8_imm
  ld a,$ff
                     ;Fragment: push_byte_a
  push af
  inc sp
;IL-19: _ funccall procedure WriteCell(Buffer: Pointer; X: Word; Y: Word; Value: Boolean); StackLocal;
  call _WriteCell
;114: WriteCell(Buffer, 4,4, True)
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
;IL-23: PUSHBYTE:Boolean/None=move:Boolean True:Boolean 
                     ;Fragment: load_p1r8_imm
  ld a,$ff
                     ;Fragment: push_byte_a
  push af
  inc sp
;IL-24: _ funccall procedure WriteCell(Buffer: Pointer; X: Word; Y: Word; Value: Boolean); StackLocal;
  call _WriteCell
_setinitialstate10:
                     ;Fragment: stacklocal_exit
  ld hl,$0002 + $0004	;+ Stack frame size (return addr, prev IX)
  jp stacklocal_exit

;----------SetInitialState

;==========UpdateCell

_updatecell:
                     ;Fragment: stacklocal_enter
  ld hl,-$0003
  call stacklocal_enter
;120: Alive := 0
;IL-0: %Alive_1:U8/None=storeimm:U8 $00:Byte 
_updatecell10:
                     ;Fragment: assign_rel8_imm8
  ld (ix-$01),$00	;Alive
;121: if ReadCell(OldBuf, X-1, Y-1) then
;IL-1: PUSH:U16/None=move:U16 %OldBuf_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$0a)	;OldBuf  
  ld h,(ix+$0b)
                     ;Fragment: push_word
  push hl
;IL-2: PUSH:U16/None=subtract:U16 %X_0:Word $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;X  
  ld h,(ix+$07)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: sub16
  and a
  sbc hl,de	
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: push_word
  push hl
;IL-3: PUSH:U16/None=subtract:U16 %Y_0:Word $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Y  
  ld h,(ix+$05)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: sub16
  and a
  sbc hl,de	
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: push_word
  push hl
;IL-4: CondBranch {11,12} :Boolean funccall:Boolean function ReadCell(Buffer: Pointer; X: Word; Y: Word): Boolean; StackLocal;
  call _ReadCell
                     ;Fragment: atozf
  and a
  jp z,_updatecell12
;122: inc(Alive)
;IL-5: %Alive_2:U8/None=inc:U8 %Alive_1:Byte 1:Integer
_updatecell11:
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$01)	;Alive
                     ;Fragment: inc8_reg
  inc a
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$01),a	;Alive
;123: if ReadCell(OldBuf, X, Y-1) then
;IL-6: Branch {12} 
;IL-7: %Alive_3/None=phi [%Alive_2 {11}]  [%Alive_1 {10}] 
_updatecell12:
;IL-8: PUSH:U16/None=move:U16 %OldBuf_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$0a)	;OldBuf  
  ld h,(ix+$0b)
                     ;Fragment: push_word
  push hl
;IL-9: PUSH:U16/None=move:U16 %X_0:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;X  
  ld h,(ix+$07)
                     ;Fragment: push_word
  push hl
;IL-10: PUSH:U16/None=subtract:U16 %Y_0:Word $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Y  
  ld h,(ix+$05)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: sub16
  and a
  sbc hl,de	
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: push_word
  push hl
;IL-11: CondBranch {13,14} :Boolean funccall:Boolean  function ReadCell(Buffer: Pointer; X: Word; Y: Word): Boolean; StackLocal;
  call _ReadCell
                     ;Fragment: atozf
  and a
  jp z,_updatecell14
;124: inc(Alive)
;IL-12: %Alive_4:U8/None=inc:U8 %Alive_3:Byte 1:Integer
_updatecell13:
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$01)	;Alive
                     ;Fragment: inc8_reg
  inc a
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$01),a	;Alive
;125: if ReadCell(OldBuf, X+1, Y-1) then
;IL-13: Branch {14} 
;IL-14: %Alive_5/None=phi [%Alive_4 {13}]  [%Alive_2 {12}] 
_updatecell14:
;IL-15: PUSH:U16/None=move:U16 %OldBuf_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$0a)	;OldBuf  
  ld h,(ix+$0b)
                     ;Fragment: push_word
  push hl
;IL-16: PUSH:U16/None=add:U16 %X_0:Word $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;X  
  ld h,(ix+$07)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: push_word
  push hl
;IL-17: PUSH:U16/None=subtract:U16 %Y_0:Word $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Y  
  ld h,(ix+$05)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: sub16
  and a
  sbc hl,de	
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: push_word
  push hl
;IL-18: CondBranch {15,16} :Boolean funccall:Boolean  function ReadCell(Buffer: Pointer; X: Word; Y: Word): Boolean; StackLocal;
  call _ReadCell
                     ;Fragment: atozf
  and a
  jp z,_updatecell16
;126: inc(Alive)
;IL-19: %Alive_6:U8/None=inc:U8 %Alive_5:Byte 1:Integer
_updatecell15:
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$01)	;Alive
                     ;Fragment: inc8_reg
  inc a
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$01),a	;Alive
;127: if ReadCell(OldBuf, X-1, Y) then
;IL-20: Branch {16} 
;IL-21: %Alive_7/None=phi [%Alive_6 {15}]  [%Alive_4 {14}] 
_updatecell16:
;IL-22: PUSH:U16/None=move:U16 %OldBuf_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$0a)	;OldBuf  
  ld h,(ix+$0b)
                     ;Fragment: push_word
  push hl
;IL-23: PUSH:U16/None=subtract:U16 %X_0:Word $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;X  
  ld h,(ix+$07)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: sub16
  and a
  sbc hl,de	
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: push_word
  push hl
;IL-24: PUSH:U16/None=move:U16 %Y_0:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Y  
  ld h,(ix+$05)
                     ;Fragment: push_word
  push hl
;IL-25: CondBranch {17,18} :Boolean funccall:Boolean  function ReadCell(Buffer: Pointer; X: Word; Y: Word): Boolean; StackLocal;
  call _ReadCell
                     ;Fragment: atozf
  and a
  jp z,_updatecell18
;128: inc(Alive)
;IL-26: %Alive_8:U8/None=inc:U8 %Alive_7:Byte 1:Integer
_updatecell17:
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$01)	;Alive
                     ;Fragment: inc8_reg
  inc a
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$01),a	;Alive
;129: if ReadCell(OldBuf, X+1, Y) then
;IL-27: Branch {18} 
;IL-28: %Alive_9/None=phi [%Alive_8 {17}]  [%Alive_6 {16}] 
_updatecell18:
;IL-29: PUSH:U16/None=move:U16 %OldBuf_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$0a)	;OldBuf  
  ld h,(ix+$0b)
                     ;Fragment: push_word
  push hl
;IL-30: PUSH:U16/None=add:U16 %X_0:Word $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;X  
  ld h,(ix+$07)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: push_word
  push hl
;IL-31: PUSH:U16/None=move:U16 %Y_0:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Y  
  ld h,(ix+$05)
                     ;Fragment: push_word
  push hl
;IL-32: CondBranch {19,20} :Boolean funccall:Boolean  function ReadCell(Buffer: Pointer; X: Word; Y: Word): Boolean; StackLocal;
  call _ReadCell
                     ;Fragment: atozf
  and a
  jp z,_updatecell20
;130: inc(Alive)
;IL-33: %Alive_10:U8/None=inc:U8 %Alive_9:Byte 1:Integer
_updatecell19:
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$01)	;Alive
                     ;Fragment: inc8_reg
  inc a
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$01),a	;Alive
;131: if ReadCell(OldBuf, X-1, Y+1) then
;IL-34: Branch {20} 
;IL-35: %Alive_11/None=phi [%Alive_10 {19}]  [%Alive_8 {18}] 
_updatecell20:
;IL-36: PUSH:U16/None=move:U16 %OldBuf_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$0a)	;OldBuf  
  ld h,(ix+$0b)
                     ;Fragment: push_word
  push hl
;IL-37: PUSH:U16/None=subtract:U16 %X_0:Word $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;X  
  ld h,(ix+$07)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: sub16
  and a
  sbc hl,de	
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: push_word
  push hl
;IL-38: PUSH:U16/None=add:U16 %Y_0:Word $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Y  
  ld h,(ix+$05)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: push_word
  push hl
;IL-39: CondBranch {21,22} :Boolean funccall:Boolean  function ReadCell(Buffer: Pointer; X: Word; Y: Word): Boolean; StackLocal;
  call _ReadCell
                     ;Fragment: atozf
  and a
  jp z,_updatecell22
;132: inc(Alive)
;IL-40: %Alive_12:U8/None=inc:U8 %Alive_11:Byte 1:Integer
_updatecell21:
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$01)	;Alive
                     ;Fragment: inc8_reg
  inc a
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$01),a	;Alive
;133: if ReadCell(OldBuf, X, Y+1) then
;IL-41: Branch {22} 
;IL-42: %Alive_13/None=phi [%Alive_12 {21}]  [%Alive_10 {20}] 
_updatecell22:
;IL-43: PUSH:U16/None=move:U16 %OldBuf_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$0a)	;OldBuf  
  ld h,(ix+$0b)
                     ;Fragment: push_word
  push hl
;IL-44: PUSH:U16/None=move:U16 %X_0:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;X  
  ld h,(ix+$07)
                     ;Fragment: push_word
  push hl
;IL-45: PUSH:U16/None=add:U16 %Y_0:Word $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Y  
  ld h,(ix+$05)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: push_word
  push hl
;IL-46: CondBranch {23,24} :Boolean funccall:Boolean function ReadCell(Buffer: Pointer; X: Word; Y: Word): Boolean; StackLocal;
  call _ReadCell
                     ;Fragment: atozf
  and a
  jp z,_updatecell24
;134: inc(Alive)
;IL-47: %Alive_14:U8/None=inc:U8 %Alive_13:Byte 1:Integer
_updatecell23:
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$01)	;Alive
                     ;Fragment: inc8_reg
  inc a
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$01),a	;Alive
;135: if ReadCell(OldBuf, X+1, Y+1) then
;IL-48: Branch {24} 
;IL-49: %Alive_15/None=phi [%Alive_14 {23}]  [%Alive_12 {22}] 
_updatecell24:
;IL-50: PUSH:U16/None=move:U16 %OldBuf_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$0a)	;OldBuf  
  ld h,(ix+$0b)
                     ;Fragment: push_word
  push hl
;IL-51: PUSH:U16/None=add:U16 %X_0:Word $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;X  
  ld h,(ix+$07)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: push_word
  push hl
;IL-52: PUSH:U16/None=add:U16 %Y_0:Word $0001:Word
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Y  
  ld h,(ix+$05)
                     ;Fragment: load_p2r16_imm
  ld de,$0001
                     ;Fragment: add16_overflow
  or a				;Clear carry
  adc hl,de
                     ;Fragment: c_overflow
  jp c,raise_overflow
                     ;Fragment: push_word
  push hl
;IL-53: CondBranch {25,26} :Boolean funccall:Boolean  function ReadCell(Buffer: Pointer; X: Word; Y: Word): Boolean; StackLocal;
  call _ReadCell
                     ;Fragment: atozf
  and a
  jp z,_updatecell26
;136: inc(Alive)
;IL-54: %Alive_16:U8/None=inc:U8 %Alive_15:Byte 1:Integer
_updatecell25:
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$01)	;Alive
                     ;Fragment: inc8_reg
  inc a
                     ;Prim: empty
                     ;Fragment: store_rel8_r8
  ld (ix-$01),a	;Alive
;138: if ReadCell(OldBuf, X, Y) then
;IL-55: Branch {26} 
;IL-56: %Alive_17/None=phi [%Alive_16 {25}]  [%Alive_14 {24}] 
_updatecell26:
;IL-57: PUSH:U16/None=move:U16 %OldBuf_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$0a)	;OldBuf  
  ld h,(ix+$0b)
                     ;Fragment: push_word
  push hl
;IL-58: PUSH:U16/None=move:U16 %X_0:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;X  
  ld h,(ix+$07)
                     ;Fragment: push_word
  push hl
;IL-59: PUSH:U16/None=move:U16 %Y_0:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Y  
  ld h,(ix+$05)
                     ;Fragment: push_word
  push hl
;IL-60: CondBranch {27,28} :Boolean funccall:Boolean function ReadCell(Buffer: Pointer; X: Word; Y: Word): Boolean; StackLocal;
  call _ReadCell
                     ;Fragment: atozf
  and a
  jp z,_updatecell28
;139: WriteCell(NewBuf, X, Y, (Alive = 2) or (Alive = 3))
;IL-61: PUSH:U16/None=move:U16 %NewBuf_0:Pointer 
_updatecell27:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$08)	;NewBuf  
  ld h,(ix+$09)
                     ;Fragment: push_word
  push hl
;IL-62: PUSH:U16/None=move:U16 %X_0:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;X  
  ld h,(ix+$07)
                     ;Fragment: push_word
  push hl
;IL-63: PUSH:U16/None=move:U16 %Y_0:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Y  
  ld h,(ix+$05)
                     ;Fragment: push_word
  push hl
;IL-64: %_0:Boolean/None=equal:Boolean %Alive_17:Byte $02:Byte
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$01)	;Alive
                     ;Fragment: compare8_reg_imm
  cp $02
                     ;Prim: empty
                     ;Fragment: zftoboolean
  ld a,0
  jr nz,$+3
  dec a
                     ;Fragment: store_rel8_a
  ld (ix-$02),a        ;$$  
;IL-65: %_0:Boolean/None=equal:Boolean %Alive_17:Byte $03:Byte
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$01)	;Alive
                     ;Fragment: compare8_reg_imm
  cp $03
                     ;Prim: empty
                     ;Fragment: zftoboolean
  ld a,0
  jr nz,$+3
  dec a
                     ;Fragment: store_rel8_a
  ld (ix-$03),a        ;$$  
;IL-66: PUSHBYTE:Boolean/None=or:Boolean %_0:Boolean %_0:Boolean
                     ;Fragment: load_p2r8_rel
  ld b,(ix-$03)	;$$
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$02)	;$$
                     ;Fragment: or8_reg_reg
  or b
                     ;Prim: empty
                     ;Fragment: push_byte_a
  push af
  inc sp
;IL-67: _ funccall procedure WriteCell(Buffer: Pointer; X: Word; Y: Word; Value: Boolean); StackLocal;
  call _WriteCell
;140: else
;IL-68: Branch {29} 
  jp _updatecell29
;141: WriteCell(NewBuf, X, Y, Alive = 3)
;IL-69: PUSH:U16/None=move:U16 %NewBuf_0:Pointer 
_updatecell28:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$08)	;NewBuf  
  ld h,(ix+$09)
                     ;Fragment: push_word
  push hl
;IL-70: PUSH:U16/None=move:U16 %X_0:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;X  
  ld h,(ix+$07)
                     ;Fragment: push_word
  push hl
;IL-71: PUSH:U16/None=move:U16 %Y_0:Word 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;Y  
  ld h,(ix+$05)
                     ;Fragment: push_word
  push hl
;IL-72: PUSHBYTE:Boolean/None=equal:Boolean %Alive_17:Byte $03:Byte
                     ;Fragment: load_p1r8_rel
  ld a,(ix-$01)	;Alive
                     ;Fragment: sub8_reg_imm
  sub a,$03
                     ;Prim: empty
                     ;Fragment: notatoboolean
  neg				;Set carry if A not equal 0
  sbc a,a			;Set A to -1 or 0
  cpl				;Invert bits of A
                     ;Fragment: push_byte_a
  push af
  inc sp
;IL-73: _ funccall procedure WriteCell(Buffer: Pointer; X: Word; Y: Word; Value: Boolean); StackLocal;
  call _WriteCell
;142: end
;IL-74: Branch {29} 
;IL-75: %_1/None=phi [%_0 {27}]  [%_0 {28}] 
_updatecell29:
;IL-76: %_1/None=phi [%_0 {27}]  [%_0 {28}] 
_updatecell30:
                     ;Fragment: stacklocal_exit
  ld hl,$0008 + $0004	;+ Stack frame size (return addr, prev IX)
  jp stacklocal_exit

;----------UpdateCell

;==========UpdateBuffer

_updatebuffer:
                     ;Fragment: stacklocal_enter
  ld hl,-$0008
  call stacklocal_enter
;149: for Y := 1 to RowCount do
;IL-0: %Y_1:U16/None=storeimm:U16 $01:Byte 
_updatebuffer30:
                     ;Fragment: assign_rel16_imm16
  ld (ix-$04),$01	;Y
  ld (ix-$03),$00
;IL-1: %_1:U16/None=move:U16 %RowCount_0:Word 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_RowCount)	;RowCount
                     ;Fragment: store_rel16_r16
  ld (ix-$06),l	;$$
  ld (ix-$05),h
;IL-2: Branch {31} 
;IL-3: %Y_2/None=phi [%Y_1 {30}]  [%Y_0 {37}] 
_updatebuffer31:
;IL-4: %_2/None=phi [%_1 {37}]  [%_0 {30}] 
;IL-5: %X_4/None=phi [%X_3 {37}]  [%X_0 {30}] 
;IL-6: CondBranch {32,38} :Boolean lessequal:U16 %Y_2:Word %_1:Word
                     ;Fragment: load_p1r16_rel
  ld e,(ix-$04)	;Y  
  ld d,(ix-$03)
                     ;Fragment: load_p2r16_rel
  ld l,(ix-$06)	;$$  
  ld h,(ix-$05)
  call compare_u16 ;Call
                     ;Prim: empty
  jp c,_updatebuffer38
;150: for X := 1 to ColCount do
;IL-7: %X_1:U16/None=storeimm:U16 $01:Byte 
_updatebuffer32:
                     ;Fragment: assign_rel16_imm16
  ld (ix-$02),$01	;X
  ld (ix-$01),$00
;IL-8: %_1:U16/None=move:U16 %ColCount_0:Word 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_ColCount)	;ColCount
                     ;Fragment: store_rel16_r16
  ld (ix-$08),l	;$$
  ld (ix-$07),h
;IL-9: Branch {33} 
;IL-10: %X_2/None=phi [%X_1 {32}]  [%X_0 {35}] 
_updatebuffer33:
;IL-11: CondBranch {34,36} :Boolean lessequal:U16 %X_2:Word %_1:Word
                     ;Fragment: load_p1r16_rel
  ld e,(ix-$02)	;X  
  ld d,(ix-$01)
                     ;Fragment: load_p2r16_rel
  ld l,(ix-$08)	;$$  
  ld h,(ix-$07)
  call compare_u16 ;Call
                     ;Prim: empty
  jp c,_updatebuffer36
;151: UpdateCell(OldBuf, NewBuf, X, Y)
;IL-12: PUSH:U16/None=move:U16 %OldBuf_0:Pointer 
_updatebuffer34:
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$06)	;OldBuf  
  ld h,(ix+$07)
                     ;Fragment: push_word
  push hl
;IL-13: PUSH:U16/None=move:U16 %NewBuf_0:Pointer 
                     ;Fragment: load_p1r16_rel
  ld l,(ix+$04)	;NewBuf  
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
;IL-16: _ funccall procedure UpdateCell(OldBuf: Pointer; NewBuf: Pointer; X: Word; Y: Word); StackLocal;
  call _UpdateCell
;152: end
;IL-17: Branch {35} 
;IL-18: %X_3:U16/None=add:U16 %X_2:Word $01:Byte
_updatebuffer35:
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
;IL-19: Branch {33} 
  jp _updatebuffer33
;IL-20: Branch {37} 
_updatebuffer36:
;IL-21: %Y_3:U16/None=add:U16 %Y_2:Word $01:Byte
_updatebuffer37:
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
;IL-22: Branch {31} 
  jp _updatebuffer31
_updatebuffer38:
                     ;Fragment: stacklocal_exit
  ld hl,$0004 + $0004	;+ Stack frame size (return addr, prev IX)
  jp stacklocal_exit

;----------UpdateBuffer

;==========UpdateScreen

_updatescreen:
                     ;Fragment: stacklocal_enter
  ld hl,-$0008
  call stacklocal_enter
;159: for Y := 1 to RowCount do
;IL-0: %Y_1:U16/None=storeimm:U16 $01:Byte 
_updatescreen38:
                     ;Fragment: assign_rel16_imm16
  ld (ix-$04),$01	;Y
  ld (ix-$03),$00
;IL-1: %_1:U16/None=move:U16 %RowCount_0:Word 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_RowCount)	;RowCount
                     ;Fragment: store_rel16_r16
  ld (ix-$06),l	;$$
  ld (ix-$05),h
;IL-2: Branch {39} 
;IL-3: %Y_2/None=phi [%Y_1 {38}]  [%Y_0 {48}] 
_updatescreen39:
;IL-4: %_2/None=phi [%_1 {48}]  [%_0 {38}] 
;IL-5: %X_4/None=phi [%X_3 {48}]  [%X_0 {38}] 
;IL-6: CondBranch {40,49} :Boolean lessequal:U16 %Y_2:Word %_1:Word
                     ;Fragment: load_p1r16_rel
  ld e,(ix-$04)	;Y  
  ld d,(ix-$03)
                     ;Fragment: load_p2r16_rel
  ld l,(ix-$06)	;$$  
  ld h,(ix-$05)
  call compare_u16 ;Call
                     ;Prim: empty
  jp c,_updatescreen49
;162: TXT_SET_CURSOR(1, Y)
;IL-7: _ funccall $01:Byte %Y_2:Wordprocedure TXT_SET_CURSOR(LogicalColumn: H as Byte; LogicalRow: L as Byte); Register; extern $BB75;
_updatescreen40:
                     ;Fragment: load_p1r8_imm
  ld h,$01
                     ;Fragment: load_p2r8_rel
  ld l,(ix-$04)	;Y
  call $BB75
;164: for X := 1 to ColCount do
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
;IL-10: Branch {41} 
;IL-11: %X_2/None=phi [%X_1 {40}]  [%X_0 {46}] 
_updatescreen41:
;IL-12: CondBranch {42,47} :Boolean lessequal:U16 %X_2:Word %_1:Word
                     ;Fragment: load_p1r16_rel
  ld e,(ix-$02)	;X  
  ld d,(ix-$01)
                     ;Fragment: load_p2r16_rel
  ld l,(ix-$08)	;$$  
  ld h,(ix-$07)
  call compare_u16 ;Call
                     ;Prim: empty
  jp c,_updatescreen47
;165: if ReadCell(Buffer, X, Y) then
;IL-13: PUSH:U16/None=move:U16 %Buffer_0:Pointer 
_updatescreen42:
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
;IL-16: CondBranch {43,44} :Boolean funccall:Boolean  function ReadCell(Buffer: Pointer; X: Word; Y: Word): Boolean; StackLocal;
  call _ReadCell
                     ;Fragment: atozf
  and a
  jp z,_updatescreen44
;169: TXT_OUTPUT(#$e9)
;IL-17: _ funccall é:Charprocedure TXT_OUTPUT(Character: A as Char); Register; extern $BB5A;
_updatescreen43:
                     ;Fragment: load_p1r8_imm
  ld a,$e9
  call $BB5A
;170: else
;IL-18: Branch {45} 
  jp _updatescreen45
;171: TXT_OUTPUT(' ')
;IL-19: _ funccall  :Charprocedure TXT_OUTPUT(Character: A as Char); Register; extern $BB5A;
_updatescreen44:
                     ;Fragment: load_p1r8_imm
  ld a,$20
  call $BB5A
;172: end
;IL-20: Branch {45} 
;IL-21: Branch {46} 
_updatescreen45:
;IL-22: %X_3:U16/None=add:U16 %X_2:Word $01:Byte
_updatescreen46:
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
;IL-23: Branch {41} 
  jp _updatescreen41
;IL-24: Branch {48} 
_updatescreen47:
;IL-25: %Y_3:U16/None=add:U16 %Y_2:Word $01:Byte
_updatescreen48:
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
;IL-26: Branch {39} 
  jp _updatescreen39
_updatescreen49:
                     ;Fragment: stacklocal_exit
  ld hl,$0002 + $0004	;+ Stack frame size (return addr, prev IX)
  jp stacklocal_exit

;----------UpdateScreen

;==========_Global

__global:
;183: ColCount := 20
;IL-0: %ColCount_1:U16/None=storeimm:U16 $14:Byte 
__global49:
                     ;Fragment: assign_abs16_imm16
  ld a,$14
  ld (v__Global_ColCount),a		;ColCount
  ld a,$00
  ld (v__Global_ColCount + 1),a
;184: RowCount := 20
;IL-1: %RowCount_1:U16/None=storeimm:U16 $14:Byte 
                     ;Fragment: assign_abs16_imm16
  ld a,$14
  ld (v__Global_RowCount),a		;RowCount
  ld a,$00
  ld (v__Global_RowCount + 1),a
;185: BufSize := (ColCount + 2) * (RowCount + 2)
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
;188: Buffer1 := $4000
;IL-5: %Buffer1_1:U16/None=storeimm:U16 $4000:Pointer 
                     ;Fragment: assign_abs16_imm16
  ld a,$00
  ld (v__Global_Buffer1),a		;Buffer1
  ld a,$40
  ld (v__Global_Buffer1 + 1),a
;189: Buffer2 := Buffer1 + BufSize
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
;192: InitBuffer(Buffer1)
;IL-7: PUSH:U16/None=move:U16 %Buffer1_1:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer1)	;Buffer1
                     ;Fragment: push_word
  push hl
;IL-8: _ funccall procedure InitBuffer(Buffer: Pointer); StackLocal;
  call _InitBuffer
;193: InitBuffer(Buffer2)
;IL-9: PUSH:U16/None=move:U16 %Buffer2_1:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer2)	;Buffer2
                     ;Fragment: push_word
  push hl
;IL-10: _ funccall procedure InitBuffer(Buffer: Pointer); StackLocal;
  call _InitBuffer
;195: InitScreen
;IL-11: _ funccall procedure InitScreen; StackLocal;
  call _InitScreen
;198: SetInitialState(Buffer1)
;IL-12: PUSH:U16/None=move:U16 %Buffer1_1:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer1)	;Buffer1
                     ;Fragment: push_word
  push hl
;IL-13: _ funccall procedure SetInitialState(Buffer: Pointer); StackLocal;
  call _SetInitialState
;201: for var gen:=1 to 20 do
;IL-14: %gen_1:S16/None=storeimm:X8 $01:Byte 
                     ;Fragment: assign_abs16_imm8
  ld a,$01
  ld (v__Global_gen),a		;gen
  xor a
  ld (v__Global_gen + 1),a
;IL-15: %_1:S16/None=storeimm:S16 $14:Byte 
                     ;Fragment: assign_abs16_imm16
  ld a,$14
  ld (v__Global__temp_8),a		;$$
  ld a,$00
  ld (v__Global__temp_8 + 1),a
;IL-16: Branch {50} 
;IL-17: %gen_2/None=phi [%gen_1 {49}]  [%gen_0 {52}] 
__global50:
;IL-18: %Temp_2/None=phi [%Temp_1 {52}]  [%Temp_0 {49}] 
;IL-19: %Buffer1_3/None=phi [%Buffer1_2 {52}]  [%Buffer1_1 {49}] 
;IL-20: %Buffer2_3/None=phi [%Buffer2_2 {52}]  [%Buffer2_1 {49}] 
;IL-21: CondBranch {51,53} :Boolean lessequal:S16 %gen_2:Integer
                     ;Fragment: load_p1r16_abs
  ld de,(v__Global_gen)	;gen
                     ;Fragment: load_p2r16_abs
  ld hl,(v__Global__temp_8)	;$$
  call compare_s16 ;Call
                     ;Prim: empty
  jp c,__global53
;203: UpdateScreen(Buffer1)
;IL-22: PUSH:U16/None=move:U16 %Buffer1_3:Pointer 
__global51:
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer1)	;Buffer1
                     ;Fragment: push_word
  push hl
;IL-23: _ funccall procedure UpdateScreen(Buffer: Pointer); StackLocal;
  call _UpdateScreen
;204: UpdateBuffer(Buffer1, Buffer2)
;IL-24: PUSH:U16/None=move:U16 %Buffer1_3:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer1)	;Buffer1
                     ;Fragment: push_word
  push hl
;IL-25: PUSH:U16/None=move:U16 %Buffer2_3:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer2)	;Buffer2
                     ;Fragment: push_word
  push hl
;IL-26: _ funccall procedure UpdateBuffer(OldBuf: Pointer; NewBuf: Pointer); StackLocal;
  call _UpdateBuffer
;207: var Temp := Buffer1
;IL-27: %Temp_1:U16/None=move:U16 %Buffer1_3:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer1)	;Buffer1
                     ;Fragment: store_abs16_r16
  ld (v__Global_Temp),hl	;Temp
;208: Buffer1 := Buffer2
;IL-28: %Buffer1_2:U16/None=move:U16 %Buffer2_3:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Buffer2)	;Buffer2
                     ;Fragment: store_abs16_r16
  ld (v__Global_Buffer1),hl	;Buffer1
;209: Buffer2 := Temp
;IL-29: %Buffer2_2:U16/None=move:U16 %Temp_1:Pointer 
                     ;Fragment: load_p1r16_abs
  ld hl,(v__Global_Temp)	;Temp
                     ;Fragment: store_abs16_r16
  ld (v__Global_Buffer2),hl	;Buffer2
;210: end
;IL-30: Branch {52} 
;IL-31: %gen_3:S16/None=add:S16 %gen_2:Integer $01:Byte
__global52:
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
;IL-32: Branch {50} 
  jp __global50
__global53:
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
