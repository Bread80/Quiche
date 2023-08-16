;Quiche template file for RC2014 running SCM (Small Computer Monitor)
;----
;Designed to be assembled with RASM.

org $8000

  jp quiche					;Jump to generated code


  
;Writes a new line to the serial port
;Entry: A=character
;Exit:  All resgisters and flags preserved
s_newline:
  push af
  push bc
  push de
  push hl
  ld c,$07					;New line
  rst $30					;SCM API
  pop hl
  pop de
  pop bc
  pop af
  ret
  
;;===============================================
;Writes a character to the serial port.
;Waits for character to be sent.
;Entry: A=character
;Exit:  All resgisters and flags preserved
s_writechar:
  push af
  push bc
  push de
  push hl
  ld c,$02					;Write character
  rst $30					;SCM API
  pop hl
  pop de
  pop bc
  pop af
  ret
  
;Write a four digit hex word to the serial port
;Entry: HL=the value to write
;Exit:  All registers and flags corrupt
s_writehex16:
  push af
  push bc
  push de
  push hl
  ld a,h
  call .hdigit
  pop hl
  push hl
  ld a,h
  call .ldigit
  pop hl
  push hl
  ld a,l
  call .hdigit
  pop hl
  push hl
  ld a,l
  call .ldigit
  pop hl
  pop de
  pop bc
  pop af
  ret
  
.hdigit
  rra
  rra
  rra
  rra
.ldigit
  and $0f
  add '0'
  cp '9'+1
  jp c,s_sendchar
  add 'a'-':'

;Write a character to the serial port
;Entry: A=character to output
;Exit: All registers and flags corrupt
s_sendchar:
  ld c,$02		;Output character and wait
  rst $30		;Call firmware
  ret

  
;;===============================================
;Insert device independant Z80 core library

;include "quichecore.asm"




;;===============================================
;Insert Quiche generated code here
quiche:
  ld ix,$b000		;Initialise stack frame (temporary)
  
include "../Output/quicheoutput.asm"


  
;;==============
;The following is some early test code until quiche output is actually being generated
  ld hl,$0000
.loop
  call s_writehex16
  ld a,' '
  call s_writechar
  
  inc h
  jr nz,.loop
  
  ret