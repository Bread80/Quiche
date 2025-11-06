;Quiche platform file for use with the included emulator
;----
;Designed to be assembled with RASM.

;CP/M BDOS call info, (also for HoG emulator)
bdos equ $0005
bdos_console_output 	equ 2
bdos_direct_console_io 	equ 6
bdos_console_status		equ 11

org $8000
  ld ix,$0000		;Initial stack frame
  ld sp,ix			;Initial stack pointer - playing it safe here as using stack for locals in compiler at the moment
  call __global		;Jump to generated code
  halt
  
;Error codes etc.
last_error_code: 	;$800a
  db 0				;Address to store error code
last_error_addr: 	;$800b
  dw 0    			;Address to store last error address


;Write a character to the console
;Entry: E is the character to output
;Exit: All registers corrupt  
s_raw_bdos_out
	ld c,bdos_console_output
	jp bdos


;Writes a new line to the serial port
;Entry: A=character
;Exit:  All registers and flags preserved
s_newline:
	push hl
	push de
	push bc
	push af
	ld e,13
	call s_raw_bdos_out
	ld e,10
	call s_raw_bdos_out
	pop af
	pop bc
	pop de
	pop hl
	ret
  
;;===============================================

;Write a character to the serial port
;Entry: A=character to output
;Exit: All registers and flags corrupt
s_sendchar:
  
;Writes a character to the default output
;For testing we write the characters to a buffer.
;The buffer will wrap every 256 characters
;Entry: A=character
;Exit:  All registers and flags preserved
s_writechar:
    push hl
	push de
	push bc
	push af
	ld c,bdos_console_output	;BDOS Console Output
	ld e,a				;Char to output
	call s_raw_bdos_out
	pop af
	pop bc
	pop de
	pop hl
ret
  
;Write a four digit hex word to the serial port
;Entry: HL=the value to write
;Exit:  All registers and flags corrupt
s_writehex16:
  ret

;Has a key been pressed?
;Entry: None
;Exit: Carry flag set if a key has been pressed
;      Carry flag clear if no key pressed
;      A and other flags corrupt
_s_keypressed:
	push hl
	push de
	push bc
	ld c,bdos_console_status
	call bdos			;Returns $00 if no char available
	cp 1				;Set carry flag if value is zero
	ccf					;Set carry flag in value is non-zero
	pop bc
	pop de
	pop hl
	ret

;Reads a key from the key buffer
;Entry: None
;Exit: If a key (char) was available,
;			A contains the key (char) read.
;      If no key is available,
;			A is corrupt
;		All other registers are preserved
_s_readkey:
	push hl
	push de
	push bc
	ld c,bdos_direct_console_io
	ld e,$ff			;No echo, no wait
	call bdos			;Returns $00 if no char available
	pop bc
	pop de
	pop hl
	ret	

