;Quiche template file for running test cases in Z80Emulator
;----
;Designed to be assembled with RASM.

TXT_OUTPUT equ $bb5a

org $1000
__quiche_start:
	push ix
				;TODO: Preserve system stack and initialise quiche stack
	ld ix,$8000	;TEMP: Initial starting point for stack variables
	ld sp,ix
	call __global					;Jump to generated code
	halt
  
;Error codes etc.
last_error_code: 	;$800a
  db 0				;Address to store error code
last_error_addr: 	;$800b
  dw 0    			;Address to store last error address

  
;Writes a new line to the serial port
;Entry: A=character
;Exit:  All registers and flags preserved
s_newline:
	push af
	ld a,13
	call TXT_OUTPUT
	ld a,10
	call TXT_OUTPUT
	pop af
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
;Exit:  All resgisters and flags preserved
s_writechar:
	jp TXT_OUTPUT

  
;Write a four digit hex word to the serial port
;Entry: HL=the value to write
;Exit:  All registers and flags corrupt
s_writehex16:
  ret


SAVE 'quiche.bin',__quiche_start,__quiche_end - __quiche_start,DSK,'c:\RetroTools\Quiche\quiche.dsk'
