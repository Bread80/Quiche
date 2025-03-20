;Quiche platform file for use with the included emulator
;----
;Designed to be assembled with RASM.

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

;Round robin buffer to store text/characters written to the output
out_buf_head:		;$800d
  db 0				;Buffer head pointer (points to next offset to be written to)
out_buffer:			;$800e..$810d
org $+256			;Buffer to store written characters


  
;Writes a new line to the serial port
;Entry: A=character
;Exit:  All registers and flags preserved
s_newline:
	push af
	ld a,13
	call s_writechar
	ld a,10
	call s_writechar
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
	push hl				;Preserve registers
	push bc
	push af
	ld a,(out_buf_head)	;Get buffer head offset
	ld c,a				;LD BC,A
	ld b,0
	inc a				;INC head pointer. Note it'll wrap nicely
	ld (out_buf_head),a	;Store new head pointer
	ld hl,out_buffer	;Get buffer base address
	add hl,bc			;Add offset
	pop af				;Retrieve data
	ld (hl),a			;Store data in buffer
	pop bc				;Restore registers
	pop hl
  ret
  
;Write a four digit hex word to the serial port
;Entry: HL=the value to write
;Exit:  All registers and flags corrupt
s_writehex16:
  ret


