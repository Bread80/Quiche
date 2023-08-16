;Quiche template file for running test cases in Z80Emulator
;----
;Designed to be assembled with RASM.

org $8000
  ld sp,$0000
  ld ix,$b000
  call __global					;Jump to generated code
  halt
  

last_error_code: ;$800a
	db 0	;Address to store error code
last_error_addr: ;$800b
	dw 0    ;Address to store last error address


  
;Writes a new line to the serial port
;Entry: A=character
;Exit:  All resgisters and flags preserved
s_newline:
  ret
  
;;===============================================
;Writes a character to the serial port.
;Waits for character to be sent.
;Entry: A=character
;Exit:  All resgisters and flags preserved
s_writechar:
  ret
  
;Write a four digit hex word to the serial port
;Entry: HL=the value to write
;Exit:  All registers and flags corrupt
s_writehex16:
  ret

;Write a character to the serial port
;Entry: A=character to output
;Exit: All registers and flags corrupt
s_sendchar:
  ret
