;Core libraries for Quiche compiler

err_none		equ $00
err_overflow 	equ $01
err_divbyzero 	equ $02



;Raise an overflow error
;Entry: Top of Stack = address error occured (I.e. CALL to here)
;Exit: Never does
raise_overflow:
		ld a,err_overflow
		jr raise_error

;Raise a division by zero error
;Entry: Top of Stack = address error occured (I.e. CALL to here)
;Exit: Never does
raise_divbyzero:
		ld a,err_divbyzero
		jr raise_error
		
;Raise and error
;Entry: 
;	A=error code
;	Top of Stack = address error occured (I.e. CALL to here)
;Exit: Never does
raise_error:
		ld (last_error_code), a
		pop hl
		ld (last_error_addr),hl
		ld hl,.error
		call write_ascii7
;.self:		
;		jr .self
		halt
.error: 
	db "Runtime erro","r"+$80
		
;;==================================================MATHS

;----Addition

;HL:=HL+DE where HL is signed value and DE is unsigned value
;with overflow checking
;Returns HL as a signed number.
;AF corrupt
;If overflow never returns
;Overflow tests (and bit 15 values):
;Case	Signed	Unsigned	Test
;A		-ve (1) <$8000(0)	Can't overflow
;B		-ve (1) >=$8000(1)	Overflow if result negative
;C		+ve (0) <$8000(0)	Overflow if result negative
;D		+ve (0) >=$8000(1)	Always overflow
add16_s_u__s_raise_errors:
	ld a,h					;Compare sign (signed) and size (unsigned)
	xor d					;Also, clears carry
	jp m,.always_or_never	;Case A or D

;.ov_if_result_neg			;Cases B or C
	adc hl,de
	ret p					;Positive result: no overflow
	jr raise_overflow		;Negative result: overflow

.always_or_never			;Cases A or D
	adc hl,de
	xor d					;Bit 7 of a is set, Test bit 7 of D
	ret m					;Case A, can't overflow
	jr raise_overflow		;Case D, always overflow

;As above but returning HL as an unsigned number
;Overflow tests (and bit 15 values):
;Case	Signed	Unsigned	Test
;A		-ve (1) <$8000(0)	Overflow if not carry
;B		-ve (1) >=$8000(1)	Can't overflow
;C		+ve (0) <$8000(0)	Can't overflow
;D		+ve (0) >=$8000(1)	Overflow if carry
add16_s_u__u_raise_errors:
	bit 7,h					;Get sign of signed value
	jr nz,.ov_if_notcarry	;Negative - Case A or B
							
	add hl,de				;Cases C or D
	ret nc					;Overflows if carry set
	jr raise_overflow

.ov_if_notcarry				;Cases A or B
	add hl,de
	ret c					;Overflows if carry not set
	jr raise_overflow

;----Subtraction
	
;HL:=HL-DE where HL is signed and DE is unsigned
;with overflow checking
;Returns HL as a signed number.
;AF corrupt
;If overflow never returns
;AF corrupt
;Case	Signed	Unsigned	Test
;A		-ve (1) <$8000(0)	Overflow if result +ve
;B		-ve (1) >=$8000(1)	Always overflow
;C		+ve (0) <$8000(0)	Can't overflow
;D		+ve (0) >=$8000(1)	Overflow if result +ve
sub16_s_u__s_raise_errors:
	ld a,h					;Compare sign (signed) and size (unsigned)
	xor d					;Also, clears carry
	jp p,.always_or_never	;Case B or C

;.ov_if_result_pos			;Cases A and D
	sbc hl,de
	ret m					;Negative result: no overflow
	jr raise_overflow		;Positive result: overflow

.always_or_never			;Cases B and C
	sbc hl,de
	or d					;Bit 7 of a is clear, Test bit 7 of D
	ret p					;Case C, can't overflow
	jr raise_overflow		;Case B, always overflow

;HL:=HL-DE
;where HL is unsigned and DE is signed
;with overflow checking
;Returns HL as a signed number.
;AF corrupt
;If overflow never returns
;Case	Unsigned	Signed	Unsigned	Test
;A		>=$8000(1)	+ve (0) Overflow if result -ve
;B		>=$8000(1)	-ve (1)	Always overflow
;C		<$8000 (0)	+ve (0)	Can't overflow
;D		<$8000 (0)	-ve (1) Overflow if result -ve
sub16_u_s__s_raise_errors:
	ld a,h					;Compare sign (signed) and size (unsigned)
	xor d					;Also, clears carry
	jp p,.always_or_never	;Case B or C

;.ov_if_result_pos			;Cases A and D
	sbc hl,de
	ret p					;Positive result: no overflow
	jr raise_overflow		;Negative result: overflow

.always_or_never			;Cases B and C
	sbc hl,de
	or d					;Bit 7 of a is clear, Test bit 7 of D
	ret p					;Case C, can't overflow
	jr raise_overflow		;Case B, always overflow


;HL:=HL-DE where HL is signed and DE is unsigned
;with overflow checking
;Returns HL as an unsigned number.
;AF corrupt
;If overflow never returns
;AF corrupt
;Case	Signed	Unsigned	Test
;A		-ve (1) <$8000(0)	Always overflow
;B		-ve (1) >=$8000(1)	Always overflow
;C		+ve (0) <$8000(0)	Overflow if carry
;D		+ve (0) >=$8000(1)	Always overflow/Overflow if carry
sub16_s_u__u_raise_errors:
	bit 7,h					;Get sign of signed number
	jr nz,raise_overflow	;Overflow if negative
	and a					;Clear carry
	sbc hl,de
	ret nc					;Error if carry
	jr raise_overflow

;HL:=HL-DE
;where HL is unsigned and DE is signed
;with overflow checking
;Returns HL as an unsigned number.
;AF corrupt
;If overflow never returns
;Case	Unsigned	Signed	Unsigned	Test
;A		>=$8000(1)	+ve (0) Can't overflow
;B		>=$8000(1)	-ve (1)	Overflow if not carry
;C		<$8000 (0)	+ve (0)	Overflow if carry
;D		<$8000 (0)	-ve (1) Can't overflow
sub16_u_s__u_raise_errors:
	ld a,h					;Compare sign (signed) and size (unsigned)
	xor d					;Also, clears carry
	jp p,.can_overflow		;Case B or C

;.can't overflow			;Cases A and D
	sbc hl,de
	ret 					;Can't overflow

.can_overflow				;Cases B and C
	sbc hl,de
							;We need bit 15 of inputs to match the carry flag
							;(Note that both input will have the same bit 15)
	rra						;Carry into bit 7 of A
	xor d					;Test if carry matches bit 15 of inputs
	ret p					;Result will be positive if they match
	jr raise_overflow		;Otherwise overflow
	
;----Negation

;Negate signed 16-bit number in HL
;Returns the result in HL
;No overflow tests
negate_s16:
    xor a
    sub l
    ld l,a
    sbc a,a
    sub h
    ld h,a
	ret
	
;----Multiplication

;8*8 multiply
;HL:=H*E
;Output: A,B,DE corrupt
;From: https://wikiti.brandonw.net/index.php?title=Z80_Routines:Math:Multiplication
mult8_h_e__hl
   ld	d, 0				; Combining the overhead and
   sla	h					; optimised first iteration
   sbc	a, a
   and	e
   ld	l, a
   
   ld	b, 7
.loop:
   add	hl, hl          
   jr	nc, $+3
   add	hl, de
   
   djnz	.loop
   
   ret
   
;Inputs:
;     C is the numerator
;     D is the denominator
;Outputs:
;     A is the remainder
;     B is 0
;     C is the result of C/D
;     D,E,H,L are not changed
;	  If the denominator is zero, jumps to raise_divbyzero and never returns
;From: http://z80-heaven.wikidot.com/math#toc14
div8_c_d__c_a:
	ld a,d
	and a
	jp z,raise_divbyzero
	
    ld b,8
    xor a
      sla c
      rla
      cp d
      jr c,$+4
        inc c
        sub d
      djnz $-8
    ret
;------------------FROM CPC BASIC 1.1

;;=unknown maths fixup
;Bit 7 of B = invert value in HL
unknown_maths_fixup:           	
        ld      a,h              
        or      a               ;Is HL>=$8000? 
        jp      m,unknown_maths_fixup_B ;yes
        or      b               ;Do we need to invert (bit 7 of B set)
        jp      m,negate_HL_with_overflow
        scf                    	;Success
        ret                		 

;;--------------------------------------------------------------
;Negate HL and validate it's a valid INT 
;HL is an unsigned integer >= $8000
unknown_maths_fixup_B:            ;HL=negative value. A is copy of H
        xor     $80               ;Toggle bit 7 (A=H)
        or      l                 ;Zero here mean HL is $8000 (32768). We can negate 32768
        ret     nz                ;Return overflow

        ld      a,b               ;+32768 is $8000. -32768 is $8000. I.e. unchanged
        scf                       ;But +32768 is overflow 
        adc     a,a               ;So, clear CF (overflow) if we're negating, set it if not.
        ret                        

;;=============================================
;Multiply S16 x U16 returning S16 with overflow
;HL:=HL*DE where HL is signed and DE is unsigned
;If overflow, returns CF clear
;	otherwise returns CF set (opposite of unsigned routine)
;Corrupts AF, B, DE registers
mult16_s_u__s:
		ld b,h					;Negate result flag
		bit 7,b
		call nz,negate_HL_with_overflow
		call mult16_u_u__u
		jp nc,unknown_maths_fixup
		or $ff					;Overflow - Clear carry
		ret
		
;Multiply S16 x U16 returning U16 with overflow
;HL:=HL*DE where HL is signed and DE is unsigned
;If overflow, returns CF clear
;	otherwise returns CF set (opposite of unsigned routine)
;Corrupts AF, B, DE registers
mult16_s_u__u:
		ld b,h					;Negate result flag
		bit 7,b
		call nz,negate_HL_with_overflow
		call mult16_u_u__u
		jp nc,.error_if_negative
		or $ff					;Overflow - Clear carry
		ret
		
.error_if_negative
		ld a,h					;If result is zero then sign is immaterial
		or l
		scf
		ret z
		
		ld a,b					;Bit 7 of B is set if the result is negative
		rla						;Bit 7 to carry
		ccf						;Overflow (NC) if negative
		ret
		
;Signed 16*16 multiply with overflow
;HL:=HL*DE
;If overflow, returns CF clear
;	otherwise returns CF set (opposite of unsigned routine)
;Corrupts AF, B, DE registers
mult16_s_s__s:
        call    make_both_operands_positive
        call    mult16_u_u__u	;Returns CF set if overflow
        jp      nc,unknown_maths_fixup	;negate result if needed (B reg), and test for overflow
        or      $ff              
        ret                      ;Clear carry 

;;=make both operands positive
make_both_operands_positive:      
        ld      a,h               
        xor     d                 
        ld      b,a              ;Bit 7 of B set are both operands the different signs
        ex      de,hl            
        call    negate_HL_if_negative_and_test_if_INT
        ex      de,hl             
        jp      negate_HL_if_negative_and_test_if_INT

;Unsigned 16x16 multiply with overflow
;HL:=HL*DE
;If overflow, returns CF set
;	otherwise, CF clear
;Corrupts AF, DE registers
mult16_u_u__u: 
        ld      a,h              
        or      a               ;H=0?
        jr      z,.h_zero
        ld      a,d           
        or      a             	;D=0?
        scf                   
        ret     nz              ;if HL<255 and DE<255 then must overflow so return error

        ex      de,hl          	;If either value had zero as the high byte it's now in HL 
								;with it's high byte in A
.h_zero:
        or      l                 
        ret     z               ;One of the values was zero so result will also be zero
								;And if so HL is already zero. Nice :)

        ld      a,d             
        or      e               ;Is the other value (DE) zero?
        ld      a,l             ;A:=byte to multiply by
        ld      l,e             ;HL:=DE
        ld      h,d             
        ret     z               ;Return zero 

								;At this point, first operand is in HL and DE, second in A
        cp      $03           	;Is multiplier one or two? Is we can short circuit
        jr      c,.one_or_two
        scf                  	;Shift a 1 into bit 0. This is our 'stop marker'
.find_first:
        adc     a,a             ;Left shift to find the first bit. Note that we know A is non-zero
        jr      nc,.find_first
.mult_loop:
        add     hl,hl         	;Left shift the result
        ret     c               ;Overflow

        add     a,a             ;Next bit to CF
        jr      nc,.skip_bit
        add     hl,de             
        ret     c               ;Overflow

.skip_bit:
        cp      $80        		;Finish when the 'stop marker' hits bit 7 (other bits will be zero)
        jr      nz,.mult_loop
        ret                       

.one_or_two:				 	;Multiply by one or two
        cp      $01           
        ret     z            	;Mutliply by 1  

        add     hl,hl        	;Multiply by two
        ret                

;;===================Division and Modulus
;****All these routines:
;Return Carry set if success,
;       Carry clear if overflow
;AF,BC,DE,HL corrupt

;For division the result will be negative if the signed parameter is negative
;B register stores the sign flag (i.e. bit 7)

;HL(signed) := HL(unsigned) / DE(signed)
div16_u_s__s:
		ld b,d					;Invert flag
		call div_mod_invert_param2
		jr do_div_to_s16

;HL(signed) := HL(signed) / DE(unsigned)
div16_s_u__s:
		ld b,h					;Invert flag
		call negate_HL_if_negative_and_test_if_INT
do_div_to_s16:		
		call div16_u_u__u		;Do the division
div_mod_done_s16:
		jp c,unknown_maths_fixup	;Invert if necessary
		ret
	
;HL(unsigned) := HL(signed) / DE(unsigned)
div16_s_u__u:
		ld b,h
		call negate_HL_if_negative_and_test_if_INT
		jr do_div_to_u16

;HL(unsigned) := HL(unsigned) / DE(signed)
div16_u_s__u:
		ld b,d					;Get sign of signed parameter
		call div_mod_invert_param2
do_div_to_u16:
		call div16_u_u__u		;Do the division
do_mod_to_u16:					;Resurns an error if result is negative (ie is B is negative)
								;but /not/ if HL is zero
		ld a,h					;If result is zero, no overflow
		or l
		ccf
		ret z
		rl b					;Sign bit to carry flag
		ccf						;Error (NC) if parameter was negative
		ret
		
;;---Modulus

;For modulus the result is negative if the first parameter is negative,
;(which, obviously) can only happen if the first parameter is signed)

;Note that the resuls is returned in DE is the first parameter is unsigned,
;HL if the first parameter is signed

;DE(signed) := HL(unsigned) mod DE(signed)
mod16_u_s__s:
		call div_mod_invert_param2
		jr div16_u_u__u			;Divide and return
				
;HL(signed) := HL(signed) mod DE(unsigned)
mod16_s_u__s:
		ld b,h					;Invert flag
		call negate_HL_if_negative_and_test_if_INT
		call div16_u_u__u
		ex hl,de
		jr div_mod_done_s16		;Error check
	
;HL(unsigned) := HL(signed) mod DE(unsigned)
mod16_s_u__u:
		ld b,h					;Invert flag
		call negate_HL_if_negative_and_test_if_INT
		call div16_u_u__u
		ex hl,de
		jr do_mod_to_u16		;Error check
		
;DE(unsigned) := HL(unsigned) mod DE(signed)
mod16_u_s__u:
		call div_mod_invert_param2
		jr div16_u_u__u

div_mod_invert_param2:
		ex hl,de
		call negate_HL_if_negative_and_test_if_INT
		ex hl,de
		ret
		

;;INTeger (signed) division
div16_s_s__s: 
        call    _int_modulo_5      
_int_division_1:
        jp      c,unknown_maths_fixup 
        ret                        

;;=INT modulo
mod16_s_s__s:                       
        ld      c,h            	;Preserve H to use as invert flag   
									;(In Pascal mod result is always positive)
        call    _int_modulo_5      
        ex      de,hl              
        ld      b,c           	;B:=Invert flag
        jr      _int_division_1	;Invert as needed and return

;Do the division HL:= HL / DE remainder in DE?
_int_modulo_5:                    
        call    make_both_operands_positive ;Returns bit 7 of B set if result needs to be inverted
		
;HL div DE
;Output: HL is quotient
;		 DE is remainder
;Returns with Carry set (i.e. success)
;If DE is zero, raises Division By Zero error and never returns
div16_u_u__u:               
        ld      a,d         	;Test for division by zero  
        or      e                 
        jp      z,raise_divbyzero

        push    bc                
        ex      de,hl             
        ld      b,$01            
        ld      a,h               
        or      a                 
        jr      nz,_int_modulo_21	;If high by of dividend is non-zero
        ld      a,d               
        cp      l                 
        jr      c,_int_modulo_21 	;if L>D?
        ld      h,l               
        ld      l,$00             
        ld      b,$09             
_int_modulo_21:                  
        ld      a,e               
        sub     l                 
        ld      a,d               
        sbc     a,h               
        jr      c,_int_modulo_30 
        inc     b                 
        add     hl,hl             
        jr      nc,_int_modulo_21	;Loop
        ccf                     
_int_modulo_30:                 
        ccf                      
        ld      a,b              
        ld      b,h              
        ld      c,l              
        ld      hl,$0000         
        jr      _int_modulo_45  

_int_modulo_36:                  
        rr      b                 
        rr      c                 
        ex      de,hl             
        sbc     hl,bc            
        jr      nc,_int_modulo_42
        add     hl,bc             
_int_modulo_42:                  
        ex      de,hl             
        ccf                      
        adc     hl,hl             
_int_modulo_45:                  
        dec     a                 
        jr      nz,_int_modulo_36	;Loop
        scf                        
        pop     bc                
        ret                       

;;--------------------------------------------------------------
abs_hl_with_overflow
		bit 7,h
		jr nz,negate_HL_with_overflow
		scf
		ret
		
;=negate HL and test for overflow
;Result will overflow if input >= 32769
;Entry: HL is unsiged 16
;Exit: HL is signed 16
negate_u16_with_overflow:
	ld a,h
	add a						;Bit 15 to carry
	jr nc,negate_HL_with_overflow	;If bit 15 clear result will be good
								;If bit 15 high only valid value is $8000 (32768)
								;which inverts to -32768 ($8000). Unchanged!
								;We just test if bits 14..0 are zero. If so good, if not overflow
	or l						;Left shift. If A,L are both now zero result will be good
	ret nz						;Carry cleared by OR
	scf
	ret							

;;=negate HL if negative and test if INT
negate_HL_if_negative_and_test_if_INT:
        ld      a,h                
        or      a                  
        ret     p                 ;if HL is positive

;;=negate HL and test if INT
;HL = -HL, then test if it's a valid INT value
;Returns NC if the result is not a valid INT
negate_HL_with_overflow:        
        xor     a                 ;A :=0 
        sub     l                 ;AL:=-L 
        ld      l,a               ;L := -L
        sbc     a,h               ;A := -L - H
        sub     l                 ;A := -L - H - (- L)
        cp      h                 ;Test for 32768? i.e. possible overflow (depending on use case) 
        ld      h,a                
        scf                       
        ret     nz                 

        cp      $01                
        ret                        
		

;Left shift an 8-bit value by a 16-bit count
;Input: A: Value to be shifted
;		BC: Shift count
;Output: A: Shifted value
;		 F,BC corrupt
left_shift_8_by_16:
	dec b					;Is B <> 0?
	inc b
	jr nz,shift_oversize_8    ;Value too large - return zero
	ld b,c

;Left shift an 8-bit value by a 8-bit count
;Input: A: Value to be shifted
;		B: Shift count
;Output: A: Shifted value
;		 F,B corrupt
left_shift_8_by_8
	dec b					;Test if shift count is zero
	inc b
	ret z					;No change

.loop
	add a
	djnz .loop
	ret

shift_oversize_8:
	xor a
	ret
	
;Left shift an 8-bit value by a 16-bit count
;Input: A: Value to be shifted
;		BC: Shift count
;Output: A: Shifted value
;		 F,BC corrupt
right_shift_8_by_16:
	dec b
	inc b
	jr nz,shift_oversize_8  ;Value too large - return zero
	ld b,c

;Left shift an 8-bit value by a 8-bit count
;Input: A: Value to be shifted
;		B: Shift count
;Output: A: Shifted value
;		 F,B corrupt
right_shift_8_by_8
	dec b
	inc b
	ret z					;Shift count is zero
	
.loop
	srl a
	djnz .loop
	ret
	
;Left shift an 16-bit value by a 16-bit count
;Input: HL: Value to be shifted
;		BC: Shift count
;Output: HL: Shifted value
;		 F,B corrupt
left_shift_16_by_16:
	dec b
	inc b
	jr nz,shift_oversize_16  ;Value too large - return zero
	ld b,c

;Left shift an 16-bit value by a 8-bit count
;Input: HL: Value to be shifted
;		B: Shift count
;Output: HL: Shifted value
;		 F,B corrupt
left_shift_16_by_8
	dec b
	inc b
	ret z					;Shift count is zero
.loop
	add hl,hl
	djnz .loop
	ret

shift_oversize_16:
	ld hl,0
	ret
	
;Right shift an 16-bit value by a 16-bit count
;Input: HL: Value to be shifted
;		BC: Shift count
;Output: HL: Shifted value
;		 F,B corrupt
right_shift_16_by_16:
	dec b
	inc b
	jr nz,shift_oversize_16  ;Value too large - return zero
	ld b,c

;Right shift an 16-bit value by a 8-bit count
;Input: HL: Value to be shifted
;		B: Shift count
;Output: HL: Shifted value
;		 F,B corrupt
right_shift_16_by_8
	dec b
	inc b
	ret z					;Shift count is zero
.loop
	srl h
	rr l
	djnz .loop
	ret
  
;==================================================COMPARISONS   
		
;Compare signed and unsigned 16 bit numbers.
;One value must be signed and the other unsigned, it doesn't
;matter which is which
;In: HL, DE are the two values to compare
;Out: ZF set if they are equal, ZF clear if they are different
;	  A, HL corrupt
equal_m16:
	ld a,h					;If signed is < 0 or unsigned >= $8000 then they must be unequal
	or d					;I.e. if either value has bit 15 set
	and $80					;Also clears carry
	ret nz
	sbc hl,de
	ret
	
;1) Compare equality of either signed or unsignd 16-bit values
;2) Compare magnitude of unsigned 16-bit values
;Both: 
;Parameters in HL and DE.
;Corrupts: A and Flags
;For equality: 
;Both values must be either signed or unsigned
;Returns ZF set if they are equal, ZF clear if they differ
;For magnitude:
;Returns CF set if HL <= DE, CF clear if HL > DE
compare_u16:
	ld a,h
	cp d
	ret nz
	ld a,l
	cp e
	ret
	
;Compare HL to DE - Taken from BASIC 1.1
;if HL > DE: A:=1;  CF clear, ZF clear
;if HL >= DE:       CF clear, A<=0
;if HL = DE: A:=0;  ZF set, CF clear
;if HL <= DE:       CF set or ZF set, A>=0
;if HL < DE: A:=-1; CF set, ZF clear
compare_s16:   
    ld      a,h  
    xor     d    			;Will clear negative flag (PF) if both are same sign
    ld      a,h  
    jp      p,.comp_magnitude	;Same sign, compare magnitides
    add     a,a             ;Set CF if HL is negative (bit 15 high)
							;If HL is negative and signs are different then DE must be > HL
ret                      

.comp_magnitude:         
    cp      d            	;CF set if D > H
	ret nz
    ld      a,l              
    sub     e               ;CF set if E > L. A:=0
ret
  
;Compare HL and DE where HL is unsigned number and DE is signed number
;Reurns CF set if DE > HL
compare_u16_s16:
	ld a,h					;Is HL >= $8000
	rla
	ccf
	ret nc					;If so, it's always greater

	ld a,d					;Is DE negative
	rla
	ccf
	ret nc					;If so, it's always less

	and a					;Clear carry
	sbc hl,de				;CF must be clear from rotate
	ret
	
;Compare HL and DE where HL is signed number and DE is unsigned number
;Returns CF set if DE > HL
compare_s16_u16:
	ld a,h					;Is HL negative
	rla
	ret c					;If so, it's always less

	ld a,d					;Is DE >= $8000
	rla
	ret c					;If so, it's always greater

	sbc hl,de				;CF must be clear from rotate
	ret


;;============================================
;Numbers to strings

;HL=value
;Output:
;  AF,HL corrupt
write_integer:
	bit 7,h
	jp z,write_word
;	push af
	ld a,'-'
	call s_writechar
	call negate_HL_with_overflow
;	pop af
	jr write_word

;HL=value
;Output:
;  AF,HL corrupt
write_byte:
	ld h,0
	ld a,2			;2 digits
	jr do_write_word

;L=value
;Output:
;  AF,HL corrupt
write_word:
	xor a			;No padding/leading digits
do_write_word
	push bc
	ld bc,$040f		;Hex: 4 bit per digit; mask for 4 bits
	call convert_based_number_to_string
	pop bc
	jp write_asciiz

;Conversion buffer
	org $+16		;17 byte buffer (16 bits...
end_of_conversion_buffer:
	db 0			;... plus trailing zero)

;Extracted from Amstrad CPC BASIC:

;;===============================
;;convert based number to string
;HL=number to convert
;C=base (01=binary, 0f=hex)
;B=number of bits per output digit (01 for binary, 04 for hex)
;A: $01 to $80=minimum number of digits to output. I.e. pad with leading zeros. 
;   $81 to $ff or $00=no padding.

;Returns: ASCIIZ string at HL
convert_based_number_to_string:   ;{{Addr=$f1df Code Calls/jump count: 2 Data use count: 0}}
        push    de                ;{{f1df:d5}} 
        ex      de,hl             ;{{f1e0:eb}} 
        ld      hl,end_of_conversion_buffer;{{f1e1:213eae}} 
        ld      (hl),$00          ;{{f1e4:3600}} Returns a zero terminated string
        dec     a                 ;{{f1e6:3d}} 

;;=convert digit loop
convert_digit_loop:               ;{{Addr=$f1e7 Code Calls/jump count: 2 Data use count: 0}}
        push    af                ;{{f1e7:f5}} 
        ld      a,e               ;{{f1e8:7b}} A=byte
        and     c                 ;{{f1e9:a1}} C=mask for bits we're interested in

;These four lines convert nybble to hex ASCII. 
;See 'Analysis of the binary to ASCII hex conversion' below
        or      $f0               ;{{f1ea:f6f0}} 
        daa                       ;{{f1ec:27}} 
        add     a,$a0             ;{{f1ed:c6a0}} 
        adc     a,$40             ;{{f1ef:ce40}} ; 'A'-1

        dec     hl                ;{{f1f1:2b}} 
        ld      (hl),a            ;{{f1f2:77}} Write to buffer
        ld      a,b               ;{{f1f3:78}} Cache bits per digit

;;=convert shift loop
convert_shift_loop:               ;{{Addr=$f1f4 Code Calls/jump count: 1 Data use count: 0}}
        srl     d                 ;{{f1f4:cb3a}} DE=number to convert
        rr      e                 ;{{f1f6:cb1b}} Shift for next digit
        djnz    convert_shift_loop;{{f1f8:10fa}}  (-$06) Next digit

        ld      b,a               ;{{f1fa:47}} Restore bits per digit
        pop     af                ;{{f1fb:f1}} A=minimum width
        dec     a                 ;{{f1fc:3d}} 
        jp      p,convert_digit_loop;{{f1fd:f2e7f1}} If A still > 0 then loop

        ld      a,d               ;{{f200:7a}} If A < 0 then check if number is now zero
        or      e                 ;{{f201:b3}} 
        ld      a,$00             ;{{f202:3e00}} Force no padding
        jr      nz,convert_digit_loop;{{f204:20e1}}  (-$1f) Not zero? => next digit

        pop     de                ;{{f206:d1}} 
        ret                       ;{{f207:c9}} 

	
str_false: 
	db "FALS","E"+$80
str_true: 
	db "TRU","E"+$80
	
;Outputs the string 'FALSE' or 'TRUE' depending on the value of A
;Inputs: A is a boolean value
;Outputs: A,HL and Flags corrupt. All other registers preserved
write_bool:	
	ld hl,str_false
	and a
	jr z,write_ascii7
	ld hl,str_true

;Outputs an ASCII7 string (i.e. bit 7 of the last character is set)
;Inputs: HL is the address of an ASCII7 string
;Outputs: A,HL and flags corrupt. All other registers preserved
write_ascii7:
	ld a,(hl)			;Get character
	and $7f				;Mask out bit 7
	call s_writechar	;Output
	xor (hl)			;Compare to original
	ret nz				;If it's different bit 7 was set so return
	inc hl				;Next char
	jr write_ascii7

;Outputs an ASCIIZ string (i.e one which is followed by a #0 byte)
;Inputs: HL is the address of the string
;Outputs: A,HL and flags corrupt. All other registers preserved
write_asciiz:
	ld a,(hl)
	and a
	ret z
	call s_writechar
	inc hl
	jr write_asciiz
	
;===================================
;Stack and stack frames

;Called on function entry with HL=-<vars.localsbytesize>  
stacklocal_enter:
  pop de 			;Return address
  push ix			;Old stackframe
  ld ix,$0000
  add ix,sp         ;New stackframe
;  ld hl,-<vars.localsbytesize> ;Done by caller
  add hl,sp			;Allocate stack space for locals
;###Check for stack overflow###
;ld a,sp_high_minimum
;cp h
;jr nc,raise_stack_overflow
  ld sp,hl
  ex hl,de		;Return address
  jp (hl)
  
  
;JumPed to on function exit. de=16 bit return value (if any), hl=<vars.paramsbytesize>  
:stacklocal_exit:
;exit 
  ld sp,ix 			;remove locals from stack
;  ld hl,<vars.paramsbytesize> ;remove parameters - passed in
  add hl,sp         ;SP after params removed
  pop ix 			;restore original value/stackframe
  pop bc 			;return addr
  ld sp,hl			;Restore original SP
  ex hl,de			;hl=return value
  push bc			;Return address
  ret  