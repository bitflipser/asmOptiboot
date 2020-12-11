; ################################################################################ ;
;                                                                                  ;
; asmOptiboot - Optiboot rewrite in Assembler with many extras                     ; 
;              (https://github.com/bitflipser/asmOptiboot)                         ;
;                                                                                  ;
; by bitflipser, 2018                                                              ;
;                                                                                  ;
; based on Optiboot (https://githib.com/Optiboot/optiboot)                         ;
;  "Although it has evolved considerably, Optiboot builds on the original work     ;
;   of Jason P. Kyle (stk500boot.c), Arduino group (bootloader),                   ;
;   Spiff (1K bootloader), AVR-Libc group and Ladyada (Adaboot).                   ;
;                                                                                  ;
;   Optiboot is the work of Peter Knight (aka Cathedrow). Despite some             ;
;   misattributions, it is not sponsored or supported by any organisation          ;
;   or company including Tinker London, Tinker.it! and Arduino.                    ;
;   Maintenance of optiboot was taken over by Bill Westfield (aka WestfW) in 2011.";
;                                                                                  ;
; fits into 256 words of FLASH                                                     ;
;      ( minimum size: 390 bytes/185 words)                                        ;
;      ( maximum size: 502 bytes/251 words)                                        ;
;                                                                                  ;
; extended functions:                                                              ;
;   - save MCUSR reset status in R2                                                ;
;   - read/write EEPROM                                                            ;
;   - read SIGNATURE from MCU                                                      ;
;   - read FUSE                                                                    ;
;   - read OSCCAL                                                                  ;
;   - read/write LOCK bits                                                         ;
;   - auto increment address                                                       ;
;   - increased USART speed                                                        ;
;   - Do_spm vector                                                                ;
;   - WRITE_FLASH_PAGE function for user application                               ;
;                                                                                  ;
; MIT License                                                                      ;
;                                                                                  ;
; Copyright (c) 2018 bitflipser                                                    ;
;                                                                                  ;
; Permission is hereby granted, free of charge, to any person obtaining a copy     ;
; of this software and associated documentation files (the "Software"), to deal    ;
; in the Software without restriction, including without limitation the rights     ;
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell        ;
; copies of the Software, and to permit persons to whom the Software is            ;
; furnished to do so, subject to the following conditions:                         ;
;                                                                                  ;
; The above copyright notice and this permission notice shall be included in all   ;
; copies or substantial portions of the Software.                                  ;
;                                                                                  ;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR       ;
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,         ;
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE      ;
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER           ;
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,    ;
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE    ;
; SOFTWARE.                                                                        ;
;                                                                                  ;
; ################################################################################ ;

.NOLIST
.include "macros.inc"
.LIST

.equ HW_VER	  = 3
.equ SW_MAJOR = 207
.equ SW_MINOR = 1

; 1 - ###############
;     select MCU type

;.equ ATmega =644
.equ ATmega =328
;.equ ATmega =168
;.equ ATmega = 88
;.equ ATmega = 48

; 2 - ###################
;     select MCU sub type

;.equ subType=0			; no letter
;.equ subType=1			; A
;.equ subType=2			; L
.equ subType=3			; P/PV
;.equ subType=4			; PA
;.equ subType=6			; PB
;.equ subType=5			; V

; 3 - ################
;     select frequency

;.equ F_CPU=20			; 20 MHz
.equ F_CPU=16			; 16 MHz
;.equ F_CPU=10			; 10 MHz
;.equ F_CPU= 8			;  8 MHz
;.equ F_CPU= 1			;  1 MHz

; 4 - ##############
;     select Vtarget

.equ Vtarget =50		; 5,0 V
;.equ Vtarget =33		; 3,3 V
;.equ Vtarget =xy		; x,y V - set value

; 5 - ##############################################
;     select standard or high-speed USART connection

;.equ USARTspeed=0			; standard baud rates 
.equ USARTspeed=1			; increased USART speed, better timing accuracy

; 6 - ######################
;     memory saving specials

;.equ noADDRpreset		 =1	; uncomment to save  2 words: (it's not needed with avrdude)
;.equ noLOADwhileERASE	 =1	; uncomment to save  6 words: takes 1 s longer to burn 31,5 kB of FLASH
;.equ noLED_START_FLASH	 =1	; uncomment to save 14 words: LED will NOT blink at start
;.equ noAUTO_INCREMENT	 =1	; uncomment to save  4 words: read/write address is not auto incremented and needs to be send for successive reads/writes (avrdude always does)
;.equ noVtarget			 =1	; uncomment to save  3 words: Vtarget-reply will be 0.0 V
;.equ allPARAM_0x14		 =1	; uncomment to save 10 words: reply all params '0x14' (instead of Optiboot's 0x03)
   ; includes Vtarget = 1.4 V
;.equ noOSCCAL			 =1	; uncomment to save  5 words: STK_READ_OSCCAL will return 0
;.equ noSIGfromMCU		 =1	; uncomment to save 10 words: SIGNATURE taken from bootloader code instead of MCU
   ; includes noOSCCAL = 1
;.equ noWRITE_FLASH_func =1	; uncomment to save  5 words: NO user app callable WRITE_FLASH_PAGE function
;.equ noSW_MAJOR_MINOR	 =1	; uncomment to save  1 word : do not put Optiboot version number at flashend

; 7 - ###################
;     set LED port values

.equ LED_DDR=DDRB
.equ LED_PORT=PORTB
.equ LED_PIN=PINB
.equ LEDpin=5

.equ SIG_1=0x1e

.if ATmega == 644
	.NOLIST
	.include "m644def.inc"
	.LIST
	.equ SIG_2=0x96
	.equ SIG_3=0x09
	.equ SELFPRGEN=SPMEN			; different name on this device
.endif

.if ATmega == 328
	.set SIG_2=0x95
	.if subType==0
		.equ SIG_3=0x14
		.NOLIST
		.include "m328def.inc"
		.LIST
	.elif subType==3
		.equ SIG_3=0x0f
		.NOLIST
		.include "m328pdef.inc"
		.LIST
	.else
		.error "No such ATmega328 sub type"
	.endif
.endif

.if ATmega == 168
 .NOLIST
 .include "m168def.inc"
 .LIST
 .equ SIG_2=0x94
 .if	subType == 0
	.equ SIG_3=0x06
 .elif	subType == 1
	.equ SIG_3=0x06
 .elif	subType == 2
	.equ SIG_3=0x0b
 .elif	subType == 3
	.equ SIG_3=0x0b
 .elif	subType == 4
 	.equ SIG_3=0x0b
 .elif	subType == 5
 	.equ SIG_3=0x06
 .elif	subType == 6
 	.equ SIG_3=0x15
 .endif
.endif

.if ATmega == 88
 .NOLIST
 .include "m88def.inc"
 .LIST
 .equ SIG_2=0x93
 .if	subType == 0
	.equ SIG_3=0x0a
 .elif	subType == 1
	.equ SIG_3=0x0a
 .elif	subType == 2
	.equ SIG_3=0x0f
 .elif	subType == 3
	.equ SIG_3=0x0f
 .elif	subType == 4
 	.equ SIG_3=0x0f
 .elif	subType == 5
 	.equ SIG_3=0x0a
 .elif	subType == 6
 	.equ SIG_3=0x16
 .endif
.endif

.if ATmega == 48
	.NOLIST
	.include "m48def.inc"
	.LIST
	.equ SIG_2=0x92
 .if	subType == 0
	.equ SIG_3=0x05
 .elif	subType == 1
	.equ SIG_3=0x05
 .elif	subType == 2
	.equ SIG_3=????
 .elif	subType == 3
	.equ SIG_3=0x0a
 .elif	subType == 4
 	.equ SIG_3=0x0a
 .elif	subType == 5
 	.equ SIG_3=0x05
 .elif	subType == 6
 	.equ SIG_3=0x10
 .endif
.endif

.ifndef SIGRD
	.equ SIGRD=5				; missing definition in .inc-file
.endif


.if F_CPU == 20
	.if USARTspeed == 0
		; standard baud rates 							 Error
		.set Baud = 0x0015				;  115.200 baud: -1,4%
		.set doubleSpeed = 1
	.elif USARTspeed == 1
		; high-speed baud rates
		.set Baud = 0x0004				;  250.000 baud:  0,0%
		.set doubleSpeed = 0
	.endif
.endif

.if F_CPU == 16
	.if USARTspeed == 0
		; standard baud rates 							 Error
		.set Baud = 0x0010				;  115.200 baud:  2,1%
		.set doubleSpeed = 1
	.elif USARTspeed == 1
		; high-speed baud rates
		.set Baud = 0x0003				;  250.000 baud:  0,0%
		;.set Baud = 0x0001				;  500.000 baud:  0,0%
		;.set Baud = 0x0000				;1.000.000 baud:  0,0%
		.set doubleSpeed = 0
	.endif
.endif

.if F_CPU == 10
	.if USARTspeed == 0
		; standard baud rates 							 Error
		.set Baud = 0x0015				;   57.600 baud: -1,4%
		;.set Baud = 0x000a				;  115.200 baud: -1,4% 
		.set doubleSpeed = 1
	.elif USARTspeed == 1
		; high-speed baud rates
		.set Baud = 0x0004				;  250.000 baud:  0,0%
		.set doubleSpeed = 1
	.endif
.endif

.if F_CPU == 8
	.if USARTspeed == 0
		; standard baud rates 							 Error
		.set Baud = 0x0010				;   57.600 baud: -0,8%
		.set doubleSpeed = 1
	.elif USARTspeed == 1
		; high-speed baud rates
		.set Baud = 0x0001				;  250.000 baud:  0,0%
		;.set Baud = 0x0000				;  500.000 baud:  0,0%
		.set doubleSpeed = 0
	.endif
.endif

.if F_CPU == 1
	.if USARTspeed == 0
		; standard baud rates 							 Error
		
		.set Baud = 0x000c				;    4.800 baud:  0,2%
		.set doubleSpeed = 0
		/*
		.set Baud = 0x000c				;    9.600 baud:  0,2%
		.set doubleSpeed = 1
		*/
	.elif USARTspeed == 1
		; high-speed baud rates
		.set Baud = 0x0000				;   62.500 baud:  0,0%
		.set doubleSpeed = 0
	.endif
.endif

.equ USARTbase	= UCSR0A
.equ oUCSR0A	= UCSR0A-USARTbase		; register offsets to be accessed with ldd/std and Y+d
.equ oUCSR0B	= UCSR0B-USARTbase
.equ oUCSR0C	= UCSR0C-USARTbase
.equ oUBRR0L	= UBRR0L-USARTbase
.equ oUBRR0H	= UBRR0H-USARTbase
.equ oUDR0		= UDR0  -USARTbase

.cseg
.org FLASHEND-0xff

	rjmp BOOT
	rjmp _doSPM
.ifndef noWRITE_FLASH_func
	rjmp WRITE_FLASH_PAGE
.endif

BOOT:
	clr YH						; const YH = 0 (fix all program)
	LOAD R2, MCUSR				; save MCUSR for application program
	STORE MCUSR, YH
	bst R2, 1
	  brts extReset
appStart:						; no external reset
	clr R24						; -> stop watchdog		
	rcall watchdogConfig
	rjmp FLASHEND+1				; 0x0000 with wrap around (start app)

extReset:
	ldi YL, LOW(USARTbase)		; use Y as IObase pointer (fix all program)
;	ldi YH,HIGH(USARTbase)		; (already there)

.ifndef noADDRpreset
	ldi R22, 0x00				; set addressH:L to 0x0000 by default
	ldi R23, 0x00
.endif

	ldi XH,HIGH(buff)			; const XH = 2 (fix all program)
.if doubleSpeed == 1
	std Y+oUCSR0A, XH			; USART0 double speed mode
.endif
	ldi R18, 0x18				; (fix all program - do not change)
	std Y+oUCSR0B, R18			; RX enable, TX enable
;	ldi R16, 0x06				; reset value - no need to write
;	std Y+oUCSR0C, R16			; 8.1 no parity
.if Baud == 0
;	std Y+oUBRR0L, YH			; reset value - no need to write
.else
	ldi R25, Baud
	std Y+oUBRR0L, R25
.endif

	ldi R24, 0x0d
	rcall watchdogConfig		; trigger after 500 ms

.ifndef noLED_START_FLASH
						; LED_START_FLASH
 .if F_CPU == 16 || F_CPU == 20
	ldi R16, 12					; blink 6 times
 .else
	ldi R16, 6					; blink 3 times
 .endif

	SETB LED_DDR, LEDpin		; LED 'output'
 ;	CLRB LED_PORT, LEDpin		; reset value - no need to write
	ldi R25, 1

 .if F_CPU > 1					; start TC1
	STORE TCCR1B, XH			; const XH = 2 -> prescale /8
 .else
	STORE TCCR1B, R25			; prescale /1
 .endif

 blinkLED:
	  STORE TIFR1, R25			; clear TOV1
   TC1wait:
	    SKBS TIFR1, TOV1			; wait TOV1
	  rjmp TC1wait
	  SETB LED_PIN, LEDpin		; toggle LED 'on/off'
	  wdr
	dec R16
	brne blinkLED
	STORE TCCR1B, R16			; stop TC1
.else
						; noLED_START_FLASH	
.endif

foreverLoop:
	rcall getch
checkA:
	cpi R24, 0x41				; 'A' - STK_GET_PARAMETER
	  brne checkB

getParameter:
	rcall getch
  .ifndef allPARAM_0x14
		mov R17, R24				; save 2nd command byte
		rcall verifySpace
		.ifndef noVtarget
			ldi R24, Vtarget			; = 3.3 / 5.0 V
			cpi R17, 0x84
			  breq byte_response_R24
		.endif
		ldi R24, SW_MINOR
		cpi R17, 0x82				; STK_SW_MINOR
		  breq byte_response_R24
		ldi R24, SW_MAJOR
		cpi R17, 0x81				; STK_SW_MAJOR
		  breq byte_response_R24
		ldi R24, HW_VER
		cpi R17, 0x80				; STK_HW_VER
		  breq byte_response_R24
	byte_response_0:				; all other 'A'-params
		clr R24						; 0
  .else				; allParam_0x14 == 1
		rcall verifySpace
  .endif
byte_response_R24:
	rcall putch					; send response
	rjmp putchSTK_OK			; send 'OK'

checkB:
	cpi R24, 0x42				; 'B' - STK_SET_DEVICE
	ldi R25, 0x14
	  breq getThat				; go ignore cmd by ..

checkE:
	cpi R24, 0x45				; 'E' - STK_SET_DEVICE_EXT
	  brne checkU

setDeviceExt:
	ldi R25, 0x05				; ignore cmd by ..
getThat:
	rcall getNch_R25			; .. dropping next #R25 chars + verifySpace
	rjmp putchSTK_OK			; send 'OK'

checkU:
	cpi R24, 0x55				; 'U' - STK_LOAD_ADDRESS
	  brne checkV

loadAddress:					; little endian, in words
	rcall getch
	mov R22, R24				; addressH:L in R23:R22
	rcall getch
	mov R23, R24
	lsl R22						; make it byte address
	rol R23
	rjmp nothing_response		; verifySpace + STK_OK

checkV:
	cpi	R24, 0x56				; 'V' - STK_UNIVERSAL
	  brne check_d

Universal:
	rcall getch
	ldi ZL, 1
	clr ZH
	ldi R25, 2
	cpi R24, 0xac
	  brne maybe_fuse

write_cmd:						; check for 'write lock bits'
	rcall getch
	cpi R24, 0xe0
	  brne getThat
write_lock:
	rcall getNch_R25			; last received in R0 + verifySpace
	ldi R25, 1<<BLBSET|1<<SELFPRGEN
	STORE SPMCSR, R25
	spm
	rjmp putchSTK_OK

maybe_fuse:
	cpi R24, 0x58
	  breq fuse_cmd
	clr ZL
	cpi R24, 0x50
	  breq fuse_cmd
no_fuse_read:
	ldi	R25, 0x03				; not a fuse read
fuse_read_wrong:
	rcall getNch_R25			; ..drop next . chars
  .ifndef allPARAM_0x14
		rjmp byte_response_0
  .else
		clr R24
		rjmp byte_response_R24
  .endif

fuse_cmd:
	rcall getch
	cpi R24, 0x00
	  breq go_read_fuses
	cpi R24, 0x08
	  brne fuse_read_wrong
	sbr ZL, 1<<1
go_read_fuses:
	rcall getNch_R25			; ..drop next . chars
	ldi R25, 1<<BLBSET|1<<SELFPRGEN
	STORE SPMCSR, R25
	lpm R24, Z
	rjmp byte_response_R24

check_d:
	cpi R24, 0x64				; 'd' - STK_PROG_PAGE
	  brne check_t
	
progPage:
	rcall page_header 			; get length and set T upon memtype
	movw Z, R23:R22				; get target address

.ifndef noLOADwhileERASE
		brts skip_page_erase		; page erase not for EEPROM

		cpi R23,HIGH(NRWW_START_ADDR*2)
		brcc skip_page_erase		; for NRWW fillBuff first!!
		  rcall _page_erase
  skip_page_erase:
.endif

	ldi XL, LOW(buff)			; XL = 0 !! -> buff on page boundary !!
;	ldi XH,HIGH(buff)			; const XH = 2 (fix all program)
fillBuff:
	  rcall getch
	  st X+, R24
	cp R19, XL					; keep R19 as lengthL !! buff on page boundary !!
	brne fillBuff

	ldi XL, LOW(buff)			; XL = 0 !! -> buff on page boundary !!
;	ldi XH,HIGH(buff)			; const XH =2 (fix all program)

	rcall verifySpace
progPage_memtype:
	brts progPage_eeprom		; T = 0 -> FLASH, T = 1 -> EEPROM

; FLASH							; memtype 'even' (='F') -> flash
	.ifndef noLOADwhileERASE
		cpi R23,HIGH(NRWW_START_ADDR*2)
		brcs label2
		  rcall _page_erase			; for NRWW section  erase now
	 label2:
	.endif
	rcall progPage_flash
	rjmp putchSTK_OK

progPage_eeprom:				; memtype 'odd' (='E')  -> eeprom
  .if ATmega == 48
							; no EEARH on ATmega48
  .else
	  STORE EEARH, ZH
  .endif
	  STORE EEARL, ZL				; load EEPROM target address
	  ld	R24, X+					; get value from buff
	  STORE EEDR, R24				; put as EEPROM target data
	  ldi R24, 1<<EEMPE
	  STORE EECR, R24				; enable Prog
	  SETB EECR, EEPE				; start Prog

  wait_eeprom_ready:
		SKBC EECR, EEPE				; wait EEPROM ready
	  rjmp wait_eeprom_ready

	  adiw Z, 1					; targetAdr += 1
	dec R19						; lengthL (scratched)
	brne progPage_eeprom

mem_loop_ready:
	.ifndef noAUTO_INCREMENT
		movw R23:R22, Z				; auto increment address				
	.endif
	rjmp putchSTK_OK

check_t:
	cpi R24, 0x74				; 't' - STK_READ_PAGE
	  brne check_u

readPage:
	rcall page_header
	rcall verifySpace
	movw Z, R23:R22				; get address
readPage_loop:
	  brtc read_flash

  read_eeprom:
  .if ATmega == 48
							; no EEARH on Atmega48
  .else
	  STORE EEARH, ZH
  .endif
	  STORE EEARL, ZL
	  SETB EECR, EERE			; EEReadEnable
	  LOAD r24, EEDR
	  adiw Z,1
	  rjmp rP_0

  read_flash:
	  lpm R24, Z+
  rP_0:
	  rcall putch				; send 
	dec R19						; lengthL (scratched)
	brne readPage_loop

	rjmp mem_loop_ready		; auto increment + STK_OK

check_u:
	cpi R24, 0x75				; 'u' - STK_READ_SIGN
	  brne check_v

readSignature:
	rcall verifySpace
.ifndef noSIGfromMCU
		ldi ZL, 0x00				; SIG0
		rcall read_sig_
		ldi ZL, 0x02				; SIG1
		rcall read_sig_
		ldi ZL, 0x04				; SIG2
	readLastSig:
		rcall read_sig_
		rjmp putchSTK_OK

	read_sig_:
		clr ZH
		ldi R24, 1<<SIGRD|1<<SELFPRGEN
		STORE SPMCSR, R24
		lpm R24, Z
		rjmp putch
.else
		ldi R24, SIG_1				; 0x1e - Atmel
		rcall putch
		ldi R24, SIG_2				; 0x9. - ATmega ...
		rcall putch
		ldi R24, SIG_3
		rcall putch
		rjmp putchSTK_OK
.endif

check_v:
.ifndef noOSCCAL
 .ifndef noSIGfromMCU		
		cpi R24, 0x76				; 'v' - STK_READ_OSCCAL
		  brne checkQ

	readOSCCAL:
		rcall verifySpace
		ldi ZL, 0x01
		rjmp readLastSig
  .endif
.endif

checkQ:
	cpi	R24, 0x51				; 'Q' - STK_LEAVE_PROGMODE
	  brne allElse

leaveProgmode:
	ldi	R24, 0x08				; force WD-RESET w/i 16 ms
	rcall watchdogConfig

allElse:
nothing_response:
	rcall verifySpace
putchSTK_OK:
	ldi	R24, 0x10				; STK_OK
	rcall putch
	rjmp foreverLoop

page_header:
	rcall getch					; drop lengthH (=0) 
	rcall getch					; get  lengthL - big endian
	mov R19, R24				; R19 = lengthL
	rcall getch
	bst R24, 0					; put Bit0 into T ('E' set, 'F' clear)
	ret

putchSTK_INSYNC:
	ldi	R24, 0x14				; STK_INSYNC

putch:
	  ldd	R25, Y+oUCSR0A		; wait USART Data Register empty
	  sbrs R25, UDRE0
	rjmp putch
	std Y+oUDR0, R24			; put Data
	ret

getch:
	  ldd	R24, Y+oUCSR0A		; wait USART Receive complete
	  sbrs R24, RXC0
	rjmp getch

	sbrs R24, FE0				; skip WDR on Framing Error
	  wdr
	ldd	R24, Y+oUDR0			; get char received
	ret

watchdogConfig:
;	ldi	R18, (1>>WDE | 1>>WDCE)	; (still there)
	STORE WDTCSR, R18			; const R18 = 0x18 (fix all program - do not change)
	STORE WDTCSR, R24
	ret

getNch_R25:
	  rcall getch
	dec R25
	brne getNch_R25

	mov R0, R24					; save last received
;	rjmp verifySpace

verifySpace:
	rcall getch
	cpi	R24, 0x20				; ' '
 	  breq putchSTK_INSYNC
not_INSYNC:
	ldi	R24, 0x08				; set WD time-out 16 ms
	rcall watchdogConfig
wait_watchdog:
	rjmp wait_watchdog

WRITE_FLASH_PAGE:				; (R23:R22) = WRITE_FLASH_PAGE(R23:R22, R25:R24)

.ifndef noWRITE_FLASH_func
		ldi R19, LOW(PAGESIZE*2)	; lenghtL fix to one page (=0 for 256 bytes)
		movw X, R25:R24				; get buffAdr from C arguments
		movw Z, R23:R22				; get targetAdr from C arguments
.endif

.ifdef noLOADwhileERASE
  progPage_flash:
		rcall _page_erase
.else
		rcall _page_erase
  progPage_flash:
.endif

loadPageBuf:
	  ld R0, X+					; Data from Buffer
	  ld R1, X+
	  ldi R25, 1<<SELFPRGEN
	  rcall _doSPM
	  adiw Z, 2
	subi R19,2					; lengthL (scratched)
	brne loadPageBuf

.ifndef noAUTO_INCREMENT
		movw R25:R24, Z				; save incremented address
  writePage:
		movw Z, R23:R22				; get start address - same page !!
		movw R23:R22, R25:R24		; place auto increment address
.else
  writePage:
		movw Z, R23:R22
.endif

	ldi R25, 1<<PGWRT | 1<<SELFPRGEN
	rcall _doSPM				; write page
	ldi R25, 1<<RWWSRE | 1<<SELFPRGEN
	rcall _doSPM				; enable RWW-section
exit_prog_flash:

.ifndef noWRITE_FLASH_func
 .ifndef noAUTO_INCREMENT
		movw R25:R24, X				; return auto increment address to C-function
 .endif
		clr R1						; R1 = 0 GNU convention
.endif

	ret

_page_erase:
	ldi R25, 1<<PGERS | 1<<SELFPRGEN
_doSPM:
	  SKBC SPMCSR, SELFPRGEN, R24
	rjmp _doSPM

	STORE SPMCSR, R25
	spm
	ret

.ifndef noSW_MAJOR_MINOR
 .org FLASHEND
 .db SW_MINOR, SW_MAJOR
.endif

.dseg
.org 0x0200						; (do NOT change)
buff:	.byte PAGESIZE*2
