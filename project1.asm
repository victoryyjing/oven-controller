$NOLIST
$MODLP51RC2
$LIST

SHIFT_BUTTON equ P4.5 ;all push button positions are variable up to us
TEMP_SOAK_PB equ P4.6
TIME_SOAK_PB equ P4.7
TEMP_REFL_PB equ P4.8
TIME_REFL_PB equ P4.9 

CE_ADC EQU P0.0
MY_MOSI EQU P0.1
MY_MISO EQU P0.2
MY_SCLK EQU P0.3
;PMW P0.4 
SOAK_TEMP EQU P0.5
SOAK_TIME EQU P0.6
REFLOW_TIME EQU P0.7
REFLOW_TEMP EQU P2.2
start_button EQU P2.3

CLK  EQU 22118400
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BAUD equ 115200
BRG_VAL equ (0x100-(CLK/(16*BAUD)))

; Reset vector
org 0x0000
    ljmp main

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
temp_soak: ds 1
time_soak: ds 1
temp_refl: ds 1
time_refl: ds 1

current_state: ds 1
temp: ds 1
pwm: ds 1
sec: ds 1
x: ds 4
y: ds 4
Count1ms: ds 1
result: ds 2
bcd: ds 4


BSEG
	mf: 	dbit 1

Change_8bit_Variable MAC
	jb %0, %2
	Wait_Milli_Seconds(#50) ; de-bounce
	jb %0, %2
	jnb %0, $
	jb SHIFT_BUTTON, skip%Mb
	dec %1
	sjmp skip%Ma
skip%Mb:
	inc %1
skip%Ma:
ENDMAC

getbyte mac
	clr a
	movc a, @a+dptr
	mov %0, a
	inc dptr
Endmac

loadbyte mac
	mov a, %0
	movx @dptr, a
	inc dptr
endmac


cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P3.2
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$include(math32.inc)
$LIST

lf:                 db  '\r', '\n', 0
Initial_Message:    db 'Ts tS Tr tR', 0
	
WaitmilliSec:
    push AR0
    push AR1
D3: mov R4, #45
D2: mov R3, #166
D1: djnz R3, D1 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R4, D2 ; 22.51519us*45=1.013ms
    djnz R2, D3 ; number of millisecons to wait passed in R2
    pop AR1
    pop AR0
    ret

; Waits 50ms
Delay:
    mov R2, #0x50
    lcall WaitmilliSec
    ret

; Code from the slides
INIT_SPI:
	setb MY_MISO 
	clr MY_SCLK 
	ret

DO_SPI_G:
	push acc
	mov R1, #0 
	mov R2, #8 
DO_SPI_G_LOOP:
	mov a, R0 ; Byte to write is in R0
	rlc a ; Carry flag has bit to write
	mov R0, a
	mov MY_MOSI, c
	setb MY_SCLK ; Transmit
	mov c, MY_MISO ; Read received bit
	mov a, R1 ; Save received bit in R1
	rlc a
	mov R1, a
	clr MY_SCLK
	djnz R2, DO_SPI_G_LOOP
	pop acc
	ret

InitSerialPort:
    mov R1, #222
    mov R0, #166
    djnz R0, $
    djnz R1, $-4 

	orl	PCON,#0x80
	mov	SCON,#0x52
	mov	BDRCON,#0x00
	mov	BRL,#BRG_VAL
	mov	BDRCON,#0x1E 
    ret

; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

temperature_logic:
	mov x+0, result+0
	mov x+1, result+1
	mov x+2, #0
	mov x+3, #0
	lcall hex2bcd

	;Send_BCD(bcd) FIND OUR WHERE THIS IS

	Set_Cursor(2, 1) ; not sure if correct, debug later
	Display_BCD(bcd+4)
	Display_BCD(bcd+3)
	Display_BCD(bcd+2)
	Display_BCD(bcd+1)
	Display_BCD(bcd+0)

    mov a, #'\r'
	lcall putchar
	
	mov a, #'\n'
	lcall putchar

	ret
	
Load_Configuration:
	mov dptr, #0x7f84 ; First key value location.
	getbyte(R0) ; 0x7f84 should contain 0x55
	cjne R0, #0x55, Load_Defaults
	getbyte(R0) ; 0x7f85 should contain 0xAA
	cjne R0, #0xAA, Load_Defaults
	; Keys are good. Get stored values.
	mov dptr, #0x7f80
	getbyte(temp_soak) ; 0x7f80
	getbyte(time_soak) ; 0x7f81
	getbyte(temp_refl) ; 0x7f82
	getbyte(time_refl) ; 0x7f83
	ret

Load_Defaults:
	mov temp_soak, #150
	mov time_soak, #45
	mov temp_refl, #225
	mov time_refl, #30
	ret
	
	
Save_Configuration:
  push IE ; Save the current state of bit EA in the stack
  clr EA ; Disable interrupts
  mov FCON, #0x08 ; Page Buffer Mapping Enabled (FPS = 1)
  mov dptr, #0x7f80 ; Last page of flash memory
  ; Save variables
  loadbyte(temp_soak) ; @0x7f80
  loadbyte(time_soak) ; @0x7f81
  loadbyte(temp_refl) ; @0x7f82
  loadbyte(time_refl) ; @0x7f83
  loadbyte(#0x55) ; First key value @0x7f84
  loadbyte(#0xAA) ; Second key value @0x7f85
  mov FCON, #0x00 ; Page Buffer Mapping Disabled (FPS = 0) 
  orl EECON, #0b01000000 ; Enable auto-erase on next write sequence  
  mov FCON, #0x50 ; Write trigger first byte
  mov FCON, #0xA0 ; Write trigger second byte
  ; CPU idles until writing of flash completes.
  mov FCON, #0x00 ; Page Buffer Mapping Disabled (FPS = 0)
  anl EECON, #0b10111111 ; Disable auto-erase
  pop IE ; Restore the state of bit EA from the stack
  ret

Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR

	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz MainProgram
	inc Count1ms+1

MainProgram:
    mov SP, #7FH ; Set the stack pointer to the begining of idata
    
    lcall InitSerialPort
    mov DPTR, #Initial_Message
    lcall SendString

    mov DPTR, #lf
    lcall SendString

    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts

    lcall LCD_4BIT
    lcall INIT_SPI

    Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)

loopTEMP:
	clr CE_ADC
	mov R0, #00000001B 
	lcall DO_SPI_G
	
	mov R0, #10000000B 
	lcall DO_SPI_G
	mov a, R1 
	anl a, #00000011B 
	mov Result+1, a 
	
	mov R0, #55H 
	lcall DO_SPI_G
	mov Result, R1 
	setb CE_ADC
	lcall Delay
	
	lcall temperature_logic
	;sjmp loopTEMP
    ret
    
SendToLCD: ;check slides
	mov b, #100
	div ab
	orl a, #0x30 ; Convert hundreds to ASCII
	lcall ?WriteData ; Send to LCD
	mov a, b ; Remainder is in register b
	mov b, #10
	div ab
	orl a, #0x30 ; Convert tens to ASCII
	lcall ?WriteData; Send to LCD
	mov a, b
	orl a, #0x30 ; Convert units to ASCII
	lcall ?WriteData; Send to LCD
	ret

; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
    
SendStringDone:
    ret


FSM_state0: ; INITIAL IDLE STATE
    mov a, current_state
    cjne a, #0, FSM_state1 ; If 0, then continue
    
    ; Settings
    mov pwm, #0

    ; Check for state change
    jb start_button, FSM_state0 ; If button (assuming it's active low) is not pressed, loop to FSM: (could be main: or loop:)
    jnb start_button, $ ; If button is pressed, wait for key release then proceed to next line
    mov current_state, #1
;FSM1_state0_done:
;    ljmp FSM2  

FSM_state1: ; HEAT TO TARGET SOAK TEMPERATURE ~150 CELCIUS
    mov a, current_state
    cjne a, #1, FSM_state2 ; If 1, then continue

    ; Settings
    mov pwm, #100
    mov sec, #0             ; "sec" likely refers to seconds counter from lab 2

    ; Check for state change
    mov a, temp_soak
    clr c
    subb a, temp
    jnc FSM_state1  ; ASSUME when temp reaches temp_soak, carry flag is set and we go to state 2, otherwise loop back
    mov current_state, #2
;FSM1_state1_done:
;    ljmp FSM2

FSM_state2: ; MAINTAIN SOAK TEMPERATURE FOR ~60 SECONDS
    mov a, current_state
    cjne a, #2, FSM_state3 ; If 2, then continue

    ; Settings
    mov pwm, #20

    ; Check for state change
    mov a, time_soak
    clr c
    subb a, sec
    jnc FSM_state2 ; ASSUME when time reaches time_soak, carry flag is set and we go to state 3, otherwise loop back
    mov current_state, #3
;FSM1_state2_done:
;    ljmp FSM2

FSM_state3: ; HEAT TO TARGET REFLOW TEMPERATURE ~220 CELCIUS
    mov a, current_state
    cjne a, #3, FSM_state4 ; If 3, then continue

    ; Settings
    mov pwm, #100
    mov sec, #0             ; "sec" likely refers to seconds counter from lab 2
    
    ; Check for state change
    mov a, temp_refl
    clr c
    subb a, temp
    jnc FSM_state3 ; ASSUME when temp reaches temp_refl, carry flag is set and we go to state 4, otherwise loop back
    mov current_state, #4
;FSM1_state3_done:
;    ljmp FSM2

FSM_state4:
    mov a, current_state
    cjne a, #4, FSM_state5 ; If 4, then continue

    ; Settings
    mov pwm, #20

    ; Check for state change
    mov a, time_refl
    clr c
    subb a, sec
    jnc FSM_state4 ; ASSUME when time reaches time_refl, carry flag is set and we go to state 5, otherwise loop back
    mov current_state, #5
;FSM1_state4_done:
;    ljmp FSM2

FSM_state5:
    mov a, current_state
    cjne a, #5, FSM_state0 ; If 5, then continue

    ; Settings 
    mov pwm, #0

    ; Check for state change
    mov a, temp
    cjne a, #60, FSM_state5
    mov current_state, #0
    ljmp FSM_state0
;FSM1_state5_done:
;    ljmp FSM2
;    ret 

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    mov P0M0, #0
    mov P0M1, #0
    lcall Load_Configuration
    lcall Timer2_Init
    lcall loopTEMP
    lcall LCD_4bit
    lcall FSM_state0
    
    Set_Cursor(1,1)
    Send_Constant_String(#Initial_Message)
    
    ; display variables
    Set_Cursor(2,1)
    mov a, #42
    lcall SendToLCD
    
    Set_Cursor(2,5)
    mov a, temp_soak
    lcall SendToLCD
    
    Set_Cursor(2,9)
    mov a, Temp_refl
    lcall SendToLCD
    
    Set_Cursor(2,13)
    mov a, Time_refl
    lcall SendToLCD
	
    ; After initialization the program stays in this 'forever' loop
loop:
    Change_8bit_Variable(TEMP_SOAK_PB, temp_soak, loop_a)
    Set_Cursor(2, 1)
    mov a, temp_soak
    lcall SendToLCD
    lcall Save_Configuration

loop_a:
    Change_8bit_Variable(TIME_SOAK_PB, time_soak, loop_b)
    Set_Cursor(2, 5)
    mov a, time_soak
    lcall SendToLCD
    lcall Save_Configuration

loop_b:
	Change_8bit_Variable(TEMP_REFL_PB, temp_refl, loop_c)
	Set_Cursor(2, 9)
	mov a, temp_refl
	lcall SendToLCD
	lcall Save_Configuration
	
loop_c:
	Change_8bit_Variable(TIME_REFL_PB, time_refl, loop_d)
	Set_Cursor(2, 13)
    mov a, time_refl
    lcall SendToLCD
    lcall Save_Configuration
    ljmp main

loop_d: 

END