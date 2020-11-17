;Replacement 8080A cpu board for ALTAIR/IMSAI
;Boot Loader program for 8080A based computer.
;Written by Josh Bensadon
;Free for public use, following all the shareware policies & disclaimers.
;
;
;Hardware: 8080 CPU ALTAIR / IMSAI REPLACEMENT
;
;Jumper JP5 (Shadow ROM KILL) (above EPROM) must be set 2-3 (upper position)
;
;Version 2.4 - Josh Bensadon.
;
;This boot loader program will:
; -display a quick message
; -Optionally test upper RAM at F000
; -load the hex file BIOS.HEX from the SD Card
;
;Version:
;2.3 Jan 3, 2015- Corrected for configuration where the Console I/O board is NOT installed.
;		- by testing incoming data for FF's or 00's
;2.4 Feb 22	- Corrected bug in RAM Test routine that would print HL (after it got nuked)
;2.5 Sep 3, 2018- Asserted RTS and DTR



		.area   CODE1   (ABS)   ; ASXXXX directive, absolute addressing

;----------------------------; IMSAI CONSOLE PORTS
FPLED		.EQU 255	;Front Panel LED
FPSW		.EQU 255	;Front Panel Switches
;----------------------------;


PROPELLERS	.EQU 0		;Propeller Console Status Port
PROPELLERD	.EQU 1		;Propeller Console Data Port

CPU_IO		.EQU 0x20	;Base address for onboard CPU I/O
UART0		.EQU CPU_IO
UART1		.EQU CPU_IO+0x08
SPI		.EQU CPU_IO+0x10
SPI_SS		.EQU SPI+1
PPORT		.EQU CPU_IO+0x18

		.ORG 0x0000

		MVI	A,2
START_DELAY:	LXI	H,0
		LXI	B,1
1$:		DAD	B
		JNC	1$
		DCR	A
		JNZ	START_DELAY

		MVI	A,0x80	;Set baud rate
		OUT	UART0+3
		OUT	UART1+3
		MVI	A,12	;12=9600 baud
		OUT	UART0
		OUT	UART1
		MVI	A,0
		OUT	UART0+1
		OUT	UART1+1
		MVI	A,3	;Set 8 data bits, no parity, 1 stop
		OUT	UART0+3
		OUT	UART1+3
		OUT	UART0+4
		OUT	UART1+4
		IN	UART0		;Clear any rxd flag


		LXI	H,MSG1		;System Start, Display Welcome Message
		JMP	PRINTI_NS
MSG1:		.ascii "\r\nALTAIR/IMSAI 8080 CPU BOARD BOOT LOADER - Josh Bensadon v2.5 Sep 3, 2018"
		.ascii "\r\n<D> -SD Card Directory"
		.ascii "\r\n<R> -RAM Test"
		.ascii "\r\n<V> -View Load"
		.ascii "\r\n> \000"

		XRA	A
		STA	GH_VIEW

		LXI	B,3
DDLP0:		MVI	E,5

DDLP1:		LXI	H,0
DDLP2:		IN	UART0+5	;10	;TEST FOR RX DATA
		ANI	1	;7
		JZ	DDLP3	;10
		IN	UART0
		JMP	DDLP4

DDLP3:		IN	PROPELLERS
		ANI	2
		JZ	DDLP5
		IN	PROPELLERD
		ORA	A
		JZ	DDLP5
		CPI	0xFF
		JZ	DDLP5

DDLP4:		OUT	UART0
		OUT	PROPELLERD
		CPI	27
		JZ	BOOT_SD
		CPI	32
		JZ	DDSKP
		ANI	0x5F		;Upper Case
		CPI	"D"
		JZ	DO_DIR
		CPI	"R"
		JZ	TEST_RAM
		CPI	"V"
		JZ	BOOT_SDVIEW

DDLP5:		DAD	B	;10
		JNC	DDLP2	;10	47*20K/2=~ 0.5 Sec

DDSKP:		MVI	A,"."
		OUT	UART0
		OUT	PROPELLERD
		DCR	E
		JNZ	DDLP1

		JMP	BOOT_SD

BOOT_SDVIEW:	MVI	A,1
		STA	GH_VIEW
		JMP	DDLP0

;-------------------------------------------------
DO_DIR:		LXI	SP,HIGHSTACK ;128 Bytes of stack available.

		CALL	PRINTI
		.ascii "\r\nINIT_FAT \000"

		CALL	INIT_FAT

		CALL	PRINTI
		.ascii "\r\nDIRECTORY:\r\n\000"
		CALL	SD_LDIR1
SDLF_LP: 	JZ	INPUT_FNAME		;End of list
		MOV	A,M
		CPI	33
		JM	DD_NEXT
		CPI	127
		JP	DD_NEXT
		PUSH	H			;Test if starting cluster is 0, skip file
		LXI	B,0x1A
		DAD	B
		MOV	A,M
		INX	H
		ORA	M
		POP	H
		JZ	DD_NEXT

		CALL	PRINT_FILENAME

;		CALL	PRINTI
;		.ascii "\r\n\000"
;		CALL	SD_LDIRN
;		JMP	SDLF_LP

		LDA	PC_POS
		CPI	64
		JM	DD_SAMELINE
		CALL	PRINTI
		.ascii "\r\n\000"
DD_SAMELINE:	LDA	PC_POS			;TAB OUT 16 CHARS
		ANI	0x0F
		JZ	DD_NEXT
		MVI	A," "
		CALL	PUT_CHAR
		JMP	DD_SAMELINE

DD_NEXT:		CALL	SD_LDIRN
		JMP	SDLF_LP


;-----------------------------------------------------------------------------------------------------
INPUT_FNAME:	CALL 	PRINTI		;Display Menu Prompt
		.ascii "\r\nENTER 8.3 FILE NAME> \000"
		LXI	H,FILENAME
		MVI	B,11
		MVI	A," "
		CALL	FILL_BLOCK
		MVI	C,"."
		MVI	B,8
		CALL	GET_STRING
		JC	INPUT_FNAME
		CPI	13
		JZ	LOADIT
		LXI	H,FILEEXT
		MVI	B,3
		CALL	GET_STRING
LOADIT:		CALL 	PRINTI
		.ascii "\r\n\000"
		JMP	SD_MANUAL


;----------------------------------------------------------------------------------------------------; RAM TEST
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;----------------------------------------------------------------------------------------------------; RAM TEST
;B=START PAGE (Hard coded at F0)
;C=END PAGE   (Hard coded at FF)

TEST_RAM:	LXI	H,0
		LXI	B,1
1$:		DAD	B
		JNC	1$

		LXI	H,MSG2		;TEST RAM Message
		JMP	PRINTI_NS
MSG2:		.ascii "\r\nTesting SYSTEM RAM @F000-FFFF\r\nRAM PAGE MARCH\000"

RAM_TEST:

;B AND C Have been hard coded to F0 AND FF

;Page March Test.  1 Sec/K
;
; FOR E = 00 TO FF STEP FF   'March 00 then March FF
;   FOR H = B TO C
;      PAGE(H) = E
;   NEXT H
;   FOR D = B TO C
;      PAGE(D) = NOT E
;      FOR H = B TO C
;         A = E
;         IF H = D THEN A = NOT E
;         IF PAGE(H) <> A THEN ERROR1
;      NEXT H
;   NEXT D
; NEXT E
;

		MVI	E,0xFF		;E selects the polarity of the test, ie March a page of 1'S or 0's

;Clear/Set all pages
RT1_LP0:	LXI	H,0xF000	;HL = BASE RAM ADDRESS

RT1_LP1:	MOV	A,E		;CLEAR A
		CMA
RT1_LP2:	MOV	M,A		;WRITE PAGE
		INR	L
		JNZ	RT1_LP2		;LOOP TO QUICKLY WRITE 1 PAGE
		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CPI	0xFF		;COMPARE WITH END PAGE
		JNZ	RT1_LP1		;LOOP UNTIL = END PAGE

;March 1 PAGE through RAM
		MVI	D,0xF0		;Begin with START PAGE

;Write FF to page D
RT1_LP3:	MOV	H,D		;HL = Marched Page ADDRESS
		;MVI	L,0

		MVI	A,"."
		OUT	UART0
		OUT	PROPELLERD

		MOV	A,D
		CMA
		OUT	FPLED
		;MOV	A,E		;SET A
RT1_LP4:	MOV	M,E		;WRITE PAGE
		INR	L
		JNZ	RT1_LP4		;LOOP TO QUICKLY WRITE 1 PAGE

;Test all pages for 0 (except page D = FF)
		LXI	H,0xF000	;HL = BASE RAM ADDRESS

RT1_LP5:	MOV	A,H		;IF H = D
		CMP	D
		MOV	A,E		;THEN Value = FF
		JZ	RT1_LP6
		CMA			;ELSE Value = 00

RT1_LP6:	CMP	M		;TEST RAM
		JNZ	RT_FAIL
		INR	L
		JNZ	RT1_LP6		;LOOP TO QUICKLY TEST 1 PAGE
		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CPI	0xFF		;COMPARE WITH END PAGE
		JNZ	RT1_LP5		;LOOP UNTIL = END PAGE

;Write 00 back to page D
		MOV	H,D		;HL = Marched Page ADDRESS
		;MVI	L,0
		MOV	A,E
		CMA
RT1_LP7:	MOV	M,A		;WRITE PAGE
		INR	L
		JNZ	RT1_LP7		;LOOP TO QUICKLY WRITE 1 PAGE

		MOV	A,D
		INR	D		;ADVANCE TO NEXT PAGE
		CPI	0xFF		;COMPARE WITH END PAGE
		JNZ	RT1_LP3		;LOOP UNTIL = END PAGE

		INR	E
		JZ	RT1_LP0

		LXI	H,MSG3
		JMP	PRINTI_NS
MSG3:		.ascii "PASSED\r\nRAM BYTE MARCH A\000"

1$:		DCR	B	;5
		JNZ	1$	;10    15*256 ~= 4,000 ~= 2mSec


;Byte March Test.  7 Sec/K
;
; FOR E = 00 TO FF STEP FF   'March 00 then March FF
;   FOR H = B TO C
;      PAGE(H) = E
;      FOR D = 00 TO FF
;         PAGE(H).D = NOT E
;         FOR L=0 TO FF
;            IF PAGE(H).L <> E THEN
;               IF PAGE(H).L <> NOT E THEN ERROR2
;               IF L<>D THEN ERROR2
;            ENDIF
;         NEXT L
;      NEXT D
;   NEXT H
; NEXT E

		MVI	E,0xFF		;E selects the polarity of the test, ie March a page of 1'S or 0's

;Clear/Set all pages

RT2_LP0:	MVI	H,0xF0		;HL = BASE RAM ADDRESS
RT2_LP1:	MVI	L,0

		MOV	A,H
		CMA
		OUT	FPLED
		MVI	A,"."
		OUT	UART0
		OUT	PROPELLERD


		MOV	A,E		;CLEAR A
		CMA
RT2_LP2:	MOV	M,A		;WRITE PAGE
		INR	L
		JNZ	RT2_LP2		;LOOP TO QUICKLY WRITE 1 PAGE


		MVI	D,0		;Starting with BYTE 00 of page

RT2_LP3:	MOV	L,D		;Save at byte march ptr
		MOV	A,E		;SET A
		MOV	M,A

		;MOV	A,E
		CMA			;CLEAR A
		MVI	L,0

RT2_LP4:	CMP	M		;TEST BYTE FOR CLEAR
		JZ	RT2_NX1
		CMA			;SET A
		CMP	M		;TEST BYTE FOR SET
		JNZ	RT_FAIL		;IF NOT FULLY SET, THEN DEFINITELY FAIL
		MOV	A,L		;ELSE CHECK WE ARE ON MARCHED BYTE
		CMP	D
		JNZ	RT_FAIL
		MOV	A,E		;CLEAR A
		CMA
RT2_NX1:	INR	L
		JNZ	RT2_LP4		;LOOP TO QUICKLY WRITE 1 PAGE

		MOV	L,D		;Save at byte march ptr
		MOV	A,E
		CMA			;CLEAR A
		MOV	M,A

		INR	D
		JNZ	RT2_LP3

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CPI	0xFF		;COMPARE WITH END PAGE
		JNZ	RT2_LP1		;LOOP UNTIL = END PAGE

		INR	E
		JZ	RT2_LP0

		LXI	H,MSG4
		JMP	PRINTI_NS
MSG4:		.ascii "PASSED\r\nRAM BYTE MARCH B\000"

;26 Sec/K

BYTEMARCH2:
		MVI	E,0xFF		;E selects the polarity of the test, ie March a page of 1'S or 0's

RT4_LP0:	MVI	D,0		;Starting with BYTE 00 of page

;CLEAR all pages

		LXI	H,0xF000	;HL = BASE RAM ADDRESS


RT4_LP1:	MOV	A,E		;CLEAR A
		CMA
RT4_LP2:	MOV	M,A		;WRITE PAGE
		INR	L
		JNZ	RT4_LP2		;LOOP TO QUICKLY WRITE 1 PAGE

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CPI	0xFF		;COMPARE WITH END PAGE
		JNZ	RT4_LP1		;LOOP UNTIL = END PAGE


RT4_LP3:	MOV	A,D
		CMA
		OUT	FPLED

		ANI	0x0F
		JNZ	RT4_SKP
		MVI	A,"."
		OUT	UART0
		OUT	PROPELLERD


RT4_SKP:

					;Write SET byte at "D" in every page
		MVI	H,0xF0		;HL = BASE RAM ADDRESS
		MOV	L,D		;Save at byte march ptr
RT4_LP4:	MOV	M,E

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CPI	0xFF		;COMPARE WITH END PAGE
		JNZ	RT4_LP4		;LOOP UNTIL = END PAGE


		MVI	L,0

RT4_LP5:	MVI	H,0xF0		;HL = BASE RAM ADDRESS
		MOV	A,L
		CMP	D
		JZ	RT4_LP7		;Test for marked byte in all pages

RT4_LP6:	MOV	A,E
		CMA			;CLEAR A
		CMP	M		;TEST BYTE FOR CLEAR
		JNZ	RT_FAIL

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CPI	0xFF		;COMPARE WITH END PAGE
		JNZ	RT4_LP6		;LOOP UNTIL = END PAGE
		JMP	RT4_NX

RT4_LP7:	MOV	A,E
		CMP	M		;TEST BYTE FOR SET
		JNZ	RT_FAIL

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CPI	0xFF		;COMPARE WITH END PAGE
		JNZ	RT4_LP7		;LOOP UNTIL = END PAGE

RT4_NX:		INR	L
		JNZ	RT4_LP5

					;Write CLEAR byte at "D" in every page
		MVI	H,0xF0		;HL = BASE RAM ADDRESS
		MOV	L,D		;Save at byte march ptr
RT4_LP8:	MOV	A,E
		CMA
		MOV	M,A

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CPI	0xFF		;COMPARE WITH END PAGE
		JNZ	RT4_LP8		;LOOP UNTIL = END PAGE

		INR	D
		JNZ	RT4_LP3


		INR	E
		JZ	RT4_LP0

		LXI	H,MSG5
		JMP	PRINTI_NS
MSG5:		.ascii "PASSED\r\nRAM BIT MARCH \000"


BIT_MARCH:
;Bit March Test.  0.1 Sec/K

		MVI	E,01		;E selects the bit to march

;Clear/Set all pages

RT3_LP1:	LXI	H,0xF000	;HL = BASE RAM ADDRESS


		MOV	A,E		;Display bit pattern on LED PORT
		CMA
		OUT	FPLED
		MVI	A,"."
		OUT	UART0
		OUT	PROPELLERD

RT3_LP2:	MOV	A,E		;FETCH MARCHING BIT PATTERN
RT3_LP3:	MOV	M,A		;WRITE PAGE
		INR	L
		JNZ	RT3_LP3		;LOOP TO QUICKLY WRITE 1 PAGE

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CPI	0xFF		;COMPARE WITH END PAGE
		JNZ	RT3_LP2		;LOOP UNTIL = END PAGE

		LXI	H,0xF000	;HL = BASE RAM ADDRESS

RT3_LP4:	MOV	A,E		;FETCH MARCHING BIT PATTERN
RT3_LP5:	CMP	M
		JNZ	RT_FAIL
		INR	L
		JNZ	RT3_LP5		;LOOP TO QUICKLY WRITE 1 PAGE

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CPI	0xFF		;COMPARE WITH END PAGE
		JNZ	RT3_LP4		;LOOP UNTIL = END PAGE


					;0000 0010
					;...
					;1000 0000

		MOV	A,E
		RAL			;ROTATE THE 01 UNTIL 00
		MOV	A,E
		RLC
		MOV	E,A
		CPI	1
		JNZ	RT3_NX1
		CMA			;INVERT ALL BITS
		MOV	E,A
		JMP	RT3_LP1
RT3_NX1:	CPI	0xFE
		JNZ	RT3_LP1


		LXI	H,MSG6
		JMP	PRINTI_NS
MSG6:		.ascii "PASSED\r\nRAM SEQUENCE TEST\000"



		MVI	E,01		;E selects the start sequence

;Clear/Set all pages

RT5_LP1:	MOV	A,E		;Display bit pattern on LED PORT
		ANI	0x7
		JNZ	RT5_SKP
		MVI	A,"."
		OUT	UART0
		OUT	PROPELLERD

RT5_SKP:
		MOV	A,E		;Display bit pattern on LED PORT
		CMA
		OUT	FPLED

		LXI	H,0xF000	;HL = BASE RAM ADDRESS

		MOV	D,E

RT5_LP2:	INR	D
		JNZ	RT5_NX1
		INR	D
RT5_NX1:	MOV	M,D		;WRITE PAGE
		INR	L
		JNZ	RT5_LP2		;LOOP TO QUICKLY WRITE 1 PAGE

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CPI	0xFF		;COMPARE WITH END PAGE
		JNZ	RT5_LP2		;LOOP UNTIL = END PAGE

		LXI	H,0xF000	;HL = BASE RAM ADDRESS

		MOV	D,E

RT5_LP3:	INR	D
		JNZ	RT5_NX2
		INR	D
RT5_NX2:	MOV	A,D
		CMP	M		;TEST
		JNZ	RT_FAIL
		INR	L
		JNZ	RT5_LP3		;LOOP TO QUICKLY WRITE 1 PAGE

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CPI	0xFF		;COMPARE WITH END PAGE
		JNZ	RT5_LP3		;LOOP UNTIL = END PAGE

		INR	E
		JNZ	RT5_LP1

		LXI	H,MSG7
		JMP	PRINTI_NS
MSG7:		.ascii "PASSED\000"

		JMP	BOOT_SD


RT_FAIL:	MOV	D,H		;Save the address in HL to DE
		MOV	E,L
		LXI	H,MSG8
		JMP	PRINTI_NS
MSG8:		.ascii "\r\nFAILED AT:\000"

		MOV	A, D

		RRC
		RRC
		RRC
		RRC

		ANI	0x0F
		ADI	0x90
		DAA
		ACI	0x40
		DAA

2$:		DCR	B	;5
		JNZ	2$	;10    15*256 ~= 4,000 ~= 2mSec
		OUT	UART0
		OUT	PROPELLERD

		MOV	A, D

		ANI	0x0F
		ADI	0x90
		DAA
		ACI	0x40
		DAA

3$:		DCR	B	;5
		JNZ	3$	;10    15*256 ~= 4,000 ~= 2mSec
		OUT	UART0
		OUT	PROPELLERD

		MOV	A, E

		RRC
		RRC
		RRC
		RRC

		ANI	0x0F
		ADI	0x90
		DAA
		ACI	0x40
		DAA

4$:		DCR	B	;5
		JNZ	4$	;10    15*256 ~= 4,000 ~= 2mSec
		OUT	UART0
		OUT	PROPELLERD

		MOV	A, E

		ANI	0x0F
		ADI	0x90
		DAA
		ACI	0x40
		DAA

5$:		DCR	B	;5
		JNZ	5$	;10    15*256 ~= 4,000 ~= 2mSec
		OUT	UART0
		OUT	PROPELLERD

SYS_HALT:
		LXI	H,MSG12
		JMP	PRINTI_NS
MSG12:		.ascii " - SYSTEM HALTED\r\n\000"

6$:		HLT
		JMP	6$


;===============================================
;PRINT IMMEDIATE  (NO STACK USED, RETURNS VIA HL, NUKES B)
;PRINT -- Print a null-terminated string
;-----------------------------------------------
PRINTI_NS:	MOV	A, M
		INX	H
		ORA	A
		JNZ	PRINTI_DO
		PCHL		;RETURN TO NEXT INSTRUCTION
PRINTI_DO:
		DCR	B	 ;5
		JNZ	PRINTI_DO ;10    15*256 ~= 4,000 ~= 2mSec

		OUT	UART0
		OUT	PROPELLERD
		JMP	PRINTI_NS




;----------------------------------------------------------------------------------------------------; BOOT SD
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;----------------------------------------------------------------------------------------------------; BOOT SD
BOOT_SD:		LXI	SP,HIGHSTACK ;128 Bytes of stack available.

		CALL	PRINTI
		.ascii "\r\nBOOT BIOS.HEX, INIT_FAT \000"

		CALL	INIT_FAT

		CALL	PRINTI
		.ascii "\r\nLOADING FILE \000"

		LXI	D,SDISKA+1
		CALL	INIT_BLOCK	;Preload SD_FCB with file name
		.ascii	"BIOS    HEX\000"

;=====================================================================================================
;Open File

SD_OPEN:	LXI	H,SDISKA
		MVI	M,0		;FSTAT=0, Clear Open Status
		INX	H		;+1 = FNAME
		LXI	D,FILENAME	;Write FCB File name to FILENAME for finding
		MVI	B,11
		CALL	COPY_RAM
SD_MANUAL:	CALL	SDV_FIND_FILE	;H=Directory Entry  ;PRINT FILE NAME, FOUND OR NOT
		JZ	DO_DIR		;Exit if file not found

SDO_DO:		PUSH	H
		LXI	B,0x1C		;File Size Offset (into Directory Entry)
		DAD	B
		CALL	MOV_32_HL	;Move (HL) to 32 bit register BCDE
		LXI	H,FSIZE
		CALL	MOV_HL_32	;Save 32 bits to RAM at HL

		POP	H
		LXI	B,0x1A		;H=(START CLUSTER)
		DAD	B
		CALL	LD_HL_HL	;Fetch Starting Custer
		XCHG			;D=Starting Cluster
		LXI	H,SDISKA	;H=FCB
		MVI	M,1		;FSTAT=1
		LXI	H,SDISKA+AFClus0 ;offset to AFClus0
		MOV	M,E		;Save Starting Cluster
		INX	H
		MOV	M,D
		INX	H
		MVI	B,14
		MVI	A,0xFF
		CALL	FILL_BLOCK	;Fill 14 bytes of FF (Nuke pointers to force new calculations)

		CALL	PRINTI
		.ascii "\r\nFILE SIZE=0x\000"

		LHLD	FSIZE+2
		CALL	PUT_HL
		LHLD	FSIZE
		CALL	PUT_HL

		CALL	PRINTI
		.ascii "\r\n\000"

		LXI	H,0
		SHLD	LOGICAL_SEC
		LXI	H,0xFFFF	;Start INB_PTR AT END to Force a Sector Read
		SHLD	INB_PTR

		MVI	A,0		;CLEAR START ADDRESS VALID
		STA	GH_VALID

		LXI	H,GH0
		SHLD	GH_STATE

READ_FILE_LOOP:	LXI	H,FSIZE
		CALL	MOV_32_HL	;Fetch File Size (count)

		CALL	TSTZ_32
		JZ	DO_EOF		;END OF FILE REACHED

		CALL	DEC_32		;Decrement count of bytes
		LXI	H,FSIZE
		CALL	MOV_HL_32	;Save count


		;Input:	LOGICAL_SEC = 0=First Sector

		LHLD	INB_PTR
		INX	H
		MOV	A,H
		ORA	L
		JNZ	RFL_1

		CALL	DISK_READ	;Read next or first sector
		LHLD	LOGICAL_SEC	;advance to next sector
		INX	H
		SHLD	LOGICAL_SEC

		LXI	H,SD_RAM_BUFFER
RFL_1:		SHLD	INB_PTR

		LDA	GH_VIEW
		ORA	A
		JZ	RFL_2
		MOV	A,M
		CALL	PUT_CHAR

RFL_2:		MOV	A,M

		LXI	H,READ_FILE_LOOP
		PUSH	H		;RETURN ADDRESS
		LHLD	GH_STATE	;
		PCHL			;Jump to state of Get HEX File

					;End of File Reached
DO_EOF:		CALL	PRINTI
		.ascii "\r\n-EOF-\000"

		JMP	GH_EXEC

					;========================================================== STATE 0
GH0:		CPI	":"		;State 0, Wait for start of a record, look for colon
		RNZ

		XRA	A
		STA	GH_CHKSUM	;Init CHKSUM=0
					;Next state is to fetch an ascii pair into a BYTE, then process this as line count
		LXI	H,GH1
GH_NEXT_BYTE:	SHLD	GH_STATE2	;- - - - - - - - - - - - NEXT = FETCH ASCII PAIR

		LXI	H,GH_MSD	;Fetch MSD char
		SHLD	GH_STATE
		RET

					;- - - - - - - - - - - - FETCH MSD
GH_MSD:		CALL	IS_HEX_CHAR	;Process MSD char
		JC	CHAR_ERROR
		RLC
		RLC
		RLC
		RLC
		STA	GH_BYTE		;Save it at GH_BYTE for combining with LSD
		LXI	H,GH_LSD
		SHLD	GH_STATE
		RET
					;- - - - - - - - - - - - FETCH LSD
GH_LSD:		CALL	IS_HEX_CHAR	;Process LSD char
		JC	CHAR_ERROR
		MOV	H,A
		LDA	GH_BYTE
		ORA	H		;Combine the MSD with LSD
		MOV	H,A
		LDA	GH_CHKSUM	;Update CHECK SUM
		ADD	H
		STA	GH_CHKSUM
		MOV	A,H
		LHLD	GH_STATE2
		PCHL			;Execute the Get HEX

CHAR_ERROR:	CALL	PRINTI
		.ascii "\r\n!!! ERROR, NOT A HEX CHAR\000"
		JMP	SYS_HALT
					;========================================================== STATE 1
GH1:		STA	GH_COUNT	;Set Count of bytes on line
		LXI	H,GH2
		JMP	GH_NEXT_BYTE
					;========================================================== STATE 2
GH2:		STA	GH_ADDR+1	;Set High Address
		LXI	H,GH3
		JMP	GH_NEXT_BYTE
					;========================================================== STATE 3
GH3:		STA	GH_ADDR		;Set Low Address
		LDA	GH_VALID
		ORA	A
		JNZ	GH3_RET
		INR	A
		STA	GH_VALID
		LHLD	GH_ADDR		;Save first address in HEX file as START address
		SHLD	GH_START
GH3_RET:		LXI	H,GH4
		JMP	GH_NEXT_BYTE
					;========================================================== STATE 4
GH4:		STA	GH_TYPE		;Record Type
		CPI	2
		JP	GH_IGNORED	;Process types 00 and 01, ignore all others

GH5_NEXT:	LDA	GH_COUNT
		ORA	A
		LXI	H,GH6		;Set next state depending on BYTE COUNT
		JZ	GH_NEXT_BYTE
		DCR	A
		STA	GH_COUNT
		LXI	H,GH5
		JMP	GH_NEXT_BYTE
					;========================================================== STATE 5
GH5:		LHLD	GH_ADDR		;Write Data (for count of bytes)
		MOV	M,A
		INX	H
		SHLD	GH_ADDR
		JMP	GH5_NEXT	;Count down bytes

GH6:		LDA	GH_CHKSUM
		ORA	A
		JNZ	CHKSUM_ERROR
		LDA	GH_TYPE
		CPI	1
		JZ	GH_EXEC

GH0_NEXT:	LXI	H,GH0
		SHLD	GH_STATE
		RET

GH_IGNORED:	CALL	PRINTI
		.ascii "-Ignored \000"
		JMP	GH0_NEXT

CHKSUM_ERROR:	CALL	PRINTI
		.ascii "\r\n!!! CHECKSUM ERROR\000"
		JMP	SYS_HALT


GH_EXEC:		LDA	GH_VALID
		ORA	A
		JNZ	GH_EXEC_GO
		CALL	PRINTI
		.ascii "\r\n!!! START ADDRESS NOT SET\000"
		JMP	SYS_HALT

GH_EXEC_GO:	CALL	PRINTI
		.ascii "\r\nExecute at:\000"

		LHLD	KILL_SHADOW	;FETCH INSTRUCTIONS TO KILL ROM AND PUT IN RAM
		SHLD	0xFFFD		;FFFD D3 18
		LDA	KILL_SHADOW+2
		STA	0xFFFF		;FFFF xx

		LHLD	GH_START	;HL = JUMP ADDRESS
		CALL	PUT_HL
		CALL	PRINTI
		.ascii "\r\n\000"

		MVI	A,1		;KILL SHADOW
		JMP	0xFFFD		;JUMP TO SET PORT & JUMP TO BIOS.HEX ENTRY

KILL_SHADOW:	OUT	PPORT		;TURNS OFF SHADOW ROM
		PCHL			;Execute the Get HEX



;in:	A = CHAR
;out:	A = Value of HEX Char when CY=0
;	A = Received (non-hex) char when CY=1
IS_HEX_CHAR:	CPI	"0"
		JM	GHC_NOT_RET
		CPI	"9"+1
		JM	GHC_NRET
		CPI	"A"
		JM	GHC_NOT_RET
		CPI	"F"+1
		JM	GHC_ARET
		CPI	"a"
		JM	GHC_NOT_RET
		CPI	"f"+1
		JM	GHC_ARET
GHC_NOT_RET:	STC
		RET
GHC_ARET:	SUI	0x07
GHC_NRET:	ANI	0x0F
		RET



;------------------------

INIT_BLOCK:	XTHL	;HL = Top of Stack
IB_LP:		MOV	A,M	;Copy all data up to 0x00 to SD_FCB
		INX	H	;Inc HL, so on finding 0x00, exit to next instruction
		ORA	A
		JZ	IB_RET
		STAX	D
		INX	D
		JMP	IB_LP
IB_RET:		XTHL		;Move updated return address back to stack
		RET





;----------------------------------------------------------------------------------------------------; CONSOLE BIOS
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;----------------------------------------------------------------------------------------------------; CONSOLE BIOS


;===============================================
;PUT_BC Prints BC Word
;-----------------------------------------------
PUT_BC:		PUSH	PSW
		MOV	A, B
		CALL	PUT_BYTE
		MOV	A, C
		CALL	PUT_BYTE
		POP	PSW
		RET

;===============================================
;PUT_DE Prints DE Word
;-----------------------------------------------
PUT_DE:		PUSH	PSW
		MOV	A, D
		CALL	PUT_BYTE
		MOV	A, E
		CALL	PUT_BYTE
		POP	PSW
		RET

;===============================================
;PUT_HL Prints HL Word
;-----------------------------------------------
PUT_HL:		PUSH	PSW
		MOV	A, H
		CALL	PUT_BYTE
		MOV	A, L
		CALL	PUT_BYTE
		POP	PSW
		RET

;===============================================
;PUT_BYTE -- Output byte to console as hex
;
;pre:	A register contains byte to be output
;post:
;-----------------------------------------------
PUT_BYTE:	PUSH	PSW
		RRC
		RRC
		RRC
		RRC
		CALL	PUT_HEX
		POP	PSW
		PUSH	PSW
		CALL	PUT_HEX
		POP	PSW
		RET

;===============================================
;PUT_HEX -- Convert nibble to ASCII char
;
;pre: A register contains nibble
;post: A register contains ASCII char
;-----------------------------------------------
PUT_HEX:	ANI	0x0F
		ADI	0x90
		DAA
		ACI	0x40
		DAA
		JMP	PUT_CHAR


;===============================================
;PRINT -- Print a null-terminated string
;
;pre: HL contains pointer to start of a null-
;     terminated string
;-----------------------------------------------
PRINT:		MOV	A, M
		INX	H
		ORA	A
		RZ
		CALL	PUT_CHAR
		JMP	PRINT

;===============================================
;PRINT IMMEDIATE
;-----------------------------------------------
PRINTI:		XTHL	;HL = Top of Stack
		PUSH	PSW
		CALL	PRINT
		POP	PSW
		XTHL	;Move updated return address back to stack
		RET


;===============================================
;PRINT B-LENGTH
;-----------------------------------------------
PRINTB:		MOV	A, M
		CALL	PUT_CHAR
		INX	H
		DCR	B
		JNZ	PRINTB
		RET

;===============================================
;GET_CHAR -- Get a char from the console
;-----------------------------------------------
GET_CHAR:	IN	UART0+5	;10	;TEST FOR RX DATA
		ANI	1	;7
		JZ	GC_0	;10
		IN	UART0
		JMP	GC_DO

GC_0:		IN	PROPELLERS
		ANI	2
		JZ	GET_CHAR
		IN	PROPELLERD
		ORA	A
		JZ	GET_CHAR
		CPI	0xFF
		JZ	GET_CHAR

GC_DO:		CPI	" "	;Do not echo control chars
		RM

;===============================================
;PUT_CHAR -- Output a character to the console
;-----------------------------------------------
PUT_CHAR:	PUSH	PSW
		CPI	13
		JZ	PC_NL
		CPI	32
		JM	PC_LP
		LDA	PC_POS
		INR	A
		STA	PC_POS
PC_LP:		IN	UART0+5
		ANI	0x20	;TEST FOR TX HOLD REG EMPTY
		JZ	PC_LP
		POP	PSW
		OUT	UART0
		OUT	PROPELLERD
		RET
PC_NL:		XRA	A
		STA	PC_POS
		JMP	PC_LP


;----------------------------------------------------------------------------------------------------; FLOPPY DISK BIOS
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;----------------------------------------------------------------------------------------------------; FLOPPY DISK BIOS




;SDFCB:
FSTAT		.EQU	0	;BLOCK	1	;+0  Status of FCB, 00=File Not Open
FNAME		.EQU	1	;BLOCK	11	;+1  File name
AFClus0		.EQU	12	;BLOCK	2	;+12 First Cluster of File as given by the Directory Entry.
CRFClus		.EQU	14	;BLOCK	2	;+14 Current Relative Cluster location in file, (0 to F for a system with 32 Sectors per Cluster)
CAFClus		.EQU	16	;BLOCK	2	;+16 Current Absolute Cluster location in file, set to AFClus0, then updated with FAT
RFSec		.EQU	18	;BLOCK	2	;+18 Relative Sector being addressed (0 to 500, based 26 sectors per track and 77 tracks Divide by 4)
SSOC		.EQU	20	;BLOCK	4	;+20 Starting Sector of Cluster, this is the first Sector for that Cluster
ABS_SEC		.EQU	24	;BLOCK	4	;+24 Absolute Sector of Current Relative Sector



;=====================================================================================================
;Directory Routines.  1st Routine to start/init the search, 2nd routine to continue the search
;=====================================================================================================

;-----------------------------------------------------------------------------------------------------
;Call this routine to initialize and start the HL Pointer to the first Directory Entry
SD_LDIR1:	LXI	H,DIR_SECTOR	;SEC_PTR = DIR_SECTOR
		CALL	MOV_32_HL
		LHLD	ROOTDIR_SIZE	;ENT_COUNT = ROOTDIR_SIZE (to count down directory entries searched)
		SHLD	ENT_COUNT
		ORA	A		;Clear Carry, Read only if Necessary

SD_FETCH:	CALL	SD_READ_SEC	;Fetch a ROOT DIRECTORY sector
		LXI	H,SD_RAM_BUFFER	;(Re)start H at start of Sector
SD_TEST:	XRA	A		;EXIT Z=0 if there is a File at this entry
		CMP	M
		RET

;-----------------------------------------------------------------------------------------------------
;Call this routine to advance to the next Directory Entry (loads next sector and restarts HL as needed)
;-----------------------------------------------------------------------------------------------------
SD_LDIRN:	LXI	B,0x20		;Advance to next file entry
		DAD	B
		JNC	SD_TEST		;Check if extended beyond this sector

		XRA	A		;Return Z=1 if no more files
		LHLD	ENT_COUNT
		LXI	B,-16
		DAD	B
		SHLD	ENT_COUNT
		RNC			;Out of Directory entries
		MOV	A,H
		ORA	L
		RZ			;Out of Directory entries

		LXI	H,SEC_PTR	;Advance to next SECTOR
		CALL	MOV_32_HL
		CALL	INC_32
		JMP	SD_FETCH

;-----------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------
		;Find File w/ Verbose Output
		;Call with File Name set in FILENAME.EXT
		;Return Z=1 File Not Found
		;	Z=0 File Found, HL = Ptr to Directory Entry in SD_RAM_BUFFER
SDV_FIND_FILE:	LXI	H,FILENAME
		CALL	PRINT_FILENAME
SDV_FIND_FILE1:	CALL	SD_FIND_FILE	;Call the Find File routine
		JNZ	SDV_FOUND	;Print Yah or Nah
		CALL 	PRINTI
		.ascii " -NOT FOUND\000"
		RET
SDV_FOUND:	CALL 	PRINTI
		.ascii " -EXISTS\000"
		RET

;-----------------------------------------------------------------------------------------------------
		;Find File
		;Call with File Name set in RAM variable: FILENAME.EXT
		;Return Z=1 File Not Found
		;	Z=0 File Found, HL = Ptr to Directory Entry in SD_RAM_BUFFER
SD_FIND_FILE:	CALL	SD_LDIR1
SDFF_LP: 	RZ			;End of list
		CALL	CMP_FILENAME
		RNZ			;FILE FOUND
		CALL	SD_LDIRN
		JMP	SDFF_LP



;=====================================================================================================
;Read of Logical Disk Sector.
;=====================================================================================================
	;Start-of-Directory = Size-of-Fat * Number-of-fats + 1 (boot sector)
	;Start-of-Data-Area = Start-of-Directory + #Entries/32/bytes_per_sector

	;Input:	Disk Sector required (0 to 2001) based on 26 sectors per track by 77 tracks, counting from 0
	;Disk FCB in HL

	;if AFClus0 = 0x0000 then attempt to open the file report Disk not loaded if fail
	;Relative file sector:
	;DISK_SEC is the input to this routine, it holds Virtual Disk Sector 0 to 2001
	;because every SD sector has 512 bytes, each SD sector holds 4 CP/M Virtual Disk sectors (that's 128 bytes)
	;RFSec is the Relative File Sector, it spans from 0 to 500 (this accomodates 501 SD Sectors or 256,512 bytes)
	;If RFSec = DISK_SEC / 4 Then...
	;If RFSec has not changed, then read that sector into RAM and be done.
	;That Relative File sector is located on the SD card at address set in the absolute sector (ABS_SEC)
	;
	;If RFSec has changed... then determine is the new RFSec is within the same cluster or not.
	;On a 1Gig SD card, the system uses 32 sectors per cluster.  This means, 32 sequential SD Memory Card sectors form 1 cluster.
	;If a sector within the same cluster is being accessed, then the cluster does not have to be found again.
	;If it's NOT in the same cluster, then find new cluster by looking through the FAT
	;If it is in the same cluster, then skip to the part were we can just offset the RFSec into the current cluster
	;
	;...else
	;RFSec = DISK_SEC / 4  'Set the new sector as the current one.
	;
	;Find the Relative File Cluster (RFClus).  This number will be from 0 to 15 on a 1Gig SD Card = 262,144 bytes (to hold a 256,256 file)
	;
	;Relative file cluster:
	;RFClus = RFSec / SEC_PER_CLUS
	;if RFClus has changed, then recalculate the File Cluster and then update the SSOC.
	;  if RFClus < CRFClus then CRFClus=0, CAFClus = AFClus0  'start FAT search from 0 if going backward
	;  RFClus = RFClus - CRFClus
	;  do while RFClus>0
	;      if CAFClus = 0xFFFF Then EOF reached, file too small to be a disk. (Report as disk error)
	;      CAFClus = FAT(CAFClus)
	;      CRFClus = CRFClus + 1
	;      RFClus = RFClus - 1
	;  loop
	;  SSOC = (CAFClus - 2) * SEC_PER_CLUS + Start-of-Data-Area
	;if RFClus has NOT changed, then ABS_SEC = SSOC + (RFSec MOD SEC_PER_CLUS), then read ABS_SEC into the buffer.

;=====================================================================================================
;Read of Logical Disk Sector.
;Input:	LOGICAL_SEC = 0=First Sector
DISK_READ:
;		CALL	PRINTI
;		.ascii " R-\000"
;		LHLD	LOGICAL_SEC
;		CALL	PUT_HL

		LXI	H,SDISKA	;Get Current Disk FCB
		MOV	A,M		;Is file open?
		ORA	A		;Test FSTAT
		JNZ	DR_1		;Jump YES

;		CALL	PRINTI
;		.ascii " HL:\000"
;		CALL	PUT_HL

		CALL	SD_OPEN		;ELSE, Attempt to open file
		LDA	SDISKA		;Is file open?
		ORA	A		;Test FSTAT
		JNZ	DR_1		;Jump YES
		CALL	PRINTI
		.ascii " -Disk Not Loaded\000"
		RET			;Exit if file could not open

DR_1:		LXI	H,SDISKA+RFSec	;If file open, Check if Read is from same Data Sector
		MOV	E,M
		INX	H
		MOV	D,M		;D=RFSec
		LHLD	LOGICAL_SEC	;Fetch sector to be read
		CALL	CMP_DE_HL
		JNZ	DR_NEW_SEC	;Jump if Read is from a different Data Sector

					;LOGICAL SECTOR = LAST READ SECTOR, Fetch Absolute Sector and read it to RAM (if wasn't last read)
		LXI	H,SDISKA+ABS_SEC ;H=FCB(ABS_SEC)

		CALL	MOV_32_HL
		JMP	DR_READ_IT


	;RFClus = RFSec / SEC_PER_CLUS
	;if RFClus has changed, then recalculate the File Cluster and then update the SSOC.
	;  if RFClus < CRFClus then
	;     CRFClus=0, CAFClus = AFClus0  'start FAT search from 0 if going backward
	;  eles
	;     RFClus = RFClus - CRFClus	   'else, continue FAT search from point of
	;  endif
	;  do while RFClus>0
	;      if CAFClus = 0xFFFF Then EOF reached, file too small to be a disk. (Report as disk error)
	;      CAFClus = FAT(CAFClus)
	;      CRFClus = CRFClus + 1
	;      RFClus = RFClus - 1
	;  loop
	;  SSOC = (CAFClus - 2) * SEC_PER_CLUS + Start-of-Data-Area
	;if RFClus has NOT changed, then ABS_SEC = SSOC + (RFSec MOD SEC_PER_CLUS), then read ABS_SEC into the buffer.
DR_NEW_SEC:
		XCHG			;D=LOGICAL_SEC = Relative File Sector (Update FCB with this new Rel-File-Sec
		LXI	H,SDISKA+RFSec	;Set FCB(RFSec)
		MOV	M,E		;MOV (HL),DE
		INX	H
		MOV	M,D

		LHLD	DIVIDE_FUNC	;DE = DE / Sectors-Per-Cluster
		CALL	VCALL
		LXI	H,SDISKA+CRFClus ;H=FCB(CRFClus)
		PUSH	H
		CALL	LD_HL_HL
		MOV	B,H		;BC = CRFClus
		MOV	C,L

		POP	H		;H->FCB(CRFClus)
					;TEST DE - BC  aka NewRFClus vs FCB-RFClus
					;Speed Optimize the above code
		MOV	A,D
		CMP	B
		JNZ	DR_DIFF_CLUS
		MOV	A,E
		CMP	C
		JZ	DR_SAME_CLUS	;IF they are the same, then the new sector is in the same cluster
DR_DIFF_CLUS:	JNC	DR_BIGGER_CLUS

					;If going to a smaller cluster, restart the FAT search from the begining
		LXI	B,0		;CRFClus = 0
		DCX	H
		DCX	H
		JMP	DR_SEEK_FAT	;HL will load with AFClus0

DR_BIGGER_CLUS:
		MOV	A,E		;NewRFClus = NewRFClus - FCB-RFClus,  ie Set counter for number of new FAT hops.
		SUB	C
		MOV	E,A
		MOV	A,D
		SBB	B
		MOV	D,A

		INX	H
		INX	H		;HL will load with CAFClus

	;  do while RFClus>0
	;      if CAFClus = 0xFFFF Then EOF reached, file too small to be a disk. (Report as disk error)
	;      CAFClus = FAT(CAFClus)
	;      CRFClus = CRFClus + 1
	;      RFClus = RFClus - 1
	;  loop

DR_SEEK_FAT:
		CALL	LD_HL_HL	;HL = CAFClus or AFClus0

;		CALL	PRINTI
;		.ascii "\r\nCAFClus=\000"
;		CALL	PUT_HL
;		CALL	PRINTI
;		.ascii "\r\nCRFClus=\000"
;		CALL	PUT_BC
;		CALL	PRINTI
;		.ascii "\r\nRFClus=\000"
;		CALL	PUT_DE

					;BC = CRFClus
DR_SEEK_LP:	MOV	A,D		;DE = RFClus
		ORA	E
		JZ	DR_SEEK_DONE

;		CALL	PRINTI
;		.ascii "\r\nseek=\000"
;		CALL	PUT_HL

		INX	H		;IF CAFClus = 0xFFFF...
		MOV	A,H
		ORA	L
		JNZ	DR_SEEK_1
					;Error, File too small
		CALL	PRINTI
		.ascii " -ERROR, NO MORE ALLOCATED CLUSTERS!\000"
1$:		HLT
		JMP	1$

DR_SEEK_1:	DCX	H

	;Here comes the FAT Hopping FUN...
	;      CAFClus = FAT(CAFClus)
	;it's convenient that 1 Sector is 512 bytes, that's 256 words = 256 FAT Entries, therefore...
	;H = Sector of FAT
	;L = Word within that Sector of FAT

		PUSH	B
		PUSH	D
		PUSH	H
		MOV	E,H		;E=Sector of FAT
		LHLD	FAT1START	;DE = E + FAT1START
		MOV	A,L
		ADD	E
		MOV	E,A
		MOV	A,H
		ACI	0		;Carry it forward
		MOV	D,A
		LHLD	FAT1START+2
		JNC	DRS_0		;Test for Carry
		INX	H		;Carry it forward
DRS_0:		MOV	B,H
		MOV	C,L		;BCDE now have Sector of FAT desired
		CALL	SD_READ_SEC
		POP	D		;Fetch DE, E=Word within that FAT sector
		LXI	H,SD_RAM_BUFFER
		ORA	A		;Clear Carry
		MOV	A,E		;Fetch offset into FAT sector read
		RAL
		MOV	L,A		;
		MOV	A,H
		ACI	0
		MOV	H,A		;HL -> FAT Entry
		CALL	LD_HL_HL	;HL = FAT Entry
		POP	D
		POP	B

		INX	B
		DCX	D
		JMP	DR_SEEK_LP

DR_SEEK_DONE:	;Write Registers to FCB
		;BC = CRFClus
		;DE = RFClus - Not required (it's a counter down to zero to find the correct cluster)
		;HL = CAFClus

		XCHG			;DE = CAFClus
		LXI	H,SDISKA+CRFClus ;Set FCB(CRFClus)
		MOV	M,C		;Save CRFClust to FCB
		INX	H
		MOV	M,B
		INX	H
		MOV	M,E		;Save CAFClus to FCB
		INX	H
		MOV	M,D

	;Now, let's find the Data Sector to be loaded....
	;First, calculate the Starting Sector of Cluster (SSOC)
	;  SSOC = (CAFClus - 2) * SEC_PER_CLUS + Start-of-Data-Area

		XCHG			;HL = CAFClus
		DCX	H		;CAFClus - 2
		DCX	H

;-------------------------------------	Multiply Routine.  16bit by 8 bit -> 24bit
		LXI	B,0		;BHL = 16bit input (need 24 bits to shift)
		LXI	D,0		;CDE = 24bit output
		MVI	A,8		;Go through 8 bits
		STA	M_COUNTER
		LDA	SEC_PER_CLUS	;Fetch Multiplier
DRSS_LP:	RAR
		STA	MUL8
		JNC	DRSS_SHIFT
		XCHG
		DAD	D		;DE=DE+HL
		XCHG
		MOV	A,C
		ADC	B
		MOV	C,A

DRSS_SHIFT:	DAD	H		;BHL=BHL*2
		MOV	A,B		;
		RAL
		MOV	B,A
		LDA	M_COUNTER	;Count down 8 bits
		DCR	A
		STA	M_COUNTER
		LDA	MUL8		;Fetch next Multiplier bit
		JNZ	DRSS_LP
;-------
					;Add to BCDE, the DATASTART sector
		MVI	B,0		;BCDE = 32bit Absolute sector
		LHLD	DATASTART	;32 Bit ADD DATASTART
		DAD	D
		XCHG			;DE=DE+START (LSB)
		LHLD	DATASTART+2
		JNC	DRSS_ABC
		INX	B		;Add Carry out of 16 Bit ADD
DRSS_ABC:	DAD	B
		PUSH	H
		POP	B		;BC=BC+START (MSB)
;-------
					;Save the result to RAM variable SSOC
		LXI	H,SDISKA+SSOC	;Set FCB(SSOC)
		CALL	MOV_HL_32	;Save the 32 bit register BCDE to (HL)
;-------

	;ABS_SEC = SSOC + (RFSec MOD SEC_PER_CLUS), then read ABS_SEC into the buffer.
DR_SAME_CLUS:				;Fetch the RFSec
		LXI	H,SDISKA+RFSec	;Set FCB(RFSec)

		MOV	E,M		;DE=RFSec
		INX	H
		MOV	D,M

		LHLD	MOD_FUNC	;DE = DE % Sectors-Per-Cluster
		CALL	VCALL		;A = RFSec MOD SEC_PER_CLUS

		LXI	H,SDISKA+SSOC	;Set FCB(SSOC)
		LXI	B,0		;BCDE = (RFSec MOD SEC_PER_CLUS)
		MVI	D,0
		MOV	E,A
		CALL	ADD_32_HL	;BCDE = SSOC + (RFSec MOD SEC_PER_CLUS)

		INX	H		;Advance HL to ABS_SEC
		CALL	MOV_HL_32	;Save the ABS_SEC

DR_READ_IT:	CALL	SD_READ_SEC	;Fetch the Sector

		LXI	H,SD_RAM_BUFFER
		RET




;-----------------------------------------------------------------------------------------------------
CMP_FILENAME:	PUSH	H		;Save H pointer into Directory
		MVI	B,8		;Compare 8 characters
		LXI	D,FILENAME
CMPF_LP1:	LDAX	D
		CMP	M
		JNZ	CMPF_RETFAIL	;Exit if not equal
		INX	H
		INX	D
		DCR	B
		JNZ	CMPF_LP1

		POP	H
		PUSH	H
		LXI	D,8		;Point to Ext in Dir
		DAD	D

		MVI	B,3
		LXI	D,FILEEXT
CMPF_LP2:	LDAX	D
		CMP	M
		JNZ	CMPF_RETFAIL
		INX	H
		INX	D
		DCR	B
		JNZ	CMPF_LP2
		INR	B		;Z=0
		POP	H
		RET
CMPF_RETFAIL:	XRA	A		;Z=1
		POP	H
		RET

;=====================================================================================================
;=====================================================================================================
INIT_FAT:	;LXI	H,CLEAR_RAM	;Clear RAM
		;MVI	B,0
		;XRA	A
		;CALL	FILL_BLOCK

		CALL	INIT_SDCARD
		RNZ

		CALL 	PRINTI		;
		.ascii "MBR\000"


		LXI	B,0		;BCDE = 0x00000000
		LXI	D,0
		STC			;Force Read
		CALL	SD_READ_SEC	;READ MBR
		CALL	TEST_SIGNATURE
		RNZ

		CALL 	PRINTI		;
		.ascii " Type\000"
		LDA	SD_RAM_BUFFER+0x01C2
		CALL	PUT_BYTE
		STA	SD_PART_TYPE
		CPI	4
		JZ	INITFAT_PGOOD
		CPI	6
		JZ	INITFAT_PGOOD
		CPI	0x86
		JNZ	INITFAT_FAIL

INITFAT_PGOOD:	LXI	H,SD_RAM_BUFFER+0x01C6
		LXI	D,SD_PART_BASE
		MVI	B,8
		CALL	COPY_RAM	;Copy BASE & SIZE from BUFFER to RAM Variables
		CALL 	PRINTI		;
		.ascii " PBR\000"

		LXI	H,SD_PART_BASE
		CALL	MOV_32_HL	;Copy BASE to SEC_PTR
		CALL	SD_READ_SEC	;READ BOOT RECORD OF PARTITION
		CALL	TEST_SIGNATURE
		RNZ

		LXI	H,SD_RAM_BUFFER+0x0B
		LXI	D,BYTE_P_SEC
		MVI	B,10
		CALL	COPY_RAM	;Copy Description Table to RAM Variables (Up to Total Filesys Sectors)
		XCHG			;Test TOTAL_FILESYS_SECTORS = 0
		DCX	H
		DCX	H
		MOV	A,M
		INX	H
		ORA	M
		JNZ	INITFAT_TFS_OK
		DCX	H
		XCHG
		LXI	H,SD_RAM_BUFFER+0x20

		CALL	COPY_RAM4
		JMP	INITFAT_TFS_DONE

INITFAT_TFS_OK:	XRA	A
		INX	H
		MOV	M,A
		INX	H
		MOV	M,A
		INX	H
		XCHG
INITFAT_TFS_DONE:

		LXI	H,SD_RAM_BUFFER+0x1C
		CALL	COPY_RAM4	;Copy HIDDEN_SECTORS to RAM Variables
		LXI	H,SD_RAM_BUFFER+0x16
		CALL	COPY_RAM2	;Copy SECTORS_PER_FAT to RAM Variables

;BS.fat1Start = MBR.part1Start + BS.reservedSectors;
		LHLD	RESERVED_SEC	;H=Reserved Sectors
		XCHG
		LHLD	SD_PART_BASE	;FAT1START = SD_PART_BASE + RESERVED_SEC
		DAD	D
		SHLD	FAT1START
		LHLD	SD_PART_BASE+2
		JNC	INITFAT_C1_DONE
		INX	H
INITFAT_C1_DONE:	SHLD	FAT1START+2

;firstDirSector = BS.fat1Start + (BS.fatCopies * BS.sectorsPerFAT);
		LDA	FAT_COPIES
		MOV	B,A
		LHLD	SEC_PER_FAT
		XCHG
		LXI	H,0
INITFAT_C2_LP:	DAD	D
		DCR	B
		JNZ	INITFAT_C2_LP	;H = FAT_COPIES * SEC_PER_FAT
		XCHG			;D = H
		LHLD	FAT1START
		DAD	D		;DIR_SECTOR = FAT1START + FAT_COPIES * SEC_PER_FAT
		SHLD	DIR_SECTOR
		LHLD	FAT1START+2
		JNC	INITFAT_C2_DONE
		INX	H
INITFAT_C2_DONE:	SHLD	DIR_SECTOR+2

;DATASTART = DIR_SECTOR + LEN(Directory)
;          = DIR_SECTOR + ROOTDIR_SIZE * 32 / BYTE_P_SEC
		MVI	B,16		;Maximum # of Reductions
		LHLD	BYTE_P_SEC	;To fit math into 16 bits, let's reduce "ROOTDIR_SIZE / BYTE_P_SEC"
		XCHG			;Divide each by 2 while dividable
		LHLD	ROOTDIR_SIZE	;H=ROOTDIR_SIZE, D=BYTE_P_SEC
INITFAT_C3_LP:	MOV	A,E
		RAR
		JC	INITFAT_C3_0	;If lsb of D is 1, no more Reduction possible
		MOV	A,L
		RAR
		JC	INITFAT_C3_0	;If lsb of H is 1, no more Reduction possible
		MOV	A,D
		RAR
		MOV	D,A
		MOV	A,E
		RAR
		MOV	E,A
		MOV	A,H
		RAR
		MOV	H,A
		MOV	A,L
		RAR
		MOV	L,A
		DCR	B
		JNZ	INITFAT_C3_LP
INITFAT_C3_ERR:	CALL 	PRINTI		;
		.ascii " Error DATASTART\000"
		RET
INITFAT_C3_0:	MVI	B,5		;5 shifts = Multiply 32
INITFAT_C3_LP2:	DAD	H		;Double H
		JC	INITFAT_C3_ERR
		DCR	B
		JNZ	INITFAT_C3_LP2

		MOV	A,E		;2'S Complement BYTE_P_SEC
		CMA
		MOV	C,A
		MOV	A,D
		CMA
		MOV	B,A
		INX	B
		LXI	D,0xFFFF	;Start with -1
INITFAT_C3_LP3:	DAD	B		;Divide by counting Subtractions
		INX	D
		JC	INITFAT_C3_LP3
		LHLD	DIR_SECTOR	;Add the Dword at DIR_SECTOR
		DAD	D
		SHLD	DATASTART
		LHLD	DIR_SECTOR+2
		JNC	INITFAT_C3_1
		INX	H
INITFAT_C3_1:	SHLD	DATASTART+2

		LDA	SEC_PER_CLUS	;Determine the best way to divide Sectors into cluster#
		DCR	A
		STA	MODMASK
		INR	A
		JZ	INITFAT_FAIL1
		LXI	B,0x0800
INITFAT_C4_LP:	RAR
		JNC	INITFAT_C4_1
		MOV	D,B		;Save location of "1" bit
		INR	C		;Count of 1 bits.
INITFAT_C4_1:	DCR	B
		JNZ	INITFAT_C4_LP
		MVI	A,1
		CMP	C
		JNZ	INITFAT_C4_2	;More than 1 "1" bit, cannot do divide by simple shift.
		MOV	A,D		;Fetch position of the 1 bit.  8=lsb, 1=msb
		CMA
		ADI	10		;Re-adjust to make 1=lsb AND 8=msb  A=9-A
		STA	DF_SHIFTCNT
		LXI	H,DIVBYSHIFT	;Use fast shift divider
		LXI	D,MODBYMASK
		JMP	INITFAT_C4_3

INITFAT_C4_2:	LXI	H,DIV16BY8SPC	;Use Full Divide function for Sectors Per Cluster
		PUSH	H
		POP	D
INITFAT_C4_3:	SHLD	DIVIDE_FUNC
		XCHG
		SHLD	MOD_FUNC

		CALL 	PRINTI		;
		.ascii " VOL=\000"
		LXI	H,0x0FE2B
		MVI	B,11
		CALL	PRINTB
		CALL 	PRINTI		;
		.ascii " SYS=\000"
		MVI	B,8
		CALL	PRINTB
		RET

INITFAT_FAIL1:	CALL 	PRINTI		;
		.ascii "\r\nError=0 Sec/Clus\000"
INITFAT_FAIL:	CALL 	PRINTI		;
		.ascii "\r\nFAT Init FAILED\000"
		RET

;SD_CARD_TYPE	.blkb	1	;SD CARD TYPE
;SDC_STATUS	.blkb	1	;SD Status Code returned
;SD_PARAM	.blkb	4	;32 bit address parameter for SD Commands
;SD_PART_TYPE	.blkb	1	;SD PARTITION TYPE
;SD_PART_BASE	.blkb	4	;SD PARTITION STARTING RECORD
;SD_PART_SIZE	.blkb	4	;SD PARTITION SIZE (Must follow SD_PART_BASE)
;SEC_PER_CLUS	.blkb	1	;0x0D
;RESERVED_SEC	.blkb	2	;0x0E - 0x0F
;FAT_COPIES	.blkb	1	;0x10
;RT_DIR_ENTRIES	.blkb	2	;0x11 - 0x12
;TOT_FILESYS_SEC.blkb	4	;0x13 - 0x14 or 0x20 - 0x23
;HIDDEN_SECTORS	.blkb	4	;0x1C - 0x1F
;SEC_PER_FAT	.blkb	2	;0x16 - 0x17
;FAT1START	.blkb	4	;Calculated
;DIR_SECTOR	.blkb	4	;Calculated
;DATASTART	.blkb	4	;Calculated

;-------------------------------------------------
TEST_SIGNATURE:	CALL 	PRINTI		;
		.ascii " S\000"
		DCX	H
		MVI	A,0xAA
		CMP	M
		JNZ	INITFAT_FAIL
		DCX	H
		MVI	A,0x55
		CMP	M
		JNZ	INITFAT_FAIL
		RET


;=====================================================================================================
;SD Memory Car Routines, Mid Level, Send/Recieve Data Sectors (Writes out Dirty Data)
;=====================================================================================================

;-----------------------------------------------------------------------------------------------------
;Read to the SD_RAM_BUFFER from the SD Card at Sector BCDE
;-----------------------------------------------------------------------------------------------------
		;Sector in SEC_PTR
SD_READ_SEC:	LXI	H,SEC_PTR	;READ SECTOR
		JC	SD_RS_FORCED
		CALL	CMP_HL_32
		RZ			;Return if no change to sector being read/written

SD_RS_FORCED:
		;CALL	PRINTI		;DEBUG
		;.ascii " Read:\000"
		;CALL	PUT_BC
		;CALL	PUT_DE

		LXI	H,SEC_PTR
		CALL	MOV_HL_32	;Save Sector in SEC_PTR
		CALL	SET_PARAM	;READ SECTOR
		MVI	B,5	;5 Retries to read
SD_RS_LP0:	MVI	A,17 	;Read Sector Command
		CALL	SD_CMD
		JZ	SD_RS_0
		DCR	B
		JNZ	SD_RS_LP0
				;Read failed
		DCR	B	;Clear Zero flag
		CALL	SD_DESELECT	;Deselect card
		RET
SD_RS_0:	MVI	B,0		;256 Attempts to recieve the DATASTART
SD_RS_LP1:	CALL	SPI_RX
		CPI	0xFE		;IS DATASTART?
		JZ	SD_RS_1
		DCR	B
		JNZ	SD_RS_LP1
		CALL	SD_DESELECT	;Deselect card
		RET

SD_RS_1:	LXI	B,0x0200
SD_RS_LP2:	CALL	SPI_RX	;Fetch 512 Bytes to M(HL)
		MOV	M,A
		INX	H
		DCR	C
		JNZ	SD_RS_LP2
		DCR	B
		JNZ	SD_RS_LP2

		CALL	SPI_RX	;BURN 2 BYTES (CRC)
		CALL	SPI_RX	;
		CALL	SD_DESELECT	;Deselect card
		XRA	A
		RET

;-----------------------------------------------------------------------------------------------------
;Input:	Sector in 32 bit register BCDE
SET_PARAM:	LDA	SD_CARD_TYPE	;IF CARD_TYPE <> 3 THEN SHIFT SECTOR << 9 Bits
		CPI	3
		JZ	SP_RET

		MOV	A,C
		XCHG
		DAD	H
		RAL
		MOV	B,A
		MOV	C,H
		MOV	D,L
		MVI	E,0

SP_RET:		LXI	H,SD_PARAM
		CALL	MOV_HL_32	;Save Parameter
		LXI	H,SD_RAM_BUFFER	;Set buffer space
		RET


;=====================================================================================================
;SD Memory Car Routines, Low Level, INIT CARD, Send/Recieve Data, Send Commands
;=====================================================================================================
;-------------------------------- INIT SDCARD --------------------------------
INIT_SDCARD:	CALL	SD_DESELECT	;Deselect and clock the card many cycles
		MVI	C,0x80
		MVI	A,0xFF
		STA	SD_CARD_TYPE
ISD_0:		OUT	SPI		;CLOCK many cycles
		DCR	C
		JNZ	ISD_0
		CALL	SD_SELECT

		CALL 	PRINTI		;
		.ascii "\r\nInit SD\000"

		CALL	SD_CLEAR_ARG	;Fetch the 01 response
		MVI	B,0		;256 retries
ISD_LP1:		MVI	A,0		;CMD 0
		CALL	SD_CMD
		CPI	1		;Test 01 response
		JZ	ISD_1
		DCR	B
		JNZ	ISD_LP1
		;JMP	INIT_FAIL
INIT_FAIL:	CALL 	PRINTI		;
		.ascii "-FAILED\000"
		CALL	SD_DESELECT
		XRA	A		;Return Zero Flag cleared = Failure
		DCR	A
		RET

ISD_1:		CALL 	PRINTI		;
		.ascii " Type#\000"
		LXI	H,0x01AA		;Deterimine Card Type
		SHLD	SD_PARAM
		MVI	A,8		;CMD 8
		CALL	SD_CMD
		ANI	4
		JZ	ISD_2
		MVI	A,1		;If CMD8 is Illegal Cmd, CARD_TYPE=1
		STA	SD_CARD_TYPE
		JMP	ISD_3

ISD_2:		CALL	SPI_RX
		CALL	SPI_RX
		CALL	SPI_RX
		CALL	SPI_RX
		STA	SDC_STATUS
		CPI	0xAA
		MVI	A,0xAA		;Error code
		JNZ	INIT_FAIL
		MVI	A,2
		STA	SD_CARD_TYPE

ISD_3:		CALL	PUT_HEX
		CALL 	PRINTI		;
		.ascii " ACMD41\000"
		CALL	SD_CLEAR_ARG

		MVI	B,0
ISD_LP2:		MVI	A,55		;CMD 55 (ACMD)
		CALL	SD_CMD
		MVI	A,41		;CMD 41
		CALL	SD_CMD
		CPI	0
		JZ	ISD_4
		XRA	A		;256 ~= 2mSec Delay
		CALL	SD_DELAY
		DCR	B
		JNZ	ISD_LP2
		JMP	INIT_FAIL

ISD_4:		CALL 	PRINTI		;
		.ascii "+\000"
		LDA	SD_CARD_TYPE
		CPI	2
		JNZ	ISD_6
		MVI	A,58		;CMD 58
		CALL	SD_CMD
		CPI	0
		JNZ	INIT_FAIL
		CALL	SPI_RX
		ANI	0xC0
		CPI	0xC0
		JNZ	ISD_5
		MVI	A,3
		STA	SD_CARD_TYPE
		CALL 	PRINTI		;
		.ascii " Type#3\000"
ISD_5:		CALL	SPI_RX
		CALL	SPI_RX
		CALL	SPI_RX

ISD_6:		CALL	SD_DESELECT
		XRA	A		;Set Zero Flag = Success
		RET

;-----------------------------------------------------------------------------------------------------
SD_DESELECT:	PUSH	PSW
		MVI	A,1	;Deselect SDCARD
		OUT	SPI_SS
		POP	PSW
		RET

;-----------------------------------------------------------------------------------------------------
SD_SELECT:	PUSH	PSW
		MVI	A,0	;Select SDCARD
		OUT	SPI_SS
		CALL	SD_DELAY100
		POP	PSW
		RET

;-----------------------------------------------------------------------------------------------------
SD_DELAY100:	MVI	A,13	 ;Small delay after selecting card
SD_DELAY:	DCR	A	 ;5
		JNZ	SD_DELAY ;10    15*13 ~= 200 ~= 100uSec
		RET

;-----------------------------------------------------------------------------------------------------
;Send command to SD card
SD_CMD:		PUSH	B
		CALL	SD_SELECT
		CALL	WAIT_NOT_BUSY

		MVI	B,0xFF	;Default CRC
		CPI	0
		JNZ	SDC_1
		MVI	B,0x95
SDC_1:		CPI	8
		JNZ	SDC_2
		MVI	B,0x87
SDC_2:

		ORI	0x40	;All Commands start with 40h
		OUT	SPI
		LDA	SD_PARAM+3
		OUT	SPI
		LDA	SD_PARAM+2
		OUT	SPI
		LDA	SD_PARAM+1
		OUT	SPI
		LDA	SD_PARAM
		OUT	SPI
		NOP
		MOV	A,B
		OUT	SPI

		MVI	B,0
SDC_LP:		CALL	SPI_RX	;Read Respsonse?
		STA	SDC_STATUS
		ORA	A
		JP	SDC_RET
		DCR	B
		JNZ	SDC_LP
		ORA	A
SDC_RET:	POP	B
		RET

;-----------------------------------------------------------------------------------------------------
;------------------------------- Receive a byte from SPI
SPI_RX:		MVI	A,0xFF	;Read Respsonse
		OUT	SPI
		NOP		;4
		IN	SPI	;10
		RET

;-----------------------------------------------------------------------------------------------------
;------------------------------- Wait until FF's come back from Card (ie NOT BUSY)
WAIT_NOT_BUSY:	PUSH	PSW	;Do not destroy Acc
		PUSH	B	;Fetch 1 consecutive FF's to be sure SD card NOT BUSY
		MVI	B,0
WNB_LP:		MVI	C,1	;Set count for 1 trys
WNB_LP2:	CALL	SPI_RX
		INR	A
		JNZ	WNB_0	;NOT FF RETURNED, JUMP TO COUNT DOWN TRYS
		DCR	C	;Count Down Consecutive FF's
		JNZ	WNB_LP2
		POP	B
		POP	PSW
		STC		;Return NOT BUSY (Clear Carry)
		CMC
		RET

WNB_0:		XRA	A
		CALL	SD_DELAY
		DCR	B	;Count Down Trys
		JNZ	WNB_LP
		POP	B
		POP	PSW
		STC		;Return STILL BUSY (Set Carry)
		RET

;-----------------------------------------------------------------------------------------------------
SD_CLEAR_ARG:	XRA	A
		STA	SD_PARAM
		STA	SD_PARAM+1
		STA	SD_PARAM+2
		STA	SD_PARAM+3
		RET


;=====================================================================================================
;General Support Routines, Strings
;=====================================================================================================

;-----------------------------------------------------------------------------------------------------
GET_STRING:	CALL	GET_CHAR
		CPI	27
		STC			;Set Carry to indicate Abort
		RZ
		CPI	13		;Exit on <CR>
		RZ
		CMP	C		;Exit on Selectable Char (dot for file input)
		RZ
		CALL	TO_UPPER
		CPI	" "+1		;Test if ACC is Control or Space
		JC	GET_STRING	;Skip such characters
		DCR	B
		INR	B		;Exit if B charcters are already inputed
		RZ			;Exit if no more characters allowed
		MOV	M,A
		INX	H
		DCR	B
		JMP	GET_STRING

;-----------------------------------------------------------------------------------------------------
TO_UPPER:	CPI	"a"
		RC		;Return if ACC < 'a'
		CPI	"z"+1
		RNC		;Return if ACC > 'z'
		ANI	0x5F	;Flag upper case
		RET


;-----------------------------------------------------------------------------------------------------
;	Prints Filename at HL
;-----------------------------------------------------------------------------------------------------
PRINT_FILENAME:	PUSH	H
		MVI	B,8
PF_LP1:		MOV	A,M		;PRINT 8 CHARS OR UP TO EITHER A NULL OR SPACE.
		ORA	A
		JZ	PFE
		CPI	" "
		JZ	PFE
		CALL	PUT_CHAR
		INX	H
		DCR	B
		JNZ	PF_LP1
PFE:		MVI	A,"."
		CALL	PUT_CHAR
		POP	H
		PUSH	H
		LXI	B,8
		DAD	B
		MVI	B,3
PF_LP2:		MOV	A,M
		ORA	A
		JZ	PF_RET
		CPI	" "
		JZ	PF_RET
		CALL	PUT_CHAR
		INX	H
		DCR	B
		JNZ	PF_LP2
PF_RET:		POP	H
		RET


;=====================================================================================================
;General Support Routines, 32 Bit stuff and other math
;=====================================================================================================

;------------------------- Move (HL) to 32 bit register BCDE
MOV_32_HL:	MOV	E,M
		INX	H
		MOV	D,M
		INX	H
		MOV	C,M
		INX	H
		MOV	B,M
		RET

;------------------------- Move 32 bit register BCDE to (HL)
MOV_HL_32:	MOV	M,E
		INX	H
		MOV	M,D
		INX	H
		MOV	M,C
		INX	H
		MOV	M,B
		RET

;------------------------- ADD (HL) to 32 bit register BCDE
ADD_32_HL:	MOV	A,E
		ADD	M
		MOV	E,A
		INX	H
		MOV	A,D
		ADC	M
		MOV	D,A
		INX	H
		MOV	A,C
		ADC	M
		MOV	C,A
		INX	H
		MOV	A,B
		ADC	M
		MOV	B,A
		RET

;-----------------------------------------------------------------------------------------------------
INC_32:		INX	D
		MOV	A,D
		ORA	E
		RNZ
		INX	B
		RET

;-----------------------------------------------------------------------------------------------------
DEC_32:		MOV	A,D
		ORA	E
		JNZ	DEC_32NOBORROW
		DCX	B
DEC_32NOBORROW:	DCX	D
		RET

;-----------------------------------------------------------------------------------------------------
TSTZ_32:		MOV	A,D
		ORA	E
		RNZ
		ORA	C
		RNZ
		ORA	B
		RET




;-----------------------------------------------------------------------------------------------------
;Compare BCDE with 32bit word at HL
CMP_HL_32:	INX	H		;Point to MSB
		INX	H
		INX	H
		MOV	A,B		;Compare with B
		CMP	M
		JNZ	CH3_R1
		DCX	H
		MOV	A,C
		CMP	M
		JNZ	CH3_R2
		DCX	H
		MOV	A,D
		CMP	M
		JNZ	CH3_R3
		DCX	H
		MOV	A,E
		CMP	M
		RET
CH3_R1:		DCX	H
CH3_R2:		DCX	H
CH3_R3:		DCX	H
		RET

;------------------------- COMPARE DE WITH HL
CMP_DE_HL:	MOV	A,D		;Compare the MSB first
		CMP	H
		RNZ
		MOV	A,E
		CMP	L
		RET

;-----------------------------------------------------------------------------------------------------
COPY_RAM2:	MVI	B,2	;2 BYTES
		JMP	COPY_RAM
COPY_RAM4:	MVI	B,4	;4 BYTES PER WORD
COPY_RAM:	MOV	A,M
		STAX	D
		INX	H
		INX	D
		DCR	B
		JNZ	COPY_RAM
		RET

;-----------------------------------------------------------------------------------------------------
		;HL = (HL) word at memory location HL
LD_HL_HL:	MOV	A,M		;Fetch L from (HL)
		INX	H
		MOV	H,M		;Fetch H from (HL+1)
		MOV	L,A
		RET

;-----------------------------------------------------------------------------------------------------
;	FILL_BLOCK, Fills a block of RAM with value in A
;	Input:	A = value
;		HL = Start Address
;		B = Length of Fill (MAX = 0 = 256 bytes)
;-----------------------------------------------------------------------------------------------------
FILL_BLOCK:	PUSH	PSW
		PUSH	B
		PUSH	H
FB_LP:		MOV	M,A
		INX	H
		DCR	B
		JNZ	FB_LP
		POP	H
		POP	B
		POP	PSW
		RET

;-----------------------------------------------------------------------------------------------------
VCALL:		PCHL		;Jump to HL

;-----------------------------------------------------------------------------------------------------
;Maximum number to divide is Logical Sector 2001/4 = 500
;If dividing by powers of 2, then we can shift the number for fast divide
DIV16BY8SPC:	LDA	SEC_PER_CLUS
;Input:	DE=Dividend, A=Divisor
;Out:	DE=Result, A=Remainder
DIV16BY8:	XCHG		; HL = Dividend
		MVI	E,00	; Quotient = 0
		;LHLD	2200H	; Get Dividend
		;LDA	2300	; Get Divisor
		MOV	B, A	; Store        Divisor
		MVI	C, 08	; Count = 8
DIV16BY8_LP:	DAD H		; Dividend = Dividend x 2
		MOV	A, E
		RLC
		MOV	E, A	; Quotient = Quotient x 2
		MOV	A, H
		SUB	B	; Is most significant byte of Dividend > divisor
		JC	DIV16BY8_SK	; No, go to Next step
		MOV	H, A	; Yes, subtract divisor
		INR	E	; and Quotient = Quotient + 1
DIV16BY8_SK:	DCR	C	; Count = Count - 1
		JNZ	DIV16BY8_LP ; Is count =0 repeat
		;MOV	A, E
		;STA	2401H	; Store Quotient
		MOV	A, H
		;STA	2410H	; Store remainder
		MVI	D,0	; Quotient in DE
		RET

;-----------------------------------------------------------------------------------------------------
DIVBYSHIFT:	LDA	DF_SHIFTCNT	; DE = Dividend
		MOV	B,A
DBS_LP:		DCR	B
		RZ
		ORA	A	;Clear Carry
		MOV	A,D
		RAR
		MOV	D,A
		MOV	A,E
		RAR
		MOV	E,A
		JMP	DBS_LP

;-----------------------------------------------------------------------------------------------------
MODBYMASK:	LDA	MODMASK
		ANA	E
		RET




;----------------------------------------------------------------------------------------------------; RAM SPACE
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;----------------------------------------------------------------------------------------------------; RAM SPACE
		.ORG	0xFD00
;		.DSEG
HIGHSTACK:			;Stack starts saveing at FCFFh and grows downward

;SDFCB:
;FSTAT		.blkb	1	;+0  Status of FCB, 00=File Not Open
;FNAME		.blkb	11	;+1
;AFClus0	.blkb	2	;+12 First Cluster of File as given by the Directory Entry.
;CRFClus	.blkb	2	;+14 Current Relative Cluster location in file, (0 to F for a system with 32 Sectors per Cluster)
;CAFClus	.blkb	2	;+16 Current Absolute Cluster location in file, set to AFClus0, then updated with FAT
;RFSec		.blkb	2	;+18 Relative Sector being addressed (0 to 500, based 26 sectors per track and 77 tracks Divide by 4)
;SSOC		.blkb	4	;+20 Starting Sector of Cluster, this is the first Sector for that Cluster
;ABS_SEC	.blkb	4	;+24 Absolute Sector of Current Relative Sector
;RESV		.blkb	4	;Filler for 32 bytes
SDISKA:		.blkb	32	;File Control Block
FSIZE:		.blkb	4	;File Size

GH_VIEW:	.blkb	1	;View File Load

; SPECIAL FLAGS.

CLEAR_RAM	.EQU	.	;Clear all RAM after this point on INIT

INB_PTR:	.blkb	2	;Pointer to In Byte
GH_STATE:	.blkb	2	;Pointer to Get HEX State
GH_ERR_CNT:	.blkb	1	;Count of ERRORS
GH_STATE2:	.blkb	2	;Pointer to Get HEX State
GH_BYTE:	.blkb	1	;Temp to hold MSD
GH_CHKSUM:	.blkb	1	;HEX File, record Checksum
GH_COUNT:	.blkb	1	;HEX File, record data count
GH_ADDR:	.blkb	2	;HEX File, record address
GH_TYPE:	.blkb	1	;HEX File, record type
GH_START:	.blkb	2	;HEX File, start address
GH_VALID:	.blkb	1	;HEX File, start address valid (set)
PC_POS:		.blkb	1

DIVIDE_FUNC:	.blkb	2	;Pointer to the Divide Function
DF_SHIFTCNT:	.blkb	1	;Count of shifts required for Fast Divide
MUL8:		.blkb	1	;8 bit multiplier
M_COUNTER:	.blkb	1	;8 bit counter for multiply routine
MOD_FUNC:	.blkb	2	;Pointer to the Mod Function
MODMASK:	.blkb	1	;8 bit mask to get Relative Sector within a cluster from a Relative File sector

LOGICAL_SEC:	.blkb	2	;Logical Sector for next Read/Write Operation

SD_CARD_TYPE:	.blkb	1	;SD CARD TYPE
SDC_STATUS:	.blkb	1	;SD Status Code returned
SD_PARAM:	.blkb	4	;32 bit address parameter for SD Commands
SD_PART_TYPE:	.blkb	1	;SD PARTITION TYPE
SD_PART_BASE:	.blkb	4	;SD PARTITION STARTING RECORD
SD_PART_SIZE:	.blkb	4	;SD PARTITION SIZE (Must follow SD_PART_BASE)
BYTE_P_SEC:	.blkb	2	;0x0B Bytes per Sector (Almost always 512)
SEC_PER_CLUS:	.blkb	1	;0x0D
RESERVED_SEC:	.blkb	2	;0x0E - 0x0F
FAT_COPIES:	.blkb	1	;0x10
ROOTDIR_SIZE:	.blkb	2	;0x11 - 0x12
FILESYS_SEC:	.blkb	4	;0x13 - 0x14 or 0x20 - 0x23
HIDDEN_SEC:	.blkb	4	;0x1C - 0x1F
SEC_PER_FAT:	.blkb	2	;0x16 - 0x17
FAT1START:	.blkb	4	;Calculated Sector to FAT1
DIR_SECTOR:	.blkb	4	;Calculated Sector to Root Directory
DATASTART:	.blkb	4	;Calculated Sector to Data Area
FILENAME:	.blkb	8	;File Name
FILEEXT:	.blkb	3	;File Extension
SEC_PTR:	.blkb	4	;Sector Pointer, general use variable that holds the last sector read
DIRTY_DATA:	.blkb	1	;Indicates when data Read has been altered, ie. Requires flushing back to SD Card
ENT_COUNT:	.blkb	2	;Directory Entry Counter, Counts down maximum directory entries in Find File




		.ORG	0xFE00
SD_RAM_BUFFER:	.blkb	512		;512 BYTE SD CARD BUFFER


		.end



;----------------------------------------------------------------------------------------------------; INSTRUCTION LIST REFERENCE
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;----------------------------------------------------------------------------------------------------;

;DATA TRANSFER GROUP
;'Mnem.     'Description                 'Notes                '
;'----------+----------------------------+---------------------'
; MOV r1,r2 | r1 <- r2                   |r = A,B,C,D,E,H,L
; MOV r,M   | r <- (HL)
; MOV M,r   | (HL) <- r
; HLT       | HALT
; MVI r,d   | r <- d, Move Immediate data
; MVI M     | (HL) <- d, Immediate data
; INR r     | r <- r+1
; DCR r     | r <- r-1
; INR M     | (HL) <- (HL)+1
; DCR M     | (HL) <- (HL)-1
; ADD r     | A <- A+r
; ADC r     | A <- A+r+CY                |Add with Carry
; SUB r     | A <- A-r
; SBB r     | A <- A-r-CY                |Subtract with Borrow
; ANA r     | A <- A AND r
; XRA r     | A <- A XOR r
; ORA r     | A <- A OR r
; CMP r     | A-r                        |Compare
; ADD M     | A <- A+(HL)
; ADC M     |
; SUB M     |
; SBB M     |
; ANA M     |
; XRA M     |
; ORA M     |
; CMP M     |
; ADI d     | A <- A+d, ADD Immediate data
; ACI d     |
; SUI d     |
; SBI d     |
; ANI d     |
; XRI d     |
; ORI d     |
; CPI d     |
; RLC       | Rotate A Left, CY<-MSB   Only Carry Affected
; RRC       | Rotate A Right, CY<-LSB  Only Carry Affected
; RAL       | Rotate A Left through Carry  Only Carry Affected
; RAR       | Rotate A Right through Carry Only Carry Affected
; JMP addr  | Jump Address
; JC  addr  | Jump on Carry
; JNC addr  | Jump on NOT Cary
; JZ  addr  | Jump on ZERO
; JNZ addr  | Jump on NOT ZERO
; JP  addr  | Jump on Positive (MSB=0)
; JM  addr  | Jump on Minus (MSB=1)
; JPE addr  | Jump on Parity Even (Parity bit =1)
; JPO addr  | Jump on Parity Odd (Parity bit =0)
; CALL addr | Call subroutine
; CC  addr
; CNC addr
; CZ  addr
; CNZ addr
; CP  addr
; CM  addr
; CPE addr
; CPO addr
; RET       | Return from subroutine
; RC
; RNC
; RZ
; RNZ
; RP
; RM
; RPE
; RPO
; RST n     | Restart to Vector n        | n=0,1,2,3,4,5,6,7
; IN  p     | A <- Port p, Input
; OUT p     | Port p <- A, Output
; LXI B,dd  | BC <- dd, Load Immediate data, 16 bit to Register Pair
; LXI D,dd  | DE <- dd
; LXI H,dd  | HL <- dd
; LXI SP,dd | SP <- dd
; PUSH B    | PUSH BC register pair to STACK
; PUSH D    | PUSH DE register pair to STACK
; PUSH H    | PUSH HL register pair to STACK
; PUSH PSW  | PUSH A,Flags register pair to STACK
; POP B     | POP BC register pair from STACK
; POP D     | POP DE register pair from STACK
; POP H     | POP HL register pair from STACK
; POP PSW   | POP A,Flags register pair from STACK
; STA addr  | (addr) <- A, Store A Direct
; LDA addr  | A <- (addr), Load A Direct
; XCHG      | Exchange HL <> DE
; XTHL      | Exchange HL <> (SP), Exchange HL with Top of Stack
; SPHL      | SP <- HL, Move HL to SP
; PCHL      | PC <- HL, Move HL to PC
; DAD B     | HL <- HL+BC, Add 16 bit register pairs
; DAD D     | HL <- HL+DE, Add 16 bit register pairs
; DAD H     | HL <- HL+HL, Add 16 bit register pairs
; DAD SP    | HL <- HL+SP, Add 16 bit register pairs
; STAX B    | (BC) <- A, Store A Indirect
; STAX D    | (DE) <- A, Store A Indirect
; LDAX B    | A <- (BC), Load A Indirect
; LDAX D    | A <- (DE), Load A Indirect
; INX B     | BC <- BC+1, Increment 16 bit register pair
; INX D     | DE <- DE+1, Increment 16 bit register pair
; INX H     | HL <- HL+1, Increment 16 bit register pair
; INX SP    | SP <- SP+1, Increment 16 bit register
; DCX B     | BC <- BC-1, Decrement 16 bit register pair
; DCX D     | DE <- DE-1, Decrement 16 bit register pair
; DCX H     | HL <- HL-1, Decrement 16 bit register pair
; DCX SP    | SP <- SP-1, Decrement 16 bit register
; CMA       | A <- /A, Complement Accumulator
; STC       | Set Carry
; CMC       | Complement Carry
; DAA       | Decimal Adjust Accumulator
; SHLD addr | (addr) <- HL, Store HL Direct
; LHLD addr | HL <- (addr), Load HL Direct
; EI        | Enable Interrupts
; DI        | Disable Interrupts
; NOP       | No Op
