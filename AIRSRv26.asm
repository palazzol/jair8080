;Replacement 8080A cpu board for ALTAIR/IMSAI
;Monitor program for 8080A based computer.
;Written by Josh Bensadon
;Free for public use, following all the shareware policies & disclaimers.
;
;CONSOLE BIOS is near end of file, can be modified to talk to any UART
;FLOPPY DISK BIOS is also near end of file
;
;The boot loader in the EPROM will load this file automatically if this file
;is renamed to BIOS.HEX.  The batch file to assemble this file will do this.
;
;
;Version 1.0 - Josh Bensadon.  Reused all code from previous Monitor program ver 1.7
;1.1	-JB. Corrected problem with BIOS DISK read/write by changing to my own temp stack
;	-Corrected hidden problem with Drive Parameter Table
;1.2	-Set Stack to high RAM
;	-Corrected problem with Sectors starting at 0
;2.0	-Created a more elaborate Boot Loader to be installed in EPROM.
;	-This System ROM can now be loaded via the BIOS.HEX file on the SD Card
;2.1	-Fixed an issue with output to both Propeller and UART Com-port
;2.2	-Dropped UART Com-Port to observe a 33% console speed increase
;2.3	-Resuming from v2.1 (dual support).  Corrected firmware for configuration
;	-when Propeller I/O board is NOT present.
;2.4	-Added Config File to select the default disk files and DPB settins for various sized disk images
;2.5
;2.6	-Made Track handle 16 bit
;
;


;Monitor for the IMSAI 8080
;Functions:
; -Dump, Edit & Execute Memory.
; -Input Port and Output Port.
; -RAM Test
; -ASCII Upload text file
; -XMODEM up/down load to Memory
; -XMODEM up/down load to Disk
;
; AA8080.ASM works with the Standard Console port of the Altair (Port 0=Status, Port 1=RX/TX)
; Tested on the Solid State Music Computer Systems IO4 Serial/Parallel interface board installed on an IMSAI 8080
; Floppy functions work with the Tarbell 1101 (using the Western Digital FD1771 Floppy Disk Controller chip).
;
; D XXXX YYYY	Dump memory from XXXX to YYYY
; E XXXX	Edit memory starting at XXXX (type an X and press enter to exit entry)
; G XXXX	GO starting at address XXXX (Monitor program address left on stack)
; I XX		Input from I/O port XX and display as hex
; O XX YY	Output to I/O port XX byte YY
; X U XXXX	XMODEM Upload to memory at XXXX (CRC or CHECKSUM)
; X D XXXX CCCC	XMODEM Download from memory at XXXX for CCCC number of 128 byte blocks
; :ssHHLLttDDDDDD...CS   -ASCII UPLOAD Intel HEX file to Memory.  Monitor auto downloads with the reception of a colon.
; F 		Floppy commands
; R XX YY	RAM TEST from pages XX to YY

;
; Search for BIOS sections to adapt to other systems



        .area   CODE1   (ABS)   ; ASXXXX directive, absolute addressing

;----------------------------; IMSAI CONSOLE PORTS
FPLED		.EQU 255	;Front Panel LED
FPSW		.EQU 255	;Front Panel Switches
;----------------------------;

PROPELLERS	.EQU 0		;Propeller Console Status Port
PROPELLERD	.EQU 1		;Propeller Console Data Port

CPU_IO		.EQU 0x20	;Base address for onboard CPU I/O
UART0		.EQU CPU_IO
UART1		.EQU CPU_IO+0x8
SPI		.EQU CPU_IO+0x10
SPI_SS		.EQU SPI+1
PORT2		.EQU CPU_IO+0x18

DEBUG		.EQU 0b00000001
		;1 = CP/M BIOS
		;2 = LOW level SD Card
		;0x80 = UART1


CPM_DSK_BUFF	.EQU 0x80	;Default CPM Buffer of 128 Bytes

		.ORG 0xDB00
CODE_START:	LXI	SP,HIGHSTACK ;128 Bytes of stack available.
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

		MVI	A,0xFF
		STA	PROP_CHECK

		CALL	PRINTI		;System Start, Display Welcome Message
		.ascii "\r\nALTAIR/IMSAI 8080 CPU BOARD - Josh Bensadon v2.6 (Oct 2016)\r\n\000"

		CALL	INIT_FAT
		CALL	PRINTI
		.ascii "\r\n\000"


		LXI	H,0
		SHLD	LOGICAL_SEC

		LXI	H,FCB_CONFIG
		SHLD	FCB_PTR
		CALL	SD_OPEN
		JNZ	FILEOK
GO_HALT:		CALL	PRINTI
		.ascii " - HALT\000"
1$:		HLT
		JMP	1$

INPUT_NAME:	MVI	B,12
		CALL	COPY_RAM
		INX	H
		INX	H
		RET

FILEOK:		LHLD	FILESIZE	;Save file size of Config File
		SHLD	CONFIGSIZE
		CALL	DISK_READ	;HL=Buffer

		LXI	D,SDISKA	;Preload all SD_FCB's with file names for 4 disks
		CALL	INPUT_NAME
		LXI	D,SDISKB
		CALL	INPUT_NAME
		LXI	D,SDISKC
		CALL	INPUT_NAME
		LXI	D,SDISKD
		CALL	INPUT_NAME


BOOT_MENU:	CALL	PRINTI		;BOOT Menu
		.ascii "\r\n\r\nM - Monitor"
		.ascii "\r\nC - Boot CP/M\000"

		XRA	A		;Loop through the 4 SD_FCB's and try to open/init the FCB's
BMD_LP:		PUSH	PSW
		MOV	D,A		;D=A=File (0 to 3) (D for printing)
		CALL	SET_FCB_PTR
		LXI	B,10
		DAD	B
		CALL	LD_HL_HL
		SHLD	DPBPTR		;Save the DPB location
		CALL	PRINTI
		.ascii "\r\n  \000"
		MOV	A,D
		INR	A
		CALL	PUT_HEX
		CALL	PRINTI
		.ascii " - Disk \000"
		MOV	A,D
		ADI	"A"
		CALL	PUT_CHAR
		CALL	PRINTI
		.ascii " = \000"
		CALL	SD_OPEN

		LXI	H,FILESIZE+3		;Convert the 4 byte file size into 8 byte ASCII
		LXI	D,FILESIZEHEX+3
		MVI	B,4			;4 bytes
FS2ASC_LP:	MOV	A,M
		CALL	BIN2HEX
		STAX	D
		MOV	A,M
		RRC
		RRC
		RRC
		RRC
		CALL	BIN2HEX
		DCX	D
		STAX	D
		DCX	D
		DCX	H
		DCR	B
		JNZ	FS2ASC_LP


		LXI	H,0		;Read Config file to find paramaters for a file of this size
		SHLD	LOGICAL_SEC
		LXI	H,FCB_CONFIG
		SHLD	FCB_PTR

					;Get Line
		LXI	B,LINE_BUFF
		LHLD	CONFIGSIZE
		SHLD	CONFIGSIZECNT
		XCHG			;DE = CONFIG FILE SIZE

GL_LP1:		PUSH	B
		PUSH	D
		CALL	DISK_READ	;HL=BUFF
		POP	D		;DE=Count of Remaining Bytes in file
		POP	B		;BC=LINE_BUF PTR

GL_LP2:		MOV	A,M		;Get a char
		CPI	" "
		JC	GL_CTRL		;Jump out when a control char is found
		STAX	B		;Save char to Line Buff

		MVI	A, <(LINE_BUFFEND) ;If BC <> End of Line Buffer, then BC = BC + 1
		CMP	C
		JZ	GL_BF
		INX	B
GL_BF:		INX	H
		DCX	D
		MOV	A,D
		ORA	E
		JZ	GL_EOF

		XRA	A
		CMP	H		;IF HL <> End of Disk Buffer, then Loop back for next char
		JNZ	GL_LP2

		LDA	LOGICAL_SEC	;Advance to next sector (4x128 = 512)
		ADI	4
		STA	LOGICAL_SEC
		JMP	GL_LP1

GL_CTRL:		XRA	A		;Save 000 at end of line
		STAX	B
		PUSH	H

		LXI	B,FILESIZE
		LXI	H,LINE_BUFF
GLT_LP:		LDAX	B
		CMP	M
		JNZ	GLT_NOTEQ
		INX	B
		INX	H
		MVI	A, <(LINE_BUFF+8)
		CMP	L
		JNZ	GLT_LP
					;HURRAY, WE HAVE FOUND THE RIGHT DISK SIZE
					;COPY DPB, PRINT MSG
		INX	H
		XCHG
		LHLD	DPBPTR
		MVI	B,15
GTL_COPY_LP:	CALL	GTL_GETHEX
		RLC
		RLC
		RLC
		RLC
		MOV	M,A
		CALL	GTL_GETHEX
		ORA	M
		MOV	M,A
		INX	H

		DCR	B
		JNZ	GTL_COPY_LP
		XCHG
		CALL	PRINT		;Print rest of line.
		POP	H
		JMP	BMD_NEXT

GTL_GETHEX:	LDAX	D
		INX	D
		CALL	ASC2HEX
		RNC
		CALL	PRINTI
		.ascii " BAD HEX\000"
		JMP	GO_HALT


GLT_NOTEQ:	POP	H
		LXI	B,LINE_BUFF
		JMP	GL_BF

GL_EOF:		CALL	PRINTI
		.ascii " - DISK SIZE NOT LISTED\000"

BMD_NEXT:	POP	PSW
		INR	A
		CPI	4
		JNZ	BMD_LP

		LXI	H,CPM_DSK_BUFF		;Default CPM DMAADD
		SHLD	DMAADD
		LXI	H,SDISKA		;Default FCB_PTR for Disk-A
		SHLD	FCB_PTR

 		CALL	PRINTI		;BOOT Menu
		.ascii "\r\n>\000"

		MVI	A,0xFF
		STA	ECHO_ON		;TURN ON ECHO
		CALL 	GET_CHAR	;get char
		CALL	TO_UPPER
		CPI 	"C"		;Branch to Command entered
		JZ 	CBOOTV		; C = BOOT CP/M
		CPI 	"M"		;Branch to Command entered
		JZ 	MAIN_MENU	; C = BOOT CP/M
		CPI	"1"
		JC	BOOT_MENU
		CPI	"5"
		JNC	BOOT_MENU
		DCR	A
		CALL	SET_FCB_PTR
		CALL	INPUT_FNAME
		LHLD	FCB_PTR
		INX	H		;FNAME
		XCHG
		LXI	H,FILENAME
		MVI	B,11
		CALL	COPY_RAM
		JMP	BOOT_MENU

;------------------------
INIT_BLOCK:	XTHL	;HL = Top of Stack
		MOV	E,M	;Fetch address of SD_FCB to DE
		INX	H
		MOV	D,M
		INX	H
IB_LP:		MOV	A,M	;Copy all data up to 0x00 to SD_FCB
		INX	H	;Inc HL, so on finding 0x00, exit to next instruction
		ORA	A
		JZ	IB_RET
		STAX	D
		INX	D
		JMP	IB_LP
IB_RET:		XTHL		;Move updated return address back to stack
		RET

;LOCAL RAM SPACE

FCB_CONFIG:	.DB	0	;FSTAT		.blkb	1	;+0  Status of FCB, 00=File Not Open
		.ascii "CPMDISKSTXT"		;+1
FCB_CONFIG_C1:	.blkb	2	;AFClus0	;+12 First Cluster of File as given by the Directory Entry.
		.DW	0xFFFF	;CRFClus	;+14 Current Relative Cluster location in file, (0 to F for a system with 32 Sectors per Cluster)
		.DW	0xFFFF	;CAFClus	;+16 Current Absolute Cluster location in file, set to AFClus0, then updated with FAT
		.DW	0xFFFF	;RFSec		;+18 Relative Sector being addressed (0 to 500, based 26 sectors per track and 77 tracks Divide by 4)
		.DW	0xFFFF	;SSOC		;+20 Starting Sector of Cluster, this is the first Sector for that Cluster
		.DW	0xFFFF
		.DW	0xFFFF	;ABS_SEC	;+24 Absolute Sector of Current Relative Sector
		.DW	0xFFFF

CONFIGSIZE:	.DW	0			;Size of Config File
CONFIGSIZECNT:	.DW	0
CONFIGPTR:	.DW	0
DPBPTR:		.DW	DPB0

LINE_BUFF:	.blkb 128
LINE_BUFFEND:	.DB	0



;----------------------------------------------------------------------------------------------------; MONITOR MAIN MENU
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
;----------------------------------------------------------------------------------------------------; MAIN MENU
MAIN_MENU:	LXI	H, MAIN_MENU	;Push Mainmenu onto stack as default return address
		PUSH	H
		CALL	PRINTI		;Monitor Start, Display Welcome Message
		.ascii "\r\nMENU>\000"
		MVI	A,0xFF
		STA	ECHO_ON		;TURN ON ECHO
		CALL 	GET_CHAR	;get char
		CPI	":"
		JZ 	GETHEXFILE	; : = START HEX FILE LOAD
		ANI 	0x5F		;to upper case
		CPI 	"D"		;Branch to Command entered
		JZ 	MEM_DUMP	; D = Memory Dump
		CPI 	"E"
		JZ 	MEM_EDIT	; E = Edit Memory
		CPI 	"G"
		JZ 	MEM_EXEC	; G = Go (Execute at)
		CPI 	"O"
		JZ 	PORT_OUT	; O = Output to port
		CPI 	"I"
		JZ 	PORT_INP	; I = Input from Port
		CPI 	"S"
		JZ 	SD_MENU		; S = SD Card Functions
		CPI 	"X"
		JZ 	XMODEM		; X = XMODEM
		CPI 	"R"
		JZ	RAM_TEST	; R = RAM TEST
		CPI 	"U"
		JZ	MEM_UNASM	; U = UNASSEMBLE
		CALL 	PRINTI		;Display Err when input is invalid
		.ascii "\r\nINVALID CMD\000"

		CALL 	PRINTI		;Display Err when input is invalid
		.ascii "\r\nHELP"
		.ascii "\r\nD -Dump"
		.ascii "\r\nE -Edit"
		.ascii "\r\nG -Go (Exec)"
		.ascii "\r\nO -Output to port"
		.ascii "\r\nI -Input to port"
		.ascii "\r\nS -SD Card Functions"
		.ascii "\r\nX -XModem Up/Down Load"
		.ascii "\r\nR -RAM TEST"
		.ascii "\r\nU -Unassemble"
		.ascii "\r\n>\000"
		JMP 	MAIN_MENU


;=============================================================================
;MEMORY UNASSEMBLE
;-----------------------------------------------------------------------------
MEM_UNASM:	CALL	SPACE_GET_WORD	;Input start address
		XCHG			;HL = Start
		CALL	PUT_NEW_LINE
		XRA	A
		STA	ECHO_ON		;TURN OFF ECHO
MU_LP1:		MVI	B,10
MU_LP2:		PUSH	B
		CALL	DISASM
		POP	B
		DCR	B
		JNZ	MU_LP2
		CALL	GET_CHAR
		CPI	27
		JNZ	MU_LP1
		RET

;=============================================================================
;MEMORY DUMP
;-----------------------------------------------------------------------------
MEM_DUMP:	CALL	SPACE_GET_WORD	;Input start address
		XCHG			;HL = Start
		CALL	SPACE_GET_WORD	;Input end address (DE = end)

MEM_DUMP_LP:	CALL	PUT_NEW_LINE
		CALL	DUMP_LINE	;Dump 16 byte lines (advances HL)
		RZ			;RETURN WHEN HL=DE
		MOV	A,L
		ORA	A
		JNZ	MEM_DUMP_LP	;Dump 1 Page, then prompt for continue
		CALL	GET_CONTINUE
		JMP	MEM_DUMP_LP


GET_CONTINUE:	CALL	PUT_NEW_LINE
		CALL	PRINTI
		.ascii "Press any key to continue\000"
		CALL	GET_CHAR
		CPI	27
		RNZ
		POP	H		;Scrap return address
		RET


;-----------------------------------------------------------------------------
;DUMP_LINE -- Dumps a line
;xxx0:  <pre spaces> XX XX XX XX XX After spaces | ....ASCII....
;-----------------------------------------------------------------------------
DUMP_LINE:	PUSH	B		;+1
		PUSH	H		;+2 Save H for 2nd part of display
		PUSH	H		;+3 Start line with xxx0 address
		MOV	A,L
		ANI	0xF0		;Mask FFF0
		MOV	L,A
		CALL	PUT_HL		;Print Address
		CALL	PRINTI
		.ascii ": \000"
		POP	H		;-3
		MOV	A,L
		ANI	0x0F		;Fetch how many prespaces to print
		MOV	C,A
		MOV	B,A		;Save count of prespaces for part 2 of display
		CALL	PUT_3C_SPACES

DL_P1L:		CALL	PUT_SPACE
		MOV	A,M
		CALL	PUT_BYTE
		CALL	CMP_HL_DE
		JZ	DL_P1E
		INX	H
		MOV	A,L
		ANI	0x0F
		JNZ	DL_P1L
		JMP	DL_P2

DL_P1E:		MOV	A,L
		CMA
		ANI	0x0F
		MOV	C,A
		CALL	PUT_3C_SPACES

DL_P2:		CALL	PRINTI		;Print Seperator between part 1 and part 2
		.ascii " | \000"

DL_PSL2:		MOV	A,B		;Print prespaces for part 2
		ORA	A
		JZ	DL_PSE2
		CALL	PUT_SPACE
		DCR	B
		JMP	DL_PSL2
DL_PSE2:
		POP	H		;-2
		POP	B		;-1
DL_P2L:		MOV	A,M
		CPI	" "		;A - 0x20	Test for Valid ASCII characters
		JP	DL_P2K1
		MVI	A,"."				;Replace with . if not ASCII
DL_P2K1:		CPI	0x7F		;A - 07Fh
		JM	DL_P2K2
		MVI	A,"."
DL_P2K2:		CALL	PUT_CHAR

		CALL	CMP_HL_DE
		RZ
		INX	H
		MOV	A,L
		ANI	0x0F
		JNZ	DL_P2L

;-----------------------------------------------------------------------------
;Compare HL with DE
;Exit:		Z=1 if HL=DE
;		M=1 if DE > HL
CMP_HL_DE:	MOV	A,H
		CMP	D		;H-D
		RNZ			;M flag set if D > H
		MOV	A,L
		CMP	E		;L-E
		RET


PUT_3C_SPACES:	MOV	A,C		;Print 3C Spaces
		ORA	A
		RZ
		DCR	C		;Count down Prespaces
		CALL	PRINTI		;Print pre spaces
		.ascii "   \000"
		JMP	PUT_3C_SPACES


;-----------------------------------------------------------------------------
;EDIT MEMORY
;Edit memory from a starting address until X is pressed.
;Display mem loc, contents, and results of write.
;-----------------------------------------------------------------------------
MEM_EDIT:	CALL	SPACE_GET_WORD	;Input Address
		XCHG			;HL = Address to edit
ME_LP:		CALL	PUT_NEW_LINE
		CALL	PUT_HL		;Print current contents of memory
		CALL	PUT_SPACE
		MVI	A, ":"
		CALL	PUT_CHAR
		CALL	PUT_SPACE
		MOV	A, M
		CALL	PUT_BYTE
		CALL	SPACE_GET_BYTE	;Input new value or Exit if invalid
		RC			;Exit to Command Loop
		MOV	M, A		;or Save new value
		CALL	PUT_SPACE
		MOV	A, M
		CALL	PUT_BYTE
		INX	H		;Advance to next location
		JMP	ME_LP		;repeat input


;=============================================================================
;	MEM_EXEC - Execute at
;	Get an address and jump to it
;-----------------------------------------------------------------------------
MEM_EXEC:	CALL	SPACE_GET_WORD	;Input address
		XCHG			;HL = Address
		PCHL			;Jump to HL


;===============================================
;Input from port, print contents
PORT_INP:	CALL	SPACE_GET_BYTE
		MOV	B, A
		CALL	PUT_SPACE
		MVI	C, 0xDB
		CALL	GOBYTE
		CALL	PUT_BYTE
		RET

;Get a port address, write byte out
PORT_OUT:	CALL	SPACE_GET_BYTE
		MOV	B, A
		CALL	SPACE_GET_BYTE
		MVI	C, 0xD3

;===============================================
;GOBYTE -- Push a two-byte instruction and RET
;         and jump to it
;
;pre: B register contains operand
;pre: C register contains opcode
;post: code executed, returns to caller
;-----------------------------------------------
GOBYTE:		LXI	H, 0000
		DAD	SP	;HL = STACK
		DCX	H
		MVI	M, 0xC9	;Stuff RET instruction in STACK RAM
		DCX	H
		MOV	M, B	;Stuff Port
		DCX	H
		MOV	M, C	;Stuff Input or Output instruction
		PCHL



;=====================================================================================================
;Monitor Section for the SD Memory Card.  Not required for BIOS
;=====================================================================================================


SD_MENU:	CALL 	PRINTI		;Display Menu
		.ascii "\r\n+ -Select"
		.ascii "\r\n- -Deselct"
		.ascii "\r\nC -Clock/RX"
		.ascii "\r\nX -TX MSG"
		.ascii "\r\nI -Init"
		.ascii "\r\nS -SET SECTOR"
		.ascii "\r\nR -READ SECTOR"
		.ascii "\r\nW -WRITE SECTOR"
		.ascii "\r\nF -INITFAT"
		.ascii "\r\nQ -Find File"
		.ascii "\r\nD -Directory"
		.ascii "\r\nK -READ LOG-D/F SECTOR"
		.ascii "\r\nB  -Select FCB#"
		.ascii "\r\n1-4 -Open File #"
		.ascii "\000"
SD_MENUCMD:	CALL 	PRINTI		;Display Menu Prompt
		.ascii "\r\nSD>\000"
		MVI	A,0xFF
		STA	ECHO_ON		;TURN ON ECHO
		CALL 	GET_CHAR	;get char
		CPI	27		;<Esc>?
		JZ	MAIN_MENU
		CALL	TO_UPPER	;to upper case
		CPI 	"I"		;Branch to Command entered
		JZ 	DO_INIT_SDCARD	; I = Init SD CARD
		CPI 	"+"		;
		JZ 	DO_SD_SELECT	; + = Select SD CARD
		CPI 	"-"		;
		JZ 	DO_SD_DESELECT	; - = Deselect SD CARD
		CPI 	"C"		;
		JZ 	DO_SD_CLK	; C = Clock SD CARD
		CPI 	"X"		;
		JZ 	DO_SD_TXM	; T = TX MSG SD CARD
		CPI 	"S"		;
		JZ 	DO_SD_PARAM	; S = SET SECTOR
		CPI 	"R"		;
		JZ 	DO_SD_READSEC	; R = READ SECTOR
		CPI 	"W"		;
		JZ 	DO_SD_WRITESEC	; W = WRITE SECTOR
		CPI 	"F"		;
		JZ 	DO_SD_IFAT	; F = INIT FAT
		CPI 	"Q"		;
		JZ 	DO_SD_FINDFILE	; Q = FIND FILE
		CPI 	"D"		;
		JZ 	DO_DIR		; D = DIRECTORY
		CPI 	"K"		;
		JZ 	DO_DK_READSEC	; K = READ LOGICAL DISK SECTOR
		CPI 	"B"		;
		JZ 	DO_SEL_FCB	; B = SELECT FCB#
		LXI	H,SDISKA
		CPI 	"1"		;
		JZ 	DO_OPEN		; 1 = OPEN 1
		LXI	H,SDISKB
		CPI 	"2"		;
		JZ 	DO_OPEN		; 2 = OPEN 2
		LXI	H,SDISKC
		CPI 	"3"		;
		JZ 	DO_OPEN		; 3 = OPEN 3
		LXI	H,SDISKD
		CPI 	"4"		;
		JZ 	DO_OPEN		; 4 = OPEN 4
		JMP 	SD_MENU


;-------------------------------------------------
DO_OPEN:		SHLD	FCB_PTR
		CALL	INPUT_FNAME
		LHLD	FCB_PTR
		INX	H		;FNAME
		XCHG
		LXI	H,FILENAME
		MVI	B,11
		CALL	COPY_RAM
		CALL	SD_OPEN
		JMP	SD_MENUCMD

;-------------------------------------------------
DO_DIR:		CALL	PUT_NEW_LINE
		CALL	SD_LDIR1
SDLF_LP: 	JZ	SD_MENUCMD		;End of list
		CALL	PRINT_FILENAME
		CALL	PUT_NEW_LINE
		CALL	SD_LDIRN
		JMP	SDLF_LP

;-------------------------------------------------
DO_SD_FINDFILE:	CALL	INPUT_FNAME
		JC	SD_MENU		;ABORT ON <ESC>
		CALL	SDV_FIND_FILE1
		JMP	SD_MENUCMD

;-------------------------------------------------
DO_SD_IFAT:	CALL	INIT_FAT
		JMP 	SD_MENU

;-------------------------------------------------
DO_SEL_FCB:	CALL 	PRINTI		;
		.ascii "\r\nFCB#\000"
		CALL	GET_CHAR
		CPI 	"1"		;
		JC	SD_MENUCMD
		CPI 	"5"		;
		JNC	SD_MENUCMD
		DCR	A
		CALL	SET_FCB_PTR
		PUSH	H
		CALL 	PRINTI		;
		.ascii " FCB_PTR:\000"
		LHLD	FCB_PTR
		CALL	PUT_HL
		POP	H
		CALL 	PRINTI		;
		.ascii " HL:\000"
		CALL	PUT_HL
		JMP	SD_MENUCMD

;-------------------------------------------------
DO_SD_PARAM:	CALL 	PRINTI		;
		.ascii "\r\n<\000"
		LXI	H,SEC_PTR
		CALL	MOV_32_HL
		CALL	PUT_BC
		CALL	PUT_DE
		CALL 	PRINTI		;
		.ascii "> Enter 4 BYTE SECTOR#> \000"
		MVI	B,4
GDW_LP:		CALL	GET_BYTE
		MOV	M,A
		DCR	B
		JZ	SD_MENU
		DCX	H
		CALL	PUT_SPACE
		JMP	GDW_LP

;-------------------------------------------------
DO_SD_WRITESEC:	CALL	SD_WRITE_SEC
		JNZ	DSDR_FAIL
		CALL 	PRINTI		;
		.ascii "\r\n-OK\000"
		JMP	SD_MENU

;-------------------------------------------------
DO_SD_READSEC:	LXI	H,SEC_PTR
		CALL	MOV_32_HL	;Fetch SEC_PTR to 32bit BCDE
		STC			;Set Carry to force Read
		CALL	SD_READ_SEC
		JNZ	DSDR_FAIL
		XCHG			;Set end of buffer
		LXI	H,SD_RAM_BUFFER	;Set buffer space
		CALL	MEM_DUMP_LP
		JMP	SD_MENUCMD
DSDR_FAIL:	CALL 	PRINTI		;
		.ascii "-FAILED\000"
		JMP	SD_MENU

;-------------------------------------------------
DO_DK_READSEC:	CALL 	PRINTI		;
		.ascii "\r\nEnter Log-Sector> \000"
		CALL	SPACE_GET_WORD
		CPI	27
		JZ	SD_MENUCMD	;On Abort...EXIT
		XCHG
		SHLD	LOGICAL_SEC
		CALL	DISK_READ	;HL = Quarter Buffer
		PUSH	H
		POP	D		;DE = Quarter Buffer
		LXI	B,128
		DAD	B		;HL = Quarter Buffer +128
		XCHG			;HL=START, DE=STOP of Dump
		CALL	MEM_DUMP_LP
		JMP	SD_MENUCMD

;-------------------------------------------------
DO_SD_TXM:	CALL 	PRINTI		;
		.ascii "\r\nEnter HEX> \000"
DST_LP:		CALL	GET_BYTE
		JC	SD_MENU
		OUT	SPI
		MVI	A,"."
		CALL	PUT_CHAR
		CALL	PUT_SPACE
		JMP	DST_LP

;-------------------------------------------------
DO_INIT_SDCARD:	CALL	INIT_SDCARD
		JMP	SD_MENUCMD

;-------------------------------------------------
DO_SD_SELECT:	CALL	SD_SELECT
		JMP	SD_MENUCMD

;-------------------------------------------------
DO_SD_DESELECT:	CALL	SD_DESELECT
		JMP	SD_MENUCMD

;-------------------------------------------------
DO_SD_CLK:	MVI	B,0x08
DSC_0:		CALL	PUT_SPACE
		CALL	SPI_RX
		CALL	PUT_BYTE
		DCR	B
		JNZ	DSC_0
		JMP	SD_MENUCMD


;=============================================================================
SPACE_GET_BYTE:	CALL	PUT_SPACE

;=============================================================================
;GET_BYTE -- Get byte from console as hex
;
;in:	Nothing
;out:	A = Byte (if CY=0)
;	A = non-hex char input (if CY=1)
;-----------------------------------------------------------------------------
GET_BYTE:	CALL	GET_HEX_CHAR	;Get 1st HEX CHAR
		JNC	GB_1
		CPI	" "		;Exit if not HEX CHAR (ignoring SPACE)
		JZ	GET_BYTE	;Loop back if first char is a SPACE
		STC			;Set Carry
		RET			;or EXIT with delimiting char
GB_1:		PUSH	D		;Process 1st HEX CHAR
		RLC
		RLC
		RLC
		RLC
		ANI	0xF0
		MOV	D,A
		CALL	GET_HEX_CHAR
		JNC	GB_2		;If 2nd char is HEX CHAR
		CPI	" "
		JZ	GB_UNDO		;If 2nd char is SPACE, Move 1st back to lower nibble
		STC			;Set Carry
		POP	D
		RET			;or EXIT with delimiting char
GB_2:		ORA	D
		POP	D
		RET
GB_UNDO:		MOV	A,D		;Move 1st back to lower nibble
		RRC
		RRC
		RRC
		RRC
GB_RET:		ORA	A
		POP	D
		RET


;=============================================================================
SPACE_GET_WORD:	CALL	PUT_SPACE

;=============================================================================
;GET_WORD -- Get word from console as hex
;
;in:	Nothing
;out:	A = non-hex char input
;	DE = Word
;-----------------------------------------------------------------------------
GET_WORD:	LXI	D,0
		CALL	GET_HEX_CHAR	;Get 1st HEX CHAR
		JNC	GW_LP
		CPI	" "		;Exit if not HEX CHAR (ignoring SPACE)
		JZ	GET_WORD	;Loop back if first char is a SPACE
		ORA	A		;Clear Carry
		RET			;or EXIT with delimiting char
GW_LP:		MOV	E,A
		CALL	GET_HEX_CHAR
		RC			;EXIT when a delimiting char is entered
		XCHG			;Else, shift new HEX Char Value into DE
		DAD	H
		DAD	H
		DAD	H
		DAD	H
		XCHG
		ORA	E
		JMP	GW_LP



;===============================================
;Get HEX CHAR
;in:	Nothing
;out:	A = Value of HEX Char when CY=0
;	A = Received (non-hex) char when CY=1
;-----------------------------------------------
GET_HEX_CHAR:	CALL	GET_CHAR
ASC2HEX:		CPI	"0"
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




;===============================================
;ASCHEX -- Convert ASCII coded hex to nibble
;
;pre:	A register contains ASCII coded nibble
;post:	A register contains nibble
;-----------------------------------------------
ASCHEX:		SUI	0x30
		CPI	0x0A
		RM
		ANI	0x5F
		SUI	0x07
		RET






;===============================================
;PUT_SPACE -- Print a space to the console
;
;pre: none
;post: 0x20 printed to console
;-----------------------------------------------
PUT_SPACE:	MVI	A, " "
		JMP	PUT_CHAR





;----------------------------------------------------------------------------------------------------; ASCII HEXFILE TRANSFER
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
;----------------------------------------------------------------------------------------------------; ASCII HEXFILE TRANSFER
GETHEXFILE:	MVI	A,0
		MOV	E,A		;ZERO ERROR COUNTER
		STA	ECHO_ON		;TURN OFF ECHO
		JMP	GHDOLINE

GHWAIT:		CALL	GET_CHAR
		CPI	":"
		JNZ	GHWAIT

GHDOLINE:	CALL	GET_BYTE	;GET BYTE COUNT
		MOV	C,A		;BYTE COUNTER
		MOV	D,A		;CHECKSUM

		CALL	GET_BYTE	;GET HIGH ADDRESS
		MOV	H,A
		ADD	D
		MOV	D,A

		CALL	GET_BYTE	;GET LOW ADDRESS
		MOV	L,A
		ADD	D
		MOV	D,A

		CALL	GET_BYTE	;GET RECORD TYPE
		CPI	1
		JZ	GHEND	;IF RECORD TYPE IS 01 THEN END
		ADD	D
		MOV	D,A

GHLOOP:		CALL	GET_BYTE	;GET DATA
		MOV	M,A
		ADD	D
		MOV	D,A
		INX	H

		DCR	C
		JNZ	GHLOOP

		CALL	GET_BYTE	;GET CHECKSUM
		ADD	D
		JZ	GHWAIT
		INR	E
		JNZ	GHWAIT
		DCR	E
		JMP	GHWAIT

GHEND:		CALL	PRINTI
		.ascii "\r\nHEX TRANSFER COMPLETE ERRORS=\000"
		MOV	A,E
		CALL	PUT_BYTE
		JMP	PURGE


;----------------------------------------------------------------------------------------------------; XMODEM ROUTINES
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
;----------------------------------------------------------------------------------------------------; XMODEM ROUTINES

SOH	.equ	1	;Start of Header
EOT	.equ	4	;End of Transmission
ACK	.equ	6
DLE	.equ	16
DC1	.equ	17	; (X-ON)
DC3	.equ	19	; (X-OFF)
NAK	.equ	21
SYN	.equ	22
CAN	.equ	24	;(Cancel)

;---------------------------------------------------------------------------------
;XMODEM MENU
;ENTRY:	TOP OF STACK HOLDS RETURN ADDRESS (EXIT MECHANISM IF XMODEM IS CANCELLED)
;---------------------------------------------------------------------------------
XMODEM:		CALL	PUT_SPACE
		CALL	GET_CHAR	;get char
		ANI	0x5F		;to upper case
		CPI	"D"
		JZ	XMDN		; D = DOWNLOAD
		CPI	"U"
		JZ	XMUP		; U = UPLOAD
		CALL 	PRINTI
		.ascii "?\000"
		RET

;---------------------------------------------------------------------------------
;XMDN - XMODEM DOWNLOAD (send file from IMSAI to Terminal)
;INPUT STARTING ADDRESS AND COUNT OF BLOCKS (WORD)
;WAIT FOR 'C' OR NAK FROM HOST TO START CRC/CS TRANSFER
;---------------------------------------------------------------------------------
XMDN:		CALL	SPACE_GET_WORD	;Input Address
		XCHG			;HL = Address to SAVE DATA
		CALL	SPACE_GET_WORD	;Input #Blocks to Send
					;DE = Count of Blocks

		MOV	A,D
		ORA	E
		RZ			;Exit if Block Count = 0

	;HL = Address of data to send from the IMSAI 8080
	;DE = Count of Blocks to send.

		CALL	XMS_INIT	;Starts the Seq, Sets the CS/CRC format
					;Cancelled Transfers will cause a RET

XMDN_LP:		CALL	XMS_SEND	;Sends the packet @HL, Resends if NAK
					;Cancelled Transfers will cause a RET
		DCX	D
		MOV	A,D
		ORA	E
		JNZ	XMDN_LP

		CALL	XMS_EOT		;Send End of Transmission
		JMP	PURGE


;---------------------------------------------------------------------------------
;XMUP - XMODEM UPLOAD (receive file from Terminal to IMSAI 8080)
;INPUT STARTING ADDRESS
;SEND 'C' OR NAK TO HOST TO START CRC/CS TRANSFER
;---------------------------------------------------------------------------------
XMUP:		CALL	SPACE_GET_WORD	;Input Address
		XCHG			;HL = Address to SAVE DATA

	;HL = Address of where data is to be received

		CALL	XMR_INIT	;Starts the transfer & Receives first PACKET
					;Cancelled Transfers will cause a RET

XMUP_LP:		CALL	XMR_RECV	;Receives the next packet @HL, Resends if NAK
					;Cancelled Transfers will cause a RET
		JC	XMUP_LP		;Jump until EOT Received
		JMP	PURGE



;---------------------------------------------------------------------------------
;INIT FOR SENDING XMODEM PROTOCOL, GET NAK OR 'C', SAVE THE XMTYPE
;---------------------------------------------------------------------------------
XMS_INIT:	MVI	A,1		;First SEQ number
		STA	XMSEQ

		MVI	B,10		;10 retries for initiating the transfer
XMS_INIT_LP:	MVI	A,45		;GET CHAR, 45 SECONDS TIMEOUT (EXPECT C OR NAK)
		CALL	TIMED_GETCHAR
		JC	XM_CANCEL	;Cancel if Host Timed out

		CPI	NAK		;If NAK, Start Checksum Download
		JZ	XMS_DO
		CPI	"C"		;If C, Start CRC Download
		JZ	XMS_DO
		DCR	B		;Count down Retries
		JNZ	XMS_INIT_LP
		JMP	XM_CANCEL	;Cancel XModem if all retries exhausted

XMS_DO:		STA	XMTYPE
		RET

;---------------------------------------------------------------------------------
;SEND A PACKET (RESEND UPON NAK)
;---------------------------------------------------------------------------------
XMS_RESEND:	LXI	B,0xFF80
		DAD	B
XMS_SEND:	PUSH	D
		MVI	A,SOH		;SEND THE HEADER FOR CRC OR CHECKSUM
		CALL	PUT_CHAR
		LDA	XMSEQ
		CALL	PUT_CHAR
		CMA
		CALL	PUT_CHAR
		LXI	D,0x0000		;Init DE=0000 (CRC Accumulator)
		MVI	C,0		;Init C=00 (CS Accumulator)
		MVI	B,128		;Count 128 bytes per block
XMS_BLP:		MOV	A,M		;Fetch bytes to send  -------------------\
		CALL	PUT_CHAR	;Send them
		ADD	C		;Update the CS
		MOV	C,A
		MOV	A,M
		CALL	CRC_UPDATE	;Update the CRC
		INX	H		;Advance to next byte in block
		DCR	B		;Count down bytes sent
		JNZ	XMS_BLP		;Loop back until 128 bytes are sent -----^
		LDA	XMTYPE
		CPI	NAK		;If NAK, send Checksum
		JZ	XMS_CS		;----------------------v
		MOV	A,D		;else, Send the CRC next
		CALL	PUT_CHAR
		MOV	C,E
XMS_CS:		MOV	A,C		;----------------------/
		CALL	PUT_CHAR
					;Packet Sent, get Ack/Nak Response
		MVI	A,45		;GET CHAR, 45 SECONDS TIMEOUT (EXPECT C OR NAK)
		CALL	TIMED_GETCHAR
		POP	D

		JC	XM_CANCEL	;Cancel download if no response within 45 seconds
		CPI	NAK
		JZ	XMS_RESEND	;Loop back to resend packet
		CPI	CAN
		JZ	XM_CANCEL
		CPI	ACK
		JNZ	XM_CANCEL

		LDA	XMSEQ
		INR	A		;NEXT SEQ
		STA	XMSEQ
		RET


;---------------------------------------------------------------------------------
;XMDN - DOWNLOAD XMODEM PACKET
;---------------------------------------------------------------------------------
XMS_EOT:		MVI	A,EOT		;HANDLE THE END OF TRANSFER FOR CRC OR CHECKSUM
		CALL	PUT_CHAR
		MVI	A,45		;GET CHAR, 45 SECONDS TIMEOUT (EXPECT C OR NAK)
		CALL	TIMED_GETCHAR
		JC	XM_CANCEL
		CPI	NAK
		JZ	XMS_EOT
		CPI	ACK
		JNZ	XM_CANCEL

XM_DONE:		CALL	PRINTI
		.ascii "\r\nTRANSFER COMPLETE\r\n\000"
		XRA	A		;CLEAR A, CY
		RET

;FINISHING CODE PRIOR TO LEAVING XMODEM
XM_CANCEL:	MVI	A,CAN
		CALL	PUT_CHAR
		CALL	PUT_CHAR
		CALL	PURGE
		CALL	PRINTI
		.ascii "TRANSFER CANCELED\r\n\000"
		POP	B		;SCRAP CALLING ROUTINE AND HEAD TO PARENT
		RET






;---------------------------------------------------------------------------------
;START XMODEM RECEIVING and RECEIVE FIRST PACKET
;---------------------------------------------------------------------------------
XMR_INIT:	MVI	E,5		;5 ATTEMPTS TO INITIATE XMODEM CRC TRANSFER
		MVI	A,1		;EXPECTED SEQ NUMBER starts at 1
		STA	XMSEQ
XMR_CRC:		CALL	PURGE
		MVI	A,"C"		;Send C
		STA	XMTYPE		;Save as XM Type (CRC or CS)
		CALL	PUT_CHAR
		CALL	XMGET_HDR	;Await a packet
		JNC	XMR_TSEQ	;Jump if first packet received
		JNZ	XM_CANCEL	;Cancel if there was a response that was not a header
		DCR	E		;Otherwise, if no response, retry a few times
		JNZ	XMR_CRC

		MVI	E,5		;5 ATTEMPTS TO INITIATE XMODEM CHECKSUM TRANSFER
XMR_CS:		CALL	PURGE
		MVI	A,NAK		;Send NAK
		STA	XMTYPE		;Save as XM Type (CRC or CS)
		CALL	PUT_CHAR
		CALL	XMGET_HDR	;Await a packet
		JNC	XMR_TSEQ	;Jump if first packet received
		JNZ	XM_CANCEL	;Cancel if there was a response that was not a header
		DCR	E		;Otherwise, if no response, retry a few times
		JNZ	XMR_CS
		JMP	XM_CANCEL	;Abort


;--------------------- XMODEM RECEIVE
;Entry:	XMR_TSEQ in the middle of the routine
;Pre:	C=1 (expected first block as received when negogiating CRC or Checksum)
;	HL=Memory to dump the file to
;Uses:	B to count the 128 bytes per block
;	C to track Block Number expected
;	DE as CRC (Within Loop) (D is destroyed when Getting Header)
;------------------------------------
XMR_RECV:	MVI	A,ACK		;Send Ack to start Receiving next packet
		CALL	PUT_CHAR
XMR_LP:		CALL	XMGET_HDR
		JNC	XMR_TSEQ
		PUSH	H
		JZ	XMR_NAK		;NACK IF TIMED OUT
		POP	H
		CPI	EOT
		JNZ	XM_CANCEL	;CANCEL IF CAN RECEIVED (OR JUST NOT EOT)
		MVI	A,ACK
		CALL	PUT_CHAR
		JMP	XM_DONE

XMR_TSEQ:	MOV	C,A
		LDA	XMSEQ
		CMP	C		;CHECK IF THIS SEQ IS EXPECTED
		JZ	XMR_SEQ_OK	;Jump if CORRECT SEQ
		DCR	A		;Else test if Previous SEQ
		STA	XMSEQ
		CMP	C
		JNZ	XM_CANCEL	;CANCEL IF SEQUENCE ISN'T PREVIOUS BLOCK
		CALL	PURGE		;ELSE, PURGE AND SEND ACK (ASSUMING PREVIOUS ACK WAS NOT RECEIVED)
		JMP	XMR_ACK

XMR_SEQ_OK:	MVI	B,128		;128 BYTES PER BLOCK
		MVI	C,0		;Clear Checksum
		LXI	D,0x0000		;CLEAR CRC
		PUSH	H		;Save HL where block is to go
XMR_BLK_LP:	CALL	TIMED1_GETCHAR
		JC	XMR_NAK
		MOV	M,A		;SAVE DATA BYTE
		CALL	CRC_UPDATE
		MOV	A,M		;Update checksum
		ADD	C
		MOV	C,A
		INX	H		;ADVANCE
		DCR	B
		JNZ	XMR_BLK_LP
					;After 128 byte packet, verify error checking byte(s)
		LDA	XMTYPE		;Determine if we are using CRC or Checksum
		CPI	NAK		;If NAK, then use Checksum
		JZ	XMR_CCS
		CALL	TIMED1_GETCHAR
		JC	XMR_NAK
		CMP	D
		JNZ	XMR_NAK
		CALL	TIMED1_GETCHAR
		JC	XMR_NAK
		CMP	E
		JNZ	XMR_NAK
		JMP	XMR_ACK

XMR_CCS:		CALL	TIMED1_GETCHAR
		JC	XMR_NAK
		CMP	C
		JNZ	XMR_NAK

		;If we were transfering to a FILE, this is where we would write the
		;sector and reset HL to the same 128 byte sector buffer.
		;CALL	WRITE_SECTOR

XMR_ACK:		;MVI	A,ACK		;The sending of the Ack is done by
		;CALL	PUT_CHAR	;the calling routine, to allow writes to disk
		LDA	XMSEQ
		INR	A		;Advance to next SEQ BLOCK
		STA	XMSEQ
		POP	B
		STC			;Carry set when NOT last packet
		RET

XMR_NAK:		POP	H		;Return HL to start of block
		CALL	PURGE
		MVI	A,NAK
		CALL	PUT_CHAR
		JMP	XMR_LP


;--------------------- XMODEM - GET HEADER
;
;pre:	Nothing
;post:	Carry Set: A=0, (Zero set) if Timeout
;	Carry Set: A=CAN (Not Zero) if Cancel received
;	Carry Set: A=EOT (Not Zero) if End of Tranmission received
;	Carry Clear and A = B = Seq if Header found and is good
;------------------------------------------
XMGET_HDR:	MVI	A,5		;GET CHAR, 5 SECONDS TIMEOUT (EXPECT SOH)
		CALL	TIMED_GETCHAR
		RC			;Return if Timed out
		CPI	SOH		;TEST IF START OF HEADER
		JZ	GS_SEQ		;IF SOH RECEIVED, GET SEQ NEXT
		CPI	EOT		;TEST IF END OF TRANSMISSION
		JZ	GS_ESC		;IF EOT RECEIVED, TERMINATE XMODEM
		CPI	CAN		;TEST IF CANCEL
		JNZ	XMGET_HDR
GS_ESC:		ORA	A		;Clear Z flag (because A<>0)
		STC
		RET
GS_SEQ:		CALL	TIMED1_GETCHAR	;GET SEQ CHAR
		RC			;Return if Timed out
		MOV	B,A		;SAVE SEQ
		CALL	TIMED1_GETCHAR	;GET SEQ COMPLEMENT
		RC			;Return if Timed out
		CMA
		CMP	B		;TEST IF SEQ VALID
		JNZ	XMGET_HDR	;LOOP BACK AND TRY AGAIN IF HEADER INCORRECT (SYNC FRAME)
		RET

;------------------------------------------ CRC_UPDATE
;HANDLE THE CRC CALCULATION FOR UP/DOWNLOADING
;Total Time=775 cycles = 388uSec
;In:	A  = New char to roll into CRC accumulator
;	DE = 16bit CRC accumulator
;Out:	DE = 16bit CRC accumulator
;------------------------------------------
;CRC_UPDATE:	XRA	D		;4
;		MOV	D,A		;5
;		PUSH	B		;11
;		MVI	B,8		;7	PRELOOP=27
;CRCU_LP:	ORA	A		;4	CLEAR CARRY
;		MOV	A,E		;5
;		RAL			;4
;		MOV	E,A		;5
;		MOV	A,D		;5
;		RAL			;4
;		MOV	D,A		;5
;		JNC	CRCU_NX		;10
;		MOV	A,D		;5
;		XRI	10h		;7
;		MOV	D,A		;5
;		MOV	A,E		;5
;		XRI	21H		;7
;		MOV	E,A		;5
;CRCU_NX:		DCR	B		;5
;		JNZ	CRCU_LP		;10	LOOP=91*8 (WORSE CASE)
;		POP	B		;10	POSTLOOP=20
;		RET			;10


;------------------------------------------ CRC_UPDATE
;HANDLE THE CRC CALCULATION FOR UP/DOWNLOADING
;Total Time=604 cycles = 302uSec MAX
;In:	A  = New char to roll into CRC accumulator
;	DE = 16bit CRC accumulator
;Out:	DE = 16bit CRC accumulator
;------------------------------------------
CRC_UPDATE:	XCHG			;4
		XRA	H		;4
		MOV	H,A		;5
		DAD	H		;10	Shift HL Left 1
		CC	CRC_UPC		;17 (10/61)
		DAD	H		;10	Shift HL Left 2
		CC	CRC_UPC		;17
		DAD	H		;10	Shift HL Left 3
		CC	CRC_UPC		;17
		DAD	H		;10	Shift HL Left 4
		CC	CRC_UPC		;17
		DAD	H		;10	Shift HL Left 5
		CC	CRC_UPC		;17
		DAD	H		;10	Shift HL Left 6
		CC	CRC_UPC		;17
		DAD	H		;10	Shift HL Left 7
		CC	CRC_UPC		;17
		DAD	H		;10	Shift HL Left 8
		CC	CRC_UPC		;17
		XCHG			;4
		RET			;10

CRC_UPC:		MOV	A,H		;5
		XRI	0x10		;7
		MOV	H,A		;5
		MOV	A,L		;5
		XRI	0x21		;7
		MOV	L,A		;5
		RET			;10


;===============================================
;TIMED1_GETCHAR - Gets a character within 1 second
;
;pre:	nothing
;post: 	Carry Set = No Char, Time Out
;	Carry Clear, A = Char
;-----------------------------------------------
TIMED1_GETCHAR:	MVI	A,1

;===============================================
;TIMED_GETCHAR - Gets a character within a time limit
;
;pre:	A contains # of seconds to wait before returning
;post: 	Carry Set & Zero Set = No Char, Time Out
;	Carry Clear, A = Char
;-----------------------------------------------
TIMED_GETCHAR:	PUSH	D
		PUSH	B
		MOV	D,A
		;MVI	C,0	;B,C=Loop Count down until timeout
TGC_LP1:		MVI	B,107		;107 * 9.3mSec = 1 Second
TGC_LP2:		CALL	CONSTV	;45  TEST FOR RX DATA
		JNZ	TGC_DO	;10
		DCR	C	;5
		JNZ	TGC_LP2	;10	;73 Cycles Loop time. 39*256*.5 ~= 9.3 mSec
		DCR	B
		JNZ	TGC_LP2	;	1 Second waiting
		DCR	D		;Count down Seconds until Time Out
		JNZ	TGC_LP1
		STC		;SET CARRY TO INDICATE TIME OUT
		;MVI	A,0
		JMP	TGC_RET
TGC_DO:		CALL	GET_CHAR_UART
TGC_RET:		POP	B
		POP	D
		RET


;===============================================
;PURGE - Clears all in coming bytes until the line is clear for a full 2 seconds
;-----------------------------------------------
PURGE:		MVI	A,2	;2 seconds for time out
		CALL	TIMED_GETCHAR
		JNC	PURGE
		RET



;XModem implementation on 8080 Monitor (CP/M-80)
;
;Terminal uploads to 8080 system:
;-Terminal user enters command "XU aaaa"
;-8080 "drives" the protocol since it's the receiver
;-8080 sends <Nak> every 10 seconds until the transmitter sends a packet
;-if transmitter does not begin within 10 trys (100 seconds), 8080 aborts XMODEM
;-a packet is:
; <SOH> [seq] [NOT seq] [128 bytes of data] [checksum or CRC]
;
;<SOH> = 1 (Start of Header)
;<EOT> = 4 (End of Transmission)
;<ACK> = 6
;<DLE> = 16
;<DC1> = 17 (X-ON)
;<DC3> = 19 (X-OFF)
;<NAK> = 21
;<SYN> = 22
;<CAN> = 24 (Cancel)
;
;Checksum is the Modulo 256 sum of all 128 data bytes
;
;                                     <<<<<          [NAK]
;       [SOH][001][255][...][csum]    >>>>>
;                                     <<<<<          [ACK]
;       [SOH][002][254][...][csum]    >>>>>
;                                     <<<<<          [ACK]
;       [SOH][003][253][...][csum]    >>>>>
;                                     <<<<<          [ACK]
;       [EOT]                         >>>>>
;                                     <<<<<          [ACK]
;
;-if we get <EOT> then ACK and terminate XModem
;-if we get <CAN> then terminate XModem
;-if checksum invalid, then NAK
;-if seq number not correct as per [NOT seq], then NAK
;-if seq number = previous number, then ACK (But ignore block)
;-if seq number not the expected number, then <CAN><CAN> and terminate XModem
;-if data not received after 10 seconds, then NAK (inc Timeout Retry)
;-if timeout retry>10 then <CAN><CAN> and terminate XModem
;
;-To keep synchronized,
;  -Look for <SOH>, qualify <SOH> by checking the [seq] / [NOT seq]
;  -if no <SOH> found after 135 chars, then NAK
;
;-False EOT condtion
;  -NAK the first EOT
;  -if the next char is EOT again, then ACK and leave XModem
;
;-False <CAN>, expect a 2nd <CAN> ?
;
;-Using CRC, send "C" instead of <NAK> for the first packet
;  -Send "C" every 3 seconds for 3 tries, then degrade to checksums by sending <NAK>
;
;
;
;* The character-receive subroutine should be called with a
;parameter specifying the number of seconds to wait.  The
;receiver should first call it with a time of 10, then <nak> and
;try again, 10 times.
;  After receiving the <soh>, the receiver should call the
;character receive subroutine with a 1-second timeout, for the
;remainder of the message and the <cksum>.  Since they are sent
;as a continuous stream, timing out of this implies a serious
;like glitch that caused, say, 127 characters to be seen instead
;of 128.
;
;* When the receiver wishes to <nak>, it should call a "PURGE"
;subroutine, to wait for the line to clear.  Recall the sender
;tosses any characters in its UART buffer immediately upon
;completing sending a block, to ensure no glitches were mis-
;interpreted.
;  The most common technique is for "PURGE" to call the
;character receive subroutine, specifying a 1-second timeout,
;and looping back to PURGE until a timeout occurs.  The <nak> is
;then sent, ensuring the other end will see it.
;
;* You may wish to add code recommended by Jonh Mahr to your
;character receive routine - to set an error flag if the UART
;shows framing error, or overrun.  This will help catch a few
;more glitches - the most common of which is a hit in the high
;bits of the byte in two consecutive bytes.  The <cksum> comes
;out OK since counting in 1-byte produces the same result of
;adding 80H + 80H as with adding 00H + 00H.



;----------------------------------------------------------------------------------------------------; RAM TEST
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
;----------------------------------------------------------------------------------------------------; RAM TEST
;B=START PAGE
;C=END PAGE
RAM_TEST:	CALL	SPACE_GET_BYTE
		MOV	B, A
		CALL	SPACE_GET_BYTE
		MOV	C, A

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

		CALL	PRINTI
		.ascii "\r\nTESTING RAM\000"
		MVI	E,0xFF		;E selects the polarity of the test, ie March a page of 1'S or 0's

;Clear/Set all pages
RT1_LP0:		MOV	H,B		;HL = BASE RAM ADDRESS
		MVI	L,0
RT1_LP1:		MOV	A,E		;CLEAR A
		CMA
RT1_LP2:		MOV	M,A		;WRITE PAGE
		INR	L
		JNZ	RT1_LP2		;LOOP TO QUICKLY WRITE 1 PAGE
		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CMP	C		;COMPARE WITH END PAGE
		JNZ	RT1_LP1		;LOOP UNTIL = END PAGE

;March 1 PAGE through RAM
		MOV	D,B		;Begin with START PAGE

;Write FF to page D
RT1_LP3:		MOV	H,D		;HL = Marched Page ADDRESS
		;MVI	L,0
		CALL	ABORT_CHECK

		MOV	A,D
		CMA
		OUT	FPLED
		;MOV	A,E		;SET A
RT1_LP4:		MOV	M,E		;WRITE PAGE
		INR	L
		JNZ	RT1_LP4		;LOOP TO QUICKLY WRITE 1 PAGE

;Test all pages for 0 (except page D = FF)
		MOV	H,B		;HL = BASE RAM ADDRESS
		;MVI	L,0

RT1_LP5:		MOV	A,H		;IF H = D
		CMP	D
		MOV	A,E		;THEN Value = FF
		JZ	RT1_LP6
		CMA			;ELSE Value = 00

RT1_LP6:		CMP	M		;TEST RAM
		JNZ	RT_FAIL1
		INR	L
		JNZ	RT1_LP6		;LOOP TO QUICKLY TEST 1 PAGE
		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CMP	C		;COMPARE WITH END PAGE
		JNZ	RT1_LP5		;LOOP UNTIL = END PAGE

;Write 00 back to page D
		MOV	H,D		;HL = Marched Page ADDRESS
		;MVI	L,0
		MOV	A,E
		CMA
RT1_LP7:		MOV	M,A		;WRITE PAGE
		INR	L
		JNZ	RT1_LP7		;LOOP TO QUICKLY WRITE 1 PAGE

		MOV	A,D
		INR	D		;ADVANCE TO NEXT PAGE
		CMP	C		;COMPARE WITH END PAGE
		JNZ	RT1_LP3		;LOOP UNTIL = END PAGE

		INR	E
		JZ	RT1_LP0

		CALL	PRINTI
		.ascii "\r\nRAM PAGE MARCH PASSED\000"


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

RT2_LP0:		MOV	H,B		;HL = BASE RAM ADDRESS
RT2_LP1:		MVI	L,0
		CALL	ABORT_CHECK

		MOV	A,H
		CMA
		OUT	FPLED

		MOV	A,E		;CLEAR A
		CMA
RT2_LP2:		MOV	M,A		;WRITE PAGE
		INR	L
		JNZ	RT2_LP2		;LOOP TO QUICKLY WRITE 1 PAGE


		MVI	D,0		;Starting with BYTE 00 of page

RT2_LP3:		MOV	L,D		;Save at byte march ptr
		MOV	A,E		;SET A
		MOV	M,A

		;MOV	A,E
		CMA			;CLEAR A
		MVI	L,0

RT2_LP4:		CMP	M		;TEST BYTE FOR CLEAR
		JZ	RT2_NX1
		CMA			;SET A
		CMP	M		;TEST BYTE FOR SET
		JNZ	RT_FAIL2	;IF NOT FULLY SET, THEN DEFINITELY FAIL
		MOV	A,L		;ELSE CHECK WE ARE ON MARCHED BYTE
		CMP	D
		JNZ	RT_FAIL2
		MOV	A,E		;CLEAR A
		CMA
RT2_NX1:		INR	L
		JNZ	RT2_LP4		;LOOP TO QUICKLY WRITE 1 PAGE

		MOV	L,D		;Save at byte march ptr
		MOV	A,E
		CMA			;CLEAR A
		MOV	M,A

		INR	D
		JNZ	RT2_LP3

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CMP	C		;COMPARE WITH END PAGE
		JNZ	RT2_LP1		;LOOP UNTIL = END PAGE

		INR	E
		JZ	RT2_LP0

		CALL	PRINTI
		.ascii "\r\nRAM BYTE MARCH 1 PASSED\000"

;26 Sec/K

BYTEMARCH2:
		MVI	E,0xFF		;E selects the polarity of the test, ie March a page of 1'S or 0's

RT4_LP0:		MVI	D,0		;Starting with BYTE 00 of page

;CLEAR all pages

		MOV	H,B		;HL = BASE RAM ADDRESS
		MVI	L,0

RT4_LP1:		MOV	A,E		;CLEAR A
		CMA
RT4_LP2:		MOV	M,A		;WRITE PAGE
		INR	L
		JNZ	RT4_LP2		;LOOP TO QUICKLY WRITE 1 PAGE

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CMP	C		;COMPARE WITH END PAGE
		JNZ	RT4_LP1		;LOOP UNTIL = END PAGE


RT4_LP3:		CALL	ABORT_CHECK
		MOV	A,D
		CMA
		OUT	FPLED

					;Write SET byte at "D" in every page
		MOV	H,B		;HL = BASE RAM ADDRESS
		MOV	L,D		;Save at byte march ptr
RT4_LP4:		MOV	M,E

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CMP	C		;COMPARE WITH END PAGE
		JNZ	RT4_LP4		;LOOP UNTIL = END PAGE


		MVI	L,0

RT4_LP5:		MOV	H,B		;HL = BASE RAM ADDRESS
		MOV	A,L
		CMP	D
		JZ	RT4_LP7		;Test for marked byte in all pages

RT4_LP6:		MOV	A,E
		CMA			;CLEAR A
		CMP	M		;TEST BYTE FOR CLEAR
		JNZ	RT_FAIL2

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CMP	C		;COMPARE WITH END PAGE
		JNZ	RT4_LP6		;LOOP UNTIL = END PAGE
		JMP	RT4_NX

RT4_LP7:		MOV	A,E
		CMP	M		;TEST BYTE FOR SET
		JNZ	RT_FAIL2

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CMP	C		;COMPARE WITH END PAGE
		JNZ	RT4_LP7		;LOOP UNTIL = END PAGE

RT4_NX:		INR	L
		JNZ	RT4_LP5

					;Write CLEAR byte at "D" in every page
		MOV	H,B		;HL = BASE RAM ADDRESS
		MOV	L,D		;Save at byte march ptr
RT4_LP8:		MOV	A,E
		CMA
		MOV	M,A

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CMP	C		;COMPARE WITH END PAGE
		JNZ	RT4_LP8		;LOOP UNTIL = END PAGE

		INR	D
		JNZ	RT4_LP3


		INR	E
		JZ	RT4_LP0

		CALL	PRINTI
		.ascii "\r\nRAM BYTE MARCH 2 PASSED\000"


BIT_MARCH:
;Bit March Test.  0.1 Sec/K

		MVI	E,01		;E selects the bit to march

;Clear/Set all pages

RT3_LP1:		MOV	H,B		;HL = BASE RAM ADDRESS
		MVI	L,0

		CALL	ABORT_CHECK

		MOV	A,E		;Display bit pattern on LED PORT
		CMA
		OUT	FPLED

RT3_LP2:		MOV	A,E		;FETCH MARCHING BIT PATTERN
RT3_LP3:		MOV	M,A		;WRITE PAGE
		INR	L
		JNZ	RT3_LP3		;LOOP TO QUICKLY WRITE 1 PAGE

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CMP	C		;COMPARE WITH END PAGE
		JNZ	RT3_LP2		;LOOP UNTIL = END PAGE

		MOV	H,B		;HL = BASE RAM ADDRESS
;		MVI	L,0

RT3_LP4:		MOV	A,E		;FETCH MARCHING BIT PATTERN
RT3_LP5:		CMP	M
		JNZ	RT_FAIL3
		INR	L
		JNZ	RT3_LP5		;LOOP TO QUICKLY WRITE 1 PAGE

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CMP	C		;COMPARE WITH END PAGE
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
RT3_NX1:		CPI	0xFE
		JNZ	RT3_LP1

		CALL	PRINTI
		.ascii "\r\nRAM BIT MARCH PASSED\000"



		MVI	E,01		;E selects the start sequence

;Clear/Set all pages

RT5_LP1:		CALL	ABORT_CHECK

		MOV	A,E		;Display bit pattern on LED PORT
		CMA
		OUT	FPLED

		MOV	H,B		;HL = BASE RAM ADDRESS
		MVI	L,0
		MOV	D,E

RT5_LP2:		INR	D
		JNZ	RT5_NX1
		INR	D
RT5_NX1:		MOV	M,D		;WRITE PAGE
		INR	L
		JNZ	RT5_LP2		;LOOP TO QUICKLY WRITE 1 PAGE

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CMP	C		;COMPARE WITH END PAGE
		JNZ	RT5_LP2		;LOOP UNTIL = END PAGE

		MOV	H,B		;HL = BASE RAM ADDRESS
		;MVI	L,0
		MOV	D,E

RT5_LP3:		INR	D
		JNZ	RT5_NX2
		INR	D
RT5_NX2:		MOV	A,D
		CMP	M		;TEST
		JNZ	RT_FAIL5
		INR	L
		JNZ	RT5_LP3		;LOOP TO QUICKLY WRITE 1 PAGE

		MOV	A,H
		INR	H		;ADVANCE TO NEXT PAGE
		CMP	C		;COMPARE WITH END PAGE
		JNZ	RT5_LP3		;LOOP UNTIL = END PAGE

		INR	E
		JNZ	RT5_LP1

		CALL	PRINTI
		.ascii "\r\nRAM SEQUENCE TEST PASSED\000"

		JMP	MAIN_MENU


RT_FAIL1:	CALL	PRINTI
		.ascii "\r\nRAM FAILED PAGE MARCH AT:\000"
		CALL	PUT_HL
		JMP	MAIN_MENU

RT_FAIL2:	CALL	PRINTI
		.ascii "\r\nRAM FAILED BYTE MARCH AT:\000"
		CALL	PUT_HL
		JMP	MAIN_MENU

RT_FAIL3:	CALL	PRINTI
		.ascii "\r\nRAM FAILED BIT MARCH AT:\000"
		CALL	PUT_HL
		JMP	MAIN_MENU

RT_FAIL5:	CALL	PRINTI
		.ascii "\r\nRAM FAILED SEQUENCE TEST AT:\000"
		CALL	PUT_HL
		JMP	MAIN_MENU


ABORT_CHECK:	CALL	CONSTV
		RZ
		CALL	GET_CHAR
		CPI	27
		RNZ
		POP	H			;SCRAP RETURN ADDRESS AND GO TO PARENT ROUTINE
		CALL	PRINTI
		.ascii "\r\nABORTED\000"
		RET


;----------------------------------------------------------------------------------------------------; FLOPPY XMODEM TRANSFERS
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
;----------------------------------------------------------------------------------------------------; FLOPPY XMODEM TRANSFERS
FXSERR:		CALL 	PRINTI		;Display Err when input is invalid
		.ascii "\r\nERROR, LOGICAL SECTOR NUMBER WILL BE OUT OF RANGE\000"

FXMODEM:	CALL 	PRINTI		;Display Err when input is invalid
		.ascii "\r\nFLOPPY XMODEM MENU"
		.ascii "\r\nU ssss      - UPLOAD (WRITE DISK) ssss=Starting Sector"
		.ascii "\r\nD ssss cccc - DOWNLOAD (READ DISK) ssss=Starting Sector, cccc=Count of Sectors"
		.ascii "\r\n  ssss = 0000 for 1st sector on track 0"
		.ascii "\r\n  cccc = 07D2 (2002) for a whole disk (77 Tracks, 26 Sectors per)"
		.ascii "\r\n"
		.ascii "\r\n\000"

FXMENU_LP:	MVI	A,":"
		CALL	PUT_CHAR
		CALL 	GET_CHAR	;get char
		CPI 	27		;Branch to Command entered
		RZ 			; <ESC> = Exit
		CPI 	"?"		;
		JZ 	FXMODEM		; ? = Help
		ANI 	0x5F		;to upper case
		CPI 	"D"		;Branch to Command entered
		JZ 	FXMDN		; D = Download to Terminal (Read Disk)  D XXXX YYYY Where XXXX is Logicial sector and YYYY is Count of Sectors to send
		CPI 	"U"
		JZ 	FXMUP		; U = Upload from Terminal (Write Disk)
		JMP 	FXMENU_LP


FXMDN:		CALL	SPACE_GET_WORD	;Input Logical Sector (to DE)
		XCHG
		SHLD	XSUM		;Save to sum up Logical sector + count of sectors
		SHLD	XPOS		;Save position of next read
		XCHG
		CALL	LOG2PHY		;Set the Physical Track and Sector based on the Logical Sector 0=Sector 1, Track 0
		JNC	FXSERR		;Sector out of range
		XCHG
		SHLD	XSECTOR		;Set XSECTOR & XTRACK (16 bit save to two 8 bit variables)
		CALL	SPACE_GET_WORD	;Input # Sectors (Blocks)  to Send
		XCHG
		SHLD	XCOUNT
		XCHG			;Now test if the read of sectors will extend out of range.
		LHLD	XSUM
		DAD	D		;HL = HL + DE.  START_SECTOR = START_SECTOR + SECTOR_COUNT
		JC	FXSERR		;Sector out of range
		LXI	D,0xFFFF
		DAD	D		;HL = HL - 1
		XCHG
		CALL	LOG2PHY		;Test Start + Count - 1 <= Valid Track/Sector
		JNC	FXSERR		;Sector out of range

		CALL	XMS_INIT	;Starts the Seq, Sets the CS/CRC format
					;Cancelled Transfers will cause a RET

FXMDN_LP:	LHLD	XCOUNT		;IF COUNT = 0 THEN EXIT
		MOV	A,H
		ORA	L
		JZ	FXMDN_DONE
		DCX	H		;ELSE, COUNT = COUNT - 1
		SHLD	XCOUNT
					;Future? Check if Drive is already on correct Track
		LDA	XTRACK		;Seek to correct Track (can't be much of a delay if already on correct track)
;		CALL	FSEEK

		LDA	XSECTOR		;Read the Sector
;		OUT	DSECTOR
		;LXI	H, TRACK_BUFFER
;		MVI	A,08Ch		;Read Sector (with 10mS delay for head load)
;		CALL	READ_CMD

		;LXI	H,TRACK_BUFFER	;Where to Send the Packet from

		CALL	XMS_SEND	;Sends the packet @HL, Resends if NAK
					;Cancelled Transfers will cause a RET

		LHLD	XPOS		;Advance to next Logical Sector
		INX	H
		SHLD	XPOS
		XCHG
		CALL	LOG2PHY		;Set the Physical Track and Sector based on the Logical Sector 0=Sector 1, Track 0
		JNC	FXMDN_DONE	;Sector out of range
		XCHG
		SHLD	XSECTOR		;Set XSECTOR & XTRACK (16 bit save to two 8 bit variables)

		JMP	FXMDN_LP

FXMDN_DONE:	CALL	XMS_EOT		;Send End of Transmission
		JMP	PURGE



;Disk XMODEM
FXMUP:		CALL	SPACE_GET_WORD	;Input Logical Sector (to DE)
		XCHG
		SHLD	XPOS		;Save position of next write
		XCHG
		CALL	LOG2PHY		;Set the Physical Track and Sector based on the Logical Sector 0=Sector 1, Track 0
		JNC	FXSERR		;Sector out of range
		XCHG
		SHLD	XSECTOR		;Set XSECTOR & XTRACK (16 bit save to two 8 bit variables)


		;LXI	H,TRACK_BUFFER	;Where to receive data
		CALL	XMR_INIT	;Starts the transfer, Sets the CS/CRC format & Receives first PACKET
					;Cancelled Transfers will cause a RET

XMU_DISK_LP:	LDA	XTRACK		;Seek to correct Track (can't be much of a delay if already on correct track)
;		CALL	FSEEK

		;LXI	H,TRACK_BUFFER	;Where to save data
		LDA	XSECTOR		;Read the Sector
;		OUT	DSECTOR
;		MVI	A,0ACh		;WRITE SECTOR (w/ 10mSec Head Load Delay)
;		CALL	WRITE_CMD

		LHLD	XPOS		;Advance to next Logical Sector
		INX	H
		SHLD	XPOS
		XCHG
		CALL	LOG2PHY		;Set the Physical Track and Sector based on the Logical Sector 0=Sector 1, Track 0
		JNC	FXSERR		;Sector out of range
		XCHG
		SHLD	XSECTOR		;Set XSECTOR & XTRACK (16 bit save to two 8 bit variables)

		;LXI	H,TRACK_BUFFER	;Where to receive data
		CALL	XMR_RECV	;Receives the next packet @HL, Resends if NAK
					;Cancelled Transfers will cause a RET

		JC	XMU_DISK_LP	;Jump until EOT Received
		JMP	PURGE





;---------------------------------------------------------------------------------------------------------------------
;Logical Sector to Physical Sector and Track #
;0=Track 0 : Sector 1
;1=0:2, 25=0:26, 26=1:1, 2001=77:26 (Last logical Sector)
;In:	DE = Logical Sector 0 to 2001
;Out:	D  = Track (0-76)
;	E  = Sector (1-26)
;	CY = Set for Valid Log Sec
;Uses:	A
;---------------------------------------------------------------------------------------------------------------------
LOG2PHY:		MOV	A,D
		ANI	0xF8
		RNZ			;Return with CY clear
		PUSH	B
		MVI	B,0		;Track=0
L2LP:		PUSH	D		;Save DE before subtracting 26 sectors per track
		MOV	A,E		;Subtrack 26 from E
		SUI	26
		MOV	E,A
		JNC	L2PNT		;If no borrow occurs, then set track
		DCR	D		;Decrement D
		JM	L2PSS		;If negative, Set Sector
L2PNT:		POP	PSW		;Scrap saved DE, accept DE as it's still positive
		INR	B		;Else, Advance Track
		JMP	L2LP
L2PSS:		POP	D
		INR	E
		MOV	A,B
		MOV	D,A
		CPI	77		;Test for invalid TRACK# (happens
		POP	B		;Return CY set for valid Logical Sector
		RET





;----------------------------------------------------------------------------------------------------; DISASSEMBLER
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
;----------------------------------------------------------------------------------------------------; DISASSEMBLER
DISASM:		CALL	PUT_HL		;Print Address	***CAUTION**** This routine has 3 tables that must NOT cross page boundaries.
		CALL	PRINTI
		.ascii	"  \000"
		MOV	C,M
		PUSH	H
		CALL	DA_LOOKUP	;Print Mnemonic code
		POP	H
		ORA	A		;Test if there are any operands to print
		JZ	DA_NOOP		;Jump if NO operands
		DCR	A
		JNZ	DA_N1OP		;Jump if 2 operands (actually, if NOT 1 operand)
		INX	H
		MOV	A,M		;Print Byte
		CALL	PUT_BYTE
		JMP	DA_NOOP
DA_N1OP:		DCR	A
		JNZ	DA_NOOP
		INX	H		;Print Word (high/low)
		MOV	B,M
		INX	H
		MOV	A,M
		CALL	PUT_BYTE
		MOV	A,B
		CALL	PUT_BYTE
DA_NOOP:		CALL	PUT_NEW_LINE
		INX	H
		RET

					;Print Operand for Machine Code in C
DA_LOOKUP:	LXI	H, TBL_1A
		MVI	A,0xFF		;Bit Mast (Mask no bits)
		LXI	D,5		;DE=LEN of table entry
		CALL	LS_SEARCH	;Search for commands without embedded codes or extra operands.
		JZ	DA_FOUND
		MOV	A,C		;Fetch Code
		ANI	0xC0
		CPI	0x40		;Test for MOV code
		JZ	DA_MOV
		MVI	A,0xCF		;Mask out register pairs
		CALL	LS_SEARCH	;Search for 1C commands (Reg Pairs)
		JZ	DA_FOUND_1C
		CALL	LS_SEARCH	;Search for 3C command (LXI Reg Pairs,Immediate)
		JZ	DA_FOUND_3C
		MVI	A,0xF8		;Mask out SSS register
		CALL	LS_SEARCH	;Search for 1B commands (Source Register)
		JZ	DA_FOUND_1B
		MVI	A,0xC7		;Mask out DDD register
		CALL	LS_SEARCH	;Search for 1B commands (Source Register)
		JZ	DA_FOUND_1D
		CPI	0x06		;Search for MVI command
		JZ	DA_FOUND_2B
		CPI	0xC7		;Search for RST command
		JZ	DA_FOUND_1R
		MVI	A,0xEF
		CALL	LS_SEARCH	;Search for 1E commands (STAX or LDAX Reg Pairs)
		JZ	DA_FOUND_1C
		MVI	A,0xFF
		CALL	LS_SEARCH	;Search for 2A commands (Acc Immediate functions)
		JZ	DA_FOUND_2A
		CALL	LS_SEARCH	;Search for 3C commands (LDA,STA,LHLD,SHLD)
		JZ	DA_FOUND_3D
		MVI	A,0xC7		;Mask out Condition Code & LSB
		MVI	E,2		;DE=LEN of table entry
		CALL	LS_SEARCH	;Search for 3B commands (Jump / Call Commands)
		JZ	DA_FOUND_3B

DA_DB:		CALL	PRINTI		;When all searchs fail, print byte as a DB
		.ascii	"DB   \000"
		MOV	A,C
		CALL	PUT_BYTE
		XRA	A
		RET

DA_FOUND_3D:	CALL	DA_FOUND	;Print opcode
		CALL	PUT_SPACE
		MVI	A,2		;Return to print 16 bit Memory operand
		RET

DA_FOUND_2A:	CALL	DA_FOUND	;Print opcode
		CALL	PUT_SPACE
		MVI	A,1		;Return to print 8 bit immediate Operand
		RET

DA_FOUND_3B:	INX	H
		MOV	A,M		;Print First letter of Op Code (J,C or R)
		CALL	PUT_CHAR
		MOV	A,C		;Fetch Code for DDD
		RRC
		RRC
		LXI	H, TBL_CC
		ANI	0x0E		;Print 2 character Condition Code
		ADD	L
		MOV	L,A
		MOV	A,M
		CALL	PUT_CHAR
		INX	H
		MOV	A,M
		CALL	PUT_CHAR
		CALL	PRINTI
		.ascii	"  \000"
DAF_3B_RET:	MOV	A,C		;Codes that end in 100 or 010 result with 2 Operands
		RRC			;This allows address to be printed for JMP's and CALL's
		ORA	C		;but not RET's
		ANI	2
		RET

TBL_CC:		.ascii	"NZZ NCC POPEP M "

DA_FOUND_1R:	CALL	PRINTI
		.ascii	"RST \000"
		MOV	A,C
		RRC
		RRC
		RRC
		ANI	7
		CALL	PUT_HEX
		XRA	A
		RET

DA_FOUND_2B:	CALL	PRINTI
		.ascii	"MVI  \000"
		CALL	DA_PUT_DDD
		CALL	PRINTI
		.ascii	",\000"
		MVI	A,1
		RET

DA_FOUND_1D:	CALL	DA_FOUND
		CALL	PUT_SPACE
		JMP	DA_PUT_DDD

DA_FOUND_1B:	CALL	DA_FOUND
		CALL	PUT_SPACE
		JMP	DA_PUT_SSS


DA_FOUND_3C:	CALL	DA_FOUND_1C
		CALL	PRINTI
		.ascii	",\000"
		MVI	A,2
		RET

DA_FOUND_1C:	CALL	DA_FOUND	;Print the opcode
		CALL	PUT_SPACE
		CALL	DA_PUT_REGPAIR
		XRA	A
		RET

DA_FOUND:	MOV	B,E		;Prints Opcode (no operands)
		DCR	B
DAF_LP:		INX	H
		MOV	A,M
		ORA	A
		RZ
		CALL	PUT_CHAR
		DCR	B
		JNZ	DAF_LP
		XRA	A
		RET

DA_MOV:		CALL	PRINTI		;Prints MOV Opcode with 2 operands DDD, SSS
		.ascii	"MOV  \000"
		CALL	DA_PUT_DDD
		CALL	PRINTI
		.ascii	",\000"
DA_PUT_SSS:	MOV	A,C		;Fetch Code for SSS
DA_PUT_SSSDDD:	LXI	H, TBL_DDDSSS
		ANI	0x07
		ADD	L
		MOV	L,A
		MOV	A,M
		CALL	PUT_CHAR
		XRA	A
		RET

DA_PUT_DDD:	MOV	A,C		;Fetch Code for DDD
		RRC
		RRC
		RRC
		JMP	DA_PUT_SSSDDD

TBL_DDDSSS:	.ascii	"BCDEHLMA"

DA_PUT_REGPAIR:	MOV	A,C		;Fetch Code for Reg Pair
		RRC
		RRC
		RRC
		LXI	H, TBL_REGPAIR
		ANI	0x0E
		CPI	0x0E		;Test for PSW Reg Pair
		JNZ	DAPR_OK
		MVI	A,9
		JMP	DAPR_OK2
DAPR_OK:		ANI	0x06
DAPR_OK2:	ADD	L
		MOV	L,A
		CALL	PRINT
		XRA	A		;Print returns with A=00
		RET

TBL_REGPAIR:	.ascii	"B\000"
		.ascii	"D\000"
		.ascii	"H\000"
		.ascii	"SP\000"
		.ascii	"PSW\000"


LS_SEARCH:	ANA	C		;Fetch Code (AND with bit mask)
		MOV	B,M		;Count of Elements
		INX	H
LS_LP:		CMP	M
		RZ
		DAD	D
		DCR	B
		JNZ	LS_LP
		DCR	B		;RETURN WITH Z=0
		RET

TBL_1A:		.DB	17		;COUNT OF TABLE ELEMENTS
		.DB	0xEB
		.ascii	"XCHG"
		.DB	0xE3
		.ascii	"XTHL"
		.DB	0xF9
		.ascii	"SPHL"
		.DB	0xE9
		.ascii	"PCHL"
		.DB	0x07
		.ascii	"RLC "
		.DB	0x0F
		.ascii	"RRC "
		.DB	0x17
		.ascii	"RAL "
		.DB	0x1F
		.ascii	"RAR "
		.DB	0x2F
		.ascii	"CMA "
		.DB	0x37
		.ascii	"STC "
		.DB	0x3F
		.ascii	"CMC "
		.DB	0x27
		.ascii	"DAA "
		.DB	0xFB
		.ascii	"EI  "
		.DB	0xF3
		.ascii	"DI  "
		.DB	0x00
		.ascii	"NOP "
		.DB	0x76
		.ascii	"HLT "
		.DB	0xC9
		.ascii	"RET "
TBL_1C:		.DB	5		;COUNT OF TABLE ELEMENTS
		.DB	0xC5
		.ascii	"PUSH"
		.DB	0xC1
		.ascii	"POP "
		.DB	0x03
		.ascii	"INX "
		.DB	0x0B
		.ascii	"DCX "
		.DB	0x09
		.ascii	"DAD "
TBL_3C:		.DB	1		;COUNT OF TABLE ELEMENTS
		.DB	0x01
		.ascii	"LXI "
TBL_1B:		.DB	8		;COUNT OF TABLE ELEMENTS
		.DB	0x80
		.ascii	"ADD "
		.DB	0x88
		.ascii	"ADC "
		.DB	0x90
		.ascii	"SUB "
		.DB	0x98
		.ascii	"SBB "
		.DB	0xA0
		.ascii	"ANA "
		.DB	0xA8
		.ascii	"XRA "
		.DB	0xB0
		.ascii	"ORA "
		.DB	0xB8
		.ascii	"CMP "
TBL_1D:		.DB	2		;COUNT OF TABLE ELEMENTS
		.DB	0x04
		.ascii	"INR "
		.DB	0x05
		.ascii	"DCR "
TBL_1E:		.DB	2		;COUNT OF TABLE ELEMENTS
		.DB	0x02
		.ascii	"STAX"
		.DB	0x0A
		.ascii	"LDAX"
TBL_2A:		.DB	10		;COUNT OF TABLE ELEMENTS
		.DB	0xC6
		.ascii	"ADI "
		.DB	0xCE
		.ascii	"ACI "
		.DB	0xD6
		.ascii	"SUI "
		.DB	0xDE
		.ascii	"SBI "
		.DB	0xE6
		.ascii	"ANI "
		.DB	0xEE
		.ascii	"XRI "
		.DB	0xF6
		.ascii	"ORI "
		.DB	0xFE
		.ascii	"CPI "
		.DB	0xDB
		.ascii	"IN  "
		.DB	0xD3
		.ascii	"OUT "
TBL_3D:		.DB	6		;COUNT OF TABLE ELEMENTS
		.DB	0x32
		.ascii	"STA "
		.DB	0x3A
		.ascii	"LDA "
		.DB	0x22
		.ascii	"SHLD"
		.DB	0x2A
		.ascii	"LHLD"
		.DB	0xC3
		.ascii	"JMP "
		.DB	0xCD
		.ascii	"CALL"
TBL_3B:		.DB	3		;COUNT OF TABLE ELEMENTS
		.DB	0xC2
		.ascii	"J"
		.DB	0xC4
		.ascii	"C"
		.DB	0xC0
		.ascii	"R"


;----------------------------------------------------------------------------------------------------; CP/M BIOS
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
;----------------------------------------------------------------------------------------------------; CP/M BIOS



;***************************************************
;*** THIS BEGINS THE AREA WHICH REQUIRES CHANGES ***
;***      FOR DIFFERENT CONSOLE I/O SYSTEMS      ***
;***************************************************

MSIZE		.EQU  61	;MEMORY SIZE IN KBYTES.

;*******************************************************
;*** THIS IS THE END OF THE AREA WHICH NORMALLY NEED ***
;***     BE CHANGED FOR MOST CONSOLE I/O SYSTEMS     ***
;*******************************************************

IOBYTE		.EQU  3		;ADDRESS OF I/O BYTE.
CCP		.EQU  (MSIZE-7)*1024	;START OF CPM (D800)
BDOS		.EQU  CCP+0x0806		;START OF BDOS (E006)
BIOS		.EQU  CCP+0x1600		;START OF BIOS (EE00)
CPMLEN		.EQU  BIOS-CCP		;LENGTH OF CPM SYSTEM (LESS BIOS)
NSECTS		.EQU  CPMLEN/128	;NUMBER OF SECTORS IN IT.

;       Page Zero Definitions.
IOBYTE		.EQU	3		;Location of IOBYTE
CDISK		.EQU	4		;Location of current disk
BIORAM		.EQU	0x40		;16 ram cells
OPTS		.EQU	BIORAM		;GBC DISK1 board switch options
;			BIORAM+1	;GBC (cell before TICK)
TICK		.EQU	BIORAM+2	;GBC Sample period
DBUF		.EQU	0x80		;Default sector buffer

		.ORG	BIOS

; I/O JUMP VECTOR
; THIS IS WHERE CP/M CALLS WHENEVER IT NEEDS
; TO DO ANY INPUT/OUTPUT OPERATION.
; USER PROGRAMS MAY USE THESE ENTRY POINTS
; ALSO, BUT NOTE THAT THE LOCATION OF THIS
; VECTOR CHANGES WITH THE MEMORY SIZE.
;
CBOOTV:	JMP  	CBOOT	;FROM COLD START LOADER.
WBOOTV:	JMP  	WBOOT	;FROM WARM BOOT.
CONSTV:	JMP  	CONST	;CHECK CONSOLE KB STATUS.
	JMP  	CONIN	;READ CONSOLE CHARACTER.
	JMP  	CONOT	;WRITE CONSOLE CHARACTER.
	JMP  	LIST	;WRITE LISTING CHAR.
	JMP  	PUNCH	;WRITE PUNCH CHAR.
	JMP  	READER	;READ READER CHAR.
	JMP  	HOME	;MOVE DISK TO TRACK ZERO.
SELDSKV:	JMP  	SELDSK	;SELECT DISK DRIVE.
	JMP  	SETTRK	;SEEK TO TRACK IN REG A.
	JMP  	SETSEC	;SET SECTOR NUMBER.
SETDMAV:	JMP  	SETDMA	;SET DISK STARTING ADR.
	JMP  	DREAD	;READ SELECTED SECTOR.
	JMP  	DWRITE	;WRITE SELECTED SECTOR.
	JMP  	LISTST	;List status (output)
	JMP  	SECTRN	;Translate sector number

					;My own BIOS routine entries
	JMP  	GET_CHAR_UART
	JMP  	GET_CHAR
	JMP  	PUT_CHAR
	JMP  	GET_CHAR_NE


NDSK	.EQU	4

;	Control Blocks for disk drives

DPBASE:	.DW	SKEW,0,0,0,DIRBUF,DPB0,CSV0,ALV0	;Drive A:
	.DW	SKEW,0,0,0,DIRBUF,DPB1,CSV1,ALV1	;Drive B:
	.DW	SKEW,0,0,0,DIRBUF,DPB2,CSV2,ALV2	;Drive C:
	.DW	SKEW,0,0,0,DIRBUF,DPB3,CSV3,ALV3	;Drive D:

;SKEW:	.DB 	01, 07, 13, 19, 25, 05, 11, 17, 23, 03, 09, 15, 21, 02, 08, 14, 20, 26, 06, 12, 18, 24, 04, 10, 16, 22
SKEW	.equ	0

;       Disk type definition blocks for each particular mode.
DPB0:	.DW	26		;SEC PER TRACK
	.DB	3		;BLOCK SHIFT
	.DB	7		;BLOCK MASK
	.DB	0		;EXTNT MASK
	.DW	242		;DISK SIZE-1
	.DW	63		;DIRECTORY MAX
	.DB	0b11000000	;ALLOC0
	.DB	0b00000000	;ALLOC1
	.DW	16		;CHECK SIZE
	.DW	2		;OFFSET

DPB1:	.DW	26		;SEC PER TRACK
	.DB	3		;BLOCK SHIFT
	.DB	7		;BLOCK MASK
	.DB	0		;EXTNT MASK
	.DW	242		;DISK SIZE-1
	.DW	63		;DIRECTORY MAX
	.DB	0b11000000	;ALLOC0
	.DB	0b00000000	;ALLOC1
	.DW	16		;CHECK SIZE
	.DW	2		;OFFSET

DPB2:	.DW	26		;SEC PER TRACK
	.DB	3		;BLOCK SHIFT
	.DB	7		;BLOCK MASK
	.DB	0		;EXTNT MASK
	.DW	242		;DISK SIZE-1
	.DW	63		;DIRECTORY MAX
	.DB	0b11000000	;ALLOC0
	.DB	0b00000000	;ALLOC1
	.DW	16		;CHECK SIZE
	.DW	2		;OFFSET

DPB3:	.DW	26		;SEC PER TRACK
	.DB	3		;BLOCK SHIFT
	.DB	7		;BLOCK MASK
	.DB	0		;EXTNT MASK
	.DW	242		;DISK SIZE-1
	.DW	63		;DIRECTORY MAX
	.DB	0b11000000	;ALLOC0
	.DB	0b00000000	;ALLOC1
	.DW	16		;CHECK SIZE
	.DW	2		;OFFSET

		;Amstrad 1.44M disk
;DPB33:	.DW	48h		;SEC PER TRACK
;	.DB	5		;BLOCK SHIFT = 4K Block size
;	.DB	0x1f		;BLOCK MASK
;	.DB	1		;EXTNT MASK
;	.DW	164H		;DISK SIZE-1
;	.DW	0ffh		;DIRECTORY MAX
;	.DB	0c0h		;ALLOC0
;	.DB	00		;ALLOC1
;	.DW	0x40		;CHECK SIZE
;	.DW	1		;OFFSET

;3.5" DSDD    80*2*18*512 = 1,474,560   1.44M
;5"   DSHD    80*2*15*512 = 1,228,800   1.2 M
;5"   DSDD    40*2* 9*512 = 368,640      360K
;5"   DSDD    40*2* 8*512 = 327,680      320K
;5"   DSDD    40*2*10*512 = 409,600      400K
;5"   DSSD    40*2*18*128 = 184,320      180K
;8"   DSDD    77*2*8*1024 = 1,261,568   1232K
;8"   SSDD    77*1*8*1024 = 630,784      616K
;8"   DSDD    77*2*15*512 = 1,182,720   1155K
;8"   DSDD    77*2*26*256 = 1,025,024   1001K
;8"   SSSD    77*1*26*128 = 256,256      250K


;       B O O T   C P / M   f r o m   d i s k.
;
;       The CBOOT entry point gets control from the cold start
;       loader and is responsible for the basic system initial-
;       ization.  This includes outputting a sign-on message and
;       initializing the following page zero locations:
;
;          0,1,2: Set to the warmstart jump vector.
;              3: Set to the initial IOBYTE value.
;              4: Default and logged on drive.
;          5,6,7: Set to a jump to BDOS.
;             40: (Set by BOOT) Board switch options.
;
;       If BANG is true (DISK1 bit serial latch is to be supported), then
;       board switch option I means to use the BitBanger for console I/O.
;       Register C must contain the selected drive, which is zero to
;       select the A drive.  The exit address is to the CCP routine.
;
;
;       The WBOOT entry point gets control when a warm start occurs,
;       a ^C from the console, a jump to BDOS (function 0), or a jump to
;       location zero.  The WBOOT routine reads the CCP and BDOS from the
;       appropriate disk sectors.  WBOOT must also re-initialize locations
;       0,1,2 and 5,6,7.  The WBOOT routines exits with the C register set
;       to the appropriate drive selection value.  The exit address is to
;       the CCP routine.
;
CBOOT:	LXI  SP,HIGHSTACK	;SET STACK POINTER.
	CALL PRINTI
	.ascii "\r\nALTAIR/IMSAI CPU CARD. "
	.DB   (MSIZE / 10) + "0", (MSIZE % 10) + "0"
	.ascii "K CP/M 2.2\r\n\000"

        XRA     A
        STA     CDISK           ;Force A drive
        STA     IOBYTE          ;Clear I/O byte

; WARM-BOOT:  READ ALL OF CPM BACK IN
; EXCEPT BIOS, THEN JUMP TO CCP.
;
WBOOT:	LXI  SP,HIGHSTACK	;SET STACK POINTER.

	CALL PRINTI
	.ascii "\r\nWBOOT\r\n\000"

        		      ;Boot CP/M

	LDA  CDISK	;SAVE DISK NUMBER.
	STA  TEMP
	MVI	C,0	;Set DISK A
	CALL	SELDSKV

	LXI	D,CCP		;Save destination address
	MVI	B,NSECTS
	LXI	H,1
WBLP:	SHLD	LOGICAL_SEC	;Set first sector to read from disk
	PUSH	B
	PUSH	D
	CALL	DISK_READ	;HL = Quarter Buffer
	POP	D
	MVI	B,128
	CALL	COPY_RAM	;Copy the SD_RAM_BUFFER to CP/M
	LHLD	LOGICAL_SEC	;Set first sector to read from disk
	INX	H
	POP	B
	DCR	B
	JNZ	WBLP

; SET UP JUMPS IN CP/M PAGE ZERO.

	LXI  B,DBUF	;SET DEFAULT DMA ADR.
	CALL SETDMAV
	MVI  A,0xC3	;PUT JMP TO WBOOT
	STA  0		;ADR AT ZERO.
	STA  5
	LXI  H,WBOOTV
	SHLD 1
	LXI  H,BDOS	;PUT JUMP TO BDOS
	SHLD 6		;AT ADR 5,6,7.

	LDA  TEMP
	STA  CDISK
        MOV  C,A
        JMP  CCP             ;Go to CPM


;===============================================
;       C O N S O L   S T A T U S
;
;       This routine samples the Console status and
;       returns the following values in the A register.
;
;       EXIT    A = 0 (zero), means no character
;               currently ready to read.
;
;               A = FFh (255), means character
;               currently ready to read.
;-----------------------------------------------
CONST:		IN	UART0+5 ;10	TEST FOR RX DATA
		ANI	1	;7
		JZ	CONST_0	;10
		ORI	0xFF
		RET		;10

CONST_0:		LDA	PROP_CHECK
		ORA	A
		RZ

		IN	PROPELLERS
		ANI	2
		RZ
		ORI	0xFF
		RET


;===============================================
;       C O N S O L   I N P U T
;
;       Read the next character into the A register, clearing the high
;       order bit.  If no character currently ready to read then wait
;       for a character to arrive before returning.
;
;       EXIT    A = character read from terminal.
;-----------------------------------------------
CONIN:	CALL	GET_CHAR_NE	;Get Char No Echo
	ANI  0x7F	;MAKE MOST SIG. BIT = 0.
	CPI  0x7F	;IS IT A RUBOUT?
	RNZ		;RETURN IF NOT.
	STA  CONOTF	;SET NO PRINT FLAG.
	RET		;RETURN FROM CONIN.

;===============================================
;       C O N S O L   O U T P U T
;
;       Send a character to the console.  If the console is not ready to
;       receive a character wait until the console is ready.
;
;       ENTRY   C = ASCII character to output to console.
;-----------------------------------------------
CONOT:	MOV  A,C	;GET CHARACTER.
	CPI  0x7F	;IS IT A RUBOUT?
	RZ		;IF SO, DON'T PRINT IT.
	LDA  CONOTF	;GET NO PRINT FLAG.
	ORA  A		;SET CPU FLAGS.
	JZ   CONOTA	;NOT SET, SO PRINT.
	XRA  A		;RESET THE FLAG
	STA  CONOTF	;TO ZERO.
	MVI  C,8	;PRINT BACKSPACE.
	CALL CONOTA
	MVI  A,0x20	;PRINT SPACE.
	CALL PUT_CHAR
			;ANOTHER BACKSPACE.
CONOTA:	MOV  A,C	;GET CHARACTER.
	CALL PUT_CHAR	;PRINT IT.
	RET		;RETURN.


;
; WRITE A CHARACTER ON LISTING DEVICE.
;
LIST:

LTBSY:	IN	UART1+5
	ANI	0x20	;TEST FOR TX HOLD REG EMPTY
	JZ	LTBSY

	MOV  A,C	;GET DATA BYTE.
	OUT	UART1	;PRINT IT.
	RET		;RETURN FROM LIST.


;
; PUNCH PAPER TAPE.
;
PUNCH:	RET		;RETURN FROM PUNCH.

;
;  NORMALLY USED TO READ PAPER TAPE.
;
READER:	RET		;RETURN FROM READER.

LISTST:	RET


;       S E L E C T   D I S K   D R I V E
;
;       Select the disk drive for subsequent disk transfers and
;       return the appropriate DPH address.
;
;       ENTRY   C = disk Selection value.
;
;       EXIT    HL = 0, if drive not selectable.
;               HL = DPH address if drive is selectable.
;
SELDSK:	LXI  H,0
	MOV  A,C	;GET NEW DISK NUMBER.
	CPI  NDSK
	RNC		;If Disk invalid...EXIT


.if (DEBUG & 0x80)
   CALL SELECT_UART1
.endif
.if (DEBUG & 1)
	CALL PRINTI		;DEBUG
	.ascii " :SEL\000"
	CALL	PUT_BYTE
.endif
.if (DEBUG & 0x80)
   CALL SELECT_UART0
.endif

	PUSH B
	PUSH D
	CALL SET_FCB_PTR ;Set the FCB PTR for the correct SD File & HL = DPBASE[A] (or 0000 if drive not exist)
	POP  D
	PUSH H
	LXI  B,10
	DAD  B
	CALL LD_HL_HL
	CALL LD_HL_HL
	SHLD SEC_PER_TRACK


.if (DEBUG & 0x80)
   CALL SELECT_UART1
.endif
.if (DEBUG & 1)
	CALL PRINTI		;DEBUG
	.ascii " :SPT\000"
	CALL	PUT_HL
.endif
.if (DEBUG & 0x80)
   CALL SELECT_UART0
.endif

	POP  H
	POP  B
	XRA  A		;SET A = 0.
	RET		;RETURN FROM SELDSK.


; MOVE DISK TO TRACK ZERO.
;
HOME:	MVI  C,0	;SEEK TO TRACK ZERO.

;
; SET TRACK NUMBER TO WHATEVER IS IN REGISTER C.
; ALSO PERFORM MOVE TO THE CORRECT TRACK (SEEK).
;
SETTRK:	MOV  H,B	;GET NEW TRACK NUMBER.
	MOV  L,C	;MOVE B&C TO H&L.
	SHLD  TRK	;UPDATE OLD WITH NEW.

; MOVE THE HEAD TO THE TRACK IN REGISTER A.


.if (DEBUG & 0x80)
   CALL SELECT_UART1
.endif
.if (DEBUG & 1)
	CALL PRINTI		;DEBUG
	.ascii " :TRK\000"
	CALL	PUT_HL
.endif
.if (DEBUG & 0x80)
   CALL SELECT_UART0
.endif


	XRA  A		;Clear flags
	RET		;RETURN FROM SEEK.

;
; SET DISK SECTOR NUMBER.
;
SETSEC:	MOV  A,C	;GET SECTOR NUMBER.
	STA  SECT	;PUT AT SECT # ADDRESS.



.if (DEBUG & 0x80)
   CALL SELECT_UART1
.endif
.if (DEBUG & 1)
	CALL PRINTI		;DEBUG
	.ascii " :SEC\000"
	CALL	PUT_BYTE
.endif
.if (DEBUG & 0x80)
   CALL SELECT_UART0
.endif


	XRA  A		;Clear flags
	RET		;RETURN FROM SETSEC.
;
; SET DISK DMA ADDRESS.
;
SETDMA:	PUSH H
	MOV  H,B	;MOVE B&C TO H&L.
	MOV  L,C
	SHLD DMAADD	;PUT AT DMA ADR ADDRESS.
	POP  H
	RET		;RETURN FROM SETDMA.


;       Translate sector number from logical to physical.
;
;       ENTRY   DE = 0, no translation required.
;               DE = translation table address.
;               BC = sector number to translate.
;
;       EXIT    HL = translated sector.

SECTRN:	MOV     L,C	;No Translation
	MOV     H,B
	RET

	;CALL PRINTI		;DEBUG
	;.ascii " :TR-\000"
	;CALL	PUT_BC
	;CALL	PUT_DE

	;XCHG	;HL=DE
	;DAD B	;HL=DE + BC
	;MOV L,M ;A=M(HL)
	;MVI H,0
	;DCR L
	;RET






;
; READ THE SECTOR AT SECT, FROM THE PRESENT DISK/TRACK/SSECT.
; USE STARTING ADDRESS AT DMAADD.
;
DREAD:	PUSH	H	;Save HL
	LXI	H,0
	DAD	SP	;HL = SP
	LXI	SP,TEMP_STACK
	PUSH	H	;Save SP on new stack

	PUSH	B
	PUSH	D
	CALL 	GETLOG	;Fetch Logical FSector of requested read


.if (DEBUG & 0x80)
   CALL SELECT_UART1
.endif
.if (DEBUG & 1)
	CALL PRINTI		;DEBUG
	.ascii " :DR\000"
	CALL	PUT_HL
.endif
.if (DEBUG & 0x80)
   CALL SELECT_UART0
.endif


	;LXI	D,-2002
	;DAD	D
	;JC	DREADE

	CALL	DISK_READ	;HL = Quarter Buffer
	XCHG
	LHLD	DMAADD	;Destination, CP/M Data Buffer
	XCHG
	MVI	B,128
	CALL	COPY_RAM	;Copy the SD_RAM_BUFFER to CP/M DMAADD
	XRA  A		;SET FLAGS.
	JMP	DREADX

DREADE:	MVI	A,1	;ERROR
	ORA	A

DREADX:	POP	D
	POP	B
	POP	H	;Restore Stack Pointer
	SPHL
	POP	H	;Restore HL
	RET



;
; WRITE THE SECTOR AT SECT, ON THE PRESENT TRACK.
; USE STARTING ADDRESS AT DMAADD.
;
DWRITE:	PUSH	H	;Save HL
	LXI	H,0
	DAD	SP	;HL = SP
	LXI	SP,TEMP_STACK
	PUSH	H	;Save SP on new stack

	PUSH	PSW
	PUSH	B
	PUSH	D
	CALL 	GETLOG	;Fetch Logical FSector of requested write
			;DISK_READ will find the right Cluster/File sector *AND* Flush any previous writes.


.if (DEBUG & 0x80)
   CALL SELECT_UART1
.endif
.if (DEBUG & 1)
	CALL PRINTI		;DEBUG
	.ascii " :DW\000"
	CALL	PUT_HL
.endif
.if (DEBUG & 0x80)
   CALL SELECT_UART0
.endif


	CALL	DISK_READ	;HL = Quarter Buffer
	XCHG
	LHLD	DMAADD		;Source, CP/M Data Buffer
	MVI	B,128
	CALL	COPY_RAM	;Copy the SD_RAM_BUFFER to CP/M DMAADD
	MVI	A,0xFF
	STA	DIRTY_DATA
	POP	D
	POP	B
	POP	PSW
	XRA  A		;SET FLAGS.
	POP	H	;Restore Stack Pointer
	SPHL
	POP	H	;Restore HL
	RET

; Return Logical Disk Sector based on TRACK and SECTOR
; Track starts at 0 for first track
; Sector starts at 0 for first sector
;
GETLOG:	LHLD  TRK	;HL = TRK


;-------------------------------------	Multiply Routine.  16bit by 8 bit -> 24bit
					;HL = 16bit input
		LXI	D,0		;DE = 16bit output
		MVI	B,8		;Go through 8 bits
		LDA	SEC_PER_TRACK	;Fetch Multiplier
GL_LP:		RAR
		JNC	GL_SHIFT
		XCHG
		DAD	D		;DE=DE+HL
		XCHG

GL_SHIFT:	DAD	H		;BHL=BHL*2
		DCR	B		;Count down 8 bits
		JNZ	GL_LP



	MVI  H,0
	LDA  SECT
	;DCR  A		;Option for Sector to start at 1
	MOV  L,A
	DAD  D		;HL= TRK * SPT + SECT = 0000 to 07D1 (0 TO 2001)
	SHLD	LOGICAL_SEC
	RET



;----------------------------------------------------------------------------------------------------; CONSOLE BIOS
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
;----------------------------------------------------------------------------------------------------; CONSOLE BIOS

;===============================================
;PUT_NEW_LINE -- Start a new line on the console
;
;pre: none
;post: 0x0A printed to console
;-----------------------------------------------
PUT_NEW_LINE:	CALL	PRINTI
		.ascii "\r\n\000"
		RET

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
PUT_HEX:	CALL	BIN2HEX
		JMP	PUT_CHAR

BIN2HEX:		ANI	0x0F
		ADI	0x90
		DAA
		ACI	0x40
		DAA
		RET

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
;GET_CHAR_UART -- Get Char from UART
;-----------------------------------------------
GET_CHAR_UART:	IN	UART0+5	;10	;TEST FOR RX DATA
		ANI	1	;7
		JNZ	GCU_UART ;10

		LDA	PROP_CHECK
		ORA	A
		JZ	GET_CHAR_UART

		IN	PROPELLERS
		ANI	2
		JZ	GET_CHAR_UART

		IN	PROPELLERD ;Char from Propeller ready
		ORA	A
		JZ	GCU_PROPSTRIKE
		CPI	0xFF
		JZ	GCU_PROPSTRIKE
		RET

GCU_PROPSTRIKE:	LDA	PROP_CHECK
		DCR	A
		STA	PROP_CHECK
		JMP	GET_CHAR_UART

GCU_UART:	IN	UART0	;Char from UART ready
		RET

;===============================================
;GET_CHAR -- Get a char from the console
;-----------------------------------------------
GET_CHAR:	LDA	ECHO_ON
		ORA	A
		JZ	GET_CHAR_NE
GET_CHAR_LP:	CALL	CONSTV	;TEST FOR RX DATA
		JZ	GET_CHAR_LP
		CALL	GET_CHAR_UART
		CPI	" "	;Do not echo control chars
		RM
		;RET		;ECHO THE CHAR

;===============================================
;PUT_CHAR -- Output a character to the console
;-----------------------------------------------
PUT_CHAR:	PUSH	PSW

		;XRA	A		;256 ~= 2mSec Delay
		;CALL	SD_DELAY

PC_LP:		IN	UART0+5
PCU5		.EQU	.-1
		ANI	0x20	;TEST FOR TX HOLD REG EMPTY
		JZ	PC_LP
		POP	PSW
		OUT	UART0
PCU0		.EQU	.-1
		PUSH	PSW
		LDA	PROP_CHECK
		ORA	A
		JZ	PC_EXIT
		PUSH	H
		LXI	H,0x2000
PC_LP2:		DCX	H	;PUT A TIME LIMIT ON PROP STATUS
		MOV	A,H
		ORA	L
		JZ	PC_TO
		IN	PROPELLERS
		ANI	4
		JZ	PC_LP2
PC_TO:		POP	H
PC_EXIT:		POP	PSW
		OUT	PROPELLERD
		RET

SELECT_UART0:
	PUSH	PSW
	MVI	A,UART0+5
	STA	PCU5
	MVI	A,UART0
	STA	PCU0
	POP	PSW
	RET

SELECT_UART1:
	PUSH	PSW
	MVI	A,UART1+5
	STA	PCU5
	MVI	A,UART1
	STA	PCU0
	POP	PSW
	RET

;===============================================
;GET_CHAR -- Get a char from the console NO ECHO
;-----------------------------------------------
GET_CHAR_NE:	CALL	CONSTV	;TEST FOR RX DATA
		JZ	GET_CHAR_NE
		CALL	GET_CHAR_UART
		RET



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
;Set FCB Pointer to one of 4 FCB blocks
;=====================================================================================================
SET_FCB_PTR:	PUSH	B
		ANI	0x03		;Limit & Clear Carry
		RRC			;Set A x 32 to offset into correct FCB
		RRC
		RRC
		MOV	C,A
		MVI	B,0
		LXI	H,SDISKA
		DAD	B		;HL = FCB[A]
		SHLD	FCB_PTR
		RRC			;Set A x 16
		MOV	C,A		;BC = A x 16
		MOV	A,M		;Fetch FCB FSTAT (Is File OPEN / Exist)
		LXI	H,0		;HL = 0000 File (disk) not ready
		ORA	A
		JZ	SFPE
		LXI	H,DPBASE
		DAD	B		;HL = DPBASE[A]
SFPE:		POP	B
		RET


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
SD_TEST:		XRA	A		;EXIT Z=0 if there is a File at this entry
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
;Input:	LOGICAL_SEC = 0=First Sector, 2001=Last Sector based on 26 Sectors/Track and 77 Tracks
;	FCB_PTR	= Points to Drive (File) selected to read from
DISK_READ:
;		CALL	PRINTI
;		.ascii " R-\000"
;		LHLD	LOGICAL_SEC
;		CALL	PUT_HL

		LHLD	FCB_PTR	;Get Current Disk FCB
		MOV	A,M		;Is file open?
		ORA	A		;Test FSTAT
		JNZ	DR_1		;Jump YES

;		CALL	PRINTI
;		.ascii " HL:\000"
;		CALL	PUT_HL

		CALL	SD_OPEN		;ELSE, Attempt to open file
		LHLD	FCB_PTR
		MOV	A,M		;Is file open?
		ORA	A		;Test FSTAT
		JNZ	DR_1		;Jump YES
		CALL	PRINTI
		.ascii " -Disk Not Loaded\000"
		RET			;Exit if file could not open

DR_1:		;LHLD	FCB_PTR		;If file open, Check if Read is from same Data Sector (4 LSectors fit in 1 DSector)
		LXI	B,RFSec
		DAD	B		;H=FCB(RFSec)
		MOV	E,M
		INX	H
		MOV	D,M		;D=RFSec
		LHLD	LOGICAL_SEC	;Fetch sector to be read
		MVI	B,2
		CALL	RHLR		;Rotate HL right 2 times
		CALL	CMP_DE_HL
		JNZ	DR_NEW_SEC	;Jump if Read is from a different Data Sector

					;LOGICAL SECTOR = LAST READ SECTOR, Fetch Absolute Sector and read it to RAM (if wasn't last read)
		LHLD	FCB_PTR		;H=FCB
		LXI	B,ABS_SEC
		DAD	B		;H=FCB(ABS_SEC)

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
		XCHG			;D=LOGICAL_SEC/4 = Relative File Sector (Update FCB with this new Rel-File-Sec
		LHLD	FCB_PTR		;Set FCB(RFSec)
		LXI	B,RFSec
		DAD	B		;H=FCB(RFSec)
		MOV	M,E		;MOV (HL),DE
		INX	H
		MOV	M,D

		LHLD	DIVIDE_FUNC	;DE = DE / Sectors-Per-Cluster
		CALL	VCALL
		LHLD	FCB_PTR		;Set FCB(CRFClus)
		LXI	B,CRFClus
		DAD	B		;H=FCB(CRFClus)
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
		JZ	SDO_FSERR2	;Error, File too small
		DCX	H

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

SDO_FSERR2:	CALL	PRINTI
		.ascii " -!EOF!\000"
		RET


DR_SEEK_DONE:	;Write Registers to FCB
		;BC = CRFClus
		;DE = RFClus - Not required (it's a counter down to zero to find the correct cluster)
		;HL = CAFClus

		XCHG			;DE = CAFClus
		LHLD	FCB_PTR		;Set FCB(CRFClus)
		PUSH	D
		LXI	D,CRFClus
		DAD	D		;H=FCB(CRFClus)
		POP	D
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
DRSS_LP:		RAR
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
		LHLD	FCB_PTR		;Set FCB(SSOC)
		PUSH	B
		LXI	B,SSOC
		DAD	B		;H=FCB(SSOC)
		POP	B
		CALL	MOV_HL_32	;Save the 32 bit register BCDE to (HL)
;-------

	;ABS_SEC = SSOC + (RFSec MOD SEC_PER_CLUS), then read ABS_SEC into the buffer.
DR_SAME_CLUS:				;Fetch the RFSec
		LHLD	FCB_PTR		;Set FCB(RFSec)
		LXI	B,RFSec
		DAD	B		;H=FCB(RFSec)

		MOV	E,M		;DE=RFSec
		INX	H
		MOV	D,M

		LHLD	MOD_FUNC	;DE = DE % Sectors-Per-Cluster
		CALL	VCALL		;A = RFSec MOD SEC_PER_CLUS

		LHLD	FCB_PTR		;Set FCB(SSOC)
		LXI	B,SSOC
		DAD	B		;H=FCB(SSOC)
		LXI	B,0		;BCDE = (RFSec MOD SEC_PER_CLUS)
		MVI	D,0
		MOV	E,A
		CALL	ADD_32_HL	;BCDE = SSOC + (RFSec MOD SEC_PER_CLUS)

		INX	H		;Advance HL to ABS_SEC
		CALL	MOV_HL_32	;Save the ABS_SEC

DR_READ_IT:	CALL	SD_READ_SEC	;Fetch the Sector

;--------- Set HL to the 128 byte Quarter Buffer of SD_RAM_BUFFER
SET_QUARTER:	LXI	H,SD_RAM_BUFFER
		LDA	LOGICAL_SEC	;Fetch sector to be read (lsb)
		RAR			;Adjust H to correct LSector of 128 Bytes
		RAR
		JNC	SQ_0
		INR	H
SQ_0:		RAL
		MVI	B,128		;Preset Length of copy = 128 Bytes
		RNC
		MVI	L,0x80
		RET


;=====================================================================================================
		;Open File (Mount Disk)	;Input:	FCB_PTR = FCB
		;Tests if file exists, right size, then copies the Starting Cluster to the FCB
SD_OPENT:	LHLD	FCB_PTR
		MOV	A,M		;Is file open?
		ANI	0xCF		;Test FSTAT
		JZ	SD_OPEN
		CALL	PRINTI
		.ascii " -FILE ALREADY OPEN\000"
		RET

;SD_OPEN   -Open FAT-16 file on SD card
;Searches Directory for file - Returns Zero flag if File not found.

SD_OPEN:		LHLD	FCB_PTR
		MVI	M,0		;FSTAT=0, Clear Open Status
		INX	H		;+1 = FNAME
		LXI	D,FILENAME	;Write FCB File name to FILENAME for finding
		MVI	B,11
		CALL	COPY_RAM
		CALL	SDV_FIND_FILE	;Return with H=Directory Entry
		RZ			;Exit if file not found

		LXI	B,0x001A		;START CLUSTER & File Size Offset (into Directory Entry)
		DAD	B		;H=(FILE SIZE)
		LXI	D,CLUSTER1
		MVI	B,6
		CALL	COPY_RAM

		LHLD	CLUSTER1
		XCHG			;DE=CLUSTER1
		LHLD	FCB_PTR		;H=FCB
		MVI	M,1		;FSTAT=1
		LXI	B,AFClus0	;offset to AFClus0
		DAD	B
		MOV	M,E		;Save Starting Cluster
		INX	H
		MOV	M,D
		INX	H
		MVI	B,14
		MVI	A,0xFF
		CALL	FILL_BLOCK	;Fill 14 bytes of FF (Nuke pointers to force new calculations)
		LHLD	FCB_PTR		;H=FCB
		DCR	A		;Z=0
		RET




;-----------------------------------------------------------------------------------------------------
CMP_FILENAME:	PUSH	H		;Save H pointer into Directory
		MVI	B,11		;Compare 11 characters
		LXI	D,FILENAME
CMPF_LP1:	LDAX	D
		CMP	M
		JNZ	CMPF_RETFAIL	;Exit if not equal
		INX	H
		INX	D
		DCR	B
		JNZ	CMPF_LP1

		INR	B		;Z=0
		POP	H
		RET
CMPF_RETFAIL:	XRA	A		;Z=1
		POP	H
		RET




;		MVI	B,8		;Compare 8 characters
;		LXI	D,FILENAME
;CMPF_LP1:	LDAX	D
;		CMP	M
;		JNZ	CMPF_RETFAIL	;Exit if not equal
;		INX	H
;		INX	D
;		DCR	B
;		JNZ	CMPF_LP1
;
;		POP	H
;		PUSH	H
;		LXI	D,8		;Point to Ext in Dir
;		DAD	D
;
;		MVI	B,3
;		LXI	D,FILEEXT
;CMPF_LP2:	LDAX	D
;		CMP	M
;		JNZ	CMPF_RETFAIL
;		INX	H
;		INX	D
;		DCR	B
;		JNZ	CMPF_LP2
;		INR	B		;Z=0
;		POP	H
;		RET
;CMPF_RETFAIL:	XRA	A		;Z=1
;		POP	H
;		RET



;=====================================================================================================
;=====================================================================================================
INIT_FAT:	LXI	H,CLEAR_RAM	;Clear RAM
		MVI	B,0
		XRA	A
		CALL	FILL_BLOCK

		;XRA	A		;Flag no more Dirty Data
		;STA	DIRTY_DATA	;which would require flushing changed data to SD Memory Card
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

		LXI	H,SD_RAM_BUFFER+0x001C
		CALL	COPY_RAM4	;Copy HIDDEN_SECTORS to RAM Variables
		LXI	H,SD_RAM_BUFFER+0x0016
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
		LXI	H,0xFE2B
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

SD_RS_FORCED:	LDA	DIRTY_DATA	;Test if flush required
		ORA	A
		JZ	SD_RS_NC	;Jump if no change in SD RAM BUFFER
		XRA	A
		STA	DIRTY_DATA	;Clear Write Flag

		PUSH	B
		PUSH	D
		PUSH	H
		CALL	MOV_32_HL	;Fetch the last SEC_PTR


.if (DEBUG & 0x80)
   CALL SELECT_UART1
.endif
.if (DEBUG & 2)
	CALL PRINTI		;DEBUG
	.ascii " Write:\000"
	CALL	PUT_BC
	CALL	PUT_DE
.endif
.if (DEBUG & 0x80)
   CALL SELECT_UART0
.endif

		CALL	SD_WRITE_SEC

		POP	H
		POP	D
		POP	B

SD_RS_NC:

.if (DEBUG & 0x80)
   CALL SELECT_UART1
.endif
.if (DEBUG & 2)
	CALL PRINTI		;DEBUG
	.ascii " Read:\000"
	CALL	PUT_BC
	CALL	PUT_DE
.endif
.if (DEBUG & 0x80)
   CALL SELECT_UART0
.endif


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
SD_RS_0:		MVI	B,0		;256 Attempts to recieve the DATASTART
SD_RS_LP1:	CALL	SPI_RX
		CPI	0xFE		;IS DATASTART?
		JZ	SD_RS_1
		DCR	B
		JNZ	SD_RS_LP1
		CALL	SD_DESELECT	;Deselect card
		RET

SD_RS_1:		LXI	B,0x0200
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
;Write the SD_RAM_BUFFER to the SD Card at Sector 'SEC_PTR'
;-----------------------------------------------------------------------------------------------------
		;Sector in SEC_PTR, H=SD_RAM_BUFFER
SD_WRITE_SEC:	CALL	SET_PARAM
		MVI	A,24 	;Write Sector Command
		CALL	SD_CMD
		MVI	A,1 	;Error Code
		JNZ	SD_WR_FAIL

		MVI	A,0xFE	;DATA START BLOCK
		OUT	SPI
		LXI	B,0x0200
SD_WR_LP:	MOV	A,M
		INX	H
		OUT	SPI
		DCR	C
		JNZ	SD_WR_LP
		DCR	B
		JNZ	SD_WR_LP

		MVI	A,0xFF
		OUT	SPI
		NOP
		MVI	A,0xFF
		OUT	SPI

		CALL	SPI_RX
		ANI	0x1f
		CPI	5
		MVI	A,2 	;Error Code
		JNZ	SD_WR_FAIL
		CALL	WAIT_NOT_BUSY
		MVI	A,3 	;Error Code
		JC	SD_WR_FAIL
		CALL	SD_CLEAR_ARG
		MVI	A,13		;SEND_STATUS
		CALL	SD_CMD
		MVI	A,4 	;Error Code
		JNZ	SD_WR_FAIL
		CALL	SPI_RX
		ORA	A
		MVI	A,5 	;Error Code
		JNZ	SD_WR_FAIL

		XRA	A		;A should be zero
		STA	DIRTY_DATA

		CALL	SD_DELAY

		CALL	SD_DESELECT	;Deselect card
		RET

SD_WR_FAIL:	CALL	SD_DESELECT	;Deselect card
		CALL	PRINTI
		.ascii "\r\n-Write Failed:\000"
		CALL	PUT_BYTE
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
		ORI	0x40	;All Commands start with 0x40
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
SDC_RET:		POP	B
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
WNB_LP2:		CALL	SPI_RX
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
INPUT_FNAME:	CALL 	PRINTI		;Display Menu Prompt
		.ascii "\r\nENTER 8.3 FILE NAME> \000"
		LXI	H,FILENAME
		MVI	B,11
		MVI	A," "
		CALL	FILL_BLOCK
		MVI	C,"."
		MVI	B,8
		CALL	GET_STRING
		RC
		CPI	13
		RZ
		LXI	H,FILEEXT
		MVI	B,3
		CALL	GET_STRING
		RET

;-----------------------------------------------------------------------------------------------------
;	Prints Filename at HL
;-----------------------------------------------------------------------------------------------------
PRINT_FILENAME:	PUSH	H
		MVI	B,8
PF_LP1:		MOV	A,M
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


;------------------------- SHIFT HL RIGHT ( 0 -> HL -> C )
RHLR:		ORA	A		;Clear Carry
		MOV	A,H
		RAR
		MOV	H,A
		MOV	A,L
		RAR
		MOV	L,A
		DCR	B
		JNZ	RHLR
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
FILL_BLOCK:	PUSH	B
		PUSH	H
FB_LP:		MOV	M,A
		INX	H
		DCR	B
		JNZ	FB_LP
		POP	H
		POP	B
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


;-----------------------------------------------------------------------------------------------------



;----------------------------------------------------------------------------------------------------; RAM SPACE
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;----------------------------------------------------------------------------------------------------; RAM SPACE
		.ORG	0xFA00
TEMP_STACK	.EQU	.	;Temp stack at 0xFA00 (grows downward)
		;.DSEG
		;DEFINE RAM AREAS FOR BDOS OPERATION
DIRBUF:	.blkb	128
ALV0:	.blkb	31
CSV0:	.blkb	16
ALV1:	.blkb	31
CSV1:	.blkb	16
ALV2:	.blkb	31
CSV2:	.blkb	16
ALV3:	.blkb	31
CSV3:	.blkb	16
;

LOWSTACK	.EQU	.


		.ORG	0xFD00
HIGHSTACK:			;Stack starts saveing at FCFFh and grows downward

STACK_SIZE	.EQU	HIGHSTACK-LOWSTACK

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
SDISKA:		.blkb	32	;File Control Blocks for Disks A to D
SDISKB:		.blkb	32
SDISKC:		.blkb	32
SDISKD:		.blkb	32



; SPECIAL FLAGS.
CONOTF:		.blkb  1	;NO-PRINT FLAG (WHEN FF).
ECHO_ON:		.blkb	1	;Echo characters
PROP_CHECK:	.blkb	1	;Enable Propeller board when not zero
FCB_PTR:		.blkb	2	;Points to current FCB in use
SEC_PER_TRACK:	.blkb	2	;Sectors Per Track

CLEAR_RAM	.EQU	.	;Clear all RAM after this point on INIT

XMSEQ:		.blkb	1	;XMODEM SEQUENCE NUMBER
XMTYPE:		.blkb	1	;XMODEM BLOCK TYPE (CRC/CS)
XSECTOR:		.blkb	1	;Sector of xmodem transfer
XTRACK:		.blkb	1	;Track of xmodem transfer
XCOUNT:		.blkb	2	;Count of sectors to read
XSUM:		.blkb	2	;Sum of Staring Sector & Count
XPOS:		.blkb	2	;Position of next read/write logical sector

DIVIDE_FUNC:	.blkb	2	;Pointer to the Divide Function
DF_SHIFTCNT:	.blkb	1	;Count of shifts required for Fast Divide
MUL8:		.blkb	1	;8 bit multiplier
M_COUNTER:	.blkb	1	;8 bit counter for multiply routine
MOD_FUNC:	.blkb	2	;Pointer to the Mod Function
MODMASK:		.blkb	1	;8 bit mask to get Relative Sector within a cluster from a Relative File sector

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
FILEEXT:		.blkb	3	;File Extension
SEC_PTR:		.blkb	4	;Sector Pointer, general use variable that holds the last sector read
DIRTY_DATA:	.blkb	1	;Indicates when data Read has been altered, ie. Requires flushing back to SD Card
ENT_COUNT:	.blkb	2	;Directory Entry Counter, Counts down maximum directory entries in Find File
CLUSTER1:	.blkb  2	;Cluster 1 of last opened file
FILESIZE:	.blkb  4	;File Size of last opened file (must follow CLUSTER1)
FILESIZEHEX:	.blkb  4


; BIOS SCRATCH AREA.
;
TRK:		.blkb   2		;CURRENT TRACK NUMBER.
SECT:		.blkb   1		;CURRENT SECTOR NUMBER.
DMAADD:		.blkb   2		;DISK TRANSFER ADDRESS.
DISKNO:		.blkb   1		;DISK NUMBER (TO CP/M).
TEMP:		.blkb   1		;TEMPORARY STORAGE.


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

;            .ifDEF  label1
;            lda     byte1
;            sta     byte2
;            .endif
;
;
;            #ifdef  label1
;            lda     byte1
;            #else
;            lda     byte2
;            #endif
;
;            #ifndef label1
;            lda     byte2
;            #else
;            lda     byte1
;            #endif
;
;            #if ($ >= 1000h)
;            ; generate an invalid statement to cause an error
;            ;  when we go over the 4K boundary.
;             !!! PROM bounds exceeded.
;            #endif
