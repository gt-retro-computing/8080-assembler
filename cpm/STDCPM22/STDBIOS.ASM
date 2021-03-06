; MODIFIED FOR HEAD LOAD SETTLING DELAY 15 DEC 80
; MODIFIED FOR BETTER ERROR RESTORAL 12 DEC 80
; MODIFIED FOR AUTO-BOOT 8 SEPT 80
;
; 1 JUL 80
;
	TITLE	'CCS 2422 STANDARD BIOS FOR CP/M 2.2'
	PAGE	56
;
; THIS BIOS IS SET UP FOR STANDARD 128 BYTE SECTORS ONLY
;
;"BIAS" IS ADDRESS OFFSET FROM 2C00H FOR MEMORY SYSTEMS
;THAN 20K (REFERRED TO AS "B" THROUGHOUT THE TEXT).
;
VERS:	EQU	22	;CP/M VERSION NUMBER
MSIZE:	EQU	20	;CP/M VERSION MEMORY SIZE IN KILOBYTES
BIAS:	EQU	(MSIZE-20)*1024
CCP:	EQU	2C00H+BIAS	;BASE OF CCP
BDOS:	EQU	CCP+806H	;BASE OF BDOS
BIOS:	EQU	CCP+1600H	;BASE OF BIOS
;
WBOOTV:	EQU	0	;VECTOR FOR WARM RESTART
IOBYTE:	EQU	3	;ADDRESS OF I/O CONTROL BYTE
CDISK:	EQU	4
BDOSV:	EQU	5
TBUF:	EQU	80H
;
FALSE:	EQU	0
TRUE:	EQU	NOT FALSE
;
MINI:	EQU	TRUE
MAXI:	EQU	TRUE
BOTH:	EQU	MINI AND MAXI
;
SDATA:	EQU	20H	;SERIAL DATA PORT
SINTEN:	EQU	SDATA+1	;SERIAL INTERRUPT ENABLE PORT
SIDENT:	EQU	SDATA+2	;SERIAL INTERRUPT IDENTIFICATION PORT
SLCTRL:	EQU	SDATA+3	;SERIAL LINE CONTROL PORT
SMDMCT:	EQU	SDATA+4	;SERIAL MODEM CONTROL PORT
SLSTAT:	EQU	SDATA+5	;SERIAL LINE STATUS PORT
SMDMST:	EQU	SDATA+6	;SERIAL MODEM STATUS PORT
;
RXRDY:	EQU	00000001B	;RECEIVE DATA AVAILABLE BIT
TXMTY:	EQU	00100000B	;TRANSMIT BUFFER EMPTY BIT
;
;  WHEN THE AUTO-BOOT JUMPER IS ENABLED, THE 2810
; SERIAL PORT WILL BE INITIALIZED TO 9600 BAUD.
; TO SELECT A DIFFERENT BAUD RATE, CHANGE SBAUD
; TO ONE OF THE FOLLOWING VALUES:
;
;     BAUD RATE		SBAUD
;	110		1047
;	134.5		857
;	150		768
;	300		384
;	600		192
;	1200		96
;	1800		64
;	2000		58
;	2400		48
;	3600		32
;	4800		24
;	7200		16
;	9600		12
;	19200		6
;	38400		3
;	56000		2
;
SBAUD:	EQU	12
;
; FLOPPY DISK CONTROLLER EQUATES
;
DSTAT:	EQU	30H	;DISK STATUS PORT
DCMMD:	EQU	DSTAT	;DISK COMMAND PORT
DTRCK:	EQU	DSTAT+1	;DISK TRACK PORT
DSCTR:	EQU	DSTAT+2	;DISK SECTOR PORT
DDATA:	EQU	DSTAT+3	;DISK DATA PORT
DFLAG:	EQU	DSTAT+4	;DISK FLAF PORT
DCTRL:	EQU	DSTAT+4	;DISK CONTROL PORT
BCTRL:	EQU	4	;DISK STATUS/CONTROL PORT 2
;
RSTR:	EQU	8	;BASIS OF RESTORE COMMAND
SEEKV:	EQU	1CH	;BASIS OF SEEK COMMAND
STEPI:	EQU	58H	;BASIS OF STEP IN COMMAND
RDSEC:	EQU	88H	;BASIS OF READ/WRITE SECTOR COMMAND
RDADD:	EQU	0C4H	;READ ADDRESS COMMAND
;
STEP5:	EQU	3	;MINI STEP RATE
STEP8:	EQU	1	;MAXI STEP RATE
;
	IF	MINI
HLWAIT:	EQU	3000H	;MINI HEAD LOAD DELAY CONSTANT
	ENDIF
	IF	NOT MINI
HLWAIT:	EQU	1600H	;8" HEAD LOAD DELAY CONSTANT
	ENDIF
;
TRIES:	EQU	10	;# OF ATTEMPTS BEFORE ABORT
;
; MISCELLANEOUS EQUATES
;
BELL:	EQU	7	;ASCII BELL CHARACTER
CR:	EQU	0DH	;ASCII CARRIAGE RETURN
LF:	EQU	0AH	;ASCII LINE FEED
;
DISKNO:	EQU	40H	;ACTIVE DISK NUMBER
TRACK:	EQU	DISKNO+1
SECTOR:	EQU	TRACK+1
SIDE:	EQU	SECTOR+1  ;SIDE SELECT HOLD AREA
SPT:	EQU	SIDE+1	;SECTORS PER TRACK HOLD
TWOSID:	EQU	SPT+1	;SINGLE/DOUBLE SIDED SWITCH HOLD
STPRAT:	EQU	46H	;STEP RATE SAVE AREA
STATUS:	EQU	47H
CMND:	EQU	STATUS+1
LUNIT:	EQU	49H	;LAST USED DRIVE
CUNIT:	EQU	LUNIT+1	;CURRENT DRIVE
RWFLG:	EQU	4BH
DMAAD:	EQU	4CH	;HOST BUFFER ADDRESS
IDSV:	EQU	4EH	;DISK ID SAVE AREA
;
	ORG	BIOS	;ORIGIN OF THIS PROGRAM
;
;JUMP VECTOR FOR INDIVIDUAL SUBROUTINES
	JMP	BOOT	;COLD START
WBOOTE:	JMP	WBOOT	;WARM START
CSTAT:	JMP	TTST	;CONSOLE STATUS
CONIN:	JMP	TTYIN	;CONSOLE CHARACTER IN
CONO:	JMP	TTYOUT	;CONSOLE CHARACTER OUT
	JMP	TTYOUT	;LIST CHARACTER OUT
	JMP	TTYOUT	;PUNCH CHARACTER OUT
	JMP	TTYIN	;READER CHARACTER OUT
	JMP	HOME	;MOVE HEAD TO HOME POSITION
	JMP	SELDSK	;SELECT DISK
	JMP	SETTRK	;SET TRACK NUMBER
	JMP	SETSEC	;SET SECTOR NUMBER
	JMP	SETDMA	;SET DMA ADDRESS
	JMP	DREAD	;READ DISK
	JMP	DWRITE	;WRITE DISK
	JMP	TTOST	;RETURN LIST STATUS
	JMP	SECTRAN	;SECTOR TRANSLATE
;
;INDIVIDUAL SUBROUTINES TO PERFORM EACH FUNCTION
;
; I/O DRIVERS FOR THE 8250 ASYNC COMM ELEMENT
;
TTST:	IN	SLSTAT	;GET 8250 LINE STATUS
	ANI	RXRDY	;SEE IF RECEIVE DATA AVAILABLE
	RZ		;RETURN IF NOT
	ADI	0FFH AND NOT RXRDY	;FLAG THAT DATA IS AVAILABLE
	RET
;
PRTRD:	CALL	PMSG
TTYIN:	CALL	TTST	;GET PORT STATUS
	JZ	TTYIN	;LOOP UNTIL DATA IS IN
	IN	SDATA	;READ THE DATA
	ANI	7FH	;STRIP OFF THE PARITY BIT
	RET
;
TTOST:	IN	SLSTAT	;GET 8250 LINE STATUS
	ANI	TXMTY	;ISOLATE TX BUFFER EMPTY BIT
	RZ		;RETURN IF NOT EMPTY
	ADI	0FFH AND NOT TXMTY	;FLAG THE EMPTY STATE
	RET
;
TTYOUT:	CALL	TTOST	;GET 8250 LINE STATUS
	JZ	TTYOUT	;WAIT UNTIL ONE OF THE REGISTERS EMPTIES
	MOV	A,C	;MOVE THE DATA OVER
	OUT	SDATA	;OUTPUT THE DATA
	RET
;
;
RETRY:	DCR	C
	JNZ	LOAD1
	LXI	H,BOTMSG
	CALL	PRTRD
	CALL	PRTWA
WBOOT:	LXI	SP,TBUF
	MVI	C,TRIES	;# OF RETRIES
LOAD1:	XRA	A
	MOV	H,A	;SET THE UNIT AND TRACK
	MOV	L,A
	SHLD	DISKNO
	LXI	H,0D002H  ;SET THE SIDE AND SECTOR
	SHLD	SECTOR
	MVI	B,8	;ZERO OUT PRMTBL
	LXI	H,PRMTBL
LOAD1A:	MOV	M,A
	INX	H
	DCR	B
	JNZ	LOAD1A
	PUSH	B
	MOV	C,A
	CALL	SELDSK
	POP	B
	IF	BOTH
	LDA	CUNIT	;GET THE SELBITS
	ANI	10H	;SEE IF A MINI
	ENDIF
	IF	MAXI
	MVI	B,26	;SET 8" SECTORS PER TRACK
	ENDIF
	IF	BOTH
	JNZ	LOAD1B	;JUMP IF 8"
	ENDIF
	IF	MINI
	MVI	B,18	;  ELSE, SET MINI SECTORS PER TRACK
	ENDIF
LOAD1B:	LXI	H,CCP
	SHLD	DMAAD
LOAD2:	PUSH	B
	CALL	DREAD
	POP	B
	ORA	A
	JNZ	RETRY
	SHLD	DMAAD
	MOV	D,H	;SAVE THE PAGE ADDRESS
	LXI	H,SECTOR  ;SET UP POINTER TO PARAMETERS
	MOV	A,M	;GET THE SECTORS PER TRACK
	SUB	B	;SEE IF READY FOR NEXT TRACK
	JC	LOAD3	;JUMP IF NOT
	MOV	M,A	;ELSE, RESET SECTOR NUMBER
	DCX	H	;POINT TO TRACK
	INR	M	;SET THE NEXT TRACK
	INX	H	;POINT BACK TO SECTOR
LOAD3:	INR	M	;SET THE NEXT SECTOR
	MOV	A,D	;GET THE PAGE ADDRESS
	CPI	BIOS/256  ;SEE IF DONE WITH WARM BOOT
	JC	LOAD2	;JUMP IF MORE TO GO
;
;END OF LOAD OPERATION, SET PARAMETERS AND GO TO CP/M
BOOT0:	MVI	A,JMP	;GET A JUMP OP CODE
	STA	WBOOTV	;RESET THE JUMP VECTORS
	STA	BDOSV
	LXI	H,WBOOTE
	SHLD	WBOOTV+1
	LXI	H,BDOS
	SHLD	BDOSV+1
	LXI	H,TBUF	;SET UP DEFAULT BUFFER ADDRESS
	SHLD	DMAAD
	LDA	CDISK	;GET CURRENT DISK NUMBER
	MOV	C,A	;SEND TO THE CCP
	JMP	CCP	;GO TO CP/M FOR FURTHER PROCESSING
;
;SELECT DISK GIVEN BY REGISTER C
;
NEWDSK:	PUSH	B	;SAVE THE ENTRY CONDITION
	INR	C	;SET UP TO DEVELOP SELECT BITS
	XRA	A
	STC
SET1:	RAL		;SHIFT DRIVE BIT INTO POSITION
	DCR	C
	JNZ	SET1
	IF	MINI
	ORI	20H	;ADD ON MOTOR ON BIT
	MOV	M,A	;SAVE IT
	OUT	DCTRL	;  AND SELECT THE DRIVE
	ENDIF
	IF	BOTH
	MVI	A,RSTR OR 3  ;RESTORE COMMAND WITH SLOWEST STEP RATE
	CALL	EOJC	;DO THE RESTORE
	IN	BCTRL	;SEE IF MINI OR MAXI
	RAR
	ENDIF
	IF	MINI
	MVI	A,STEP5	;SET THE MINI STEP RATE
	ENDIF
	IF	BOTH
	JNC	SET2	;JUMP IF A MINI
	MOV	A,M	;ELSE, SET UP AN 8" DRIVE
	ENDIF
	IF	MAXI
	ORI	30H	;ADD MOTOR ON, 8" BITS
	MOV	M,A	;SAVE THEM
	OUT	DCTRL	;SELECT THE DRIVE
	MVI	A,STEP8	;GET THE 8" STEP RATE
	ENDIF
SET2:	DCX	H	;POINT TO STEP RATE HOLD
	MOV	M,A	;SAVE THE STEP RATE
	IF	BOTH
	INX	H
	MOV	A,M	;SEE IF MINI OR MAXI
	ANI	10H
	ENDIF
	IF	MAXI
	LXI	D,T8S0	;SET UP THE DP BLOCK ADDRESSES
	LXI	B,DP8S0
	ENDIF
	IF	BOTH
	JNZ	SET3	;JUMP IF MAXI SELECTED
	ENDIF
	IF	MINI
	LXI	D,T5S0
	LXI	B,DP5S0
	ENDIF
SET3:	POP	H
	XTHL		;REGET THE DP TABLE ADDRESS
	MOV	M,E	;SET THE SKEW TABLE ADDRESS
	INX	H
	MOV	M,D
	LXI	D,9
	DAD	D	;POINT TO DP BLOCK ADDRESS STORAGE
	MOV	M,C	;SET THE ADDRESS
	INX	H
	MOV	M,B
	POP	B	;RESTORE THE (B,C) REGISTER PAIR
SELDSK:	LXI	H,0	;ERROR RETURN CODE
	MOV	A,C
	STA	DISKNO
	CPI	4	;MUST BE BETWEEN 0 AND 3
	RNC		;NO CARRY IF 4,5,...
;DISK NUMBER IS IN THE PROPER RANGE
;COMPUTE PROPER DISK PARAMETER HEADER ADDRESS
SELDSK1:	ADD	A	;2* UNIT
	MOV	B,A	;SAVE IT
	ADD	A	;4* UNIT
	ADD	A	;8* UNIT
	ADD	A	;16* UNIT
	MOV	L,A
	LXI	D,DPBASE ;TABLE BASE ADDRESS
	XCHG
	DAD	D	;HL=.DPBASE(DISKNO*16)
	PUSH	H	;SAVE (H,L)
	MOV	E,B	;GET 2* UNIT
	LXI	H,PRMTBL+1  ;POINT TO SELBITS
	DAD	D	;OFFSET THE TABLE POINTER
	MOV	A,M	;GET THE SELBITS
	ORA	A
	STA	CUNIT
	JZ	NEWDSK	;JUMP IF FIRST TIME FOR THIS DRIVE
	DCX	H	;POINT TO STEP RATE SAVE
	MOV	A,M	;GET THE STEP RATE
	STA	STPRAT	;SET IT
	POP	H
	MOV	A,C
	RET
;
;SET TRACK GIVEN BY REGISTER C
SETTRK:	MOV	A,C
	STA	TRACK
	RET
;
;SET SECTOR GIVEN BY REGISTER C
SETSEC:	MOV	A,C
	STA	SECTOR
	RET
;
;TRANSLATE THE SECTOR GIVEN BY BC USING THE
;TRANSLATE TABLE GIVEN BY DE
SECTRAN:  XCHG		;HL=.TRANS
	DAD	B	;HL=.TRANS(SECTOR)
	MOV	L,M	;L = TRANS(SECTOR)
	MVI	H,0	;HL= TRANS(SECTOR)
	RET		;WITH VALUE IN HL
;
;SET DMA ADDRESS GIVEN BY REGISTERS B AND C
SETDMA:	MOV	L,C	;LOW ORDER ADDRESS
	MOV	H,B	;HIGH ORDER ADDRESS
	SHLD	DMAAD	;SAVE THE ADDRESS
	RET
;
;MOVE TO THE TRACK 00 POSITION OF CURRENT DRIVE
HOME:	XRA	A
	STA	TRACK
	RET
;
; ROUTINE PRTWD PRINTS AN ASCII STRING ONTO THE CONSOLE.
;	THE STRING MUST BE TERMINATED BY BIT 7 SET IN THE
;	LAST CHARACTER OF THE STRING.  THE STRING WILL START
;	A NEW LINE (EP = PRTWD) OR CONTINUE ON THE SAME
;	LINE (EP = PRTWA)
;
PRTWD:	CALL	CRLF	;START A NEW LINE
PRTWA:	PUSH	B	;SAVE (B,C)
PRTA:	MOV	C,M	;GET NEXT CHARACTER FROM MEMORY
	CALL	CONO	;OUTPUT IT
	INX	H	;INCREMENT MEMORY POINTER
	MOV	A,C
	RLC		;TEST FOR BIT 7 DELIMITER
	JNC	PRTA	;NO DELIMITER, GO DO NEXT CHARACTER
PRTB:	POP	B	;RESTORE (B,C)
	RET
;
; ROUTINE CRLF GENERATES A CARRIAGE RETURN, LINE FEED
;	SEQUENCE ON THE CURRENT CONSOLE TO START A NEW LINE
;	IT INCLUDES TWO NULL CHARACTERS FOR TTY TYPE
;	DEVICES FOR THE HEAD MOVEMENT TIME.
;
PMSG:	CALL	PRTWD
CRLF:	PUSH	H	;SAVE THE CONTENTS OF (H,L)
CRLFA:	LXI	H,CRMSG	;ADDRESS OF CR,LF MESSAGE
	CALL	PRTWA	;  OUTPUT IT
	POP	H	;RESTORE (H,L)
	RET
DNRMSG:	DB	'DRIVE '
DNRMSGA:  DB	0,' NOT READ','Y'+80H
BOTMSG:	DB	7,'CANNOT BOO','T'+80H
CRMSG:	DB	CR,LF,0,80H
;
;
; THE FOLLOWING ROUTINES DO THE PRIMITIVE DISK ACCESSES.
;	IN ALL CASES, ONE SECTOR OF DATA IS TRANSFERRED.
;	IF THE DISK HAS NOT BEEN PREVIOUSLY ACCESSED,
;	THESE ROUTINES WILL AUTOMATICALLY DETERMINE THE
;	DISK TYPE (8" OR 5"), SINGLE OR DOUBLE DENSITY,
;	AND SECTOR SIZE.
;
;	BEFORE THE DESIRED DATA IS TRANSFERRED, THE DESIRED
;	TRACK IS SEEKED OUT, THE DESIRED SECTOR AND SIDE IS
;	SET, THEN THE ACTUAL DATA TRANSFER.
;
;	UP TO TEN TRIES WILL BE ATTEMPTED BEFORE THE DATA
;	TRANSFER IS ABORTED.  ON RETURN TO THE CALLING
;	ROUTINE, THE A REGISTER WILL CONTAIN A ZERO IF THE
;	OPERATION WAS SUCCESSFUL, OR NON-ZERO IF NOT
;	SUCCESSFUL.  THE FLAG REGISTER WILL NOT NECESSARILY
;	CORRESPOND WITH THE A REGISTER CONTENT.
;
;	THESE ROUTINES ARE CP/M COMPATABLE, AND MAY BE USED
;	AS PART OF THE BIOS.
;
;
DREAD:	DB	3EH	;SIM. MVI A INSTR
DWRITE:	XRA	A	;SET WRITE FLAG
	STA	RWFLG	;SAVE IT FOR LATER USE
	MVI	B,TRIES	;NUMBER OF RETRIES
AGN:	PUSH	B
	CALL	SEEK
	CZ	RDWR
READ3:	POP	B
	RZ
	ANI	10H	;SEE IF SEEK OR RNF ERROR
	CNZ	EOJB	;RESTORE THE DRIVE IF SO
	DCR	B
	JNZ	AGN
	INR	A	;INSURE ERROR IS FLAGGED
	RET
;
RDWR:	MOV	E,A	;SAVE COMMAND
	LDA	RWFLG
	ORA	A
	MOV	A,E	;REGET THE COMMAND
RDAT:	DI
	JZ	WRDAT	;WRITE IF ZERO
	OUT	DCMMD	;DISK COMMAND PORT
	STA	CMND
READ1:	IN	DDATA	;GET THE DATA
	MOV	M,A	;STORE IT
	INX	H	;INCREMENT THE MEMORY POINTER
	DCR	C	;LOOP CONTROL
	JNZ	READ1
	CALL	EOJ
	ANI	9CH	;ISOLATE READ ERROR BITS
	RET
;
WRDAT:	ORI	20H	;ADD WRITE COMMAND
	OUT	DCMMD	;DISK COMMAND PORT
	STA	CMND
WRT1:	MOV	A,M	;GET THE DATA
	OUT	DDATA
	INX	H	;BUMP THE MEMORY POINTER
	DCR	C	;LOOP CONTROL
	JNZ	WRT1
	JMP	EOJ
;
EOJB:	MVI	B,RSTR	;BASIS OF RESTORE COMMAND
EOJA:	LDA	STPRAT	;GET THE STEP RATE BITS
	ORA	B	;ADD ON THE COMMAND
EOJC:	STA	CMND
	OUT	DCMMD	;DO THE COMMAND
EOJ:	EI
	IN	DFLAG	;DISK FLAG PORT
	RAR
	JNC	EOJ
EOJ1:	IN	DSTAT	;GET THE DISK STATUS
	STA	STATUS
	ANI	0FCH
	IF	MAXI
	RP
	LDA	DISKNO
	ADI	'A'
	STA	DNRMSGA
	PUSH	H
	LXI	H,DNRMSG
	CALL	PRTRD
	POP	H
	JMP	EOJ1
	ENDIF
	IF	NOT MAXI
	RET
	ENDIF
;
SEEK:	CALL	IDRD	;INSURE HEADER HAS BEEN READ
	RNZ		;RETURN IF ERROR
SEEK1:	LDA	SECTOR	;SET THE SECTOR
	OUT	DSCTR	;DISK SECTOR PORT
	IN	DTRCK	;DISK TRACK PORT
	MOV	C,A	;SAVE IT
	LDA	TRACK	;GET DESIRED TRACK
	CMP	C
	JZ	RDWRT	;JUMP IF NO SEEK NEEDED
	OUT	DDATA	;SET THE SEEK TRACK
	MVI	B,SEEKV	;BUILD THE SEEK COMMAND
	CALL	EOJA	;DO THE SEEK
	ANI	98H	;SEEK ERROR MASK
	RNZ		;DONE IF SEEK ERROR
RDWRT:	IN	DDATA	;RESET ANY ERROR BITS
	LDA	CUNIT	;GET THE DRIVE SELECT BITS
	ORI	80H	;SET THE AUTO-WAIT BIT
	OUT	DCTRL	;SELECT THE UNIT
	IN	DFLAG	;DISK FLAG PORT
	ANI	20H	;SEE IF HEAD IS LOADED
	MVI	A,4
	JZ	RDWRT1	;JUMP IF NOT
	XRA	A	;ELSE, RESET THE HEAD LOAD FLAG
RDWRT1:	ADI	RDSEC	;BUILD A READ SECTOR COMMAND
	LHLD	DMAAD	;GET THE DMA ADDRESS
	MVI	C,80H	;SET SECTOR BYTE COUNT
RDWRT2:	CMP	A	;CLEAR THE FLAGS
	RET
;
IDRD:	LHLD	LUNIT
	MOV	A,H	;GET THE CUNIT VALUE
	CMP	L	;SEE IF SAME AS LUNIT
	RZ		;RETURN IF SO
IDRD1:	OUT	DCTRL	;SELECT THE CURRENT UNIT
	CALL	EOJ1	;INSURE IT IS READY
	IN	DDATA	;RESET STATUS BITS
	MOV	B,H	;SAVE THE SELECT BITS
	LXI	H,HLWAIT  ;WAIT FOR HEADS TO LOAD
IDRD3:	DCX	H
	MOV	A,H
	ORA	L
	JNZ	IDRD3
	MOV	A,B	;REGET THE SELECT BITS
	ORI	80H	;ADD ON THE AUTO-WAIT BIT
	OUT	DCTRL
	LXI	H,IDSV	;SET UP TO READ ADDRESS
	MVI	C,6	;READ 6 BYTES OF HEADER
	MVI	A,RDADD	;READ ADDRESS COMMAND
	CALL	RDAT
	RNZ		;DONE IF ERROR
;
IDRD2:	IN	DSCTR	;GET THE TRACK NUMBER
	OUT	DTRCK	;SET THE TRACK REGISTER
	MOV	A,B	;REGET SELBITS
	STA	LUNIT	;UPDATE LAST USED UNIT
	XRA	A	;RESET ERROR FLAGS
	RET
;
;
;FIXED DATA TABLES FOR FOUR-DRIVE STANDARD
;IBM-COMPATIBLE 8" DISKS
;DISK PARAMETER HEADER FOR DISK 00
DPBASE:	DW	0,0
	DW	0,0
	DW	DIRBF,0
	DW	CHK00,ALL00
;DISK PARAMETER HEADER FOR DISK 01
	DW	0,0
	DW	0,0
	DW	DIRBF,0
	DW	CHK01,ALL01
;DISK PARAMETER HEADER FOR DISK 02
	DW	0,0
	DW	0,0
	DW	DIRBF,0
	DW	CHK02,ALL02
;DISK PARAMETER HEADER FOR DISK 03
	DW	0,0
	DW	0,0
	DW	DIRBF,0
	DW	CHK03,ALL03
;
;PARAMETER TABLE FOR DRIVE-UNIQUE CONSTANTS
PRMTBL:	DB	0,0	;DRIVE 0 STEP RATE, SELECT BYTES
	DB	0,0	;      1
	DB	0,0	;      2
	DB	0,0	;      3
;
	IF	MAXI
DP8S0:	DW	26	;SECTORS PER TRACK
	DB	3	;BLOCK SHIFT FACTOR
	DB	7	;BLOCK MASK
	DB	0	;EXTENT MASK
	DW	242	;BLOCKS PER DISKETTE
	DW	63	;# DIRECTORY ENTRIES
	DB	192	;ALLOC 0
	DB	0	;ALLOC 1
	DW	16	;DIR CHECK VECTOR SIZE
	DW	2	;SYSTEM TRACK OFFSET
T8S0:	DB	1,7,13,19,25
	DB	5,11,17,23
	DB	3,9,15,21
	DB	2,8,14,20,26
	DB	6,12,18,24
	DB	4,10,16,22
	ENDIF
;
	IF	MINI
DP5S0:	DW	18	;SECTORS PER TRACK
	DB	3	;BLOCK SHIFT FACTOR
	DB	7	;BLOCK MASK
	DB	0	;EXTENT MASK
	DW	71	;BLOCKS PER DISKETTE
	DW	63	;# DIRECTORY ENTRIES
	DB	192	;ALLOC 0
	DB	0	;ALLOC 1
	DW	16	;DIR CHECK VECTOR SIZE
	DW	3	;SYSTEM TRACK OFFSET
T5S0:	DB	1,5,9,13,17
	DB	3,7,11,15
	DB	2,6,10,14,18
	DB	4,8,12,16
	ENDIF
;
;END OF FIXED TABLES
;
; ONE-TIME CODE, USED ONLY DURING COLD BOOTS
;
BOOT:	IN	DCTRL	;SEE IF AUTO-BOOT IN PROGRESS
	ANI	40H
	JNZ	BOOTA
	MVI	A,0FH	;TURN OFF THE HAND-SHAKE LINES
	OUT	SMDMCT
	MVI	A,83H	;ACCESS THE BAUD RATE DIVISOR REG
	OUT	SLCTRL
	MVI	A,SBAUD/256  ;SET THE DIVISOR HIGH BYTE
	OUT	SINTEN
	MVI	A,SBAUD MOD 256  ;SET THE DIVISOR LOW BYTE
	OUT	SDATA
	MVI	A,3
	OUT	SLCTRL	;SET NORMAL OPERATING CONDITIONS
	XRA	A	;SET THE HAND-SHAKE LINES
	OUT	SINTEN
	OUT	SLSTAT
BOOTA:	LXI	H,LOGMSG  ;SIGN ON TO THE SYSTEM
	CALL	PMSG
	LXI	H,0	;SET IOBYTE, CDISK
	SHLD	IOBYTE
	JMP	BOOT0
;
LOGMSG:	DB	MSIZE/10+'0',MSIZE MOD 10 + '0'
	DB	'k CP/M vers '
	DB	VERS/10+'0','.',VERS MOD 10+'0',80H
;
;THE REMAINDER OF THE CBIOS IS RESERVED UNINITIALIZED
;DATA AREA, AND DOES NOT NEED TO BE A PART OF THE
;SYSTEM MEMORY IMAGE (THE SPACE MUST BE AVAILABLE,
;HOWEVER, BETWEEN "BEGDAT" AND "ENDDAT").
;
;
;SCRATCH RAM AREA FOR BDOS USE
	ORG	BOOT
BEGDAT	EQU	$	;BEGINNING OF DATA AREA
DIRBF:	DS	128	;SCRATCH DIRECTORY AREA
ALL00:	DS	31	;ALLOCATION VECTOR 0
ALL01:	DS	31	;ALLOCATION VECTOR 1
ALL02:	DS	31	;ALLOCATION VECTOR 2
ALL03:	DS	31	;ALLOCATION VECTOR 3
CHK00:	DS	16	;CHECK VECTOR 0
CHK01:	DS	16	;CHECK VECTOR 1
CHK02:	DS	16	;CHECK VECTOR 2
CHK03:	DS	16	;CHECK VECTOR 3
;
ENDDAT	EQU	$	;END OF DATA AREA
DATSIZ	EQU	$-BEGDAT;SIZE OF DATA AREA
;
	END
