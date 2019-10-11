; STACK_TOP .equ 0x300

LF      .equ 10         ; line feed ( V )
CR      .equ 13         ; carriage return ( <- )
NUL     .equ 0          ; NUL character

TTS     .equ     03h     ;SIO channel A command port
TTI     .equ     02h     ;SIO channel A data port (yes input=output)
TTO     .equ     02h     ;SIO channel A data port

TTYDA   .equ     02h     ;tty data available (ready to receive?) 
TTYTR   .equ     01h     ;tty terminal ready (ready to transmit?)

DCOM .equ 0f8h
DSTAT .equ 0f8h
TRACK .equ 0f9h
DSECT .equ 0fah
DDATA .equ 0fbh
DWAIT .equ 0fch
DEXT .equ 0fch
DCONT .equ 0fch

RTCNT .equ 10
STPRAT .equ 2
HLAB .equ 8

MEM .equ 24
CPM_BASE .equ (MEM-7)*1024
BDOS_LOC .equ CPM_BASE + 806h

IOBYTE .equ 3
CDISK .equ 4

.org (CPM_BASE + 1600h)
bios_entry:
    jmp b_boot   ;  +0  - cold boot
    jmp b_wboot  ;  +3 - warm boot
    jmp b_const  ;  +6 - console status
    jmp b_conin  ;  +9 - console in
    jmp b_conout ; +12 - console out
    jmp b_noop   ; +15 - list
    jmp b_noop   ; +18 - punch
    jmp b_noop   ; +21 - reader
    jmp b_home   ; +24 - home
    jmp SELDSK; +27 - seldsk
    jmp SETTRK; +30 - settrk
    jmp SETSEC; +33 - setsector
    jmp SETDMA; +36 - setdma
    jmp READ; +39 - read
    jmp WRITE; +42 - write
    jmp b_noop   ; +45 - list status
    jmp b_sect_translate   ; +48 - sect translate

;
;   fixed data tables for four-drive standard
;   IBM-compatible 8" disks
;   disk parameter header for disk 00
dpbase: dw  trans,0000H
    dw  0000H,0000H
    dw  dirbf,dpblk
    dw  chk00,all00
;   disk parameter header for disk 01
    dw  trans,0000H
    dw  0000H,0000H
    dw  dirbf,dpblk
    dw  chk01,all01
;   disk parameter header for disk 02
    dw  trans,0000H
    dw  0000H,0000H
    dw  dirbf,dpblk
    dw  chk02,all02
;   disk parameter header for disk 03
    dw  trans,0000H
    dw  0000H,0000H
    dw  dirbf,dpblk
    dw  chk03,all03

;
;   sector translate vector
trans:  db  1,7,13,19   ;sectors 1,2,3,4
    db  25,5,11,17  ;sectors 5,6,7,8
    db  23,3,9,15   ;sectors 9,10,11,12
    db  21,2,8,14   ;sectors 13,14,15,16
    db  20,26,6,12  ;sectors 17,18,19,20
    db  18,24,4,10  ;sectors 21,22,23,24
    db  16,22       ;sectors 25,26

dpblk:  ;disk parameter block, common to all disks
    dw  26      ;sectors per track
    db  3       ;block shift factor
    db  7       ;block mask
    db  0       ;null mask
    dw  242     ;disk size-1
    dw  63      ;directory max
    db  192     ;alloc 0
    db  0       ;alloc 1
    dw  16      ;check size
    dw  2       ;track offset   2=for cpm disk

SELDSK:
  MOV  A,C	;GET NEW DISK NUMBER.
	ANI  3		;ONLY LOOK AT 2 LSB'S.
	LXI  H,DISKNO	;GET ADR OF OLD DISK NO.
	CMP  M		;NEW = OLD?
	RZ		;IF SO, RETURN.
	PUSH A		;SAVE DISK NUMBER.
	LDA  NODSKS	;GET NUMBER OF DISKS.
	DCR  A		;IF MORE THAN ONE DISK,
	JNZ  SELMOR	;TAKE CARE OF IT.
	POP  A		;GET DISK NUMBER.
	STA  DISKNO	;UPDATE OLD WITH NEW.
	XRA  A		;SET A=0 FOR NO ERRO IND.
	RET		;RETURN FROM SELDSK.
SELMOR:
  POP  A		;MAKE STACK RIGHT.
	MOV  A,M	;GET OLD DISK NUMBER.
	ANI  0FEH	;CLEAR OUT BIT 0.
	MOV  E,A	;PUT OLD DISK NO. IN D&E.
	MVI  D,0
	LXI  H,TRTAB	;GET ADDRESS OF TRACK TABLE.
	DAD  D		;ADD DISK NO. TO ADDRESS.
	IN   TRACK ;READ 1771 TRACK REGISTER.
	MOV  M,A	;PUT INTO TABLE.
	MOV  A,C	;GET NEW DISK NUMBER.
	ANI  0FEH	;CLEAR BIT 0.

	MOV  E,A	;PUT NEW DISK NO. IN D&E.
	LXI  H,TRTAB	;GET ADDRESS OF TRACK TABLE.
	DAD  D		;ADD DISK NO. TO ADDRESS.
	MOV  A,M	;GET NEW TRACK NUMBER.
	OUT  TRACK	;PUT INTO 1771 TRACK REG.
	MOV  A,C	;UPDATE OLD DISK NUMBER.
	STA  DISKNO
	CMA		;BITS INVERTED INTO LATCH.
	ADD  A		;PUT BITS 1&2 AT 4&5.
	ADD  A
	ADD  A
	ADD  A
	ORI  2		;MAKE LATCH COMMAND.
	STA  LATCH	;SAVE NEW LATCH CODE.
	XRA  A		;SET A = 0.
	RET		;RETURN FROM SELDSK.

HOME:	MVI  C,0	;SEEK TO TRACK ZERO.
;
; SET TRACK NUMBER TO WHATEVER IS IN REGISTER C.
; ALSO PERFORM MOVE TO THE CORRECT TRACK (SEEK).
;
SETTRK:
	PUSH H		;SAVE H&L.
	LHLD LATCH	;GET NEW & OLD LATCH.
	MOV  A,L	;GET NEW LATCH.
	OUT  DCONT;SELECT DRIVE NOW.
	STA  CLATCH	;REMEMBER CURRENT LATCH.
	CMP  H		;IS NEW SAME AS OLD?
	MVI  A,0FFH	;IF NOT, SET FLAG = FF.
	JNZ  SFLAG
	CMA		;IF NEW = OLD, FLAG = 0.
SFLAG:
  STA  HLSF	;SET HEAD-LOAD/SELECT FLAG.
	POP  H		;RESTORE H&L.
	MOV  A,C	;GET NEW TRACK NUMBER.
	STA  TRK	;UPDATE OLD WITH NEW.
;
; MOVE THE HEAD TO THE TRACK IN REGISTER A.
;
SEEK:
  PUSH B		;SAVE B&C.
	MOV  B,A	;SAVE DESTINATION TRACK.
	MVI  A,RTCNT	;GET RETRY COUNT.
SRETRY:
  STA  SERCNT	;STORE IN ERROR COUNTER.
	IN   TRACK	;READ PRESENT TRACK NO.
	MOV  C,A	;SAVE IN C.
	MOV  A,C	;DELAY.
	CMP  B		;SAME AS NEW TRACK NO.?
	MOV  A,B	;RESTORE A FROM B.
	JNZ  NOTHR	;JUMP IF NOT THERE.
THERE:
  POP  B		;RESTORE B&C.
	RET		;RETURN FROM SEEK.
NOTHR:
;
;THIS ROUTINE IS TO ALLOW TIME FOR THE DRIVE
;TUNNEL ERASE TO TERMINATE BEFORE MOVING THE
;HEAD.  THE DELAY IS APPROX. 700 MICRO-SEC. @
;4 MHZ CPU TIME, AND DOUBLE THIS FOR 2 MHZ CPU'S.
;
	PUSH  PSW	;SAVE ACCUM AND FLAGS
	MVI   A,0D0H	;DELAY COUNT = 208
BUSY1:	DCR   A		;DECREASE COUNT
	JNZ   BUSY1	;LOOP TILL DONE
	POP   PSW	;RESTORE ACCUM AND FLAGS

	OUT  DDATA	;TRACK TO DATA REGISTER.
BUSY:
	IN   DSTAT	;READ DISK STATUS.
	RRC		;LOOK AT BIT 0.
	JC   BUSY	;WAIT TILL NOT BUSY.
	MVI  A,14H+STPRAT+HLAB  ;GET STEP RATE, DO
	OUT  DCOM	;SEEK WITH VERIFY.
	IN   WAIT	;WAIT FOR INTRQ.
	IN   DSTAT	;READ STATUS.
	ANI  91H	;LOOK AT BITS.
	JZ  THERE	;OK IF ZERO.

	PUSH H		;SAVE H&L.
	LXI  H,SECNT	;GET ADR OF SEEK ERR CTR.
	INR  M		;ONE MORE SEEK ERROR.
	POP  H		;RESTORE H&L.
	LDA  SERCNT	;GET ERROR COUNT.
	DCR  A		;DECREMENT COUNT.
	JNZ  SRETRY	;RETRY SEEK.
	POP  B		;RESTORE B&C.
	IN   DSTAT	;READ DISK STATUS.
	ANI  91H	;LOOK AT ERROR BITS.
	MOV  D,A	;PUT IN REG D.

;
; SET DISK SECTOR NUMBER.
;
SETSEC:
  MOV  A,C	;GET SECTOR NUMBER.
  STA  SECT	;PUT AT SECT # ADDRESS.
  RET		;RETURN FROM SETSEC.

SETDMA:
  MOV  H,B	;MOVE B&C TO H&L.
  MOV  L,C
  SHLD DMAADD	;PUT AT DMA ADR ADDRESS.
  RET		;RETURN FROM SETDMA.
;
; HDLD - GET HEAD-LOAD BIT IF REQUIRED.
;
HDLD:
 	LDA  HLSF	;GET HEAD-LOAD FLAG.
	ORA  A		;IS A = ZERO?
	JZ   HDLD1	;HOP IF SO.
	CMA		;SET A = 0.
	STA  HLSF	;SET FLAG = 0 IF NOT.
;
;IF CHANGING TO A NEW DRIVE, PERFORM A SEEK TO
;THE SAME TRACK TO ALLOW THE HEAD TO UNLOAD.
;
	IN   TRACK	;GET PRESENT TRACK
	OUT  DDATA	;AND TELL 1771 ABOUT IT.
	MVI  A,14H+STPRAT+HLAB ;GET THE STEP RATE.
	OUT  DCOM	;SEND IT TO FLOPPY CONTROLLER.
	IN   DWAIT	;WAIT FOR INTRQ.
HDLDY:
  MVI  A,4	;SET BIT TO LOAD HEAD.
	RET		;RETURN FROM HDLD.
HDLD1:
 	IN   DSTAT	;READ 1771 STATUS.
	ANI  20H	;LOOK AT HL BIT.
	JZ   HDLDY	;LOAD IF NOT LOADED.
	XRA  A		;OTHERWISE, A=0.
	RET		;RETURN FROM HDLD.
;
; READ THE SECTOR AT SECT, FROM THE PRESENT TRACK.
; USE STARTING ADDRESS AT DMAADD.
;
READ:	MVI  A,RTCNT	;GET RETRY COUNT.
RRETRY:	STA  ERCNT	;STORE IN ERROR CTR.
	LHLD DMAADD	;GET STARTING ADR.
	MVI  A,0D0H	;CAUSE INTERRUPT.
	OUT  DCOM
	XTHL		;SOME DELAY.
	XTHL

READ3:	LDA  SECT	;GET SECTOR NUMBER.
	OUT  DSECT;SET SECTOR INTO 1771.
	CALL HDLD	;GET HEAD-LOAD BIT?
	ADI  88H	;ADD CODE FOR READ SECT.
READE:	OUT  DCOM	;SEND COMMAND TO 1771.
RLOOP:	IN   DWAIT	;WAIT FOR DRQ OR INTRQ.
	ORA  A		;SET FLAGS.
	JP   RDDONE	;DONE IF INTRQ.
	IN   DDATA	;READ A DATA BYTE FROM DISK.
	MOV  M,A	;PUT BYTE INTO MEMORY.
	INX  H		;INCREMENT MEMORY POINTER.
	JMP  RLOOP	;KEEP READING.
RDDONE:	IN   DSTAT	;READ DISK STATUS.

	ANI  9DH	;LOOK AT ERROR BITS.
	RZ		;RETURN IF NONE.
	CALL ERCHK	;CHECK FOR SEEK ERROR.
	LXI  H,RECNT	;GET RD ERR COUNT ADDR.
	INR  M		;ONE MORE ERROR.
	LDA  ERCNT	;GET ERROR COUNT.
	DCR  A		;DECREMENT COUNT.
	JNZ  RRETRY	;TRY TO READ AGAIN.
ERMSG1:
	MVI  A,1	;SET FOR PERM ERR MSG.
	ORA  A		;SET FLAGS.
	RET
;
; ERCHK - CHECK FOR RECORD NOT FOUND ERROR.
;
ERCHK:	MOV  D,A	;SAVE ERROR BITS IN D.
	ANI  10H	;IF RECORD NOT FOUND,
	JNZ  CHKSK	;DO A CHECK ON SEEK.
	MOV  A,D	;OTHERWISE RESTORE BITS
	ORA  A		;SET FLAGS,
	RET		;AND RETURN.
;CHECK FOR SEEK TO CORRECT TRACK,
;AND CHANGE IF NECESSARY.
CHKSK:	MVI  A,0C4H	;SEND COMMAND TO 1771
	OUT  DCOM	;TO READ ADDRESS.
	IN   WAIT	;WAIT FOR DRQ OR INTRQ.
	IN   DDATA	;READ THE TRACK ADDRESS.
	MOV  B,A	;SAVE IN REGISTER B.
CHKS2:	IN   WAIT	;WAIT FOR INTRQ.
	ORA  A		;SET FLAGS.
	JP   CHKS3	;DONE WITH READ ADR OP.
	IN   DDATA	;READ ANOTHER BYTE.
	JMP  CHKS2	;DO IT AGAIN.
CHKS3:	IN  DSTAT	;READ DISK STATUS.
	ORA  A		;SET FLAGS.
	JZ   CHKS4	;READ ADR OK IF 0.
	CALL HOME	;OTHERWISE, HOME FIRST.
	JMP  CHKS5
CHKS4:	MOV  A,B	;UPDATE TRACK REGISTER.
	OUT  TRACK
CHKS5:	LDA  TRK	;GET REQUIRED TRACK NO.
	CALL SEEK	;MOVE THE HEAD TO IT.
	MOV  A,D	;GET ERROR BITS.
	ORA  A		;SET FLAGS.
	RET		;RETURN FROM ERCHK.
;
; WRITE THE SECTOR AT SECT, ON THE PRESENT TRACK.
; USE STARTING ADDRESS AT DMAADD.
;
WRITE:	MVI  A,RTCNT	;GET RETRY COUNT.
WRETRY:	STA  ERCNT	;STORE IN ERROR COUNTER.
	LHLD DMAADD	;GET STARTING ADR.
	MVI  A,0D0H	;STATUS INTERUPT FOR 1771.
	OUT  DCOM	;COMMAND 1771.
	XTHL		;WAIT FOR STATUS.
	XTHL		;CHANGE IT BACK.

WRITE3:	LDA  SECT	;GET SECTOR NUMBER.
	OUT  SECTP	;SET THE SECTOR INTO 1771.
	CALL HDLD	;GET HEAD LOAD BIT?
	ADI  0A8H	;ADD CODE FOR WRITE.
WRITE2:	OUT  DCOM
WLOOP:	IN   WAIT	;WAIT FOR READY.
	ORA  A		;SET FLAGS.
	JP   WDONE	;HOP OUT WHEN DONE.
	MOV  A,M	;GET BYTE FROM MEM.
	OUT  DDATA	;WRITE ONTO DISK.
	INX  H		;INCREMENT MEM PTR.
	JMP  WLOOP	;KEEP WRITING.
WDONE:	IN   DSTAT	;READ DISK STATUS.

	ANI  0FDH	;LOOK AT THESE BITS.
	RZ		;RETURN IF NO ERR.
	CALL ERCHK	;CHECK/CORRECT SEEK ERR.
	LXI  H,WECNT	;GET ADR OF WRITE ERR CTR.
	INR  M		;ONE MORE WRITE ERROR.
	LDA  ERCNT	;GET ERROR COUNT.
	DCR  A		;DECREMENT COUNT.
	JNZ  WRETRY	;TRY TO WRITE AGAIN.
  ret


b_setup:
    MVI  A,0C3H ;PUT JMP TO WBOOT
    STA  0      ;ADR AT ZERO.
    LXI  H,b_wboot
    SHLD 1
    STA  5
    LXI  H,BDOS_LOC ;PUT JUMP TO BDOS
    SHLD 6      ;AT ADR 5,6,7.
    ; LXI  H,80H  ;SET DEFAULT DMA ADR.
    ; SHLD DMAADD

    ; init serial
    mvi a,0
    out TTS
    out TTS
    out TTS
    mvi a, 040h
    out TTS
    ; mvi a, 04eh
    mvi a, 7eh  ; 8,1,e
    out TTS
    mvi a, 037h
    out TTS
    mvi a, 2
    sta NODSKS
    RET


b_boot:
    LXI  SP,STACK_TOP
    xra a
    sta IOBYTE
    sta CDISK

    call b_setup

    push de
    mov e, a
    mvi d, 105
b_boot_wait:
    in TTS
    ani TTYTR
    jz b_boot_wait
    mov a, d
    out TTO
    mov a, e
    pop de

gocpm:
    ; jump to CPM
    mvi c, 0    ; select drive A
    ; jmp 0x3c00  ; 0x3400 + b
    jmp CPM_BASE

b_wboot:
    LXI  SP,STACK_TOP
    CALL b_setup
    ; TODO: Load CPM
    JMP  gocpm

b_const:
    ; jmp $
    in TTS
    cma
    out 0xFF
    cma
    ani TTYDA
    jz b_const_nodata
    mvi a, 0xFF
    ret
b_const_nodata:
    mvi a, 0x00
    ret


b_conin:             ; loop until data available
    ; jmp $
b_conin_loop:
    in TTS
    ani TTYDA
    jz b_conin_loop
    in TTO              ; read
    ; cma
    ; out 0xff
    ; cma
    ; ani 0x7f
    ret


b_conout:            ; loop until terminal ready
    push de
    mov e, a
    mov d, c
b_conout_wait:
    in TTS
    ani TTYTR
    jz b_conout_wait
    mov a, d
    out TTO
    mov a, e
    pop de
    ret

b_home:
    mvi a, 0d0h
    out DCOM
b_home_1:
    in DSTAT
    rrc
    jc b_home_1
    mvi a, 3
    out DCOM
    in DWAIT
    ora a
    mvi a, 1
    jm b_home_die ; die pls
    in DSTAT
    mov e, a
    ani 4
    jz b_home_herr
    mov a, e
    ani 91h
    ret
b_home_herr:
    mvi a, 1
    ora a
    ret

b_home_die:
    mvi a, 0x55
    out 0xff
    hlt


b_seldsk:
    lxi h,0000h ;error return code
    mov a,c
    sta diskno
    
    ; return now if out of range
    cpi 2
    rnc

    ; jmp foobar
    ; mvi a, 3
    ; inr a
    cma
    ani 3
    ral
    ral
    ral
    ral
    ori 2

    cma
    out 0xFF
    cma

    sta disklatch

foobar:
    mov a, c

    lda diskno
    mov l,a ;L=disk number 0,1,2,3
    mvi h,0 ;high order zero
    dad h   ;*2
    dad h   ;*4
    dad h   ;*8
    dad h   ;*16 (size of each header)
    lxi d,dpbase
    dad d   ;HL=.dpbase(diskno*16)

    xra a
    ret    

b_setsec: ;set sector given by register c
    mov a,c
    sta sector
    ret


b_settrk:
    mov a, c
    sta track
    
    lda disklatch
    out DEXT
    
    mvi a, 0d0h
b_settrk_busyl:
    dcr a
    inr a
    dcr a
    inr a
    dcr a
    inr a
    dcr a
    jnz b_settrk_busyl
    
b_settrk_s:
    in DSTAT
    rrc
    jc b_settrk_s

    ; mvi a, 0x12
    ; out DCOM
    ; in DWAIT
    

    lda track
    
    ; cma
    ; out 0xFF
    ; cma
    
    out DDATA
b_settrk_busy:
    in DSTAT
    rrc
    jc b_settrk_busy
    mvi a, 12h
    out DCOM
    ; in DWAIT

b_s:
    in DSTAT
    cma
    out 0xFF
    cma
    rrc
    jc b_s


    in DSTAT
    ani 0x91
    jz b_settrk_end


b_settrk_end:
    xra a
    ret


b_setdma:
    mov l,c ;low order address
    mov h,b ;high order address
    shld    dmaad   ;save the address
    ds  10h ;space for setting the dma address
    ret

b_read:
dsector_read:
    push hl

    lda dmaad
    mov l, a
    lda dmaad+1
    mov h, a

    lda sector
    inr a
    ; cma
    ; out 0xFF
    ; cma

    out DSECT
    mvi a, 8ch
    out DCOM
dsector_read_loop:
    in DWAIT
    ora a
    jp dsector_read_done
    in DDATA
    mov m, a ; (hl) <- a
    inx hl
    jmp dsector_read_loop
dsector_read_done:
    
    in DSTAT
    cma
    out 0xFF
    cma    

    ani 0x90
    jnz b_read_err

    pop hl
    mvi a, 0 
    ret

b_read_err:
    pop hl
    mvi a, 1
    ret

b_write:
    lda dmaad
    mov l, a
    lda dmaad+1
    mov h, a

    lda sector
    inr a
    ; cma
    ; out 0xFF
    ; cma

; Write a sector of disk
; A  = sector number
; HL = address of data to copy to disk

dsector_write:
    out DSECT
    mvi a, 0adh
    out DCOM
dsector_write_loop:
    in DWAIT
    ora a
    jp dsector_write_done
    mov a, m
    out DDATA
    inx hl
    jmp dsector_write_loop
dsector_write_done:

    ; in DSTAT
    ; cma
    ; out 0xFF
    
    xra a
    ; mov a, l
    ; cma
    ; out 0xff
    ret



b_sect_translate:
    ; no
    mov h, b
    mov l, c
    ret

    ;translate the sector given by BC using the
    ;translate table given by DE
    xchg        ;HL=.trans
    dad b   ;HL=.trans(sector)
    mov l,m ;L = trans(sector)
    mvi h,0 ;HL= trans(sector)
    ret     ;with value in HL

b_noop:
    hlt
    xra a
    ; jmp $
    ret



; MY STACK
    .db 0, 0, 0, 0, 0, 0, 0, 0
    .db 0, 0, 0, 0, 0, 0, 0, 0
    .db 0, 0, 0, 0, 0, 0, 0, 0
    .db 0, 0, 0, 0, 0, 0, 0, 0
STACK_TOP:



TRK: ds 1
SECT: ds  1   ;two bytes for expansion
DMAADD:  ds  2   ;direct memory address
NODSKS: ds 1; number of disks
ERCNT:	DS   1		;ERROR COUNT FOR RETRIES.
SERCNT:	DS   1		;SEEK RETRY COUNTER.
TEMP:	DS   1		;TEMPORARY STORAGE.
LATCH:	DS   1		;NEW CODE FOR LATCH.
CLATCH:	DS   1		;CURRENT CODE IN LATCH.

DISKNO db 0
RECNT DB 0
WECNT DB 0
CONOTF DB 0
HLSF DB 0
LFCNT DB 0
TRTAB DB 0,0,0,0



;
;   scratch ram area for BDOS use
begdat  equ $   ;beginning of data area
dirbf:  ds  128 ;scratch directory area
all00:  ds  31  ;allocation vector 0
all01:  ds  31  ;allocation vector 1
all02:  ds  31  ;allocation vector 2
all03:  ds  31  ;allocation vector 3
chk00:  ds  16  ;check vector 0
chk01:  ds  16  ;check vector 1
chk02:  ds  16  ;check vector 2
chk03:  ds  16  ;check vector 3
;
enddat  equ $   ;end of data area
datsiz  equ $-begdat;size of data area























