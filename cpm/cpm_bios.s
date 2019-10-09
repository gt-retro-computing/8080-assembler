; STACK_TOP .equ 0x300

LF      .equ 10         ; line feed ( V )
CR      .equ 13         ; carriage return ( <- )
NUL     .equ 0          ; NUL character

TTS     .equ     03h     ;SIO channel A command port
TTI     .equ     02h     ;SIO channel A data port (yes input=output)
TTO     .equ     02h     ;SIO channel A data port

TTYDA   .equ     02h     ;tty data available (ready to receive?) 
TTYTR   .equ     01h     ;tty terminal ready (ready to transmit?)

DWAIT .equ 0fch
DCOM .equ 0f8h
DDATA .equ 0fbh
DSTAT .equ 0f8h
DSECT .equ 0fah
DEXT .equ 0fch

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
    jmp b_seldsk ; +27 - seldsk
    jmp b_settrk   ; +30 - settrk
    jmp b_setsec   ; +33 - setsector
    jmp b_setdma   ; +36 - setdma
    jmp b_read   ; +39 - read
    jmp b_write   ; +42 - write
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
    mvi a, 04ah  ; 8,1,n
    out TTS
    mvi a, 037h
    out TTS

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
    JMP  gocpm
    

b_const:
    ; jmp $
    in TTS
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
    
jmp foobar
    ; cma 
    ; out 0xFF
    ; cma
    
    ; return now if out of range
    cpi 2
    rnc
    
    inr a
    cma
    ani 3
    ral
    ral
    ral
    ral
    ori 2
    out DEXT

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
    ret    

b_setsec: ;set sector given by register c
    mov a,c
    sta sector
    ret


b_settrk:
    mov a, c
    sta track
    
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
    in DWAIT
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



track:  ds  2   ;two bytes for expansion
sector: ds  2   ;two bytes for expansion
dmaad:  ds  2   ;direct memory address
diskno: ds  1   ;disk number 0-15
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























