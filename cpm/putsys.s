
DWAIT .equ 0fch
DCOM .equ 0f8h
DDATA .equ 0fbh
DSTAT .equ 0f8h
DSECT .equ 0fah

MEM .equ 24
CPM_BASE .equ (MEM-7)*1024

CCP .equ CPM_BASE
BDOS .equ CCP+0x800
BIOS .equ CCP+0x1600


.org 0x100
start:
    
    LXI SP, CCP-0x0080
    LXI HL, CCP-0x0080
    
    mvi b, 0
wr_trk:
    mvi c, 1
wr_sec:
    call write_sec
    lxi DE, 128
    dad DE
    inr c
    mov a, c
    cpi 27
    jc wr_sec

    inr b
    mov a, b
    cpi 2
    jc wr_trk

    hlt

write_sec:
    mov a, b
    call dseek
    mov a, c
    call dsector_write
    ret


; Seek to track
; A = track
dseek:
    push de
    mvi d, 76
    cmp d
    jp dseek_end
    out DDATA
    mvi a, 013h
    out DCOM
dseek_end:
    in DWAIT
    pop de
    ret

; Read sector of disk into DISK_BUF
; A = sector number

dsector_read:
    push hl
    lxi hl, DISK_BUF
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
    pop hl
    ret


; Write a sector of disk
; A  = sector number
; HL = address of data to copy to disk

dsector_write:
    push hl
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
    in DWAIT
    pop hl
    ret









