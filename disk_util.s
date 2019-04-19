STACK_TOP .equ 0x300

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

DISK_BUF        .equ 0x400
DISK_BUF_END    .equ 0x480
SERIAL_BUF      .equ 0x500


.org 0
start:
    ; init stack
    LXI hl, STACK_TOP
    sphl
    
    call serial_init        ; init serial

    lxi hl, TEXT_WELCOME
    call swrite_line

    jmp loop2

foo_loop:
    call sread_line
    lxi hl, SERIAL_BUF
    call swrite_line
    jmp foo_loop

    hlt

; foobar2:
;     call sread_byte
;     call swrite_byte
;    jmp foobar2

    
labal:
    mvi a, 65
    call swrite_byte
    ; jmp labal

    lxi hl, TEXT_LOC
    call swrite_str
    
    hlt

    mvi d, 0
loop:
    in 0ffh
    cmp d
    jz loop_end
    mov d, a

    call dseek
    call dwait_loop

    jmp loop2 ; uncomment to read

    lxi hl, DISK_BUF
    mvi a, 128
wr_loop:
    dcr a
    mov m, a
    inx hl
    jnz wr_loop


    mvi a, 1
    lxi hl, DISK_BUF
    call dsector_write

    hlt
loop2:

    mvi e, 255 ; track num

outer_foo:
    inr e
    mov a, e
    call dseek
   
    mvi d, 0
foo:
    inr d
    mov a, d
    call dsector_read
    in DSTAT
    ani 0x10
    jnz foo_end
    
    lxi hl, DISK_BUF
    mvi a, 128
    call swrite_bytes
    
    jmp foo
foo_end:
    mov a, d
    cma
    out 0xff

    mov a, e
    cpi 76   ; last track 
    jnz outer_foo
    
    hlt

loop_end:
    in DSTAT
    cma
    out 0ffh

    jmp loop


; Serial init
serial_init:
    mvi a,0    
    out TTS
    out TTS
    out TTS
    mvi a, 040h
    out TTS
    mvi a, 04eh  ; 8,1,n
    out TTS
    mvi a, 037h
    out TTS
    ret


; Read byte from serial
; A = returned byte
; 
sread_byte:             ; loop until data available
    in TTS
    ani TTYDA 
    jz sread_byte
    in TTO              ; read
    ret


; Write byte to serial
; A = byte
;
swrite_byte:            ; loop until terminal ready
    push de
    mov d, a
swrite_byte_wait:
    in TTS
    ani TTYTR
    jz swrite_byte_wait
    mov a, d
    out TTO
    pop de
    ret

; Write bytes to serial
; A = num bytes
; HL = address of bytes
;
swrite_bytes:
    push de
    mov d, a            ; bytes left counter

swrite_bytes_loop:
    mov a, d
    ora a
    jz swrite_bytes_end ; zero bytes left, exit
    dcr d
    mov a, m
    inx hl
    call swrite_byte
    jmp swrite_bytes_loop

swrite_bytes_end:
    pop de
    ret


; Write null-terminated string to serial
; HL = address of string
; 
swrite_str:
    mov a, m
    cpi 0
    jz swrite_str_end   ; exit if we hit NUL

    call swrite_byte    ; echo to serial
    inx hl
    jmp swrite_str
swrite_str_end:
    ret


; Write string to serial and go to next line
swrite_line:
    call swrite_str
    lxi hl, TEXT_ENDL
    call swrite_str
    ret


; Read a line from serial. Line end indicated by LF (\n) which is stripped.
; Result is null-terminated string in SERIAL_BUF
; 
sread_line:
    push hl
    lxi hl, SERIAL_BUF
sread_line_loop:    
    call sread_byte
    cpi LF
    jz sread_line_end
    cpi 0
    jz sread_line_end
    mov m, a
    inx hl    
    jmp sread_line_loop
sread_line_end:
    mvi a, 0
    mov m, a
    pop hl
    ret


; Wait until disk is ready
dwait_loop:
    in DSTAT
    cma
    out 0ffh
    cma
    ani 1
    jnz dwait_loop
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
    mov a, l
    cma
    out 0xff
    ret



TEXT .equ TEXT_LOC

; * * * * * * * * * * ;
;      Strings        ;
; * * * * * * * * * * ;

TEXT_WELCOME:
    db CR, LF, LF, LF, LF, "Welcome to Disk Dumper 1000", NUL

TEXT_ENDL:
    db CR, LF, NUL

TEXT_LOC:
    db 'Wow this is great!', NUL














