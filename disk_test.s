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
DEXT  .equ 0fch
DCOM .equ 0f8h
DDATA .equ 0fbh
DSTAT .equ 0f8h
DSECT .equ 0fah

DISK_BUF        .equ 0x400
DISK_BUF_END    .equ 0x480
SERIAL_BUF      .equ 0x500

.org 0
start:
    LXI H, STACK_TOP
    sphl
    call home
    mvi c, 1
    call select
    call home
    mvi a, 30
    call seek
    mvi a, 0xf0
    out 0xff
    hlt


home:
    mvi a, 0xd0
    out DCOM
HOME1:
    IN DSTAT
    RRC
    JC HOME1
    MVI a, 3
    OUT DCOM
    IN DWAIT
    ORA A
    mvi a, 1
    jm dead
    in DSTAT
    mov e, a
    ani 4
    jz dead
    ani 0x91
    ret

select:
    mov a, c
    cma
    ani 3
    ral
    ral
    ral
    ral
    ori 2
    out DEXT
    ret
    
seek:
    out DDATA
busy:
    in DSTAT
    RRC
    JC busy
    MVI a, 0x12
    out DCOM
    in DWAIT
    ret
dead:
    cma
    out 0xFF
    hlt