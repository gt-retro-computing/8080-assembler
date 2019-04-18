;fib
.org 0
.equ DELAY_TIME 0FFFFh

init:
    mvi a, 0
    mvi b, 1
    mvi l, 0xFF
fib:
    mov c, a
    add b
    jc init
    xra l
    out 0xff
    xra l
    mov b, c
    mov h, a
    LXI d, DELAY_TIME
delay:
    dcx d
    mov a, d
    ora e
    jnz delay
    mov a, h
    jmp fib

db 0xde
dw beadh
dd efh
.ascii 'dank memes'
