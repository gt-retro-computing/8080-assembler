STACK_TOP .equ 0x300

DWAIT .equ 0fch
DCOM .equ 0f8h
DDATA .equ 0fbh
DSTAT .equ 0f8h
DSECT .equ 0fah


.org 0
start:
        ; init stack
        LXI hl, STACK_TOP
    sphl

    mvi d, 0
loop:
        in 0ffh
    cmp d
    jz loop


    call dseek
    call dwait_loop
loop_end:
    in DSTAT
    cma
    out 0ffh

    jmp loop


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
