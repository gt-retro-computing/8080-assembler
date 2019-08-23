DWAIT .equ 0fch
DCOM .equ 0f8h
DDATA .equ 0fbh
DSTAT .equ 0f8h
DSECT .equ 0fah


IN DWAIT
LXI H, 0x100
MVI a, 01
OUT DSECT
mvi a, 0x8C
out DCOM
loop:
IN DWAIT
ORA A
jp done
IN DDATA
mov m, a
inx H
jmp loop
done:
in STAT
cma
OUT 0xff
hlt
