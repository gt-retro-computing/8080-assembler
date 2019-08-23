DWAIT .equ 0fch
DCOM .equ 0f8h
DDATA .equ 0fbh
DSTAT .equ 0f8h
DSECT .equ 0fah


IN DWAIT
LXI H, testData
MVI a, 01
OUT DSECT
mvi a, 0xAC
out DCOM
loop:
IN DWAIT
ORA A
jp done
mov a, m
out DDATA
inx H
jmp loop
done:
in STAT
cma
OUT 0xff
hlt

testData db64 PgAGAS7/T4DaAACt0/+tQWcR//8berPCFAB8wwYA