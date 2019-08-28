
DWAIT .equ 0fch
DCOM .equ 0f8h
DDATA .equ 0fbh
DSTAT .equ 0f8h
DSECT .equ 0fah

	mvi a, 0xf4
	out DCOM
	mvi b, 0
loop_b_begin:
	mvi a, 46
	cmp b
	jz loop_b_end
	in DWAIT
	mvi a, 0
	out DDATA
	inr b
	jmp loop_b_begin
loop_b_end:
	in DWAIT
	mvi a, 252
	out DDATA
	mvi b, 0
loop_c_begin:
	mvi a, 26
	cmp b
	jz loop_c_end
	in DWAIT
	mvi a, 0
	out DDATA
	inr b
	jmp loop_c_begin
loop_c_end:
	mvi b, 1
loop_d_begin:
	mvi a, 27
	cmp b
	jz loop_d_end
	mvi c, 0
loop_e_begin:
	mvi a, 6
	cmp c
	jz loop_e_end
	in DWAIT
	mvi a, 0
	out DDATA
	inr c
	jmp loop_e_begin
loop_e_end:
	in DWAIT
	mvi a, 254
	out DDATA
	in DWAIT
	mvi a, 0
	out DDATA
	in DWAIT
	mvi a, 0
	out DDATA
	in DWAIT
	mov a, b
	out DDATA
	in DWAIT
	mvi a, 0
	out DDATA
	in DWAIT
	mvi a, 247
	out DDATA
	mvi c, 0
loop_f_begin:
	mvi a, 17
	cmp c
	jz loop_f_end
	in DWAIT
	mvi a, 0
	out DDATA
	inr c
	jmp loop_f_begin
loop_f_end:
	in DWAIT
	mvi a, 251
	out DDATA
	mvi c, 0
loop_g_begin:
	mvi a, 128
	cmp c
	jz loop_g_end
	in DWAIT
	mvi a, 229
	out DDATA
	inr c
	jmp loop_g_begin
loop_g_end:
	in DWAIT
	mvi a, 247
	out DDATA
	mvi c, 0
loop_h_begin:
	mvi a, 27
	cmp c
	jz loop_h_end
	in DWAIT
	mvi a, 0
	out DDATA
	inr c
	jmp loop_h_begin
loop_h_end:
	inr b
	jmp loop_d_begin
loop_d_end:
end_lbl:
	in DWAIT
	ora a
	jp end
	mvi a, 0
	out DDATA
	jmp end_lbl
end:
	hlt
	mvi b, 0
loop_i_begin:
	mvi a, 10
	cmp b
	jz loop_i_end
foo
	inr b
	jmp loop_i_begin
loop_i_end:
