
DWAIT .equ 0fch
DCOM .equ 0f8h
DDATA .equ 0fbh
DSTAT .equ 0f8h
DSECT .equ 0fah

	mvi d, 0
loop_b_begin: ; 0 <= d < 77 - Track Counter
	mvi a, 77
	cmp d
	jz loop_b_end
	mvi a, 0xf4
	out DCOM
	mvi b, 0
loop_c_begin: ; 0 <= b < 46
	mvi a, 46
	cmp b
	jz loop_c_end
	in DWAIT
	mvi a, 0
	out DDATA
	inr b
	jmp loop_c_begin
loop_c_end:
	in DWAIT
	mvi a, 252
	out DDATA
	mvi b, 0
loop_d_begin: ; 0 <= b < 26
	mvi a, 26
	cmp b
	jz loop_d_end
	in DWAIT
	mvi a, 0
	out DDATA
	inr b
	jmp loop_d_begin
loop_d_end:
	mvi b, 1
loop_e_begin: ; 1 <= b < 27 - Segment Counter
	mvi a, 27
	cmp b
	jz loop_e_end
	mvi c, 0
loop_f_begin: ; 0 <= c < 6
	mvi a, 6
	cmp c
	jz loop_f_end
	in DWAIT
	mvi a, 0
	out DDATA
	inr c
	jmp loop_f_begin
loop_f_end:
	in DWAIT
	mvi a, 254
	out DDATA
	in DWAIT
	mov a, d
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
loop_g_begin: ; 0 <= c < 17
	mvi a, 17
	cmp c
	jz loop_g_end
	in DWAIT
	mvi a, 0
	out DDATA
	inr c
	jmp loop_g_begin
loop_g_end:
	in DWAIT
	mvi a, 251
	out DDATA
	mvi c, 0
loop_h_begin: ; 0 <= c < 128
	mvi a, 128
	cmp c
	jz loop_h_end
	in DWAIT
	mvi a, 229
	out DDATA
	inr c
	jmp loop_h_begin
loop_h_end:
	in DWAIT
	mvi a, 247
	out DDATA
	mvi c, 0
loop_i_begin: ; 0 <= c < 27
	mvi a, 27
	cmp c
	jz loop_i_end
	in DWAIT
	mvi a, 0
	out DDATA
	inr c
	jmp loop_i_begin
loop_i_end:
	inr b
	jmp loop_e_begin
loop_e_end:
end_seq_j_begin:
	in DWAIT
	ora a
	jp end_seq_k_end
	mvi a, 0
	out DDATA
	jmp end_seq_j_begin
end_seq_k_end:

        mvi a, 0b01011100
        out DCOM
        IN DWAIT
    
	inr d
	jmp loop_b_begin
loop_b_end:
	hlt
