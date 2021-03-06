extern printInt
extern printDouble
extern printString
extern readInt
extern readDouble


segment .text
	global main

main:
	push    rbp
	mov   rbp, rsp
	push    rbx
	push    rbx
	push    rdi
	push    rsi
	push    rdx
	push    rcx
	push    r8
	push    r9
	push    r10
	push    r11
	sub   rsp, 8
	movsd [rsp], xmm2
	sub   rsp, 8
	movsd [rsp], xmm3
	sub   rsp, 8
	movsd [rsp], xmm4
	sub   rsp, 8
	movsd [rsp], xmm5
	mov   rsi, 1
	push    rsi
	mov   rsi, 2
	push    rsi
	mov   rsi, 3
	push    rsi
	mov   rsi, 4
	push    rsi
	mov   rax, __?float64?__(100.0)
	movq  xmm2, rax
	sub   rsp, 8
	movsd [rsp], xmm2
	mov   rax, __?float64?__(100.0)
	movq  xmm2, rax
	sub   rsp, 8
	movsd [rsp], xmm2
	mov   rax, __?float64?__(100.0)
	movq  xmm2, rax
	sub   rsp, 8
	movsd [rsp], xmm2
	mov   rax, __?float64?__(100.0)
	movq  xmm2, rax
	sub   rsp, 8
	movsd [rsp], xmm2
	mov   rsi, 5
	push    rsi
	mov   rsi, 6
	push    rsi
	mov   rsi, 7
	push    rsi
	mov   rsi, 8
	push    rsi
	mov   rsi, 9
	push    rsi
	mov   rsi, 10
	push    rsi
	mov   rsi, 11
	push    rsi
	mov   rsi, 12
	push    rsi
	mov   rsi, 13
	push    rsi
	mov   rsi, 14
	push    rsi
	mov   rsi, 15
	push    rsi
	mov   rsi, 16
	push    rsi
	call  many_params
	add   rsp, 160
	movsd xmm5, [rsp]
	add   rsp, 8
	movsd xmm4, [rsp]
	add   rsp, 8
	movsd xmm3, [rsp]
	add   rsp, 8
	movsd xmm2, [rsp]
	add   rsp, 8
	pop     r11
	pop     r10
	pop     r9
	pop     r8
	pop     rcx
	pop     rdx
	pop     rsi
	pop     rdi
	pop     rbx
	pop     rbx
	mov   rsi, 0
	mov   rax, rsi
	mov   rsp, rbp
	pop     rbp
	ret


many_params:
	push    rbp
	mov   rbp, rsp
	push    rbx
	push    rbx
	push    rdi
	push    rsi
	push    rdx
	push    rcx
	push    r8
	push    r9
	push    r10
	push    r11
	sub   rsp, 8
	movsd [rsp], xmm2
	sub   rsp, 8
	movsd [rsp], xmm3
	sub   rsp, 8
	movsd [rsp], xmm4
	sub   rsp, 8
	movsd [rsp], xmm5
	mov   rdi, [rbp + 168]
	call  printInt
	movsd xmm5, [rsp]
	add   rsp, 8
	movsd xmm4, [rsp]
	add   rsp, 8
	movsd xmm3, [rsp]
	add   rsp, 8
	movsd xmm2, [rsp]
	add   rsp, 8
	pop     r11
	pop     r10
	pop     r9
	pop     r8
	pop     rcx
	pop     rdx
	pop     rsi
	pop     rdi
	pop     rbx
	pop     rbx
	push    rbx
	push    rbx
	push    rdi
	push    rsi
	push    rdx
	push    rcx
	push    r8
	push    r9
	push    r10
	push    r11
	sub   rsp, 8
	movsd [rsp], xmm2
	sub   rsp, 8
	movsd [rsp], xmm3
	sub   rsp, 8
	movsd [rsp], xmm4
	sub   rsp, 8
	movsd [rsp], xmm5
	mov   rdi, [rbp + 104]
	call  printInt
	movsd xmm5, [rsp]
	add   rsp, 8
	movsd xmm4, [rsp]
	add   rsp, 8
	movsd xmm3, [rsp]
	add   rsp, 8
	movsd xmm2, [rsp]
	add   rsp, 8
	pop     r11
	pop     r10
	pop     r9
	pop     r8
	pop     rcx
	pop     rdx
	pop     rsi
	pop     rdi
	pop     rbx
	pop     rbx
	push    rbx
	push    rbx
	push    rdi
	push    rsi
	push    rdx
	push    rcx
	push    r8
	push    r9
	push    r10
	push    r11
	sub   rsp, 8
	movsd [rsp], xmm2
	sub   rsp, 8
	movsd [rsp], xmm3
	sub   rsp, 8
	movsd [rsp], xmm4
	sub   rsp, 8
	movsd [rsp], xmm5
	mov   rdi, [rbp + 72]
	call  printInt
	movsd xmm5, [rsp]
	add   rsp, 8
	movsd xmm4, [rsp]
	add   rsp, 8
	movsd xmm3, [rsp]
	add   rsp, 8
	movsd xmm2, [rsp]
	add   rsp, 8
	pop     r11
	pop     r10
	pop     r9
	pop     r8
	pop     rcx
	pop     rdx
	pop     rsi
	pop     rdi
	pop     rbx
	pop     rbx
	push    rbx
	push    rbx
	push    rdi
	push    rsi
	push    rdx
	push    rcx
	push    r8
	push    r9
	push    r10
	push    r11
	sub   rsp, 8
	movsd [rsp], xmm2
	sub   rsp, 8
	movsd [rsp], xmm3
	sub   rsp, 8
	movsd [rsp], xmm4
	sub   rsp, 8
	movsd [rsp], xmm5
	mov   rdi, [rbp + 40]
	call  printInt
	movsd xmm5, [rsp]
	add   rsp, 8
	movsd xmm4, [rsp]
	add   rsp, 8
	movsd xmm3, [rsp]
	add   rsp, 8
	movsd xmm2, [rsp]
	add   rsp, 8
	pop     r11
	pop     r10
	pop     r9
	pop     r8
	pop     rcx
	pop     rdx
	pop     rsi
	pop     rdi
	pop     rbx
	pop     rbx
	push    rbx
	push    rbx
	push    rdi
	push    rsi
	push    rdx
	push    rcx
	push    r8
	push    r9
	push    r10
	push    r11
	sub   rsp, 8
	movsd [rsp], xmm2
	sub   rsp, 8
	movsd [rsp], xmm3
	sub   rsp, 8
	movsd [rsp], xmm4
	sub   rsp, 8
	movsd [rsp], xmm5
	mov   xmm0, [rbp + 136]
	call  printDouble
	movsd xmm5, [rsp]
	add   rsp, 8
	movsd xmm4, [rsp]
	add   rsp, 8
	movsd xmm3, [rsp]
	add   rsp, 8
	movsd xmm2, [rsp]
	add   rsp, 8
	pop     r11
	pop     r10
	pop     r9
	pop     r8
	pop     rcx
	pop     rdx
	pop     rsi
	pop     rdi
	pop     rbx
	pop     rbx
	mov   rsi, [rbp + 168]
	mov   r8, 2
	mov   rax, rsi
	cmp   rax, r8
	jne    L2
	jmp   L3
L2:
	mov   rax, 1
	jmp   L4
L3:
	mov   rax, 0
	jmp   L4
L4:
	cmp   rax, 1
	je    L0
	jmp   L1
L0:
	push    rbx
	push    rbx
	push    rdi
	push    rsi
	push    rdx
	push    rcx
	push    r8
	push    r9
	push    r10
	push    r11
	sub   rsp, 8
	movsd [rsp], xmm2
	sub   rsp, 8
	movsd [rsp], xmm3
	sub   rsp, 8
	movsd [rsp], xmm4
	sub   rsp, 8
	movsd [rsp], xmm5
	push   qword [rbp + 16]
	push   qword [rbp + 168]
	push   qword [rbp + 160]
	push   qword [rbp + 152]
	movsd xmm2, [rbp + 112]
	mov   rax, __?float64?__(2.0)
	movq  xmm3, rax
	movsd xmm0, xmm2
	divsd xmm0, xmm3
	sub   rsp, 8
	movsd [rsp], xmm0
	movsd xmm2, [rbp + 136]
	mov   rax, __?float64?__(2.0)
	movq  xmm3, rax
	movsd xmm0, xmm2
	mulsd xmm0, xmm3
	sub   rsp, 8
	movsd [rsp], xmm0
	movsd xmm2, [rbp + 128]
	mov   rax, __?float64?__(1.0)
	movq  xmm3, rax
	movsd xmm0, xmm2
	addsd xmm0, xmm3
	sub   rsp, 8
	movsd [rsp], xmm0
	movsd xmm2, [rbp + 120]
	mov   rax, __?float64?__(0.0)
	movq  xmm3, rax
	movsd xmm0, xmm2
	subsd xmm0, xmm3
	sub   rsp, 8
	movsd [rsp], xmm0
	push   qword [rbp + 144]
	push   qword [rbp + 104]
	push   qword [rbp + 96]
	push   qword [rbp + 88]
	push   qword [rbp + 80]
	push   qword [rbp + 72]
	push   qword [rbp + 64]
	push   qword [rbp + 56]
	push   qword [rbp + 48]
	push   qword [rbp + 40]
	push   qword [rbp + 32]
	push   qword [rbp + 24]
	call  many_params
	add   rsp, 160
	movsd xmm5, [rsp]
	add   rsp, 8
	movsd xmm4, [rsp]
	add   rsp, 8
	movsd xmm3, [rsp]
	add   rsp, 8
	movsd xmm2, [rsp]
	add   rsp, 8
	pop     r11
	pop     r10
	pop     r9
	pop     r8
	pop     rcx
	pop     rdx
	pop     rsi
	pop     rdi
	pop     rbx
	pop     rbx
	jmp   L1
L1:
	mov   rsp, rbp
	pop     rbp
	ret
