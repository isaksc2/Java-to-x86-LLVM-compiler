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
	sub   rsp, 48
	mov   rsi, 1
	mov   r8, rsi
	mov   rsi, 2
	mov   r9, rsi
	mov   rsi, 3
	mov   r10, rsi
	mov   rsi, 4
	mov   r11, rsi
	mov   rsi, 5
	mov   r12, rsi
	mov   rsi, 6
	mov   r13, rsi
	mov   rsi, 7
	mov   r14, rsi
	mov   rsi, 8
	mov   r15, rsi
	mov   rsi, 9
	mov   [rbp - 8], rsi
	mov   rsi, 10
	mov   [rbp - 16], rsi
	mov   rsi, 11
	mov   [rbp - 24], rsi
	mov   rsi, 12
	mov   [rbp - 32], rsi
	mov   rax, __?float64?__(1.0)
	movq  xmm2, rax
	movsd xmm3, xmm2
	mov   rax, __?float64?__(2.0)
	movq  xmm2, rax
	movsd xmm4, xmm2
	mov   rax, __?float64?__(3.0)
	movq  xmm2, rax
	movsd xmm5, xmm2
	mov   rax, __?float64?__(4.0)
	movq  xmm2, rax
	movsd xmm6, xmm2
	mov   rax, __?float64?__(5.0)
	movq  xmm2, rax
	movsd xmm7, xmm2
	mov   rax, __?float64?__(6.0)
	movq  xmm2, rax
	movsd xmm8, xmm2
	mov   rax, __?float64?__(7.0)
	movq  xmm2, rax
	movsd xmm9, xmm2
	mov   rax, __?float64?__(8.0)
	movq  xmm2, rax
	movsd xmm10, xmm2
	mov   rax, __?float64?__(9.0)
	movq  xmm2, rax
	movsd xmm11, xmm2
	mov   rax, __?float64?__(10.0)
	movq  xmm2, rax
	movsd xmm12, xmm2
	mov   rax, __?float64?__(11.0)
	movq  xmm2, rax
	movsd xmm13, xmm2
	mov   rax, __?float64?__(12.0)
	movq  xmm2, rax
	movsd xmm15, xmm2
	mov   rax, __?float64?__(13.0)
	movq  xmm2, rax
	movsd [rbp - 40], xmm2
	mov   rax, __?float64?__(14.0)
	movq  xmm2, rax
	movsd [rbp - 48], xmm2
	mov   rax, r8
	add   rax, r9
	add   rax, r10
	add   rax, r11
	add   rax, r12
	add   rax, r13
	add   rax, r14
	add   rax, r15
	add   rax, [rbp - 8]
	add   rax, [rbp - 16]
	add   rax, [rbp - 24]
	add   rax, [rbp - 32]
	mov   rsi, rax
	movsd xmm0, xmm3
	addsd xmm0, xmm4
	addsd xmm0, xmm5
	addsd xmm0, xmm6
	addsd xmm0, xmm7
	addsd xmm0, xmm8
	addsd xmm0, xmm9
	addsd xmm0, xmm10
	addsd xmm0, xmm11
	addsd xmm0, xmm12
	addsd xmm0, xmm13
	addsd xmm0, xmm15
	addsd xmm0, [rbp - 40]
	addsd xmm0, [rbp - 48]
	movsd xmm2, xmm0
	mov   rsi, 0
	mov   rax, rsi
	mov   rsp, rbp
	pop     rbp
	ret
