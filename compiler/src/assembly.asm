extern printInt
extern printDouble
extern printString
extern readInt
extern readDouble


segment .text
	global main

main:
	push rbp
	mov  rbp, rsp
	push 0
	push 1
	push 2
	push 3
	push 4
	push dword 100.0
	push dword 100.0
	push dword 100.0
	push dword 100.0
	push 5
	push 6
	push 7
	push 8
	push 9
	push 10
	push 11
	push 12
	push 13
	push 14
	push 15
	push 16
	call many_params
	add  96, rsp
	mov  rax, 0
	mov  rsp, rbp
	pop  rbp
	ret


many_params:
	push rbp
	mov  rbp, rsp
	push 0
	push [rbp + 8]
	call printInt
	add  4, rsp
	push [rbp + 56]
	call printInt
	add  4, rsp
	push [rbp + 72]
	call printInt
	add  4, rsp
	push [rbp + 88]
	call printInt
	add  4, rsp
	push [rbp + 24]
	call printDouble
	add  8, rsp
	cmp  [rbp + 8], 2
	jne  L2
	jmp  L3
L2:
	mov  rax, 1
	jmp  L4
L3:
	mov  rax, 0
	jmp  L4
L4:
	cmp  rcx, 1
	jeq  L0
	jmp  L1
L0:
	push [rbp + 100]
	push [rbp + 8]
	push [rbp + 12]
	push [rbp + 16]
	fdiv [rbp + 48], 2.0
	push dword rcx
	fmul [rbp + 24], 2.0
	push dword rcx
	addsd [rbp + 32], 1.0
	push dword rcx
	subsd [rbp + 40], 0.0
	push dword rcx
	push [rbp + 20]
	push [rbp + 56]
	push [rbp + 60]
	push [rbp + 64]
	push [rbp + 68]
	push [rbp + 72]
	push [rbp + 76]
	push [rbp + 80]
	push [rbp + 84]
	push [rbp + 88]
	push [rbp + 92]
	push [rbp + 96]
	call many_params
	add  96, rsp
	jmp  L1
L1:
	mov  rsp, rbp
	pop  rbp
	ret
