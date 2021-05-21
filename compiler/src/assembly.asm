extern printInt
extern printDouble
extern printString
extern readInt
extern readDouble


segment .text
	global main

main:
	push  rbp
	mov  rbp, rsp
	push  0
	mov  rax, 1
	cmp   rax, 1
	je  L2
	jmp  L3
L2:
	mov  rax, 1
	jmp  L4
L3:
	mov  rax, 0
	jmp  L4
L4:
	cmp   rax, 1
	je  L0
	jmp  L1
L0:
	push  42
	call printInt
	add  rsp, 4
	jmp  L1
L1:
	mov  rax, 0
	mov  rsp, rbp
	pop  rbp
	ret
