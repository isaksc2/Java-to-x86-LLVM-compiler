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
	push  42
	call printInt
	add  rsp, 4
	mov  rax, 0
	mov  rsp, rbp
	pop  rbp
	ret

