extern printInt
extern printDouble
extern printString
extern readInt
extern readDouble

segment .data
str0 db "hi"
str1 db "ayyyy"

segment .text
	global main

f:
	push rbp
	mov rbp, rsp
	mov rcx, 3
	mov ________r1, 3
	push ________r0
	call printInt
	add 4, rsp
	push [rbp + 12]
	call printInt
	add 4, rsp
	mov ________r2, 30
	push ________r2
	call printInt
	add 4, rsp
	mov rax, 0
	mov rsp, rbp
	pop rbp
	ret
	; [[True,True,True],[True,True,True],[True,True,True],[True,True,True],[True,False,True],[False,False,True],[False,False,True],[False,False,True],[False,False,True],[False,False,True],[False,False,True],[False,False,True],[False,False,False],[False,False,False],[False,False,False],[False,False,False],[False,False,False],[False,False,False]]


main:
	push rbp
	mov rbp, rsp
	cmp  1, 1
	jeq L2
	jmp L3
L2:
	mov rax, 1
	jmp L4
L3:
	mov rax, 0
	jmp L4
L4:
	cmp  ________r0, 1
	jeq L0
	jmp L1
L0:
	push 42
	call printInt
	add 4, rsp
	jmp L1
L1:
	push str0
	call printString
	add 4, rsp
	push str1
	call printString
	add 4, rsp
	mov rax, 0
	mov rsp, rbp
	pop rbp
	ret
	; [[False],[False],[False],[False],[False],[False],[False],[False],[False],[False],[False],[True],[True],[False],[False],[False],[False],[False],[False],[False],[False],[False],[False],[False],[False],[False],[False],[False],[False],[False],[False]]
