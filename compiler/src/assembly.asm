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
	mov rdx, 3
	push rcx
	call printInt
	add 4, rsp
	push [rbp + 12]
	call printInt
	add 4, rsp
	push rdx
	call printInt
	add 4, rsp
	mov rcx, 30
	push rcx
	call printInt
	add 4, rsp
	mov rax, 0
	mov rsp, rbp
	pop rbp
	ret


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
	cmp  rcx, 1
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
