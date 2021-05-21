extern printInt
extern printDouble
extern printString
extern readInt
extern readDouble

segment .data
str0 db "hello */"
str1 db "/* world"

segment .text
	global main

main:
	push  rbp
	mov  rbp, rsp
	push  0
	push  10
	call fac
	add  rsp, 4
	push  rax
	call printInt
	add  rsp, 4
	push  10
	call rfac
	add  rsp, 4
	push  rax
	call printInt
	add  rsp, 4
	push  10
	call mfac
	add  rsp, 4
	push  rax
	call printInt
	add  rsp, 4
	push  10
	call ifac
	add  rsp, 4
	push  rax
	call printInt
	add  rsp, 4
	mov  rcx, 10
	mov  rdi, 1
	jmp  L0
L0:
	cmp  rcx, 0
	ja  L3
	jmp  L4
L3:
	mov  rax, 1
	jmp  L5
L4:
	mov  rax, 0
	jmp  L5
L5:
	cmp  rcx, 1
	je  L1
	jmp  L2
L1:
	mul rdi, rcx
	mov  rdi, rsi
	dec rcx
	jmp  L0
L2:
	push  rdi
	call printInt
	add  rsp, 4
	push __?float32?__(10.0)
	call dfac
	add  rsp, 8
	push __?float32?__(xmm0)
	call printDouble
	add  rsp, 8
	push word [str0]
	call printString
	add  rsp, 4
	push word [str1]
	call printString
	add  rsp, 4
	mov  rax, 0
	mov  rsp, rbp
	pop  rbp
	ret


fac:
	push  rbp
	mov  rbp, rsp
	mov  rcx, 1
	mov  rdi, [rbp + 8]
	jmp  L6
L6:
	cmp  rdi, 0
	ja  L9
	jmp  L10
L9:
	mov  rax, 1
	jmp  L11
L10:
	mov  rax, 0
	jmp  L11
L11:
	cmp  rcx, 1
	je  L7
	jmp  L8
L7:
	mul rcx, rdi
	mov  rcx, rsi
	sub  rdi, 1
	mov  rdi, r8
	jmp  L6
L8:
	mov  rax, rcx
	mov  rsp, rbp
	pop  rbp
	ret


rfac:
	push  rbp
	mov  rbp, rsp
	cmp word [rbp + 8], 0
	je  L12
	jmp  L13
L12:
	mov  rax, 1
	jmp  L14
L13:
	mov  rax, 0
	jmp  L14
L14:
	cmp  rcx, 1
	je  L15
	jmp  L16
L15:
	mov  rax, 1
	mov  rsp, rbp
	pop  rbp
	ret
L16:
	sub  [rbp + 8], 1
	push  rdi
	call rfac
	add  rsp, 4
	mul rdi, rax
	mov  rax, rsi
	mov  rsp, rbp
	pop  rbp
	ret


mfac:
	push  rbp
	mov  rbp, rsp
	cmp word [rbp + 8], 0
	je  L17
	jmp  L18
L17:
	mov  rax, 1
	jmp  L19
L18:
	mov  rax, 0
	jmp  L19
L19:
	cmp  rcx, 1
	je  L20
	jmp  L21
L20:
	mov  rax, 1
	mov  rsp, rbp
	pop  rbp
	ret
L21:
	sub  [rbp + 8], 1
	push  rdi
	call nfac
	add  rsp, 4
	mul rdi, rax
	mov  rax, rsi
	mov  rsp, rbp
	pop  rbp
	ret


nfac:
	push  rbp
	mov  rbp, rsp
	cmp word [rbp + 8], 0
	jne  L22
	jmp  L23
L22:
	mov  rax, 1
	jmp  L24
L23:
	mov  rax, 0
	jmp  L24
L24:
	cmp  rcx, 1
	je  L25
	jmp  L26
L25:
	sub  [rbp + 8], 1
	push  rcx
	call mfac
	add  rsp, 4
	mul rax, [rbp + 8]
	mov  rax, rdi
	mov  rsp, rbp
	pop  rbp
	ret
L26:
	mov  rax, 1
	mov  rsp, rbp
	pop  rbp
	ret


dfac:
	push  rbp
	mov  rbp, rsp
	push  0
	cmpsddword [rbp + 8], 0.0
	je  L27
	jmp  L28
L27:
	mov  rax, 1
	jmp  L29
L28:
	mov  rax, 0
	jmp  L29
L29:
	cmp  rcx, 1
	je  L30
	jmp  L31
L30:
	mov  xmm0, 1.0
	mov  rsp, rbp
	pop  rbp
	ret
L31:
	subsd [rbp + 8], 1.0
	push __?float32?__(rcx)
	call dfac
	add  rsp, 8
	fmul rcx, xmm0
	mov  xmm0, rcx
	mov  rsp, rbp
	pop  rbp
	ret


ifac:
	push  rbp
	mov  rbp, rsp
	push  1
	push word [rbp + 8]
	call ifac2f
	add  rsp, 8
	mov  rsp, rbp
	pop  rbp
	ret


ifac2f:
	push  rbp
	mov  rbp, rsp
	push  0
	cmp word [rbp + 8], [rbp + 12]
	je  L34
	jmp  L35
L34:
	mov  rax, 1
	jmp  L36
L35:
	mov  rax, 0
	jmp  L36
L36:
	cmp  rcx, 1
	je  L32
	jmp  L33
L32:
	mov  rax, [rbp + 8]
	mov  rsp, rbp
	pop  rbp
	ret
	jmp  L33
L33:
	cmp word [rbp + 8], [rbp + 12]
	ja  L39
	jmp  L40
L39:
	mov  rax, 1
	jmp  L41
L40:
	mov  rax, 0
	jmp  L41
L41:
	cmp  rcx, 1
	je  L37
	jmp  L38
L37:
	mov  rax, 1
	mov  rsp, rbp
	pop  rbp
	ret
	jmp  L38
L38:
	add  [rbp + 8], [rbp + 12]
	mov  rax, rcx
	mov  rbx, 2
	div  rbx
	mov  rcx, rax
	mov  rcx, rcx
	push word [rbp + 8]
	push  rcx
	call ifac2f
	add  rsp, 8
	add  rcx, 1
	push  rdi
	push word [rbp + 12]
	call ifac2f
	add  rsp, 8
	mul [rbp + 12], rax
	mov  rax, rsi
	mov  rsp, rbp
	pop  rbp
	ret
