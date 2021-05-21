extern printInt
extern printDouble
extern printString
extern readInt
extern readDouble


segment .text
	global main

main:
	push  rbp
	mov   rbp, rsp
	mov   rcx, 17    ; 17
	mov   rbx, rcx   ; y = 17
	jmp   L0
L0:
	mov   rcx, 0    ; y > 0 ?
	mov   rax, rbx
	cmp   rax, rcx
	ja    L3
	jmp   L4
L3:
	mov   rax, 1    ; true
	jmp   L5
L4:
	mov   rax, 0    ; false
	jmp   L5
L5:
	cmp   rax, 1    ; true?
	je    L1		; goto while
	jmp   L2		; else skip while
L1:					
	mov   rcx, 2    ; y = y-2
	mov   rax, rbx
	sub   rax, rcx
	mov   rbx, rax
	jmp   L0
L2:
	mov   rcx, 0     ; y < 0 ?
	mov   rax, rbx
	cmp   rax, rcx
	jb    L6
	jmp   L7
L6:
	mov   rax, 1     : true
	jmp   L8
L7:
	mov   rax, 0     ; false
	jmp   L8
L8:
	cmp   rax, 1    ; true?
	je    L9        ; goto if
	jmp   L10		; goto else
L9:
	mov   rcx, 0    ; "push" 0 as argument to printInt
	mov   rdi, rcx
								push    rsi		; "save" registers
								push    rdx
								push    rcx
								push    r8
								push    r9
								push    r10
								push    r11
								push    r11
	call  printInt  ; print
								pop     r11     ; "restore" registers
								pop     r11
								pop     r10
								pop     r9
								pop     r8
								pop     rcx
								pop     rdx
								pop     rsi
	mov   rcx, 0    ; return 0
	mov   rax, rcx
	mov   rsp, rbp
	pop   rbp
	ret
L10:
	mov   rcx, 1    ; "push" 1 as argument to printInt 
	mov   rdi, rcx
								push    rsi		; "save" registers
								push    rdx
								push    rcx
								push    r8
								push    r9
								push    r10
								push    r11
								push    r11
	call  printInt   ; print
								pop     r11	     ; "restore" registers
								pop     r11
								pop     r10
								pop     r9
								pop     r8
								pop     rcx
								pop     rdx
								pop     rsi
	mov   rcx, 0     ; return 0
	mov   rax, rcx
	mov   rsp, rbp
	pop   rbp
	ret