declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main( ) {
entry:
	call void @many_params(i32 1, i32 2, i32 3, i32 4, double 100.0, double 100.0, double 100.0, double 100.0, i32 5, i32 6, i32 7, i32 8, i32 9, i32 10, i32 11, i32 12, i32 13, i32 14, i32 15, i32 16)
	ret i32 0
}

define void @many_params(i32 %r0, i32 %r1, i32 %r2, i32 %r3, double %r4, double %r5, double %r6, double %r7, i32 %r8, i32 %r9, i32 %r10, i32 %r11, i32 %r12, i32 %r13, i32 %r14, i32 %r15, i32 %r16, i32 %r17, i32 %r18, i32 %r19 ) {
entry:
	call void @printInt(i32 %r0)
	call void @printInt(i32 %r8)
	call void @printInt(i32 %r12)
	call void @printInt(i32 %r16)
	call void @printDouble(double %r4)
	%r20 = icmp ne i32 2, %r0
	br i1 %r20, label %lab0, label %lab1
lab0:
	%r21 = fdiv double 2.0, %r7
	%r22 = fmul double 2.0, %r4
	%r23 = fadd double 1.0, %r5
	%r24 = fsub double 0.0, %r6
	call void @many_params(i32 %r19, i32 %r0, i32 %r1, i32 %r2, double %r21, double %r22, double %r23, double %r24, i32 %r3, i32 %r8, i32 %r9, i32 %r10, i32 %r11, i32 %r12, i32 %r13, i32 %r14, i32 %r15, i32 %r16, i32 %r17, i32 %r18)
	ret void
lab1:
	ret void
}