declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main( ) {
entry:
	%r0 = call i32 @fact(i32 7)
	call void @printInt(i32 %r0)
	%r1 = call i32 @factr(i32 7)
	call void @printInt(i32 %r1)
	ret i32 0
}

define i32 @fact(i32 %r2 ) {
entry:
	%r3 = alloca i32
	store i32 %r2 , i32* %r3
	%r4 = alloca i32
	%r5 = alloca i32
	store i32 1 , i32* %r4
	store i32 1 , i32* %r5
	br label %lab0
lab0:
	%r6 = load i32 , i32* %r3
	%r7 = load i32 , i32* %r4
	%r8 = icmp sle i32 %r7, %r6
	br i1 %r8, label %lab1, label %lab2
lab1:
	%r10 = load i32 , i32* %r4
	%r11 = load i32 , i32* %r5
	%r9 = mul i32 %r11, %r10
	store i32 %r9 , i32* %r5
	%r12 = load i32 , i32* %r4
	%r13 = add i32 %r12, 1
	store i32 %r13 , i32* %r4
	br label %lab0
lab2:
	%r14 = load i32 , i32* %r5
	ret i32 %r14
}

define i32 @factr(i32 %r15 ) {
entry:
	%r16 = alloca i32
	store i32 %r15 , i32* %r16
	%r17 = load i32 , i32* %r16
	%r18 = icmp slt i32 %r17, 2
	br i1 %r18, label %lab3, label %lab4
lab3:
	ret i32 1
	br label %lab5
lab4:
	%r20 = load i32 , i32* %r16
	%r19 = sub i32 %r20, 1
	%r21 = call i32 @factr(i32 %r19)
	%r23 = load i32 , i32* %r16
	%r22 = mul i32 %r23, %r21
	ret i32 %r22
	br label %lab5
lab5:
}