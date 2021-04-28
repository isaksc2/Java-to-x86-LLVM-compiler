declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main( ) {
entry:
	%r0 = call i32 @fac(i32 5)
	call void @printInt(i32 %r0)
	ret i32 0
}

define i32 @fac(i32 %r2 ) {
entry:
	%r2 = alloca i32
	store i32 %r1 , i32* %r2
	%r3 = alloca i32
	%r4 = alloca i32
	store i32 1 , i32* %r3
	%r5 = load i32 , i32* %r2
	store i32 %r5 , i32* %r4
	br label %lab0
lab0:
	%r6 = load i32 , i32* %r4
	%r7 = icmp sgt i32 %r6, 0
	br i1 %r7, label %lab1, label %lab2
lab1:
	%r9 = load i32 , i32* %r4
	%r10 = load i32 , i32* %r3
	%r8 = mul i32 %r10, %r9
	store i32 %r8 , i32* %r3
	%r12 = load i32 , i32* %r4
	%r11 = sub i32 %r12, 1
	store i32 %r11 , i32* %r4
	br label %lab0
lab2:
	%r13 = load i32 , i32* %r3
	ret i32 %r13
}