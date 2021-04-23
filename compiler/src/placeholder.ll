@g0 = internal constant [5 x i8] c"foo2\00"
@g1 = internal constant [10 x i8] c"foasdasdo\00"
@g2 = internal constant [10 x i8] c"fssssssoo\00"
@g3 = internal constant [4 x i8] c"foo\00"

define i32 @main() {
entry:
	call void @foo()
	%r0 = alloca i32
	store i32 3 , i32* %r0
	store i32 2 , i32* %r0
	%r1 = alloca double
	store double 3.0 , double* %r1
	%r2 = fmul double 2.0, %r1
	%r3 = alloca double
	store double %r2 , double* %r3
	ret i32 0
}

define void @lol() {
entry:
L0:
	br i1 0 , Label L1 , Label L2
L1:
	br Label L0
L2:
	call void @printString(i8* @g0)
	call void @printString(i8* @g1)
	call void @printString(i8* @g2)
}

define void @foo() {
entry:
	call void @printString(i8* @g3)
L3:
	br i1 0 , Label L4 , Label L5
L4:
	call void @printInt(i32 1)
	call void @printDouble(double 2.9)
	br Label L3
L5:
	ret
	%r4 = mul i32 4, 3
	%r5 = add i32 %r4, 3
	%r6 = alloca i32
	store i32 %r5 , i32* %r6
}