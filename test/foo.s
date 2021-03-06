declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()
declare i8* @calloc(i32, i32)

%struct1 = type {i32*, [0 x i32]*}
%struct2 = type {i32*, [0 x double]*}
%struct3 = type {i32*, [0 x i1]*}

define i32 @foo(i32 %r0 ) {
entry:
	%r1 = alloca i32
	store i32 %r0 , i32* %r1
	%r2 = load i32 , i32* %r1
	%r3 = load i32 , i32* %r1
	%r4 = add i32 %r2, %r3
	ret i32 %r4
}

define i32 @main( ) {
entry:
	%r5 = call i32 @foo(i32 7)
	%r6 = add i32 %r5, 1
	%r7 = call i8* @calloc(i32 %r6, i32 4)
	%r8 = bitcast i8* %r7 to {i32, [ 0 x i32 ]}*
	%r9 = call i32 @foo(i32 7)
	%r10 = getelementptr {i32, [ 0 x i32 ]}, {i32, [ 0 x i32 ]}* %r8, i32 0, i32 0
	store i32 %r9 , i32* %r10
	%r11 = getelementptr {i32, [ 0 x i32 ]}, {i32, [ 0 x i32 ]}* %r8, i32 0, i32 0
	%r12 = load i32 , i32* %r11
	call void @printInt(i32 %r12)
	%r13 = getelementptr {i32, [ 0 x i32 ]}, {i32, [ 0 x i32 ]}* %r8, i32 0, i32 0
	%r14 = load i32 , i32* %r13
	call void @printInt(i32 %r14)
	ret i32 0
}