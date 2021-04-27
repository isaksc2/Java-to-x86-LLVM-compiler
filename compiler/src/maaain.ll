declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()
@g0 = internal constant [2 x i8] c"a\00"
@g1 = internal constant [4 x i8] c"apa\00"

define i32 @main( ) {
entry:
	%r0 = alloca i32
	store i32 4 , i32* %r0
	%r1 = icmp sle i32 %r0, 3
	br i1 %r1, label %lab3, label %lab5
lab3:
	%r2 = icmp ne i32 2, 4
	br i1 %r2, label %lab6, label %lab8
lab6:
	br i1 1, label %lab7, label %lab8
lab7:
	ret void
lab8:
	br i1 0, label %lab4, label %lab5
lab4:
	ret void
lab5:
	br i1 0, label %lab0, label %lab1
lab0:
	call void @printString(i8* @g0)
	br label lab2
lab1:
	call void @printString(i8* @g1)
	ret void
lab2:
	ret i32 0
}

define i1 @dontCallMe(i32 %r3 ) {
entry:
	call void @printInt(i32 %r1)
	ret i1 1
}

define i1 @implies(i1 %r4, i1 %r5 ) {
entry:
	br i1 %r2, label %lab12, label %lab13
lab12:
	ret void
lab13:
	br i1 1, label %lab9, label %lab10
lab10:
	%r6 = icmp eq i1 %r3, %r2
	br i1 %r6, label %lab9, label %lab11
lab11:
	ret void
lab9:
	ret i1 1
}