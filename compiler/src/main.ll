declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()
@g0 = internal constant [4 x i8] c"apa\00"
@g1 = internal constant [5 x i8] c"true\00"
@g2 = internal constant [6 x i8] c"false\00"

define i32 @main( ) {
entry:
	%r0 = alloca i32
	store i32 4 , i32* %r0
	%r1 = icmp sle i32 3, %r0
	%r2 = alloca i1
	br i1 %r1, label %lab0, label %lab1
lab0:
	%r3 = icmp ne i32 4, 2
	%r4 = alloca i1
	br i1 %r3, label %lab2, label %lab3
lab2:
	br i1 1, label %lab4, label %lab3
lab4:
	store i1 1 , i1* %r4
	br label lab5
lab3:
	store i1 0 , i1* %r4
	br label lab5
lab5:
	br i1 %r4, label %lab6, label %lab1
lab6:
	store i1 1 , i1* %r2
	br label lab7
lab1:
	store i1 0 , i1* %r2
	br label lab7
lab7:
	br i1 %r2, label %lab8, label %lab9
lab8:
	call void @printBool(i1 1)
	br label lab10
lab9:
	call void @printString(i8* @g0)
	br label lab10
lab10:
	%r5 = icmp eq i1 1, 1
	%r6 = alloca i1
	br i1 %r5, label %lab11, label %lab12
lab12:
	%r7 = call i1 @dontCallMe(i32 1)
	br i1 %r7, label %lab11, label %lab13
lab13:
	store i1 0 , i1* %r6
	br label lab14
lab11:
	store i1 1 , i1* %r6
	br label lab14
lab14:
	call void @printBool(i1 %r6)
	%r8 = fmul double -1.0, 50.0
	%r9 = fcmp slt double 4.0, %r8
	%r10 = alloca i1
	br i1 %r9, label %lab15, label %lab16
lab15:
	%r11 = call i1 @dontCallMe(i32 2)
	br i1 %r11, label %lab17, label %lab16
lab17:
	store i1 1 , i1* %r10
	br label lab18
lab16:
	store i1 0 , i1* %r10
	br label lab18
lab18:
	call void @printBool(i1 %r10)
	%r12 = icmp eq i32 4, %r0
	%r13 = alloca i1
	br i1 %r12, label %lab19, label %lab20
lab19:
	%r14 = alloca i1
	br i1 0, label %lab21, label %lab22
lab21:
	store i1 0 , i1* %r14
	br label lab23
lab22:
	store i1 1 , i1* %r14
	br label lab23
lab23:
	%r15 = icmp eq i1 1, %r14
	%r16 = alloca i1
	br i1 %r15, label %lab24, label %lab25
lab24:
	br i1 1, label %lab26, label %lab25
lab26:
	store i1 1 , i1* %r16
	br label lab27
lab25:
	store i1 0 , i1* %r16
	br label lab27
lab27:
	br i1 %r16, label %lab28, label %lab20
lab28:
	store i1 1 , i1* %r13
	br label lab29
lab20:
	store i1 0 , i1* %r13
	br label lab29
lab29:
	call void @printBool(i1 %r13)
	%r17 = call i1 @implies(i1 0, i1 0)
	call void @printBool(i1 %r17)
	%r18 = call i1 @implies(i1 0, i1 1)
	call void @printBool(i1 %r18)
	%r19 = call i1 @implies(i1 1, i1 0)
	call void @printBool(i1 %r19)
	%r20 = call i1 @implies(i1 1, i1 1)
	call void @printBool(i1 %r20)
	ret i32 0
}

define i1 @dontCallMe(i32 %r21 ) {
entry:
	call void @printInt(i32 %r1)
	ret i1 1
}

define void @printBool(i1 %r22 ) {
entry:
	br i1 %r2, label %lab30, label %lab31
lab30:
	call void @printString(i8* @g1)
	br label lab32
lab31:
	call void @printString(i8* @g2)
	br label lab32
lab32:
	ret void
}

define i1 @implies(i1 %r23, i1 %r24 ) {
entry:
	%r25 = alloca i1
	br i1 %r3, label %lab33, label %lab34
lab33:
	store i1 0 , i1* %r25
	br label lab35
lab34:
	store i1 1 , i1* %r25
	br label lab35
lab35:
	%r26 = alloca i1
	br i1 %r25, label %lab36, label %lab37
lab37:
	%r27 = icmp eq i1 %r3, %r4
	br i1 %r27, label %lab36, label %lab38
lab38:
	store i1 0 , i1* %r26
	br label lab39
lab36:
	store i1 1 , i1* %r26
	br label lab39
lab39:
	ret i1 %r26
}