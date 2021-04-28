declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

define i32 @main( ) {
entry:
	br i1 1, label %lab0, label %lab1
lab0:
	call void @printInt(i32 1)
	ret i32 0
	br label %lab1
lab1:
}