define i32 @main() {
entry:
	call void @foo()
	ret i32 0
}

define void @foo() {
entry:
	call void @printString(i8* "foo")
	ret
}