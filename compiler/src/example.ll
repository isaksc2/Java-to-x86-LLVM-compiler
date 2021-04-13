declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()


define i32 @main() {
entry:  %t0 = call i32 @fact(i32 7)             ; function call
        call void @printInt(i32 %t0)
        ret  i32 0

}

define i32 @fact(i32 %__p__n) {
entry:  %n = alloca i32                         ; allocate a variable on stack
        store i32 %__p__n , i32* %n             ; store parameter
        %i = alloca i32
        %r = alloca i32
        store i32 1 , i32* %i                   ; store initial values
        store i32 1 , i32* %r
        br label %lab0                          ; branch to lab0

lab0:   %t0 = load i32, i32* %i                 ; load i
        %t1 = load i32, i32* %n                 ; and n
        %t2 = icmp sle i32 %t0 , %t1            ; boolean %t2 will hold i <= n
        br i1 %t2 , label %lab1 , label %lab2   ; branch depending on %t2

lab1:   %t3 = load i32, i32* %r
        %t4 = load i32, i32* %i
        %t5 = mul i32 %t3 , %t4                 ; compute i * r
        store i32 %t5 , i32* %r                 ; store product
        %t6 = load i32, i32* %i                 ; fetch i,
        %t7 = add i32 %t6 , 1                   ; add 1
        store i32 %t7 , i32* %i                 ; and store
        br label %lab0

lab2:   %t8 = load i32, i32* %r
        ret  i32 %t8

}