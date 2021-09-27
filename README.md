# Java to llvm / x86 compiler

A Haskell program for compiling a subset of the Java language, including arrays, to llvm assembly code, implemented [here](https://github.com/isaksc2/TDA283/blob/main/compiler/src/LlvmBackend.hs).

The program can also compile to x86 assembly for the most part, but it is a work in progress. The compiler uses a graph coloring algorithm to calculate which registers can be recycled for better performance. you can see the code [here](https://github.com/isaksc2/TDA283/blob/main/compiler/src/X86Backend.hs).
