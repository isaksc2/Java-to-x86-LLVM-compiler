# Java to llvm / x86 compiler

A Haskell program for compiling a subset of the Java language, including arrays, to llvm assembly code, implemented [here](https://github.com/isaksc2/TDA283/blob/main/compiler/src/LlvmBackend.hs).

The program can also compile to x86 assembly for the most part, but it is a work in progress. The compiler uses registers allocation optimization, meaning registers are recycled for better performance. The essential steps to the algorithm are as follows:

* Bullet list
* Bullet list
* Bullet list

you can see the code [here](https://github.com/isaksc2/TDA283/blob/main/compiler/src/X86Backend.hs). 
