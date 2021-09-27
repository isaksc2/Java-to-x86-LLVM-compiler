# Java to llvm / x86 compiler

A Haskell program for compiling a subset of the Java language, including arrays, to llvm assembly code, implemented [here](https://github.com/isaksc2/TDA283/blob/main/compiler/src/LlvmBackend.hs).

The program can also compile to x86 assembly. The compiler uses registers allocation optimization, which can be explained as follows:

  When the compiler first generates code, it uses many unique virtual registers, so in instead of mapping most of these virtual registers to stack variables, we want as many of them as possible to recycle the same few available PHYSICAL registers.
The essential steps to the optimization algorithm are as follows:


* 
* Perform static liveness analysis to see which virtual registers are used simultaneously.
* Create a graph where each node is a virtual register, and add edges (A, B) for each pair of nodes A and B that are being used simultaneously.
* Perform a k-coloring algorithm on the graph, every node gets mapped to a color were each color represents a PHYISICAL register in x86-64.
* Then we update the code with the mapping of virtual registers to physical registers.

you can see the code [here](https://github.com/isaksc2/TDA283/blob/main/compiler/src/X86Backend.hs). 
