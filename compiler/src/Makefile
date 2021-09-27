# Makefile for PLT lab 2 in Haskell

## Variables
###########################################################################

# Files of solution.  Add more files as needed.  'lab2.hs'  has to be first!
files=jlc.hs jlcArray.hs Javalette.cf Makefile TypeChecker.hs LlvmBackend.hs X86Backend.hs

###########################################################################

# List of goals not corresponding to file names.
.PHONY : sdist clean distclean

# 'lab2' is default goal and needs to remain first goal in this file!
# The extra dependency Javalette/Test ensures that the parser is built from Javalette.cf

jlc : $(files) Javalette/Test
	ghc --make jlcArray.hs -o jlc

jlc_x64 : $(files) Javalette/Test
	ghc --make jlc.hs -o jlc_x64


# todo jlc

# Rules to build the parser:

Javalette/Test.hs Javalette/Lex.x Javalette/Par.y : Javalette.cf
	bnfc -d $<

%.hs : %.y
	happy -gcai $<
#	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

Javalette/Test : Javalette/Test.hs Javalette/Par.hs Javalette/Lex.hs
	ghc --make $< -o $@

# Rules for shipping the solution:

sdist : lab1.tar.gz

tmpdir := $(shell mktemp -d)
lab1.tar.gz : $(files)
	mkdir $(tmpdir)/lab2
	cp $^ $(tmpdir)/lab2/
	tar -C $(tmpdir) -czhf $@ lab1

# Rules for cleaning generated files:

all : 
	ghc --make jlcArray.hs -o jlc
clean :
	-rm -f Javalette/*.log Javalette/*.aux Javalette/*.hi Javalette/*.o Javalette/*.dvi *.hi *.o

distclean : clean
	-rm -f Javalette/Doc.* Javalette/Lex.* Javalette/Par.* Javalette/Layout.* Javalette/Skel.* Javalette/Print.* Javalette/Test.* Javalette/Abs.* Javalette/Test Javalette/ErrM.* Javalette/SharedString.* Javalette/ComposOp.* Javalette/Javalette.dtd Javalette/XML.* Javalette/*.bak
	-rmdir -p Javalette/
	-rm -f lab1.tar.gz lab2
	-rm jlc_x64
	-rm jlc
	-rm main.ll

# EOF
