# Makefile for PLT lab 2 in Haskell

## Variables
###########################################################################

# Files of solution.  Add more files as needed.  'lab2.hs'  has to be first!
files=a.txt

###########################################################################

# List of goals not corresponding to file names.
.PHONY : all clean distclean

a.txt : 
	cd src; make

all :
	cd src; make; mv jlc.exe ..

clean :
	cd src; make clean

distclean : clean
	cd src; make distclean
	-rm jlc.exe

# EOF
