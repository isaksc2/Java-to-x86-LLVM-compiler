# Makefile for PLT lab 2 in Haskell

## Variables
###########################################################################

# Files of solution.  Add more files as needed.  'lab2.hs'  has to be first!
files=a.txt

###########################################################################

# List of goals not corresponding to file names.
.PHONY : all clean distclean

a.txt : 
	cd src; make; mv jlc_x64 ../

all :
	cd src; make; mv jlc_x64 ../

clean :
	cd src; make clean

distclean : clean
	-rm jlc_x64
	cd src && make distclean

# EOF