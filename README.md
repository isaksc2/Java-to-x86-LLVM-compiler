# TDA283

-- llvm

@puts = print?

no global var in javalette but u will need global vars fro string lits

compiler

how to run

---

---

---

bnfc -d javalette.cf in src
make in src

---

---

---

i compiler:
bnfc -dm src/javalette.cf

cd c:;cd "\_it\git repos\TDA283\test"

tar -czvf partA-1.tar.gz -C ../compiler .

clear; python3 testing.py partA-1.tar.gz

cd ../compiler; make distclean; make; cd ../test

AjV>Z7DLF=

---

63 return 1;

---

javalette desc
int main() must exist
no array pointer module
print int double string
error()
read int double
1 file
int digit+
double
boolean
void return type for functionbs

typechecker may be easier with internal type for functions - pair (value type, param ttypes)
string can only be used in printstring, cant have string var

parameter list comma separated can be empty

type name

all funcs must return

precedure may do return wirthout value or not have return at all

stms
empty ;
var decl
assignment
inc dec
return
proceduree cal
if statement with or w0 else no sc
while no sc
block (without semi colon)

id a(\_a3)\*

reserved else if return while
decl can be anywhere but before var is used

can only declare var once in block

can shadow though from lower block

expr
var
literal + - \* / % < > >= <= != && ||
lazy - !
function call
overload arith op with int double
no casts, so must have same type

func def can come after call

parameter pass by value

parameters act as local variables that can be assgiend new value

man ska inte kunna ha int foo(String s)
