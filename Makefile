# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

.PHONY : all
all : jqer.native functions.o

jqer.native :
	opam config exec -- \
	rm -f *.o
	ocamlbuild -use-ocamlfind -pkgs llvm.bitreader jqer.native
	gcc -c functions.c
	clang -emit-llvm -o functions.bc -c functions.c -Wno-varargs

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log *.diff jqer scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.ll
	rm -rf *.err *.out *.exe *.s
	rm -f *.o functions.bc

# Testing the "printbig" example

printbig : functions.c
	cc -o functions -DBUILD_TEST functions.c

# Building the tarball

TESTS = \
	add1 arith1 arith2 arith3 fib float1 float2 float3 for1 for2 func1 func3 func4 \
	func5 func6 func7 func8 func9 gcd gcd2 global1 global2 global3 hello hello2 \
	if1 if2 if3 if4 if5 if6 local1 local2 ops1 ops2 remainder stringconcat \
	stringconcat2 var1 var2

FAILS = \
	assign1 assign2 assign3 assign4 dead1 expr1 expr2 float1 for1 for2 for3 func1  \
	func2 func3 func4 func5 func6 func7 global1 global2 if1 if2 if3 nomain \
	return1 return2 while1 while2


TESTFILES = $(TESTS:%=test-%.jqer) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.jqer) $(FAILS:%=fail-%.err)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags jqer.ml jqerparse.mly \
	README scanner.mll semant.ml testall.sh \
	functions.c arcade-font.pbm font2c \
	Dockerfile \
	$(TESTFILES:%=tests/%)

jqer.tar.gz : $(TARFILES)
	cd .. && tar czf jqer/jqer.tar.gz \
		$(TARFILES:%=jqer/%)
