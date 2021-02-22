parser: parser.mly
	ocamlyacc parser.mly

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml