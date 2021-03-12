parser: parser.mly
	ocamlyacc -v parser.mly

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo *.output parser.ml parser.mli scanner.ml