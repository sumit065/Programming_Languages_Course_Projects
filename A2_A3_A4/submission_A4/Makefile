lexer.ml: lexer.mll
	ocamllex lexer.mll

lexer: lexer.ml
	ocamlopt -o lexer lexer.ml

parser.ml: parser.mly
	ocamlyacc parser.mly

parser: parser.ml
	rm parser.mli
	ocamlopt -c backend.ml
	ocamlopt -c parser.ml	
	ocamlopt -o parser str.cmxa backend.ml parser.ml

clean_parser:
	rm -f parser.mli parser.ml parser parser.cmi parser.cmx parser.o

clean_lexer:
	rm -f lexer lexer.cmi lexer.cmx lexer.ml lexer.o

parse_codefile:
	./lexer code_file
	./parser code_file-lex data_file.csv