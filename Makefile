main: mlly mlfile

mlly:
	ocamllex lexer.mll
	ocamlyacc parser.mly

mlfile:
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c backend.ml
	ocamlc -c fileio_driver.ml
	ocamlc -c parser.ml
	ocamlc -o spsheet str.cma lexer.cmo backend.cmo parser.cmo fileio_driver.cmo 
	./spsheet sheet.csv 3 6 func.txt

