#ocamllex lexeur.mll;
#ocamlyacc parseur.mly;
#ocamlc -c parseur.mli lexeur.ml parseur.ml main.ml;
#ocamlc -o main lexeur.cmo parseur.cmo main.cmo;

ocamllex lexeur.mll 
ocamlyacc parseur.mly 
ocamlc -c AST.ml parseur.mli lexeur.ml parseur.ml main.ml 
ocamlc  unix.cma AST.cmo lexeur.cmo parseur.cmo main.cmo -o main 
