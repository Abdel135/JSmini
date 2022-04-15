type expression_a =
  | Plus  of expression_a * expression_a
  | Moins of expression_a * expression_a
  | Mult  of expression_a * expression_a
  | Div   of expression_a * expression_a
  | Mod   of expression_a * expression_a

  | Equals of expression_a * expression_a
  | Noteql  of expression_a * expression_a
  | Lostnb  of expression_a * expression_a
  | Grstnb  of expression_a * expression_a
  | Loeqnb  of expression_a * expression_a
  | Greqnb  of expression_a * expression_a
  | Not     of expression_a

  | Neg     of expression_a
  | Num     of float
;;


(* Fonctions d'affichage *)

let rec print_binaire form s g d = Format.fprintf form "@[<2>%s%s@ %a%s@ %a%s@]" s "(" print_AST g " ," print_AST d " )" 

and print_AST form = let open Format in function
  | Plus  (g,d) -> print_binaire form "Plus" g d
  | Moins (g,d) -> print_binaire form "Moins" g d
  | Mult  (g,d) -> print_binaire form "Mult" g d
  | Div   (g,d) -> print_binaire form "Div" g d
  | Mod   (g,d) -> print_binaire form "Mod" g d

  | Equals  (g,d) -> print_binaire form "Equals" g d
  | Noteql  (g,d) -> print_binaire form "Noteql" g d 
  | Lostnb  (g,d) -> print_binaire form "Lostnb" g d 
  | Grstnb  (g,d) -> print_binaire form "Grstnb" g d
  | Loeqnb  (g,d) -> print_binaire form "Loeqnb" g d
  | Greqnb  (g,d) -> print_binaire form "Greqnb" g d
  | Not    e    -> fprintf form "@[<2>%s@ %a@]" "Not" print_AST e

  | Neg    e    -> fprintf form "@[<2>%s@ %a@]" "Neg" print_AST e 
  | Num    n    -> fprintf form "@[<2>%s@ %f@]" "Num" n
;; 

let rec code (e : expression_a)    = 
	match e with 
	| Plus(l,r) -> (code l); (code r); Printf.printf "AddiNb\n"
	| Moins(l,r)-> (code l); (code r); Printf.printf "SubiNb\n"
  | Mult(l,r) -> (code l); (code r); Printf.printf "MultNb\n"
  | Mod(l,r) ->  (code l); (code r); Printf.printf "Mod\n"
  | Div(l,r) ->  (code l); (code r); Printf.printf "DiviNb\n"

  | Equals  (l,r) ->  (code l); (code r);Printf.printf "Equal\n" 
  | Noteql  (l,r) ->  (code l); (code r);Printf.printf "NotEq\n" 
  | Lostnb  (l,r) ->  (code l); (code r);Printf.printf "LoStNb\n"
  | Grstnb  (l,r) ->  (code l); (code r);Printf.printf "GrStNb\n"
  | Loeqnb  (l,r) ->  (code l); (code r);Printf.printf "LoEqNb\n"
  | Greqnb  (l,r) ->  (code l); (code r);Printf.printf "GrEqNb\n"
  | Not     e     ->  (code e); Printf.printf "Not\n"

  | Neg exp   -> (code exp); Printf.printf "NegaNb\n";
  | Num  n    -> Printf.printf "CsteNb %f\n" n 

;;










