type expression_a =
  | Plus  of expression_a * expression_a
  | Moins of expression_a * expression_a
  | Mult  of expression_a * expression_a
  | Div   of expression_a * expression_a
  | Mod   of expression_a * expression_a

  | Equals  of expression_a * expression_a
  | Noteql  of expression_a * expression_a
  | Lostnb  of expression_a * expression_a
  | Grstnb  of expression_a * expression_a
  | Loeqnb  of expression_a * expression_a
  | Greqnb  of expression_a * expression_a
  | And     of expression_a * expression_a 
  | Or      of expression_a * expression_a
  | Not     of expression_a
  | Ternary of expression_a * expression_a * expression_a
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
  | Equals  (g,d)-> print_binaire form "Equals" g d
  | Noteql  (g,d)-> print_binaire form "Noteql" g d 
  | Lostnb  (g,d)-> print_binaire form "Lostnb" g d 
  | Grstnb  (g,d)-> print_binaire form "Grstnb" g d
  | Loeqnb  (g,d)-> print_binaire form "Loeqnb" g d
  | Greqnb  (g,d)-> print_binaire form "And" g d
  | And  (g,d)-> print_binaire form "Or" g d
  | Or  (g,d)-> print_binaire form "Greqnb" g d
  | Ternary (i,t,e) -> print_binaire form "IfThenElse" t e
  | Not    e    -> fprintf form "@[<2>%s@ %a@]" "Not" print_AST e
  | Neg    e    -> fprintf form "@[<2>%s@ %a@]" "Neg" print_AST e 
  | Num    n    -> fprintf form "@[<2>%s@ %f@]" "Num" n

;; 



let rec size (e : expression_a) =
	match e with 
  | Plus  (g,d)   -> 1+ size(g)+size(d)
  | Moins (g,d)   -> 1+ size(g)+size(d)
  | Mult  (g,d)   -> 1+ size(g)+size(d)
  | Div   (g,d)   -> 1+ size(g)+size(d)
  | Mod   (g,d)   -> 1+ size(g)+size(d)
  | Equals  (g,d) -> 1+ size(g)+size(d)
  | Noteql  (g,d) -> 1+ size(g)+size(d)
  | Lostnb  (g,d) -> 1+ size(g)+size(d) 
  | Grstnb  (g,d) -> 1+ size(g)+size(d)
  | Loeqnb  (g,d) -> 1+ size(g)+size(d)
  | Greqnb  (g,d) -> 1+ size(g)+size(d)
  | And     (g,d) -> 1+ size(g)+size(d)
  | Or      (g,d) -> 1+ size(g)+size(d)
  | Ternary (c,t,e) -> size(c) +1 + size(t) + size(e)
  | Not    e    -> 1 + size(e)
  | Neg    e    -> 1 + size(e)
  | Num    n    -> 1

;; 




let rec print_list l  = 
    match l with 
    | [] -> Printf.printf ""
    | h::t -> Printf.printf h; print_list t;;



let  rec code (e : expression_a)  = 
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
  | And  (l,r) ->  (code l); (code r);Printf.printf "And\n"
  | Or (l,r) ->  (code l); (code r);Printf.printf "Or\n"

  | Not     e     ->  (code e); Printf.printf "Not\n"
  | Ternary (c,t,e) -> (code c);  let x = size(t)+1 in Printf.printf "CondJump %d\n" x; code t ; let y = size(e)+1 in Printf.printf "Jump %d\n" y; code e
  | Neg exp   -> (code exp); Printf.printf "NegaNb\n";
  | Num  n    -> Printf.printf "CsteNb %f\n" n 

;;






let code_command (c : command_a) = 
  match c with
  | Exp e -> code e ; 
  | Ass (var,exp) -> (code exp) ; match var with 
                                  | Id x -> Print.printf "SetVar %s" x | _ -> Print.printf ""
;;

let rec code_prog ( p : programme_a) = 
  match p with 
  | Com c -> (code_command c)
  | Seq(c,p) -> (code_command c) ; (code_prog p )

  ;;

