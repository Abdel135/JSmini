
type expression_a =
  | Plus  of expression_a * expression_a
  | Moins of expression_a * expression_a
  | Mult  of expression_a * expression_a
  | Neg   of expression_a
  | Num   of int
;;


(* Fonctions d'affichage *)

let rec print_binaire form s g d = Format.fprintf form "@[<2>%s%s@ %a%s@ %a%s@]" s "(" print_AST g " ," print_AST d " )" 

and print_AST form = let open Format in function
  | Plus  (g,d) -> print_binaire form "Plus" g d
  | Moins (g,d) -> print_binaire form "Moins" g d
  | Mult  (g,d) -> print_binaire form "Mult" g d
  | Neg    e    -> fprintf form "@[<2>%s@ %a@]" "Neg" print_AST e 
  | Num    n    -> fprintf form "@[<2>%s@ %i@]" "Num" n
;; 

let rec code (e : expression_a)    = 
	match e with 
	| Num  n    -> Printf.printf "CsteNb %i\n" n 
	| Plus(l,r) -> (code l); (code r); Printf.printf "AddiNb\n"
	| Moins(l,r)-> (code l); (code r); Printf.printf "SubiNb\n"
  | Mult(l,r) -> (code l); (code r); Printf.printf "MultNb\n"
  | Neg exp   -> (code exp); Printf.printf "NegaNb\n";
;;










