let _ =
  try
  let input_stream = 
    if (Array.length Sys.argv) = 1 
    then stdin else open_in Sys.argv.(1)  in

  let lexbuf = Lexing.from_channel input_stream in 
    while true do 
      Parseur.main Lexeur.token lexbuf (*parseur une ligne*)
      |>  AST.code ;flush stdout;  (**Format.printf "%a\n%!" *)
    done
  with
  | Lexeur.Eof -> exit 0 (*impossible*)
  | Lexeur.TokenInconu (*erreur de lexing*)
  | Parsing.Parse_error -> (*erreur de parsing*)
  Printf.printf ("Ceci n'est pas une expression arithmetique\n")

