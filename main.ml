let _ =

try
  let lexbuf = Lexing.from_channel stdin in (*lexeur lancé sur stdin*)
    while true do 
      let _ = Parseur.main Lexeur.token lexbuf in 
      Printf.printf("un token reconnue\n")
    done
  with
| Lexeur.Eof -> exit 0 (*impossible*)
| Lexeur.TokenInconu (*erreur de lexing*)
| Parsing.Parse_error -> (*erreur de parsing*)
Printf.printf ("Ceci n'est pas une expression arithmetique\n")