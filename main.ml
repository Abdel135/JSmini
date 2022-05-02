let _ =
  try
  let input_stream = 
    if (Array.length Sys.argv) = 1 
    then stdin else open_in Sys.argv.(1)  in
  
  let newstdout = 
    if input_stream = stdin then stdout else open_out (Sys.argv.(1)^".jsm") in 

  let lexbuf = Lexing.from_channel input_stream in  
    (*Unix.dup2 (Unix.descr_of_out_channel newstdout) Unix.stdout;*)
    Unix.dup2 (Unix.descr_of_out_channel newstdout) Unix.stdout;
    while true do 
      Parseur.main Lexeur.token lexbuf (*parseur une ligne*)
      |>  AST.code_prog  ;Printf.printf "Halt\n"; flush stdout;  (**Format.printf "%a\n%!" *)
    done
  with
  | Lexeur.Eof -> exit 0 (*impossible*)
  | Lexeur.TokenInconu (*erreur de lexing*)
  | Parsing.Parse_error -> (*erreur de parsing*)
  Printf.printf ("Ceci n'est pas un programme\n")

