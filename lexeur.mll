{
  
open Parseur
exception Eof
exception TokenInconu

}

rule token = parse
            [' ' '\t' '\n'] { token lexbuf }
            | [';'] { PT_VIRG }
            | (['0'-'9']*['.']) ? ['0'-'9']+ as lexem { NOMBRE(float_of_string lexem) }
            | ( ['0'-'9']+ (['.']['0'-'9']*)? ) "e"['-']?['0'-'9']+ as lexem{NOMBRE(float_of_string lexem)}
            | "nan" as lexem {NOMBRE(float_of_string lexem)}
            | '+'   { PLUS }
            | '-'   { MOINS }
            | '*'   { FOIS }
            | '/'   { DIV }
            | '%'   { MOD }
            | '('   { GPAREN }
            | ')'   { DPAREN }
            | '!'   { NOT }
            | "or"  { OR }
            | "=="  { EQUALS }
            | "!="  { NOTEQL }
            | '<'   { LOSTNB }
            | '>'   { GRSTNB }
            | "<="  { LOEQNB }
            | ">="  { GREQNB }  
            | eof   { raise Eof }
            | _     { raise TokenInconu }