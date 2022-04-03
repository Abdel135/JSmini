{
open Parseur
exception Eof
exception TokenInconu
}

rule token = parse
            [' ' '\t' '\n'] { token lexbuf }
            | [';'] { PT_VIRG }
            | ( ['0'-'9']*['.'] ) ? ['0'-'9']+ { NOMBRE }
            | "true" | "false" { BOOLEAN }
            | '+'   { PLUS }
            | '-'   { MOINS }
            | '*'   { FOIS }
            | '%'   { MODULO }
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