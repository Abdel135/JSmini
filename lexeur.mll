{
open Parseur
exception Eof
exception TokenInconu
}

rule token = parse
            [' ' '\t' '\n'] { token lexbuf }
            | "//" [^ '\n']* '\n'{token lexbuf }
            | "/*" ([^ '*'] | '*'+ [^ '*''/'])* "*/" {token lexbuf }
            | [';'] { PT_VIRG }
            | ( ['0'-'9']*['.'] ) ? ['0'-'9']+ { NOMBRE }
            | ( ['0'-'9']+ (['.']['0'-'9']*)? ) "e"['-']?['0'-'9']+ {NOMBRE}
            | "nan" {NOMBRE}
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