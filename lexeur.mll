{
open Parseur
exception Eof
exception TokenInconu
}

rule token = parse
            [' ' '\t' '\n'] { token lexbuf }
            | [';'] { PT_VIRG }
            | (['0'-'9']*['.']) ? ['0'-'9']+ { NOMBRE }
            | '+' { PLUS }
            | '-' { MOINS }
            | '*' { FOIS }
            | '%' { MODULO }
            | '(' { GPAREN }
            | ')' { DPAREN }
            | eof { raise Eof }
            | _ { raise TokenInconu }