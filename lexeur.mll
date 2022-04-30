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
            | (['0'-'9']*['.']) ? ['0'-'9']+ as lexem { NOMBRE(float_of_string lexem) }
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
            | ':'   { COLON }
            | '?'   { QMARK }
            | eof   { raise Eof }
            | _     { raise TokenInconu }
