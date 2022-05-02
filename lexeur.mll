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
            | "||"  { OR }
            | "&&"  { AND }
            | "=="  { EQUALS }
            | "!="  { NOTEQL }
            | '<'   { LOSTNB }
            | '>'   { GRSTNB }
            | "<="  { LOEQNB }
            | ">="  { GREQNB }  
            | ':'   { COLON }
            | '?'   { QMARK }

            | ['a'-'z']['a'-'z' '_' '0'-'9']* as lexem { IDENT(lexem) }
            | "="   { ASSG}

            | "If" { IF }
            | "Else"  { ELSE }
            | "While" { WHILE }
            | "Do"    { DO }


            | eof   { raise Eof }
            | _     { raise TokenInconu }
