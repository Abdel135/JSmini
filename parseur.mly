%{ 
open AST
%}

%token <float> NOMBRE
%token <string> IDENT

%token NOMBRE BOOLEAN PLUS MOINS FOIS DIV MOD NOT OR AND EQUALS NOTEQL LOSTNB GRSTNB LOEQNB GREQNB GPAREN DPAREN PT_VIRG COLON QMARK
%token IDENT ASSG 


%type <AST.programme_a> main programme

%left ASSG
%left  OR AND EQUALS NOTEQL LOSTNB GRSTNB LOEQNB GREQNB
%left PLUS MOINS
%left FOIS DIV 
%left MOD
%left NOT
%nonassoc UMOINS QMARK COLON IDENT


%start main
%%


main:
programme PT_VIRG {$1};

programme : 
   command  {Com ($1)}
   |command  programme  { Seq($1,$2) };

command:
  expression { Exp($1)}
  | expression ASSG expression {Assg($1,$3)};



expression:
    expression PLUS expression { Plus($1,$3) }
    | expression MOINS expression { Moins($1,$3) }
    | expression FOIS expression { Mult ($1,$3) }
    | expression DIV expression {Div ($1,$3)}
    | expression MOD expression {Mod ($1,$3)}

    | expression EQUALS expression {Equals ($1,$3)}
    | expression NOTEQL expression {Noteql ($1,$3)}
    | expression LOSTNB expression {Lostnb ($1,$3)}
    | expression GRSTNB expression {Grstnb ($1,$3)}
    | expression LOEQNB expression {Loeqnb ($1,$3)}
    | expression GREQNB expression {Greqnb ($1,$3)}
    | expression AND expression {And ($1,$3)}
    | expression OR expression {Or ($1,$3)}
    | expression QMARK expression COLON expression {Ternary($1,$3,$5)}
    | NOT expression {Not $2}
    
    | GPAREN expression DPAREN { $2 }
    | MOINS expression %prec UMOINS { Neg $2 }
    | NOMBRE { Num $1 }
    | IDENT  { Id $1};
