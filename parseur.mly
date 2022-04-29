%{ 
open AST
%}

%token <float> NOMBRE
%token NOMBRE BOOLEAN PLUS MOINS FOIS DIV MOD NOT OR EQUALS NOTEQL LOSTNB GRSTNB LOEQNB GREQNB TRUE FALSE GPAREN DPAREN PT_VIRG
%type <AST.expression_a> main expression

%left OR EQUALS NOTEQL LOSTNB GRSTNB LOEQNB GREQNB
%left PLUS MOINS
%left FOIS DIV 
%left MOD
%left NOT


%nonassoc UMOINS 

%start main
%%
main:
expression PT_VIRG { $1 };
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
    | NOT expression {Not $2}
    
    | GPAREN expression DPAREN { $2 }
    | MOINS expression %prec UMOINS { Neg $2 }
    | NOMBRE { Num $1 };
