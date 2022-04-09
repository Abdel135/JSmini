%{ 
open AST
%}

%token <int> NOMBRE
%token NOMBRE PLUS MOINS UMOINS FOIS MODULO GPAREN DPAREN PT_VIRG

%type <AST.expression_a> main expression

%left PLUS MOINS
%left FOIS 
%left MODULO

%nonassoc UMOINS

%start main
%%
main:
expression PT_VIRG { $1 };
  expression:
    NOMBRE { Num $1 };
    | MOINS expression %prec UMOINS { Neg $2 }
    | expression PLUS expression { Plus ($1,$3) }
    | expression MOINS expression { Moins($1,$3) }
    | expression FOIS expression { Mult ($1,$3) }
    | expression MODULO expression { Mod ($1,$3) }
    | GPAREN expression DPAREN {$2}
