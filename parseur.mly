%{ 
open AST
%}

%token <float> NOMBRE
%token NOMBRE PLUS MOINS DIV MOD UMOINS FOIS GPAREN DPAREN PT_VIRG

%type <AST.expression_a> main expression
%left PLUS MOINS
%left FOIS DIV 
%left MOD


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
    | GPAREN expression DPAREN { $2 }
    | MOINS expression %prec UMOINS { Neg $2 }
    | NOMBRE { Num $1 };
