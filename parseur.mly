%token <int> NOMBRE
%token NOMBRE PLUS MOINS MODULO FOIS GPAREN DPAREN PT_VIRG

%left PLUS MOINS
%left FOIS
%left MODULO

%nonassoc UMOINS
%type <int> main expression

%start main
%%
main:
expression PT_VIRG { $1 }
;
expression:
expression PLUS expression { $1+$3 }
| expression MOINS expression { $1-$3 }
| expression FOIS expression { $1*$3 }
| expression MODULO expression {$1 mod $3}
| GPAREN expression DPAREN { $2 }
| MOINS expression %prec UMOINS { -$2 }
| NOMBRE { $1 }
;
