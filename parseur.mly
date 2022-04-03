%token <int> NOMBRE
%token NOMBRE PLUS MOINS UMOINS FOIS GPAREN DPAREN EOL

%type <int> main expression 
%left PLUS MOINS
%left FOIS 

%nonassoc UMOINS

%start main
%%
main:
expression EOL { $1 };
expression:
  NOMBRE { $1 }
  |expression PLUS expression { $1+$3 }
  | expression MOINS expression { $1-$3 }
  | expression FOIS expression { $1*$3 }
  | GPAREN expression DPAREN { $2 }
  | MOINS expression %prec UMOINS { -$2 }
  ;