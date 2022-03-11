%token NOMBRE PLUS MOINS FOIS MODULO NOT EQUALS NOTEQL LOSTNB GRSTNB LOEQNB GREQNB GPAREN DPAREN PT_VIRG
%left PLUS MOINS
%nonassoc NOT 
%left EQUALS NOTEQL LOSTNB LOEQNB GRSTNB GREQNB
%left FOIS MODULO
%nonassoc UMOINS
%type <unit> main expression
%start main
%%
main:
expression PT_VIRG {}
;
expression:
expression PLUS expression {}
| expression MOINS expression {}
| expression FOIS expression {}
| expression MODULO expression {}
| expression EQUALS expression {}
| expression NOTEQL expression {}
| expression LOSTNB expression {}
| expression LOEQNB expression {}
| expression GRSTNB expression {}
| expression GREQNB expression {}
| GPAREN expression DPAREN {}
| NOT expression {}
| MOINS expression %prec UMOINS {}
| NOMBRE {}
;

