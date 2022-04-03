%token NOMBRE BOOLEAN PLUS MOINS FOIS MODULO NOT OR EQUALS NOTEQL LOSTNB GRSTNB LOEQNB GREQNB GPAREN DPAREN PT_VIRG

%left OR EQUALS NOTEQL LOSTNB GRSTNB LOEQNB GREQNB
%left PLUS MOINS
%left FOIS 
%left MODULO


%nonassoc UMOINS NOT 
%type <unit> main expression
%start main
%%
main:
expression PT_VIRG {}
;
expression:
  NOMBRE {}
  | BOOLEAN {}
  | NOT expression {}
  | expression OR expression {}
  | expression PLUS expression {}
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
  | MOINS expression %prec UMOINS {}
  ;

