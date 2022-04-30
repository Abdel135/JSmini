%token NOMBRE BOOLEAN PLUS MOINS FOIS MODULO NOT OR EQUALS NOTEQL LOSTNB GRSTNB LOEQNB GREQNB GPAREN DPAREN PT_VIRG COLON QMARK IDENT ASSG 
%token IDENT ASSG 


%left ASSG
%left OR EQUALS NOTEQL LOSTNB GRSTNB LOEQNB GREQNB
%left PLUS MOINS
%left FOIS 
%left MODULO


%nonassoc UMOINS NOT COLON QMARK 


%type <unit> main programme 



%start main programme
%%

main:
programme PT_VIRG {};

programme : 
  command {}
  | command  programme  {};

command:
  expression PT_VIRG{}
  | IDENT ASSG expression {};



expression:
  NOMBRE {}
  | IDENT {}
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
  | expression QMARK expression COLON expression {}
  | GPAREN expression DPAREN {}
  | MOINS expression %prec UMOINS {}
  ;

