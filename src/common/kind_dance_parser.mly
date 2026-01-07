%{
  open Kind_dance_type
%}

%token TIMES PLUS RPAR LPAR
%token<string> WORD
%token<int> NUMBER
%token EOF

%left PLUS
%left TIMES

%start<Kind_dance_type.t> main
%%

main:
  | e=expression EOF { e }

expression:
  | n=NUMBER w=WORD { Version (n, Kind_base.of_string w) }
  | e1=expression PLUS e2=expression { Add (e1, e2) }
  | n=NUMBER TIMES e=expression { Mul (n, e) }
  | LPAR e=expression RPAR { e }
