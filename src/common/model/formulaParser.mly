%{
  open Formula
%}

%token TRUE FALSE NOT AND OR
%token ANYOF ALLOF
%token LPAR RPAR
%token<string> PREDICATE LITERAL
%token EOF

%left OR
%left AND
%left NOT

%start<text_formula> formula
%%

formula:
  | es=expressions EOF { es }

expressions:
  | es=nonempty_list(expression) { and_l es }

expression:
  | TRUE { True }
  | FALSE { False }
  | LPAR es=expressions RPAR { es }
  | NOT e=expression { Not e }
  | e1=expression AND e2=expression { And (e1, e2) }
  | e1=expression  OR e2=expression { Or  (e1, e2) }

  | l=LITERAL { raw l }
  | p=PREDICATE e=expression { app p e }

  | p=PREDICATE ALLOF es=nonempty_list(expression) { and_l (List.map (app p) es) }
  | p=PREDICATE ANYOF es=nonempty_list(expression) { or_l  (List.map (app p) es) }
