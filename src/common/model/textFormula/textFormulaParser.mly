%{
  open Formula
  open TextFormulaType
%}

%token TRUE FALSE NOT AND OR
%token LPAR RPAR
%token<string> PREDICATE NULLARY_PREDICATE LITERAL
%token EOF

%left OR
%left AND
%left NOT
%left PREDICATE

%start<TextFormulaType.t> formula
%%

formula:
  | EOF { True }
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
  | l=LITERAL { raw' l }
  | p=NULLARY_PREDICATE { nullary' p }
  | p=PREDICATE e=expression { unary' p e }
