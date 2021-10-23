{
  open TextFormulaParser

  let keywords =
    [ "true",   TRUE;
      "false",  FALSE;
      "and",    AND;
      "or",     OR;
      "not",    NOT;
      "any-of", ANYOF;
      "all-of", ALLOF ]
}

let alpha = ['a'-'z' 'A'-'Z']
let symbol = ['_' '-']
let digit = ['0'-'9']

let identifier = (alpha | symbol | digit)+

rule token = parse

  | ' ' { token lexbuf }

  | '(' { LPAR }
  | ')' { RPAR }

  | ":" (identifier as id) {
      match List.assoc_opt id keywords with
      | Some kw -> kw
      | _ -> NULLARY_PREDICATE id
    }

  | '!'  { NOT }
  | "&&" { AND }
  | "||" { OR }

  | (identifier as id) ":" {
      PREDICATE id
    }

  | '"' {
      let buf = Buffer.create 8 in
      LITERAL (string buf lexbuf)
    }

  | (identifier as lit) {
      LITERAL lit
    }

  | eof { EOF }

and string buf = parse
  | '"'    { Buffer.contents buf }
  | _ as c { Buffer.add_char buf c; string buf lexbuf }
