{
  open FormulaParser

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

let identifier = alpha (alpha | symbol | digit)*

let literal = [^ ':' ' ']+

rule token = parse

  | ' ' { token lexbuf }

  | '(' { LPAR }
  | ')' { RPAR }

  | ":" (identifier as id) {
      match List.assoc_opt id keywords with
      | Some kw -> kw
      | _ -> failwith "unknown keyword"
    }

  | (identifier as id) ":" {
      PREDICATE id
    }

  | (literal as lit) {
      LITERAL lit
    }

  | '"' {
      let buf = Buffer.create 8 in
      LITERAL (string buf lexbuf)
    }

  | eof { EOF }

and string buf = parse
  | '"'    { Buffer.contents buf }
  | _ as c { Buffer.add_char buf c; string buf lexbuf }
