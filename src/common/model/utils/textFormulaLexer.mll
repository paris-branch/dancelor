{
  open TextFormulaParser

  exception UnexpectedCharacter of char
  exception UnterminatedQuote

  let keywords =
    [ "true",   TRUE;
      "false",  FALSE;
      "and",    AND;
      "or",     OR;
      "not",    NOT ]
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

  | (identifier as id) ":" {
      PREDICATE id
    }

  | '"' {
      let buf = Buffer.create 8 in
      LITERAL (string false buf lexbuf)
    }

  (* The following pattern must exclude all the special characters matched above. *)
  | [^ ' ' '(' ')' ':' '"']+ as lit {
      LITERAL lit
    }

  | _ as c { raise (UnexpectedCharacter c) }

  | eof { EOF }

and string esc buf = parse
  | '"' {
      if esc then
        (Buffer.add_char buf '"'; string false buf lexbuf)
      else
        Buffer.contents buf
    }
  | '\\' {
      if esc then
        (Buffer.add_char buf '\\'; string false buf lexbuf)
      else
        string true buf lexbuf
    }
  | _ as c {
      Buffer.add_char buf c; string false buf lexbuf
    }
  | eof {
      raise UnterminatedQuote
    }
