{
  open Kind_dance_parser

  exception Unexpected_character of char
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = parse
  | ' ' { token lexbuf }
  | '(' { LPAR }
  | ')' { RPAR }
  | '+' { PLUS }
  | 'x' { TIMES }
  | digit+ as n { NUMBER (int_of_string n) }
  | alpha+ as w { WORD w }
  | _ as c { raise (Unexpected_character c) }
  | eof { EOF }
