{
  open Kind_dance_parser

  exception Unexpected_character of char
}

let digit = ['0'-'9']
let alpha = ['a'-'w' 'y'-'z' 'A'-'W' 'Y'-'Z'] (* the whole alphabet but x *)
let letter =  alpha | digit | ['[' ']' '/']

rule token = parse
  | ' ' { token lexbuf }
  | '(' { LPAR }
  | ')' { RPAR }
  | '+' { PLUS }
  | 'x' { TIMES }
  | digit+ as n { NUMBER (int_of_string n) }
  | (alpha letter*) as w { WORD w }
  | _ as c { raise (Unexpected_character c) }
  | eof { EOF }
