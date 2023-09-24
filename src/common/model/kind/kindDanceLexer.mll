{
  open Nes
  open KindDanceParser

  exception UnexpectedCharacter of char
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
  | _ as c { failwith (spf "Unexpected character: %c" c) }
  | eof { EOF }
