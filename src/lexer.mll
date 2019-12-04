{
open Lexing
open Parser

let utf8 ?(n=1) lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_bol = pos.pos_bol + n }
}

let space = ' ' | '\t' | '\r'

rule token = parse
  | "U" { SET }
  | "λ" { utf8 lexbuf; LAMBDA }
  | "→" { utf8 ~n:2 lexbuf; TO }
  | "Π" { utf8 lexbuf; PI }
  | "." { DOT }
  | "Sum" { SUM }
  | "fun" { FUN }
  | ":" { COLON }
  | "=" { EQ }
  | "(" { LPAR }
  | ")" { RPAR }
  | "|" { VBAR }
  | (['A'-'Z''a'-'z''0'-'9']+ as s) { IDENT s }
  | "$"(['A'-'Z''a'-'z']+ as s) { CONS s }
  | '#'[^'\n']* { token lexbuf }
  | space+ { token lexbuf }
  | "\n" { new_line lexbuf; N }
  | eof { EOF }
