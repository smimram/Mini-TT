{
open Lexing
open Parser
}

let space = ' ' | '\t' | '\r'

rule token = parse
  | "U" { SET }
  | "λ" { LAMBDA }
  | "→" { TO }
  | "Π" { PI }
  | "." { DOT }
  | "Sum" { SUM }
  | ":" { COLON }
  | "=" { EQ }
  | (['A'-'Z''a'-'z']+ as s) { IDENT s }
  | '#'[^'\n']* { token lexbuf }
  | space+ { token lexbuf }
  | "\n" { new_line lexbuf; token lexbuf }
  | eof { EOF }
