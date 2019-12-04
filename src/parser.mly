%{
    open Minitt
%}

%token COLON EQ DOT TO
%token LAMBDA PI SUM SET
%token EOF
%token<string> IDENT

%right TO
%nonassoc DOT

%start main
%type<Minitt.expr> main
%%

main:
  | defs EOF { $1 }

defs:
  | { EUnit }
  | pattern COLON expr EQ expr defs { EDecl (Def ($1, $3, $5), $6) }

expr:
  | PI pattern COLON expr DOT expr { EPi ($2, $4, $6) }
  | expr TO expr { EPi (PVar "_", $1, $3) }
  | SET { ESet }
  | IDENT { EVar $1 }
  | LAMBDA pattern DOT expr { EAbs ($2, $4) }

pattern:
  | IDENT { PVar $1 }
