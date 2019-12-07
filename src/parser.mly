%{
    open Minitt
%}

%token COLON EQ DOT TO LPAR RPAR VBAR N
%token LAMBDA PI SUM SET FUN
%token EOF
%token<string> IDENT CONS

%nonassoc DOT
%right TO
%nonassoc CONS
%nonassoc SUM SET LPAR IDENT

%start main
%type<Minitt.expr> main
%%

main:
  | defs EOF { $1 }

defs:
  | N defs { $2 }
  | { EUnit }
  | pattern COLON expr EQ expr N defs { EDecl (Def ($1, $3, $5), $7) }

expr:
  | simple_expr { $1 }
  | simple_expr simple_expr { EApp ($1, $2) }
  | PI pattern COLON expr DOT expr { EPi ($2, $4, $6) }
  | LAMBDA pattern DOT expr { EAbs ($2, $4) }
  | expr TO expr { EPi (PVar "_", $1, $3) }
  | CONS expr { ECons ($1, $2) }
  | FUN LPAR funbranches RPAR { EFun $3 }

simple_expr:
  | SET { ESet }
  | IDENT { EVar $1 }
  | CONS { ECons ($1, EUnit) }
  | SUM LPAR sumbranches RPAR { ESum $3 }
  | LPAR expr RPAR { $2 }

pattern:
  | IDENT { PVar $1 }

sumbranches:
  | { [] }
  | sumbranch { [$1] }
  | sumbranch VBAR sumbranches { $1::$3 }

sumbranch:
  | CONS { $1, EOne }
  | CONS expr { $1, $2 }

funbranches:
  | { [] }
  | funbranch { [$1] }
  | funbranch VBAR funbranches { $1::$3 }

funbranch:
  | CONS TO expr { $1, EAbs (PUnit, $3) }
  | CONS pattern TO expr { $1, EAbs ($2, $4) }
