(** An identifier. *)
type name = string

(** An expression. *)
type expr =
  | EAbs of pattern * expr (** abstraction *)
  | ESet (** universe *)
  | EPi  of pattern * expr * expr (** Pi type *)
  | ESig of pattern * expr * expr (** Sigma type *)
  | EOne (** unit type *)
  | EUnit (** unit constructor *)
  | EPair of expr * expr (** pair *)
  | ECons of name * expr (** constructor *)
  | ESum of branch (** sum type *)
  | EFun of branch (** pattern matching *)
  | EFst of expr (** first projection *)
  | ESnd of expr (** second projection *)
  | EApp of expr * expr (** application *)
  | EVar of name (** variable *)
  | EDecl of decl * expr (** declaration *)

(** A declaration. *)
and decl =
  | Def of pattern * expr * expr (** a definition *)
  | Drec of pattern * expr * expr (** a recursive definition *)

(** A pattern. *)
and pattern =
  | PVar of name (** variable *)
  | PPair of pattern * pattern (** a pair *)
  | PUnit (** unit *)

(** Branching depending on constructor name. *)
and branch = (name * expr) list

(** A value. *)
type value =
  | Abs of clos (** abstraction *)
  | Pair of value * value (** pair *)
  | Cons of name * value (** constructor *)
  | Unit (** unit constructor *)
  | Set (** universe *)
  | Pi of value * clos (** Pi type *)
  | Sig of value * clos (** Sigma type *)
  | One (** unit type *)
  | Fun of sclos (** pattern matching *)
  | Sum of sclos (** sum type *)
  | Nt of neutral (** a neutral term *)

(** A neutral value. *)
and neutral =
  | Gen of int (** a variable *)
  | App of neutral * value (** application *)
  | Fst of neutral (** first projection *)
  | Snd of neutral (** second projection *)
  | NtFun of sclos * neutral (** closure of a match *)

(** Closure of a match. *)
and sclos = branch * rho

(** A function closure. *)
and clos =
  | Cl of pattern * expr * rho (** a function in an environment *)
  | ClCmp of clos * name

(** Environment assigning expressions to variables. *)
and rho = in_rho list

and in_rho =
  | RhoVar of pattern * value (** variable assignation *)
  | RhoDecl of decl (** declaration *)

(** First projection of a value. *)
let vfst = function
  | Pair (u1, _) -> u1
  | Nt k -> Nt (Fst k)
  | _ -> failwith "vfst"

(** Second projection of a value. *)
let vsnd = function
  | Pair (_, u2) -> u2
  | Nt k -> Nt (Snd k)
  | _ -> failwith "vsnd"

(** Whenther a variable is defined by a pattern. *)
let rec in_pattern x = function
  | PVar y -> x = y
  | PPair (p1, p2) -> in_pattern x p1 || in_pattern x p2
  | PUnit -> false

(** Value of a variable defined by a pattern. *)
let rec pattern_proj p x v =
  match p with
  | PVar y when x = y -> v
  | PPair (p1, p2) when in_pattern x p ->
    if in_pattern x p1 then pattern_proj p1 x (vfst v)
    else pattern_proj p2 x (vsnd v)
  | _ -> failwith "pattern_proj"

(** Bind a pattern to a value in rho. *)
let add_var rho p v = (RhoVar (p,v))::rho

(** String representation of an expression. *)
let rec string_of_expr = function
  | EAbs (p, e) -> Printf.sprintf "λ %s . %s" (string_of_pattern p) (string_of_expr e)
  | ESet -> "U"
  | EPi (p, a, e) -> Printf.sprintf "Π %s : %s . %s" (string_of_pattern p) (string_of_expr a) (string_of_expr e)
  | ESig (p, a, e) -> Printf.sprintf "Σ %s : %s . %s" (string_of_pattern p) (string_of_expr a) (string_of_expr e)
  | EOne -> "1"
  | EUnit -> "()"
  | EPair (e1, e2) -> Printf.sprintf "(%s , %s)" (string_of_expr e1) (string_of_expr e2)
  | ECons (c, e) -> Printf.sprintf "$%s %s" c (string_of_expr e)
  | ESum b -> Printf.sprintf "Sum %s" (string_of_branch b)
  | EFun b -> Printf.sprintf "fun %s" (string_of_branch b)
  | EFst e -> Printf.sprintf "fst %s" (string_of_expr e)
  | ESnd e -> Printf.sprintf "snd %s" (string_of_expr e)
  | EApp (e1, e2) -> Printf.sprintf "(%s %s)" (string_of_expr e1) (string_of_expr e2)
  | EVar x -> x
  | EDecl (Def (p, a, e), e') -> Printf.sprintf "%s : %s = %s\n%s" (string_of_pattern p) (string_of_expr a) (string_of_expr e) (string_of_expr e')
  | EDecl (Drec (p, a, e), e') -> Printf.sprintf "rec %s : %s = %s\n%s" (string_of_pattern p) (string_of_expr a) (string_of_expr e) (string_of_expr e')
and string_of_pattern = function
  | PVar x -> x
  | PUnit -> "()"
  | PPair (p1, p2) -> Printf.sprintf "(%s , %s)" (string_of_pattern p1) (string_of_pattern p2)
and string_of_branch b =
  "(" ^ String.concat " | " (List.map (fun (c, a) -> Printf.sprintf "$%s → %s" c (string_of_expr a)) b) ^ ")"

(** String representation of a value. *)
let rec string_of_value = function
  | Abs f -> string_of_clos f
  | Pair (u, v) -> Printf.sprintf "(%s , %s)" (string_of_value u) (string_of_value v)
  | Cons (c, v) -> Printf.sprintf "$%s %s" c (string_of_value v)
  | Unit -> "()"
  | Set -> "U"
  | Pi (v, g) -> Printf.sprintf "Π %s . %s" (string_of_value v) (string_of_clos g)
  | Sig (v, g) -> Printf.sprintf "Σ %s . %s" (string_of_value v) (string_of_clos g)
  | One -> "1"
  | Fun f -> Printf.sprintf "fun %s" (string_of_sclos f)
  | Sum s -> Printf.sprintf "Sum %s" (string_of_sclos s)
  | Nt t -> string_of_neutral t
and string_of_neutral = function
  | Gen n -> Printf.sprintf "x%d" n
  | App (n, v) -> Printf.sprintf "%s %s" (string_of_neutral n) (string_of_value v)
  | Fst n -> Printf.sprintf "fst %s" (string_of_neutral n)
  | Snd n -> Printf.sprintf "snd %s" (string_of_neutral n)
  | NtFun (_, _) -> "<fun>"
and string_of_clos _ = "<fun>"
and string_of_sclos _ = "..."

(** Instantiate a closure with a value. *)
let rec ( * ) c v =
  match c with
  | Cl (p, e, rho) -> eval (add_var rho p v) e
  | ClCmp (f, c) -> f * Cons (c, v)

(** Apply a value to another. *)
and app u v =
  match u, v with
  | Abs f, v -> f * v
  | Fun (ces, rho), Cons (c, v) -> app (eval rho (List.assoc c ces)) v
  | Fun s, Nt k -> Nt (NtFun (s, k))
  | Nt k, m -> Nt (App (k, m))
  | _, _ -> failwith "app"

(** Evaluate an expression to a value. *)
and eval rho = function
  | ESet -> Set
  | EDecl (d, e) -> eval ((RhoDecl d)::rho) e
  | EAbs (p, e) -> Abs (Cl (p, e, rho))
  | EPi (p, a, b) -> Pi (eval rho a, Cl (p, b, rho))
  | ESig (p, a, b) -> Sig (eval rho a, Cl (p, b, rho))
  | EOne -> One
  | EUnit -> Unit
  | EFst e -> vfst (eval rho e)
  | ESnd e -> vsnd (eval rho e)
  | EApp (e1, e2) -> app (eval rho e1) (eval rho e2)
  | EVar x -> get_rho rho x
  | EPair (e1, e2) -> Pair (eval rho e1, eval rho e2)
  | ECons (c, e1) -> Cons (c, eval rho e1)
  | ESum cas -> Sum (cas, rho)
  | EFun ces -> Fun (ces, rho)

(** Get the value of a variable in a context. *)
and get_rho rho x =
  match rho with
  | (RhoVar (p, v))::_ when in_pattern x p -> pattern_proj p x v
  | (RhoDecl (Def (p, _, e)))::rho when in_pattern x p -> pattern_proj p x (eval rho e)
  | (RhoDecl (Drec (p, _, e)))::_ when in_pattern x p -> pattern_proj p x (eval rho e)
  | _::rho -> get_rho rho x
  | [] -> raise Not_found

(** Normal forms. *)
type normal = 
  | NAbs of int * normal (** abstraction *)
  | NPair of normal * normal (** pair *)
  | NCons of name * normal (** constructor *)
  | NUnit (** unit constructor *)
  | NSet (** universe *)
  | NPi of normal * int * normal (** Pi types *)
  | NSig of normal * int * normal (** Sigma types *)
  | NOne (** unit type *)
  | NFun of normal_sclos (** pattern matching *)
  | NSum of normal_sclos (** sum type *)
  | NNt of normal_neutral (** neutral normal form *)

(** Neutral normal forms. *)
and normal_neutral =
  | NGen of int (** variable *)
  | NApp of normal_neutral * normal (** application *)
  | NFst of normal_neutral (** first projection *)
  | NSnd of normal_neutral (** second projection *)
  | NNtFun of normal_sclos * normal_neutral (** closure of a match *)

(** Closure of a match. *)
and normal_sclos = (branch * normal_rho)

(** Environment assigning normal forms to variables. *)
and normal_rho = in_normal_rho list

and in_normal_rho =
  | NRhoVar of pattern * normal
  | NRhoDecl of decl

(** Create a variable. *)
let gen i = Nt (Gen i)

(** Read back a value as a normal form. *)
let rec readback i v =
  let rec rneutral i = function
    | Gen j -> NGen j
    | App (k, m) -> NApp (rneutral i k, readback i m)
    | Fst k -> NFst (rneutral i k)
    | Snd k -> NSnd (rneutral i k)
    | NtFun ((s, rho), k) -> NNtFun ((s, rrho i rho), rneutral i k)
  and rrho i rho =
    List.map
      (function
        | RhoVar (p, v) -> NRhoVar (p, readback i v)
        | RhoDecl d -> NRhoDecl d
      ) rho
  in
  match v with
  | Abs f -> NAbs (i, readback (i+1) (f * gen i))
  | Pair (u, v) -> NPair (readback i u, readback i v)
  | Cons (c, v) -> NCons (c, readback i v)
  | Unit -> NUnit
  | Set -> NSet
  | Pi (t, g) -> NPi (readback i t, i, readback (i+1) (g * gen i))
  | Sig (t, g) -> NSig (readback i t, i, readback (i+1) (g * gen i))
  | One -> NOne
  | Fun (s, rho) -> NFun (s, rrho i rho)
  | Sum (s, rho) -> NSum (s, rrho i rho)
  | Nt l -> NNt (rneutral i l)

(** Environment assigning a type to variables. *)
type gamma = name * value

(** Declare a variable of given type in gamma environment. The last argument is
   the value and is needed for dependent types. *)
let rec add_type gamma p t v =
  match p, t with
  | PUnit, _  -> gamma
  | PVar x, t -> (x,t)::gamma
  | PPair (p1, p2), Sig (t, g) ->
    let gamma = add_type gamma p1 t (vfst v) in
    add_type gamma p2  (g * vfst v) (vsnd v)
  | _, _ -> failwith "add_type"

(** Check that an expression is a type. *)
let rec check_type k rho gamma = function
  | EPi (p, a, b) ->
    check_type k rho gamma a;
    let gamma = add_type gamma p (eval rho a) (gen k) in
    check_type (k+1) (add_var rho p (gen k)) gamma b
  | ESig (p, a, b) ->
    check_type k rho gamma (EPi (p, a, b))
  | ESet -> ()
  | a -> check k rho gamma a Set

(** Check that an expression has given type. *)
and check k rho gamma e t =
  Printf.printf "CHECK %s IS %s\n%!" (string_of_expr e) (string_of_value t);
  let eq k m1 m2 =
    if readback k m1 <> readback k m2 then
      (
        Printf.printf "EQ %s VS %s\n%!" (string_of_value m1) (string_of_value m2);
        failwith "eq"
      )
  in
  match e, t with
  | EAbs (p, e), Pi (t, g) ->
    let x = gen k in
    let gamma = add_type gamma p t x in
    check (k+1) (add_var rho p x) gamma e (g * x)
  | EPair (e1, e2), Sig (t, g) ->
    check k rho gamma e1 t;
    check k rho gamma e1 (g * eval rho e1)
  | ECons (c, e), Sum (cas, rho) ->
    let a = List.assoc c cas in
    check k rho gamma e (eval rho a)
  | EFun ces, Pi (Sum (cas, rho), g) ->
    if List.map fst ces <> List.map fst cas then failwith "case branches does not match the data type";
    List.iter
      (fun (c,e) ->
         let a = List.assoc c cas in
         check k rho gamma e (Pi (eval rho a, ClCmp (g, c)))
      ) ces
  | EUnit, One -> ()
  | EOne, Set -> ()
  | EPi (p, a, b), Set ->
    check k rho gamma a Set;
    let x = gen k in
    let gamma = add_type gamma p (eval rho a) x in
    check (k+1) (add_var rho p x) gamma b Set
  | ESig (p, a, b), Set ->
    check k rho gamma (EPi (p, a, b)) Set
  | ESum cas, Set ->
    List.iter (fun (_, a) -> check k rho gamma a Set) cas
  | EDecl (d, e), t ->
    let gamma = check_decl k rho gamma d in
    check k ((RhoDecl d)::rho) gamma e t
  | e, t' ->
    let t = infer k rho gamma e in
    eq k t t'

(** Infer the type of an expression. *)
and infer k rho gamma = function
  | EVar x -> List.assoc x gamma
  | EApp (e1, e2) ->
    (
      match infer k rho gamma e1 with
      | Pi (t, g) ->
        check k rho gamma e2 t;
        g * eval rho e2
      | _ -> failwith "infer"
  )
  | EFst e ->
    (
      match infer k rho gamma e with
      | Sig (a, _) -> a
      | _ -> failwith "infer"
    )
  | ESnd e ->
    (
      match infer k rho gamma e with
      | Sig (_, g) -> g * vfst (eval rho e)
      | _ -> failwith "infer"
    )
  | _ -> failwith "infer"

(** Check a declaration and return the updated gamma environment. *)
and check_decl k rho gamma = function
  | Def (p, a, e) ->
    check_type k rho gamma a;
    let t = eval rho a in
    check k rho gamma e t;
    add_type gamma p t (eval rho e)
  | Drec (p, a, e) as d ->
    check_type k rho gamma a;
    let t = eval rho a in
    let x = gen k in
    let gamma = add_type gamma p t x in
    check (k+1) (add_var rho p x) gamma e t;
    let v = eval ((RhoDecl d)::rho) e in
    add_type gamma p t v
