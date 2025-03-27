open Printf

type conc_sec =
  | TLow
  | THigh

type grad_sec =
  | TStar
  | TConc of conc_sec

let string_of_conc_sec =
  function
  | TLow -> "low"
  | THigh -> "high"

let string_of_grad_sec =
  function
  | TStar -> "*"
  | TConc c -> string_of_conc_sec c


type base_type =
  | TUnit
  | TBool

type raw_type =
  | TBase of base_type
  | TArrow of ttype *  ttype * grad_sec

and ttype =
  | Type of raw_type * grad_sec

let string_of_base_type =
  function
  | TUnit -> "Unit"
  | TBool -> "Bool"

let rec string_of_raw_type =
  function
  | TBase b -> string_of_base_type b
  | TArrow (t1, t2, g) -> sprintf "(%s -> %s %s)" (string_of_ttype t1) (string_of_ttype t2) (string_of_grad_sec g)

and string_of_ttype =
  function
  | Type (t, g) -> sprintf "%s %s" (string_of_raw_type t) (string_of_grad_sec g)


type constant =
  | Unit
  | True
  | False

type blame = string

let string_of_constant =
  function
  | Unit -> "unit"
  | True -> "true"
  | False -> "false"


type surf =
  | Var of string
  | Const of constant * conc_sec
  | Abs of conc_sec * string * ttype * surf * conc_sec
  | App of surf * surf * blame
  | If of surf * surf * surf * blame
  | Let of string * surf * surf
  | Annot of surf * ttype * blame

let rec string_of_surface (t : surf) : string =
  match t with
  | Var s -> s
  | Const (k, l) -> sprintf "($ %s %s)" (string_of_constant k) (string_of_conc_sec l)
  | Abs (pc, x, a, n, l) -> sprintf "(lam %s %s:%s.%s %s)" (string_of_conc_sec pc) x (string_of_ttype a) (string_of_surface n) (string_of_conc_sec l)
  | App (l, m, p) -> sprintf "(app %s %s %s)" (string_of_surface l) (string_of_surface m) p
  | If (l, m, n, p) -> sprintf "(if %s then %s else %s %s)" (string_of_surface l) (string_of_surface m) (string_of_surface n) p
  | Let (x, m, n) -> sprintf "(let %s = %s in %s)" x (string_of_surface m) (string_of_surface n)
  | Annot (m, a, p) -> sprintf "(%s : %s %s)" (string_of_surface m) (string_of_ttype a) p