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
  | Type (t, g) -> sprintf "[%s %s]" (string_of_raw_type t) (string_of_grad_sec g)


type const =
  | Unit
  | True
  | False

type blame =
  | Blame of string

let string_of_const =
  function
  | Unit -> "unit"
  | True -> "true"
  | False -> "false"

let string_of_blame =
  function
  | Blame s -> s


type surf =
  | Var of string
  | Const of const * conc_sec
  | If of surf * surf * surf * blame

let rec string_of_surf (t : surf) : string =
  match t with
  | Var s -> s
  | Const (k, l) -> sprintf "($ %s %s)" (string_of_const k) (string_of_conc_sec l)
  | If (l, m, n, p) -> sprintf "(if %s then %s else %s %s)" (string_of_surf l) (string_of_surf m) (string_of_surf n) (string_of_blame p)


type cast =
  | Cast of ttype * ttype * blame

let string_of_cast (c : cast) : string =
  match c with
  | Cast (a, b, p) -> sprintf "(%s -> %s %s)" (string_of_ttype a) (string_of_ttype b) (string_of_blame p)

type core =
  | Var of string
  | Const of const * conc_sec
  | If of core * ttype * core * core
  | TCast of core * cast
  | Opaque

let rec string_of_core (t : core) : string =
  match t with
  | Var s -> s
  | Const (k, l) -> sprintf "($ %s %s)" (string_of_const k) (string_of_conc_sec l)
  | If (l, a, m, n) -> sprintf "(if %s %s %s %s)" (string_of_core l) (string_of_ttype a) (string_of_core m) (string_of_core n)
  | TCast (m, c) -> sprintf "%s <%s>" (string_of_core m) (string_of_cast c)
  | Opaque -> "●"