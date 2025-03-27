open Printf
open Common.Type

type cast =
  | Cast of ttype * ttype * blame

type error =
  | NSUError
  | BlameError of blame

let string_of_cast (c : cast) : string =
  match c with
  | Cast (a, b, p) -> sprintf "(%s -> %s %s)" (string_of_ttype a) (string_of_ttype b) (string_of_blame p)

let string_of_error (e : error) : string =
  match e with
  | NSUError -> "NSU"
  | BlameError p -> (string_of_blame p)


type core =
  | Var of string
  | Const of const * conc_sec
  | Abs of conc_sec * string * ttype * core * conc_sec
  | App of core * core
  | If of core * ttype * core * core
  | Let of string * core * core
  | ECast of core * cast
  | TCast of grad_sec * core
  | Prot of conc_sec * core
  | Error of error
  | Opaque

let rec string_of_core (t : core) : string =
  match t with
  | Var s -> s
  | Const (k, l) -> sprintf "($ %s %s)" (string_of_const k) (string_of_conc_sec l)
  | Abs (pc, x, a, n, l) -> sprintf "(λ %s %s:%s.%s %s)" (string_of_conc_sec pc) x (string_of_ttype a) (string_of_core n) (string_of_conc_sec l)
  | App (l, m) -> sprintf "(app %s %s)" (string_of_core l) (string_of_core m)
  | If (l, a, m, n) -> sprintf "(if %s %s %s %s)" (string_of_core l) (string_of_ttype a) (string_of_core m) (string_of_core n)
  | Let (x, m, n) -> sprintf "(let %s = %s in %s)" x (string_of_core m) (string_of_core n)
  | ECast (m, c) -> sprintf "%s <%s>" (string_of_core m) (string_of_cast c)
  | TCast (g, m) -> sprintf "cast %s %s" (string_of_grad_sec g) (string_of_core m)
  | Prot (l, m) -> sprintf "prot %s %s" (string_of_conc_sec l) (string_of_core m)
  | Error e -> sprintf "error %s" (string_of_error e)
  | Opaque -> "●"