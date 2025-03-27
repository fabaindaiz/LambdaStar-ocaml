open Printf
open Common.Type

type surf =
  | Var of string
  | Const of const * conc_sec
  | Abs of conc_sec * string * ttype * surf * conc_sec
  | App of surf * surf * blame
  | If of surf * surf * surf * blame
  | Let of string * surf * surf
  | Annot of surf * ttype * blame

let rec string_of_surf (t : surf) : string =
  match t with
  | Var s -> s
  | Const (k, l) -> sprintf "($ %s %s)" (string_of_const k) (string_of_conc_sec l)
  | Abs (pc, x, a, n, l) -> sprintf "(λ %s %s:%s.%s %s)" (string_of_conc_sec pc) x (string_of_ttype a) (string_of_surf n) (string_of_conc_sec l)
  | App (l, m, p) -> sprintf "(app %s %s %s)" (string_of_surf l) (string_of_surf m) (string_of_blame p)
  | If (l, m, n, p) -> sprintf "(if %s then %s else %s %s)" (string_of_surf l) (string_of_surf m) (string_of_surf n) (string_of_blame p)
  | Let (x, m, n) -> sprintf "(let %s = %s in %s)" x (string_of_surf m) (string_of_surf n)
  | Annot (m, a, p) -> sprintf "(%s : %s %s)" (string_of_surf m) (string_of_ttype a) (string_of_blame p)


type 'a tsurf =
  | Var of string * 'a
  | Const of const * conc_sec * 'a
  | Abs of conc_sec * string * ttype * 'a tsurf * conc_sec * 'a
  | If of 'a tsurf * 'a tsurf * 'a tsurf * blame * 'a
  | Let of string * 'a tsurf * 'a tsurf * 'a
  | Annot of 'a tsurf * ttype * blame * 'a

let tsurf_type (m : 'a tsurf) : 'a =
  match m with
  | Var (_, t) -> t
  | Const (_, _, t) -> t
  | Abs (_, _, _, _, _, t) -> t
  | If (_, _, _, _, t) -> t
  | Let (_, _, _, t) -> t
  | Annot (_, _, _, t) -> t