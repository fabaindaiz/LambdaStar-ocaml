open Ast

let rec core_erase (u: core) : core =
  match u with
  | Var x -> Var x
  | Const (k, l) ->
    (match l with
    | TLow -> Const (k, TLow)
    | THigh -> Opaque)
  | Abs (pc, x, a, n, l) ->
    (match l with
    | TLow -> Abs (pc, x, a, core_erase n, TLow)
    | THigh -> Opaque)
  | App (l, m) -> App (core_erase l, core_erase m)
  | If (l, a, m, n) -> If (core_erase l, a, core_erase m, core_erase n)
  | ECast (m, _) -> core_erase m
  | TCast (_, m) -> core_erase m
  | _ -> Opaque