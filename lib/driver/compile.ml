open Common.Env
open Common.Type
open Common.Lattice
open Surface.Ast
open Surface.Typecheck
open Core.Ast

exception CompileError of string

let rec surf_to_core (m : 'a tsurf) : core =
  match m with
  | Var (x, _) -> Var x
  | Const (k, l, _) -> Const (k, l)
  | Abs (pc, x, a, n, l, _) ->
    Abs (pc, x, a, surf_to_core n, l)
  | App (l, m, p, _) ->
    (match tsurf_type l with
    | (Type (TArrow (a, b, gc'), g), gc) ->
      let a', _ = tsurf_type m in
      let c = merge a' a in
      let g1 = grad_sec_merge gc gc' in
      let g2 = grad_sec_merge g gc' in
      let c1 = Cast (Type (TArrow (a, b, gc'), g), Type (TArrow (a, b, grad_sec_consistent_join g1 g2), g), p) in
      let c2 = Cast (a', c, p) in
      App (ECast (surf_to_core l, c1), ECast (surf_to_core m, c2))
    | _ -> raise (CompileError "App: not an arrow"))
  | If (l, m, n, p, _) ->
    let a, _ = tsurf_type m in
    let b, _ = tsurf_type n in
    let c = consistent_join a b in
    let a' = merge a c in
    let b' = merge b c in
    let c1 = Cast (a, a', p) in
    let c2 = Cast (b, b', p) in
    If (surf_to_core l, c, ECast (surf_to_core m, c1), ECast (surf_to_core n, c2))
  | Let (x, m, n, _) ->
    Let (x, surf_to_core m, surf_to_core n)
  | Annot (m, a, p, _) ->
    let a', _ = tsurf_type m in
    let b = merge a' a in
    ECast (surf_to_core m, Cast (a', b, p))


let compile_core (m : surf) (env : typeEnv) (gc : grad_sec) : core =
  let _ = surface_typing m env gc in
  let m' = surface_typed m env gc in
  surf_to_core m'