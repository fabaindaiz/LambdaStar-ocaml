open Ast
open Surface
open Lattice
open Subtyping

exception CompileError of string

let rec surf_to_core (m : 'a tsurf) : core =
  match m with
  | Var (x, _) -> Var x
  | Const (k, l, _) -> Const (k, l)
  | If (l, m, n, p, _) ->
    let a = tsurf_type m in
    let b = tsurf_type n in
    let c = consistent_join a b in
    let a' = merge a c in
    let b' = merge b c in
    let c1 = Cast (a, a', p) in
    let c2 = Cast (b, b', p) in
    If (surf_to_core l, c, TCast (surf_to_core m, c1), TCast (surf_to_core n, c2))
  | _ -> failwith "Compile unimplemented"

let compile_core (m : surf) : core =
  let gc = TConc TLow in
  let m' = surface_typed m [] gc in
  surf_to_core m'