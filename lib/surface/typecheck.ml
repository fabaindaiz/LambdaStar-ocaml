open Common.Env
open Common.Type
open Common.Lattice
open Ast

let rec surface_typing (m : surf) (env : typeEnv) (gc : grad_sec) : ttype =
  match m with
  | Var x -> get_type x env
  | Const (k, l) -> Type (TBase (constant_typing k), TConc l)
  | If (l, m, n, _) ->
    (match surface_typing l env gc with
    | Type (TBase TBool, g) ->
      let g' = grad_sec_consistent_join gc g in
      let a = surface_typing m env g' in
      let b = surface_typing n env g' in
      let c = consistent_join a b in
      consistent_join_with_grad_sec c g
    | _ -> raise (TypeError "If type mismatch"))
  | _ -> failwith "Surface unimplemented"


let rec surface_typed (m : surf) (env : typeEnv) (gc : grad_sec) : 'a tsurf =
  match m with
  | Var x -> Var (x, surface_typing m env gc)
  | Const (k, l) -> Const (k, l, surface_typing m env gc)
  | If (l, m, n, p) ->
    let l_type = surface_typing l env gc in
    (match l_type with
    | Type (TBase TBool, g) ->
      let g' = grad_sec_consistent_join gc g in
      If (surface_typed l env gc, surface_typed m env g', surface_typed n env g', p, surface_typing m env gc)
    | _ -> raise (TypeError "If type mismatch"))
  | _ -> failwith "Surface unimplemented"