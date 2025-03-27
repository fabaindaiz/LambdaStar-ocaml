open Common.Env
open Common.Type
open Common.Lattice
open Ast

let rec core_typing (m : core) (env : typeEnv) (gc : grad_sec) : ttype =
  match m with
  | Var x -> get_type x env
  | Const (k, l) -> Type (TBase (constant_typing k), TConc l)
  | If (l, a, m, n) ->
    (match core_typing l env gc with
    | Type (TBase TBool, g) ->
      let g' = grad_sec_consistent_join gc g in
      check (=) a (core_typing m env g');
      check (=) a (core_typing n env g');
      consistent_join_with_grad_sec a g
    | _ -> raise (TypeError "If type mismatch"))
  | _ -> failwith "Core unimplemented"

let rec core_erase (u: core) : core =
  match u with
  | Var x -> Var x
  | Const (k, l) ->
    (match l with
    | TLow -> Const (k, TLow)
    | THigh -> Opaque)
  | If (l, a, m, n) -> If (core_erase l, a, core_erase m, core_erase n)
  | ECast (m, _) -> core_erase m
  | _ -> Opaque