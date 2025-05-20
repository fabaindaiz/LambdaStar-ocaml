open Common.Env
open Common.Type
open Common.Subtyping
open Common.Lattice
open Ast

let rec surface_typing (m : surf) (env : tenv) (gc : grad_sec) : ttype =
  match m with
  | Var x -> lookup_env x env
  | Const (k, l) -> Type (TBase (constant_typing k), TConc l)
  | Abs (pc, x, a, n, l) ->
    let env' = (x, a) :: env in
    let b = surface_typing n env' (TConc pc) in
    Type (TArrow (a, b, TConc pc), TConc l)
  | App (l, m, _) ->
    (match surface_typing l env gc with
    | Type (TArrow (a, b, gc'), g) ->
      let a' = surface_typing m env gc in
      check consistent_subtyping a' a;
      check grad_sec_consistent_subtyping g gc';
      check grad_sec_consistent_subtyping gc gc';
      consistent_join_with_grad_sec b g
    | _ -> raise (TypeError "Application type mismatch"))
  | If (l, m, n, _) ->
    (match surface_typing l env gc with
    | Type (TBase TBool, g) ->
      let g' = grad_sec_consistent_join gc g in
      let a = surface_typing m env g' in
      let b = surface_typing n env g' in
      let c = consistent_join a b in
      consistent_join_with_grad_sec c g
    | _ -> raise (TypeError "If type mismatch"))
  | Let (x, m, n) ->
    let a = surface_typing m env gc in
    let env' = (x, a) :: env in
    surface_typing n env' gc
  | Annot (m, a, _) ->
    let a' = surface_typing m env gc in
    check consistent_subtyping a' a;
    a


let rec surface_typed (m : surf) (env : tenv) (gc : grad_sec) : 'a tsurf =
  match m with
  | Var x -> Var (x, (surface_typing m env gc, gc))
  | Const (k, l) -> Const (k, l, (surface_typing m env gc, gc))
  | Abs (pc, x, a, n, l) ->
    let env' = (x, a) :: env in
    let b = surface_typing n env' (TConc pc) in
    Abs (pc, x, a, surface_typed n env' (TConc pc), l, (Type (TArrow (a, b, TConc pc), TConc l), TConc l))
  | App (l, m, p) ->
    (match surface_typing l env gc with
    | Type (TArrow (_, b, _), g) ->
      App (surface_typed l env gc, surface_typed m env gc, p, (consistent_join_with_grad_sec b g, consisten_join_only_grad_sec b g))
    | _ -> raise (TypeError "Application type mismatch"))
  | If (l, m, n, p) ->
    (match surface_typing l env gc with
    | Type (TBase TBool, g) ->
      let g' = grad_sec_consistent_join gc g in
      If (surface_typed l env gc, surface_typed m env g', surface_typed n env g', p, (surface_typing m env gc, gc))
    | _ -> raise (TypeError "If type mismatch"))
  | Let (x, m, n) ->
    let a = surface_typing m env gc in
    let env' = (x, a) :: env in
    Let (x, surface_typed m env gc, surface_typed n env' gc, (surface_typing n env' gc, gc))
  | Annot (m, a, p) ->
    Annot (surface_typed m env gc, a, p, (a, gc))