open Common.Env
open Common.Type
open Common.Subtyping
open Common.Lattice
open Ast

let rec core_typing (m : core) (env : tenv) (gc : grad_sec) : ttype =
  match m with
  | Var x -> lookup_env x env
  | Const (k, l) -> Type (TBase (constant_typing k), TConc l)
  | Abs (pc, x, a, n, l) ->
    let env' = (x, a) :: env in
    let b = core_typing n env' (TConc pc) in
    Type (TArrow (a, b, TConc pc), TConc l)
  | App (l, m) ->
    (match core_typing l env gc with
    | Type (TArrow (a, b, gc'), g) ->
      let a' = core_typing m env gc in
      check (=) gc' (grad_sec_consistent_join gc g);
      check (=) a' a;
      consistent_join_with_grad_sec b g
    | _ -> raise (TypeError "Application type mismatch"))
  | If (l, a, m, n) ->
    (match core_typing l env gc with
    | Type (TBase TBool, g) ->
      let g' = grad_sec_consistent_join gc g in
      check (=) a (core_typing m env g');
      check (=) a (core_typing n env g');
      consistent_join_with_grad_sec a g
    | _ -> raise (TypeError "If type mismatch"))
  | Let (x, m, n) ->
    let a = core_typing m env gc in
    let env' = (x, a) :: env in
    core_typing n env' gc
  | ECast (m, c) ->
    let Cast (a, b, _) = c in
    let a' = core_typing m env gc in
    check (=) a a';
    b
  | TCast (g, m) ->
    check grad_sec_consistency (TConc TLow) g; (* check this *)
    core_typing m env g
  | Prot (l, m) ->
    let gc' = grad_sec_consistent_join gc (TConc l) in
    let a = core_typing m env gc' in
    consistent_join_with_grad_sec a (TConc l) (* check this *)
  | Error _ -> failwith "Core unimplemented" (* check this *)
  | Opaque -> failwith "Core unimplemented"