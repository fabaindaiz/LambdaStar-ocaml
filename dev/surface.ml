open Ast
open Subtyping
open Lattice

exception TypeError of string

type typeEnv = (string * ttype) list

let get_type (x : string) (env : typeEnv) : ttype =
  try List.assoc x env
  with Not_found -> failwith ("Variable " ^ x ^ " not found")

let check (f : 'a -> 'a -> bool) (a : 'a) (b : 'a) : unit =
  if f a b then () else failwith "Type mismatch"


let constant_typing (k : constant) : base_type =
  match k with
  | Unit -> TUnit
  | True | False -> TBool

let rec surface_typing (e : surf) (env : typeEnv) (gc : grad_sec) : ttype =
  match e with
  | Var x -> get_type x env
  | Const (k, l) -> Type (TBase (constant_typing k), TConc l)
  | Abs (pc, x, a, n, l) ->
    let env' = (x, a) :: env in
    let b = surface_typing n env' (TConc pc) in
    Type (TArrow (a, b, TConc pc), TConc l)
  | App (l, m, _) ->
    (match surface_typing l env gc with
      | Type (TArrow (a, (Type (t, g1)), gc'), g) ->
        let a' = surface_typing m env gc in
        let () = check consistent_subtyping a' a in
        let () = check grad_sec_consistent_subtyping g gc' in
        let () = check grad_sec_consistent_subtyping gc gc' in
        Type (t, grad_sec_consistent_join g1 g)
      | _ -> raise (TypeError "Application type mismatch"))
  | If (l, m, n, p) ->
    (match surface_typing l env gc with
    | Type (TBase TBool, g) ->
      let g' = grad_sec_consistent_join gc g in
      let a = surface_typing m env g' in
      let b = surface_typing n env g' in
      (match consistent_subtyping a b with
      | Type (t, g1) ->
        let g' = 
      | _ -> raise (TypeError "If type mismatch"))
    | _ -> raise (TypeError "If type mismatch"))
  | Let (x, m, n) ->
    let a = surface_typing m env gc in
    let env' = (x, a) :: env in
    surface_typing n env' gc
  | Annot (m, a, _) ->
    let a' = surface_typing m env gc in
    let () = check consistent_subtyping a' a in
    a
  | _ -> failwith "unimplemented"