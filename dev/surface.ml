open Ast
open Lattice

exception TypeError of string

type typeEnv = (string * ttype) list

let get_type (x : string) (env : typeEnv) : ttype =
  try List.assoc x env
  with Not_found -> failwith ("Variable " ^ x ^ " not found")

let check (f : 'a -> 'a -> bool) (a : 'a) (b : 'a) : unit =
  if f a b then () else failwith "Type mismatch"


let constant_typing (k : const) : base_type =
  match k with
  | Unit -> TUnit
  | True | False -> TBool

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


type 'a tsurf =
  | Var of string * 'a
  | Const of const * conc_sec * 'a
  | If of 'a tsurf * 'a tsurf * 'a tsurf * blame * 'a

let tsurf_type (m : 'a tsurf) : 'a =
  match m with
  | Var (_, t) -> t
  | Const (_, _, t) -> t
  | If (_, _, _, _, t) -> t


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
