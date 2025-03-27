(** Interpreter **)
open Ast
open Core
open Parse
open Lattice
open Printf

exception RTError of string
exception CTError of string

(** Values **)
type value = 
  | Const of const * conc_sec
  | VCast of value * cast
  | Opaque

let value_with_conc_sec (v : value) (l' : conc_sec) : value =
  match v with
  | Const (k, l) -> Const (k, conc_sec_join l l')
  | VCast (v, c) -> VCast (v, c)
  | Opaque -> Opaque

(* Pretty printing *)
let rec string_of_val (v : value) : string =
  match v with
  | Const (k, l) -> sprintf "%s %s" (string_of_const k) (string_of_conc_sec l)
  | VCast (v, c) -> sprintf "(%s <%s>)" (string_of_val v) (string_of_cast c)
  | Opaque -> "●"


(* Type checks *)
let raise_type_error (expected_type_s : string) (ill_value : value) : 'a =
  raise (RTError (sprintf "Type error: Expected %s but got %s" expected_type_s (string_of_val ill_value)))


(* Lexical Environment *)
type env = (string * value) list
let empty_env : env = []

let extend_env (names : string list) (vals : value list) (env : env) : env =
  let param_vals = List.combine names vals in
  List.fold_left (fun env p -> p :: env) env param_vals

let lookup_env : string -> env -> value =
  fun s env ->
    match List.assoc_opt s env with
    | Some v -> v
    | None -> raise (CTError (sprintf "Unbound identifier: %s" s))


(* interpreter *)
let interp_erased (u : core) (env : env) : value =
  let rec interp_erased_aux (u : core) (env) : value =
    match u with
    | Var x -> lookup_env x env
    | Const (k, l) -> Const (k, l)
    | If (l, _, m, n) ->
      (match l with
      | Const (True, TLow) -> interp_erased_aux m env
      | Const (False, TLow) -> interp_erased_aux n env
      | Opaque -> Opaque
      | _ -> raise_type_error "Bool" (interp_erased_aux l env))
    | _ -> raise (RTError (sprintf "Interp unimplemented: %s" (string_of_core u))) in
  interp_erased_aux (core_erase u) env

let rec interp_casted (u : core) (env : env) : value =
  match u with
  | Var x -> lookup_env x env
  | Const (k, l) -> Const (k, l)
  | If (l, _, m, n) ->
    (match l with
    | Const (k, l) ->
      (match k with
      | True -> value_with_conc_sec (interp_casted m env) l
      | False -> value_with_conc_sec (interp_casted n env) l
      | _ -> raise_type_error "Bool" (Const (k, l)))
    | _ -> raise_type_error "Bool" (interp_casted l env))
  | TCast (m, c) -> interp_casted m env (* complete this case *)
  | _ -> raise (RTError (sprintf "Interp unimplemented: %s" (string_of_core u)))