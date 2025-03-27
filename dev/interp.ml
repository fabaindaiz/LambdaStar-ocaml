(** Interpreter **)
open Ast
open Parse
open Printf

exception RTError of string
exception CTError of string

(** Values **)
type value = 
  | Const of const * conc_sec
  | VCast of value * cast
  | Opaque

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

let rec core_erase (u: core) : core =
  match u with
  | Var x -> Var x
  | Const (k, l) ->
    (match l with
    | TLow -> Const (k, TLow)
    | THigh -> Opaque)
  | If (l, a, m, n) -> If (core_erase l, a, core_erase m, core_erase n)
  | TCast (m, _) -> core_erase m
  | Opaque -> Opaque

(* Type checks *)

(* interpreter *)
let rec interp_erased (u : core) (env) : value =
  match u with
  | Var x -> lookup_env x env
  | Const (k, l) -> Const (k, l)
  | If (l, _, m, n) ->
    (match l with
    | Const (True, TLow) -> interp_erased m env
    | Const (False, TLow) -> interp_erased n env
    | Opaque -> Opaque
    | _ -> raise_type_error "Bool" (interp_erased l env))
  | _ -> raise (RTError (sprintf "Interp unimplemented: %s" (string_of_core u)))

let interp (u: core) : value =
  interp_erased (core_erase u) empty_env