(** Interpreter **)
open Common.Type
open Core.Ast
open Core.Typecheck
open Core.Value
open Printf

exception InterpError of string

(* Type checks *)
let raise_type_error (expected_type_s : string) (ill_value : value) : 'a =
  raise (InterpError (sprintf "Type error: Expected %s but got %s" expected_type_s (string_of_val ill_value)))


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
    | _ -> raise (InterpError (sprintf "Interp unimplemented: %s" (string_of_core u))) in
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
  | ECast (m, _) -> interp_casted m env (* complete this cast *)
  | _ -> raise (InterpError (sprintf "Interp unimplemented: %s" (string_of_core u)))