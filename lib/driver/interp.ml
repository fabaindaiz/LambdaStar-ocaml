(** Interpreter **)
open Common.Type
open Surface.Ast
open Core.Ast
open Core.Typecheck
open Core.Value
open Compile
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
    | Abs (pc, x, a, n, l) -> Lambda (pc, x, a, n, l)
    | App (l, m) ->
      (match interp_erased_aux l env with
      | Lambda (_, x, _, n, TLow) ->
        let v = interp_erased_aux m env in
        let env' = (x, v) :: env in
        interp_erased_aux n env'
      | Opaque -> Opaque
      | _ -> raise_type_error "Lambda low" (interp_erased_aux l env))
    | If (l, _, m, n) ->
      (match l with
      | Const (True, TLow) -> interp_erased_aux m env
      | Const (False, TLow) -> interp_erased_aux n env
      | Opaque -> Opaque
      | _ -> raise_type_error "Bool low" (interp_erased_aux l env))
    | Let (x, m, n) ->
      let v = interp_erased_aux m env in
      let env' = (x, v) :: env in
      interp_erased_aux n env'
    | Opaque -> Opaque
    | _ -> raise (InterpError (sprintf "Interp unimplemented: %s" (string_of_core u))) in
  interp_erased_aux (core_erase u) env

let rec interp_casted (u : core) (env : env) : value =
  match u with
  | Var x -> lookup_env x env
  | Const (k, l) -> Const (k, l)
  | Abs (pc, x, a, n, l) -> Lambda (pc, x, a, n, l)
  | App (l, m) ->
    (match interp_casted l env with
    | Lambda (_, x, _, n, l) ->
      let v = interp_casted m env in
      let env' = (x, v) :: env in
      let w = interp_casted n env' in
      value_with_conc_sec w l
    | VCast (v, c) ->
      let w = interp_casted m env in
      (* elim-fun-proxy *)
      failwith "Not implemented"
    | _ -> raise_type_error "Lambda" (interp_casted l env))
  | If (l, _, m, n) ->
    (match interp_casted l env with
    | Const (True, l) -> value_with_conc_sec (interp_casted m env) l
    | Const (False, l) -> value_with_conc_sec (interp_casted n env) l
    | VCast (Const (True, l), c) ->
      let v = interp_casted m env in (* check this *)
      failwith "Not implemented"
    | VCast (Const (False, l), c) ->
      let v = interp_casted n env in (* check this *)
      failwith "Not implemented"
    | _ -> raise_type_error "Bool" (interp_casted l env))
  | Let (x, m, n) ->
    let v = interp_casted m env in
    let env' = (x, v) :: env in
    interp_casted n env'
  | ECast (m, _) ->
    let v = interp_casted m env in
    (* complete this cast *)
    v
  | _ -> raise (InterpError (sprintf "Interp unimplemented: %s" (string_of_core u)))


let interp (m : surf) (env : env) =
  let u = compile_core m [] (TConc TLow) in
  let _ = core_typing u [] (TConc TLow) in
  interp_casted u env