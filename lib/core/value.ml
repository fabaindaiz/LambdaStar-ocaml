(** Values **)
open Common.Env
open Common.Type
open Common.Lattice
open Ast
open Printf

exception VarError of string

type value = 
  | Const of const * conc_sec
  | Lambda of conc_sec * string * ttype * core * conc_sec
  | VCast of value * cast
  | Opaque

let value_with_conc_sec (v : value) (l' : conc_sec) : value =
  match v with
  | Const (k, l) -> Const (k, conc_sec_join l l')
  | Lambda (pc, x, a, n, l) -> Lambda (pc, x, a, n, conc_sec_join l l')
  | VCast (v, c) -> VCast (v, c)
  | Opaque -> Opaque

(* Pretty printing *)
let rec string_of_val (v : value) : string =
  match v with
  | Const (k, l) -> sprintf "%s %s" (string_of_const k) (string_of_conc_sec l)
  | Lambda (pc, x, a, n, l) -> sprintf "(λ %s %s:%s.%s %s)" (string_of_conc_sec pc) x (string_of_ttype a) (string_of_core n) (string_of_conc_sec l)
  | VCast (v, c) -> sprintf "(%s <%s>)" (string_of_val v) (string_of_cast c)
  | Opaque -> "●"


type venv = value env