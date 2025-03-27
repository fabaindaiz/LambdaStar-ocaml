open Common.Type
open Surface.Ast
open Printf
open CCSexp

exception ParseError of string

let parse_conc_sec (sexp : sexp) : conc_sec =
  match sexp with
  | `Atom "low" -> TLow
  | `Atom "high" -> THigh
  | _ -> raise (ParseError (sprintf "Not a valid conc_sec %s" (to_string sexp)))

let parse_grad_sec (sexp : sexp) : grad_sec =
  match sexp with
  | `Atom "*" -> TStar
  | _ -> TConc (parse_conc_sec sexp)

let parse_base_type (sexp : sexp) : base_type =
  match sexp with
  | `Atom "Unit" -> TUnit
  | `Atom "Bool" -> TBool
  | _ -> raise (ParseError (sprintf "Not a valid base_type %s" (to_string sexp)))

let rec parse_raw_type (sexp : sexp) : raw_type =
  match sexp with
  | `List [t1; `Atom "->"; t2; gc] -> TArrow (parse_ttype t1, parse_ttype t2, parse_grad_sec gc)
  | _ -> TBase (parse_base_type sexp)

and parse_ttype (sexp : sexp) : ttype =
  match sexp with
  | `List [t; g] -> Type (parse_raw_type t, parse_grad_sec g)
  | _ -> raise (ParseError (sprintf "Not a valid ttype %s" (to_string sexp)))


let parse_const (sexp : sexp) : const =
  match sexp with
  | `Atom "unit" -> Unit
  | `Atom "true" -> True
  | `Atom "false" -> False
  | _ -> raise (ParseError (sprintf "Not a valid constant %s" (to_string sexp)))

let rec parse_surface (sexp : sexp) : surf =
  match sexp with
  | `Atom s -> Var (s)
  | `List [k; l] -> Const (parse_const k, parse_conc_sec l)
  | `List [`Atom "if"; l; m; n; `Atom p] -> If (parse_surface l, parse_surface m, parse_surface n, Blame p)
  | _ -> raise (ParseError (sprintf "Not a valid term: %s" (to_string sexp)))

and parse_id (sexp : sexp) : string =
  match sexp with
  | `Atom name -> name
  | _ -> raise (ParseError (sprintf "Not a valid name: %s" (to_string sexp)))


let sexp_from_file : string -> CCSexp.sexp =
  fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> raise (ParseError (sprintf "Unable to parse file %s: %s" filename msg))

let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> raise (ParseError (sprintf "Unable to parse string %s: %s" src msg))