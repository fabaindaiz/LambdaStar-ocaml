open Ast

let conc_sec_subtyping (c1 : conc_sec) (c2 : conc_sec) : bool =
  match c1, c2 with
  | TLow, THigh -> true
  | c1, c2 -> c1 = c2


let grad_sec_subtyping (g1 : grad_sec) (g2 : grad_sec) : bool =
  match g1, g2 with
  | TStar, TStar -> true
  | TConc c1, TConc c2 -> conc_sec_subtyping c1 c2
  | _ -> false

let rec raw_subtyping (t1 : raw_type) (t2 : raw_type) : bool =
  match t1, t2 with
  | TBase b1, TBase b2 -> b1 = b2
  | TArrow (t1a, t1b, g1), TArrow (t2a, t2b, g2) ->
    grad_sec_subtyping g1 g2 &&
    subtyping t2a t1a &&
    subtyping t1b t2b
  | _ -> false

and subtyping (t1 : ttype) (t2 : ttype) : bool =
  match t1, t2 with
  | Type (t1, g1), Type (t2, g2) ->
    grad_sec_subtyping g1 g2 &&
    raw_subtyping t1 t2


let grad_sec_consistency (g1 : grad_sec) (g2 : grad_sec) : bool =
  match g1, g2 with
  | TStar, _ -> true
  | _, TStar -> true
  | TConc c1, TConc c2 -> c1 = c2

let rec raw_consistency (t1 : raw_type) (t2 : raw_type) : bool =
  match t1, t2 with
  | TBase b1, TBase b2 -> b1 = b2
  | TArrow (t1a, t1b, g1), TArrow (t2a, t2b, g2) ->
    grad_sec_consistency g1 g2 &&
    consistency t2a t1a &&
    consistency t1b t2b
  | _ -> false

and consistency (t1 : ttype) (t2 : ttype) : bool =
  match t1, t2 with
  | Type (t1, g1), Type (t2, g2) ->
    grad_sec_consistency g1 g2 &&
    raw_consistency t1 t2


let grad_sec_consistent_subtyping (g1 : grad_sec) (g2 : grad_sec) : bool =
  match g1, g2 with
  | TStar, _ -> true
  | _, TStar -> true
  | TConc _, TConc _ ->
    grad_sec_subtyping g1 g2

let rec raw_consistent_subtyping (t1 : raw_type) (t2 : raw_type) : bool =
  match t1, t2 with
  | TBase b1, TBase b2 -> b1 = b2
  | TArrow (t1a, t1b, g1), TArrow (t2a, t2b, g2) ->
    grad_sec_consistent_subtyping g1 g2 &&
    consistent_subtyping t2a t1a &&
    consistent_subtyping t1b t2b
  | _ -> false

and consistent_subtyping (t1 : ttype) (t2 : ttype) : bool =
  match t1, t2 with
  | Type (t1, g1), Type (t2, g2) ->
    grad_sec_consistent_subtyping g1 g2 &&
    raw_consistent_subtyping t1 t2