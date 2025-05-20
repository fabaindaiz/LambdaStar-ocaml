open Type

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
  | TArrow (a, b, gc1), TArrow (c, d, gc2) ->
    grad_sec_subtyping gc2 gc1 &&
    subtyping c a &&
    subtyping b d
  | _ -> false

and subtyping (t1 : ttype) (t2 : ttype) : bool =
  match t1, t2 with
  | Type (s, g1), Type (t, g2) ->
    grad_sec_subtyping g1 g2 &&
    raw_subtyping s t


let grad_sec_consistency (g1 : grad_sec) (g2 : grad_sec) : bool =
  match g1, g2 with
  | _, TStar -> true
  | TStar, _ -> true
  | TConc c1, TConc c2 -> c1 = c2

let rec raw_consistency (t1 : raw_type) (t2 : raw_type) : bool =
  match t1, t2 with
  | TBase b1, TBase b2 -> b1 = b2
  | TArrow (a, b, gc1), TArrow (c, d, gc2) ->
    grad_sec_consistency gc1 gc2 &&
    consistency a c &&
    consistency b d
  | _ -> false

and consistency (t1 : ttype) (t2 : ttype) : bool =
  match t1, t2 with
  | Type (s, g1), Type (t, g2) ->
    grad_sec_consistency g1 g2 &&
    raw_consistency s t


let grad_sec_consistent_subtyping (g1 : grad_sec) (g2 : grad_sec) : bool =
  match g1, g2 with
  | _, TStar -> true
  | TStar, _ -> true
  | TConc _, TConc _ -> grad_sec_subtyping g1 g2

let rec raw_consistent_subtyping (t1 : raw_type) (t2 : raw_type) : bool =
  match t1, t2 with
  | TBase b1, TBase b2 -> b1 = b2
  | TArrow (a, b, gc1), TArrow (c, d, gc2) ->
    grad_sec_consistent_subtyping gc2 gc1 &&
    consistent_subtyping c a &&
    consistent_subtyping b d
  | _ -> false

and consistent_subtyping (t1 : ttype) (t2 : ttype) : bool =
  match t1, t2 with
  | Type (s, g1), Type (t, g2) ->
    grad_sec_consistent_subtyping g1 g2 &&
    raw_consistent_subtyping s t