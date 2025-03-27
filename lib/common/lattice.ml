open Type

exception MeetError of string
exception JoinError of string
exception MergeError of string

let conc_sec_join (c1 : conc_sec) (c2 : conc_sec) : conc_sec =
  match c1, c2 with
  | TLow, THigh -> THigh
  | c1, c2 when c1 = c2 -> c1
  | _ -> raise (JoinError "No join")

let conc_sec_meet (c1 : conc_sec) (c2 : conc_sec) : conc_sec =
  match c1, c2 with
  | TLow, THigh -> TLow
  | c1, c2 when c1 = c2 -> c1
  | _ -> raise (MeetError "No meet")


let grad_sec_gradual_meet (g1 : grad_sec) (g2 : grad_sec) : grad_sec =
  match g1, g2 with
  | g, TStar -> g
  | TStar, g -> g
  | g1, g2 when g1 = g2 -> g1
  | _ -> raise (MeetError "No meet")

let rec raw_gradual_meet (t1 : raw_type) (t2 : raw_type) : raw_type =
  match t1, t2 with
  | TBase b1, TBase b2 when b1 = b2 -> TBase b1
  | TArrow (t1a, t1b, g1), TArrow (t2a, t2b, g2) ->
    TArrow (gradual_meet t1a t2a, gradual_meet t1b t2b, grad_sec_gradual_meet g1 g2)
  | _ -> raise (MeetError "No meet")

and gradual_meet (t1 : ttype) (t2 : ttype) : ttype =
  match t1, t2 with
  | Type (t1, g1), Type (t2, g2) ->
    Type (raw_gradual_meet t1 t2, grad_sec_gradual_meet g1 g2)


let grad_sec_consistent_join (g1 : grad_sec) (g2 : grad_sec) : grad_sec =
  match g1, g2 with
  | _, TStar -> TStar
  | TStar, _ -> TStar
  | TConc c1, TConc c2 -> TConc (conc_sec_join c1 c2)

let grad_sec_consistent_meet (g1 : grad_sec) (g2 : grad_sec) : grad_sec =
  match g1, g2 with
  | _, TStar -> TStar
  | TStar, _ -> TStar
  | TConc c1, TConc c2 -> TConc (conc_sec_meet c1 c2)


let rec raw_consistent_join (t1 : raw_type) (t2 : raw_type) : raw_type =
  match t1, t2 with
  | TBase b1, TBase b2 when b1 = b2 -> TBase b1
  | TArrow (a, b, gc1), TArrow (c, d, gc2) ->
    TArrow (consistent_meet a c, consistent_join b d, grad_sec_consistent_meet gc1 gc2)
  | _ -> raise (JoinError "No join")

and consistent_join (t1 : ttype) (t2 : ttype) : ttype =
  match t1, t2 with
  | Type (t1, g1), Type (t2, g2) ->
    Type (raw_consistent_join t1 t2, grad_sec_consistent_join g1 g2)


and raw_consistent_meet (t1 : raw_type) (t2 : raw_type) : raw_type =
  match t1, t2 with
  | TBase b1, TBase b2 when b1 = b2 -> TBase b1
  | TArrow (a, b, gc1), TArrow (c, d, gc2) ->
    TArrow (consistent_join a c, consistent_meet b d, grad_sec_consistent_join gc1 gc2)
  | _ -> raise (MeetError "No meet")

and consistent_meet (t1 : ttype) (t2 : ttype) : ttype =
  match t1, t2 with
  | Type (t1, g1), Type (t2, g2) ->
    Type (raw_consistent_meet t1 t2, grad_sec_consistent_meet g1 g2)


let consistent_join_with_grad_sec (t1 : ttype) (g2 : grad_sec) : ttype =
  match t1 with
  | Type (t1, g1) -> Type (t1, grad_sec_consistent_join g1 g2)

let consisten_join_only_grad_sec (t1 : ttype) (g2 : grad_sec) : grad_sec =
  match t1 with
  | Type (_, g1) -> grad_sec_consistent_join g1 g2

let grad_sec_merge (g1 : grad_sec) (g2 : grad_sec) : grad_sec =
  match g1, g2 with
  | _, TStar -> TStar
  | TStar, _ -> g2
  | TConc _, TConc _ -> g1

let rec raw_merge (t1 : raw_type) (t2 : raw_type) : raw_type =
  match t1, t2 with
  | TBase b1, TBase b2 -> if b1 = b2 then t1 else raise (MergeError "No merge")
  | TArrow (a, b, gc1), TArrow (c, d, gc2) ->
    TArrow (merge c a, merge b d, grad_sec_merge gc2 gc1)
  | _ -> raise (MergeError "No merge")

and merge (t1 : ttype) (t2 : ttype) : ttype =
  match t1, t2 with
  | Type (s, g1), Type (t, g2) ->
    Type (raw_merge s t, grad_sec_merge g1 g2)