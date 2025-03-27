open Printf

type conc_sec =
  | TLow
  | THigh

type grad_sec =
  | TStar
  | TConc of conc_sec

let string_of_conc_sec =
  function
  | TLow -> "low"
  | THigh -> "high"

let string_of_grad_sec =
  function
  | TStar -> "*"
  | TConc c -> string_of_conc_sec c


type base_type =
  | TUnit
  | TBool

type raw_type =
  | TBase of base_type
  | TArrow of ttype *  ttype * grad_sec

and ttype =
  | Type of raw_type * grad_sec

let string_of_base_type =
  function
  | TUnit -> "Unit"
  | TBool -> "Bool"

let rec string_of_raw_type =
  function
  | TBase b -> string_of_base_type b
  | TArrow (t1, t2, g) -> sprintf "(%s -> %s %s)" (string_of_ttype t1) (string_of_ttype t2) (string_of_grad_sec g)

and string_of_ttype =
  function
  | Type (t, g) -> sprintf "[%s %s]" (string_of_raw_type t) (string_of_grad_sec g)


type const =
  | Unit
  | True
  | False

type blame =
  | Blame of string

let string_of_const =
  function
  | Unit -> "unit"
  | True -> "true"
  | False -> "false"

let string_of_blame =
  function
  | Blame s -> s

let constant_typing (k : const) : base_type =
  match k with
  | Unit -> TUnit
  | True | False -> TBool