open Type

exception TypeError of string

type typeEnv = (string * ttype) list

let get_type (x : string) (env : typeEnv) : ttype =
  try List.assoc x env
  with Not_found -> failwith ("Variable " ^ x ^ " not found")

let check (f : 'a -> 'a -> bool) (a : 'a) (b : 'a) : unit =
  if f a b then () else failwith "Type mismatch"