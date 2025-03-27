open Parsing.Parse
open Driver.Compile
open Driver.Interp
open Core.Value
open Printf

let () =
  let args = Sys.argv in
  if Array.length args > 1 && Sys.file_exists args.(1)
  then
    let src = sexp_from_file args.(1) in
    printf "%s\n" (string_of_val (interp_casted (compile_core (parse_surface src)) empty_env))
  else
    printf "usage: run_parse.exe <filename>\n"