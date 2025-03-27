open Parsing.Parse
open Driver.Compile
open Core.Ast
open Printf

let () =
  let args = Sys.argv in
  if Array.length args > 1 && Sys.file_exists args.(1)
  then
    let src = sexp_from_file args.(1) in
    printf "%s\n" (string_of_core (compile_core (parse_surface src)))
  else
    printf "usage: run_parse.exe <filename>\n"