open Common.Env
open Parsing.Parse
open Driver.Interp
open Surface.Ast
open Core.Ast
open Core.Value
open Alcotest
open Bbctester.Type
open Bbctester.Main

(* Testing arithmetic expression using the print function defined in Interp 
   and the default equality for comparison *)
let core : core testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_core e)) (=)

let surf : surf testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_surf e)) (=)

let value : value testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_val e)) (=)


(* OCaml tests: extend with your own tests *)
let ocaml_tests = [
  "compile", [
  ] ;
  "interp", [
  ] ;
  "errors", [
  ]
]     

(* Entry point of tester *)
let () =

  let compiler : compiler = 
    SCompiler ( fun _ s -> (string_of_val (interp (parse_surface (sexp_from_string s)) empty_env)) ) in
  
  let bbc_tests =
    let name : string = "complete" in
    tests_from_dir ~name ~compiler "test" in
  
  run "Tests LambdaStar" (ocaml_tests @ bbc_tests)