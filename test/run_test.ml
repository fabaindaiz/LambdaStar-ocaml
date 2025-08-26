open Alcotest
open Bbctester.Type
open Bbctester.Main
open Parsing.Parse
open Driver.Compile
open Driver.Interp
open Surface.Ast
open Core.Ast
open Core.Value

(* Testing arithmetic expression using the print function defined in Interp 
   and the default equality for comparison *)
let core : core testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_core e)) (=)

let surf : surf testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_surf e)) (=)

let value : value testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_val e)) (=)

(* parser tests *)
let test_parse_unit () =
  check surf "same unit"
  (parse_surface (`List [`Atom "unit"; `Atom "low"]))
  (Const (Unit, TLow))

let test_parse_lambda () =
  check surf "same lambda"
  (parse_surface (`List [`Atom "lam"; `Atom "low"; `List [`Atom "x"; `List [`Atom "Unit"; `Atom "*"]]; `Atom "x"; `Atom "low"]))
  (Abs (TLow, "x", Type (TBase TUnit, TStar), Var "x", TLow))

(* compile tests *)
let test_compile_unit () =
  check core "same unit"
  (compile_core (Const (Unit, THigh)) [] (TConc TLow))
  (Const (Unit, THigh))

let test_compile_lambda () =
  check core "same lambda"
  (compile_core (Abs (TLow, "x", Type (TBase TUnit, TStar), Var "x", TLow)) [] (TConc TLow))
  (Abs(TLow, "x", Type (TBase TUnit, TStar), Var "x", TLow))

(* interp tests *)
let test_interp_unit () =
  check value "same unit"
  (interp (Const (Unit, THigh)) [])
  (Const (Unit, THigh))

let test_interp_lambda () =
  check value "same lambda"
  (interp (Abs(TLow, "x", Type (TBase TUnit, TStar), Var "x", TLow)) [])
  (Lambda (TLow, "x", Type (TBase TUnit, TStar), Var "x", TLow))

(* OCaml tests: extend with your own tests *)
let ocaml_tests = [
  "parser", [
    test_case "A unit" `Quick test_parse_unit;
    test_case "A lambda" `Quick test_parse_lambda;
  ];
  "compile", [
    test_case "A unit" `Quick test_compile_unit;
    test_case "A lambda" `Quick test_compile_lambda;
  ];
  "interp", [
    test_case "A unit" `Quick test_interp_unit;
    test_case "A lambda" `Quick test_interp_lambda;
  ];
  "errors", [
  ]
] 

(* Entry point of tester *)
let () =

  let compiler : compiler = 
    SCompiler ( fun _ s -> (string_of_val (interp (parse_surface (sexp_from_string s)) [])) ) in
  
  let bbc_tests =
    let name : string = "complete" in
    tests_from_dir ~name ~compiler "test" in
  
  run "Tests LambdaStar" (ocaml_tests @ bbc_tests)