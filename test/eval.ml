open Alcotest
open StagedS0R0

let eval_test name expected_val_str code () =
  let lexbuf = Lexing.from_string code in
  let exp =
    match Parser_wrap.parse lexbuf with
    | Ok exp -> exp
    | Error msg -> failwith ("Parse error: " ^ msg)
  in
  let trans_exp = Trans.Exp.trans exp in
  let eval_result =
    match Eval.eval Eval.ValueEnv.empty trans_exp with
    | Ok result -> result
    | Error err -> failwith ("Eval error: " ^ Eval.RuntimeError.show err)
  in
  let got_val_str = Aux.Exp.Value.show eval_result in
  check string name expected_val_str got_val_str

let () =
  run "Eval Tests"
    [
      ( "Base expression",
        [
          test_case "Integer" `Quick @@ eval_test "Integer" "42" "42";
          test_case "Boolean" `Quick @@ eval_test "Boolean" "true" "true";
          test_case "String" `Quick
          @@ eval_test "String" "\"hello\"" "\"hello\"";
        ] );
      ( "Expression",
        [
          test_case "lambda expression" `Quick
          @@ eval_test "Lambda" "(fun x -> x)" "fun x -> x";
          test_case "let expression" `Quick
          @@ eval_test "Let" "1" "let x = 1 in x";
          test_case "application" `Quick
          @@ eval_test "Application" "1" "(fun x -> x) 1";
          test_case "let and lambda" `Quick
          @@ eval_test "Let and Lambda" "(fun x -> x)" "let f = fun x -> x in f";
        ] );
      ( "Control flow",
        [
          test_case "if true" `Quick
          @@ eval_test "If true" "1" "if true then 1 else 2";
          test_case "if false" `Quick
          @@ eval_test "If false" "2" "if false then 1 else 2";
        ] );
    ]
