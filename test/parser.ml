open Alcotest
open Common.All
open Msp_eff

module TestExp = struct
  type t = (Front.Exp.t, string) result

  let equal a b =
    match (a, b) with
    | Ok exp1, Ok exp2 -> Front.Exp.equal exp1 exp2
    | _ -> a = b

  let pp fmt = function
    | Ok exp -> Format.fprintf fmt "Ok %a" Front.Exp.pp exp
    | Error err -> Format.fprintf fmt "Error %s" err
end

let parse_test name expected txt () =
  let lexbuf = Lexing.from_string txt in
  let result = Parser_wrap.parse lexbuf in
  check (module TestExp) name result
  @@ Ok { body = expected; attr = SourcePosition.dummy }

let () =
  run "Parser Tests"
    [
      ( "Base expression",
        [
          test_case "Integer" `Quick
          @@ parse_test "Integer" (Front.Exp.Const (Int 42)) "42";
          test_case "Boolean" `Quick
          @@ parse_test "Boolean" (Front.Exp.Const (Bool true)) "true";
          test_case "String" `Quick
          @@ parse_test "String" (Front.Exp.Const (String "hello")) "\"hello\"";
        ] );
      ( "Expression",
        [
          test_case "lambda expression" `Quick
          @@ parse_test "Lambda"
               (Front.Exp.Lam
                  ( { body = "x"; attr = SourcePosition.dummy },
                    {
                      body =
                        Front.Exp.Var
                          { body = "x"; attr = SourcePosition.dummy };
                      attr = SourcePosition.dummy;
                    } ))
               "fun x -> x";
          test_case "complicated lambda expression (1)" `Quick
          @@ parse_test "Complicated Lambda"
               (Front.Exp.Lam
                  ( { body = "x"; attr = SourcePosition.dummy },
                    {
                      body =
                        Front.Exp.App
                          ( {
                              body =
                                Front.Exp.Lam
                                  ( { body = "y"; attr = SourcePosition.dummy },
                                    {
                                      body =
                                        Front.Exp.Var
                                          {
                                            body = "y";
                                            attr = SourcePosition.dummy;
                                          };
                                      attr = SourcePosition.dummy;
                                    } );
                              attr = SourcePosition.dummy;
                            },
                            {
                              body =
                                Front.Exp.Var
                                  { body = "x"; attr = SourcePosition.dummy };
                              attr = SourcePosition.dummy;
                            } );
                      attr = SourcePosition.dummy;
                    } ))
               "fun x -> (fun y -> y) x";
          test_case "Complicated lambda expression (2)" `Quick
          @@ parse_test "Complicated Lambda 2";
        ] );
    ]
