open Alcotest
open Common.All
open StagedS0R0

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
          @@ parse_test "Complicated Lambda 2"
               (Front.Exp.Lam
                  ( { body = "x"; attr = SourcePosition.dummy },
                    {
                      body =
                        Front.Exp.Lam
                          ( { body = "y"; attr = SourcePosition.dummy },
                            {
                              body =
                                Front.Exp.App
                                  ( {
                                      body =
                                        Front.Exp.Var
                                          {
                                            body = "x";
                                            attr = SourcePosition.dummy;
                                          };
                                      attr = SourcePosition.dummy;
                                    },
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
                            } );
                      attr = SourcePosition.dummy;
                    } ))
               "fun x -> fun y -> x y";
        ] );
      ( "Control flow",
        [
          test_case "if expression" `Quick
          @@ parse_test "If"
               (Front.Exp.If
                  ( {
                      body = Front.Exp.Const (Bool true);
                      attr = SourcePosition.dummy;
                    },
                    {
                      body = Front.Exp.Const (Int 1);
                      attr = SourcePosition.dummy;
                    },
                    {
                      body = Front.Exp.Const (Int 2);
                      attr = SourcePosition.dummy;
                    } ))
               "if true then 1 else 2";
          test_case "let expression" `Quick
          @@ parse_test "Let"
               (Front.Exp.Let
                  ( { body = "x"; attr = SourcePosition.dummy },
                    {
                      body = Front.Exp.Const (Int 1);
                      attr = SourcePosition.dummy;
                    },
                    {
                      body =
                        Front.Exp.Var
                          { body = "x"; attr = SourcePosition.dummy };
                      attr = SourcePosition.dummy;
                    } ))
               "let x = 1 in x";
        ] );
      ( "Delimited control",
        [
          test_case "cfun expression" `Quick
          @@ parse_test "Cfun"
               (Front.Exp.CLam
                  ( { body = "k"; attr = SourcePosition.dummy },
                    {
                      body =
                        Front.Exp.Var
                          { body = "k"; attr = SourcePosition.dummy };
                      attr = SourcePosition.dummy;
                    } ))
               "cfun k -> k";
          test_case "shift0 expression" `Quick
          @@ parse_test "Shift0"
               (Front.Exp.Shift0
                  ( { body = "k"; attr = SourcePosition.dummy },
                    {
                      body =
                        Front.Exp.Var
                          { body = "k"; attr = SourcePosition.dummy };
                      attr = SourcePosition.dummy;
                    } ))
               "shift0 k in k";
          test_case "throw expression" `Quick
          @@ parse_test "Throw"
               (Front.Exp.Throw
                  ( {
                      body =
                        Front.Exp.Var
                          { body = "e1"; attr = SourcePosition.dummy };
                      attr = SourcePosition.dummy;
                    },
                    {
                      body =
                        Front.Exp.Var
                          { body = "e2"; attr = SourcePosition.dummy };
                      attr = SourcePosition.dummy;
                    } ))
               "throw e1 to e2";
        ] );
      ( "Operators",
        [
          test_case "plus operator" `Quick
          @@ parse_test "Plus"
               (Front.Exp.OpApp
                  ( Front.Exp.OP.OP "+",
                    {
                      body = Front.Exp.Const (Int 1);
                      attr = SourcePosition.dummy;
                    },
                    {
                      body = Front.Exp.Const (Int 2);
                      attr = SourcePosition.dummy;
                    } ))
               "1 + 2";
        ] );
    ]
