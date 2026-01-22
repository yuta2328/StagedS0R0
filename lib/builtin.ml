open Aux
open Eval

module Env = struct
  include Aux.Env

  let cls0 = Types.Classifier.Var.fresh ()
  let eff0 = Types.Annotation.Var.fresh ()
  let eff1 = Types.Annotation.Var.fresh ()
  let level1_1 = Types.Level1.Var.fresh ()
  let level1_2 = Types.Level1.Var.fresh ()
  let exp_var0 = Exp.Level0.Var.Var "x"

  let list : t =
    [
      ( Var "cint",
        {
          ty =
            {
              bound_vars =
                AllVarList.(
                  singleton (Types.Var.Classifier cls0)
                  @ singleton (Types.Var.Annotation eff0));
              assumptions = Assumption.empty;
              all_constraints = AllConstraintList.empty;
              ty =
                Types.(
                  Fun
                    ( Const Types.Const.Int,
                      Code (Const Types.Const.Int, Classifier.varof cls0),
                      Annotation.varof eff0 ));
            };
          value =
            BuiltIn
              (fun value ->
                match value with
                | Const (Int n) -> Bracket (Const (Int n))
                | _ -> failwith "not int value");
        } );
      ( Var "capp",
        {
          ty =
            {
              bound_vars =
                AllVarList.(
                  singleton (Types.Var.Classifier cls0)
                  @ singleton (Types.Var.Annotation eff0)
                  @ singleton (Types.Var.Annotation eff1)
                  @ singleton (Types.Var.Level1 level1_1)
                  @ singleton (Types.Var.Level1 level1_2));
              assumptions = Assumption.empty;
              all_constraints = AllConstraintList.empty;
              ty =
                Types.(
                  Fun
                    ( Code
                            ( Fun (Level1.varof level1_1, Level1.varof level1_2),
                              Classifier.varof cls0 ),
                      Fun
                        ( Code (Level1.varof level1_1, cls0),
                          Code (Level1.varof level1_2, Classifier.varof cls0),
                          Annotation.varof eff0
                        ),
                      Annotation.varof eff1 ));
            };
          value =
            BuiltIn
              (fun value1 ->
                BuiltIn
                  (fun value2 ->
                    match (value1, value2) with
                    | Bracket e1, Bracket e2 -> Bracket (App (e1, e2))
                    | _ -> failwith "(capp)"));
        } );
    ]
end
