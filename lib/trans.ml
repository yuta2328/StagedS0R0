open Common.All

module Const = struct
  type f = Front.Exp.Const.t
  type t = Syntax.Exp.Const.t

  let trans : f -> t = function
    | Int n -> Int n
    | Bool b -> Bool b
    | String s -> String s
end

module Var = struct
  type f = Front.Exp.Var.t
  type t = Syntax.Exp.Level0.Var.t

  let trans : f -> t = fun x -> Var x.body
end

module OP = struct
  type f = Front.Exp.OP.t
  type t = Syntax.Exp.Level0.Var.t

  let trans : f -> t = fun (OP x) -> Var x
end

module Exp = struct
  type f = Front.Exp.t
  type t = Syntax.Exp.Level0.t

  let rec trans : f -> t =
    fun e ->
    let open Annotated in
    match e.body with
    | Var x -> { body = Var (Var.trans x); attr = e.attr }
    | Const c -> { body = Const (Const.trans c); attr = e.attr }
    | Lam (x, e) -> { body = Lam (Var.trans x, trans e); attr = e.attr }
    | OpApp (op, e1, e2) ->
      {
        body =
          App
            ( {
              body =
                App
                  ( { body = Var (OP.trans op); attr = SourcePosition.dummy },
                    trans e1 );
              attr = e.attr;
            },
              trans e2 );
        attr = e.attr;
      }
    | App (e1, e2) -> { body = App (trans e1, trans e2); attr = e.attr }
    | If (e1, e2, e3) ->
      { body = If (trans e1, trans e2, trans e3); attr = e.attr }
    | Let (x, e1, e2) ->
      { body = Let (Var.trans x, trans e1, trans e2); attr = e.attr }
    | CLet (x, e1, e2) ->
      { body =
          App ({
            body =
              Syntax.Exp.Level0.App
                ({ body = Var (Var "capp"); attr = SourcePosition.dummy }, {
                      body = Syntax.Exp.Level0.CLam (Var.trans x, trans e2);
                      attr = e2.attr;
                    });
            attr = e2.attr;
          }, trans e1); attr = e1.attr }
    | CLam (x, e) -> { body = CLam (Var.trans x, trans e); attr = e.attr }
    | Reset0 e -> { body = Reset0 (trans e); attr = e.attr }
    | Shift0 (x, e) -> { body = Shift0 (Var.trans x, trans e); attr = e.attr }
    | Throw (e1, e2) -> { body = Throw (trans e1, trans e2); attr = e.attr }
end

module Statement = struct
  open Annotated

  type f = Front.Statement.t
  type t = Syntax.Exp.Level0.t

  let trans : f -> t =
   fun l ->
    match List.rev l with
    | [] -> failwith "Empty statement list"
    | (_, e) :: rest ->
        List.fold_left
          (fun e_acc (x, e) ->
            {
              body = Syntax.Exp.Level0.Let (Var.trans x, Exp.trans e, e_acc);
              attr = SourcePosition.dummy;
            })
          (Exp.trans e) rest
end
