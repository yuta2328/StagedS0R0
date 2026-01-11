open Common.All

module Exp = struct
  module Var = struct
    type t = (string, SourcePosition.t) Annotated.t [@@deriving show]
  end

  module Const = struct
    type t = Int of int | Bool of bool | String of string [@@deriving show]
  end

  module OP = struct
    type t = OP of string [@@deriving show]
  end

  type t = (_exp, SourcePosition.t) Annotated.t [@@deriving show]

  and _exp =
    | Var of Var.t
    | Const of Const.t
    | Lam of Var.t * t
    | OpApp of OP.t * t * t
    | App of t * t
    | If of t * t * t
    | Let of Var.t * t * t
    | CLam of Var.t * t
    | Reset0 of t
    | Shift0 of Var.t * t
    | Throw of t * t

  let rec equal : t -> t -> bool =
   fun a b ->
    match (a.body, b.body) with
    | Var x, Var y -> x.body = y.body
    | Const x, Const y -> x = y
    | Lam (x1, e1), Lam (x2, e2) -> x1.body = x2.body && equal e1 e2
    | OpApp (op1, e1a, e1b), OpApp (op2, e2a, e2b) ->
        op1 = op2 && equal e1a e2a && equal e1b e2b
    | App (e1a, e1b), App (e2a, e2b) -> equal e1a e2a && equal e1b e2b
    | If (e1c, e1t, e1f), If (e2c, e2t, e2f) ->
        equal e1c e2c && equal e1t e2t && equal e1f e2f
    | Let (x1, e1v, e1b), Let (x2, e2v, e2b) ->
        x1.body = x2.body && equal e1v e2v && equal e1b e2b
    | CLam (x1, e1), CLam (x2, e2) -> x1.body = x2.body && equal e1 e2
    | Reset0 e1, Reset0 e2 -> equal e1 e2
    | Shift0 (x1, e1), Shift0 (x2, e2) -> x1.body = x2.body && equal e1 e2
    | Throw (e1a, e1b), Throw (e2a, e2b) -> equal e1a e2a && equal e1b e2b
    | _, _ -> false
end

module Statement = struct
  type t = (Exp.Var.t * Exp.t) list
end
