open Common.All
open Format
open Subst

module Exp = struct
  include Exp

  module Level0 = struct
    include Level0

    let is_value : Level0.t -> bool =
     fun e ->
      match e.body with
      | Var _ -> true
      | Const _ -> true
      | Lam _ -> true
      | ContLam _ -> true
      | Bracket _ -> true
      | _ -> false
  end
end

open Exp

module ValueEnv = struct
  type t = (Level0.Var.t * Value.t) list [@@deriving show]

  let empty = []

  let extend env x v =
    info "[eval]: Extending the env %s with %s -> %s" (show env)
      (Level0.Var.show x) (Value.show v);
    (x, v) :: env

  let lookup x env =
    info "[eval]: Looking up the variable %s from the env %s"
      (Level0.Var.show x) (show env);
    List.assoc_opt x env
end

module EvaluationContext = struct
  type t =
    | Hole
    | AppLeft of t * Level0.t
    | AppRight of Value.t * t
    | IfZeroLeft of t * Level0.t * Level0.t
    | ULam of Level1.Var.t * t
    | LetComp of Level0.Var.t * t * Level0.t

  let rec pp fmt = function
    | Hole -> fprintf fmt "[]"
    | AppLeft (ctx, e2) -> fprintf fmt "@[<hov 1>(%a %a)@]" pp ctx Level0.pp e2
    | AppRight (v1, ctx) ->
        fprintf fmt "@[<hov 1>(%s %a)@]" (Value.show v1) pp ctx
    | IfZeroLeft (ctx, e2, e3) ->
        fprintf fmt "@[<hov 1>(if %a then %a else %a)@]" pp ctx Level0.pp e2
          Level0.pp e3
    | ULam (x, ctx) ->
        fprintf fmt "@[<hov 1>(fun %s -> %a)@]" (Level1.Var.show x) pp ctx
    | LetComp (x, ctx, e2) ->
        fprintf fmt "@[<hov 1>(let %a = %a in %a)@]" Level0.Var.pp x pp ctx
          Level0.pp e2

  let show ctx = Format.asprintf "%a" pp ctx

  let rec fill ctx e =
    info
      "[EvaluationContext.fill]: Filling the context %s with the expression %s"
      (show ctx) (Level0.show e);
    match ctx with
    | Hole -> e
    | AppLeft (ctx', e2) -> Exp.Level0.return @@ Level0.App (fill ctx' e, e2)
    | AppRight (v1, ctx') ->
        Exp.Level0.return @@ Level0.App (Value.embed v1, fill ctx' e)
    | IfZeroLeft (ctx', e2, e3) ->
        Exp.Level0.return @@ Level0.If (fill ctx' e, e2, e3)
    | ULam (x, ctx') -> Exp.Level0.return @@ Level0.ULam (x, fill ctx' e)
    | LetComp (x, ctx', e2) ->
        Exp.Level0.return @@ Level0.Let (x, fill ctx' e, e2)

  let from_exp : Level0.t -> t * Level0.t =
   fun e ->
    let rec aux (e : Level0.t) : t * Level0.t =
      match e.body with
      | App (e1, e2) when Level0.is_value e1 ->
          let ctx, e2' = aux e2 in
          (AppRight (Value.cast e1, ctx), e2')
      | App (e1, e2) ->
          let ctx, e1' = aux e1 in
          (AppLeft (ctx, e2), e1')
      | If (e1, _, _) when Level0.is_value e1 -> (Hole, e)
      | If (e1, e2, e3) ->
          let ctx, e1' = aux e1 in
          (IfZeroLeft (ctx, e2, e3), e1')
      | ULam (x, body) when Level0.is_value body ->
          let ctx, body' = aux body in
          (ULam (x, ctx), body')
      | Let (_, e1, _) when Level0.is_value e1 -> (Hole, e)
      | Let (x, e1, e2) ->
          let ctx, e1' = aux e1 in
          (LetComp (x, ctx, e2), e1')
      | _ -> (Hole, e)
    in
    let () =
      info
        "[EvaluationContext.from_exp]: Converting the expression %s into an \
         evaluation context"
        (Level0.show e)
    in
    aux e
end

module Redex = struct
  type t =
    | Beta of Level0.Var.t * Level0.t * Value.t (* (fun x -> M) V *)
    | IfTrue of Level0.t * Level0.t (* if true then M2 else M3 *)
    | IfFalse of Level0.t * Level0.t (* if false then M2 else M3 *)
    | Throw of Level0.Var.t * Level0.t * Value.t (* throw (cont x -> M) V *)
    | Let of Level0.Var.t * Value.t * Level0.t (* let x = V in M *)
    | CLam of Level0.Var.t * Level0.t (* cfun x -> M *)
    | ULam of Level1.Var.t * Level1.t (* ufun u -> <M> *)
    | ResetValue of Value.t (* reset0 <M> *)
    | Cont of
        EvaluationContext.t * Level0.Var.t * Level0.t (* {E[shift0 k. M]} *)
    | Lift of EvaluationContext.t * t
    | Value of Value.t (* Normal Form *)

  let rec pp fmt = function
    | Beta (x, e, v) ->
        fprintf fmt "E-Beta: (fun %a -> %a) %a" Level0.Var.pp x Level0.pp e
          Value.pp v
    | IfTrue (e2, e3) ->
        fprintf fmt "E-IfTrue: if true then %a else %a" Level0.pp e2 Level0.pp
          e3
    | IfFalse (e2, e3) ->
        fprintf fmt "E-IfFalse: if false then %a else %a" Level0.pp e2 Level0.pp
          e3
    | Throw (k, e, v) ->
        fprintf fmt "E-Throw: throw (cont %a -> %a) %s" Level0.Var.pp k
          Level0.pp e (Value.show v)
    | Let (x, v, e) ->
        fprintf fmt "E-Let: let %a = %s in %a" Level0.Var.pp x (Value.show v)
          Level0.pp e
    | CLam (x, e) ->
        fprintf fmt "E-CAbs: cfun %a -> %a" Level0.Var.pp x Level0.pp e
    | ULam (u, e) ->
        fprintf fmt "E-UAbs: ufun %a -> %a" Level1.Var.pp u Level1.pp e
    | ResetValue v -> fprintf fmt "E-ResetValue: {%a}" Value.pp v
    | Cont (ctx, k, e) ->
        fprintf fmt "E-Cont: { %s [shift0 %a. %a] }"
          (EvaluationContext.show ctx)
          Level0.Var.pp k Level0.pp e
    | Lift (ctx, r) ->
        fprintf fmt "E-Lift: lift %a with %a" EvaluationContext.pp ctx pp r
    | Value v -> fprintf fmt "Normal Form: %s" (Value.show v)

  let show r = Format.asprintf "%a" pp r

  let rec embed : Level0.t -> t =
   fun e ->
    let () =
      info "[eval]: Embedding the expression %s into a redex" (Level0.show e)
    in
    match EvaluationContext.from_exp e with
    | Hole, e when Exp.Level0.is_value e -> Value (Value.cast e)
    | Hole, { body = App ({ body = Lam (x, e1); _ }, e2); _ }
      when Exp.Level0.is_value e2 ->
        Beta (x, e1, Value.cast e2)
    | Hole, { body = If ({ body = Const (Const.Bool true); _ }, e2, e3); _ } ->
        IfTrue (e2, e3)
    | Hole, { body = If ({ body = Const (Const.Bool false); _ }, e2, e3); _ } ->
        IfFalse (e2, e3)
    | Hole, { body = Throw ({ body = ContLam (k, e1); _ }, e2); _ }
      when Exp.Level0.is_value e2 ->
        Throw (k, e1, Value.cast e2)
    | Hole, { body = Let (x, e1, e2); _ } when Exp.Level0.is_value e1 ->
        Let (x, Value.cast e1, e2)
    | Hole, { body = CLam (x, e1); _ } -> CLam (x, e1)
    | Hole, { body = ULam (u, { body = Bracket e1; _ }); _ } -> ULam (u, e1)
    | Hole, { body = Reset0 e1; _ } when Exp.Level0.is_value e1 ->
        ResetValue (Value.cast e1)
    | Hole, { body = Reset0 e1; _ } -> (
        let ctx, e1' = EvaluationContext.from_exp e1 in
        match e1' with
        | { body = Shift0 (k, e2); _ } -> Cont (ctx, k, e2)
        | _ -> failwith "[Redex.embed]: Unexpected expression inside reset0")
    | Hole, _ -> failwith "[Redex.embed]: Unexpected expression"
    | ctx, e1 ->
        let r = embed e1 in
        Lift (ctx, r)
end

module RuntimeError = struct
  type t = Msg of string [@@deriving show]
end

exception Error of RuntimeError.t

let eval env e =
  let rec aux : Redex.t -> Value.t = function
    | Value (Var x) -> (
        match flip ValueEnv.lookup env x with
        | Some v -> v
        | None ->
            failwith
            @@ sprintf "[eval]: Unbound variable: %s" (Level0.Var.show x))
    | Value v -> v
    | Beta (x, e1, v2) ->
        let e1' = Level0.subst x (Value.embed v2) e1 in
        info "[eval]: Performing beta-reduction: substituting %s for %s in %s"
          (Value.show v2) (Level0.Var.show x) (Level0.show e1);
        aux @@ Redex.embed e1'
    | IfTrue (e2, _) -> aux @@ Redex.embed e2
    | IfFalse (_, e3) -> aux @@ Redex.embed e3
    | Throw (k, e1, v2) ->
        let e1' = Level0.subst k (Value.embed v2) e1 in
        info "[eval]: Performing throw: substituting %s for %s in %s"
          (Value.show v2) (Level0.Var.show k) (Level0.show e1);
        aux @@ Redex.embed e1'
    | Let (x, v1, e2) ->
        let e2' = Level0.subst x (Value.embed v1) e2 in
        info "[eval]: Performing let-binding: substituting %s for %s in %s"
          (Value.show v1) (Level0.Var.show x) (Level0.show e2);
        aux @@ Redex.embed e2'
    | CLam (x, e1) ->
        (* let u = Level1.Var.fresh () in *)
        (* let v = Value.Bracket (Level1.Var u) in *)
        (* aux @@ Redex.embed @@ Level0.return *)
        (* @@ ULam (u, Level0.subst x (Value.embed v) e1) *)
        failwith "TODO fix"
    | ULam (u, e1) -> Bracket (Lam (u, e1))
    | ResetValue v -> v
    | Cont (ctx, k, e1) ->
        let x = Level0.Var.fresh () in
        aux @@ Redex.embed
        @@ Exp.Level0.subst k
             (Value.embed
             @@ Value.ContLam
                  ( x,
                    Level0.return
                    @@ Level0.Reset0
                         (EvaluationContext.fill ctx @@ Level0.return
                        @@ Level0.Var x) ))
             e1
    | Lift (ctx, r) ->
        let v = aux r in
        aux @@ Redex.embed @@ EvaluationContext.fill ctx (Value.embed v)
  in
  try Result.ok @@ aux @@ Redex.embed e
  with Failure err -> Result.error @@ RuntimeError.Msg err
