open Common.All
open Format
open Aux
module Exp = Aux.Exp

module type FV = sig
  type t

  val fv : t -> AllVarList.t
end

module Substitution = struct
  module Subst = struct
    type pack = Pack : ('a Syntax.Types.Var.t * 'a Syntax.Types.t) -> pack
    type t = pack list

    let pp =
      pp_list_comma @@ fun fmt (Pack (tv, ty)) ->
      fprintf fmt "%a/%a" Aux.Types.pp ty Aux.Types.Var.pp tv

    let show x = asprintf "%a" pp x
    let id : t = []

    let subst_of : type a. a Syntax.Types.Var.t -> a Syntax.Types.t -> t =
     fun tv ty -> [ Pack (tv, ty) ]

    (* Composition *)
    let ( **. ) : t -> t -> t =
      List.fold_right @@ fun (Pack (x, y)) sbt ->
      Pack (x, y)
      :: ( flip List.map sbt @@ fun (Pack (x1, y1)) ->
           Pack (x1, Types.subst_map x y y1) )

    let compose_all : t list -> t = fun x -> List.fold_right ( **. ) x id

    let elim : AllVarList.t -> t -> t =
     fun fvt sbt ->
      List.filter (fun (Pack (tv, _)) -> not (AllVarList.mem tv fvt)) sbt

    let refresh_and_gen_subst : type a. AllVarList.t -> a Syntax.Types.t -> t =
     fun vars ty ->
      (* let () = info "debug %s" (Types.show ty) in *)
      let result =
        compose_all @@ List.concat_option
        @@ flip List.map (AllVarList.S.to_list vars)
        @@ fun (AllVarList.Pack tv) ->
        (* let () = info "debug (var) %s" (Types.Var.show tv) in *)
        match (tv, ty) with
        | Level0 tv, Level0 ty ->
            Some (subst_of (Level0 tv) @@ Types.fresh_of @@ Level0 ty)
        | Level1 tv, Level1 ty ->
            Some (subst_of (Level1 tv) @@ Types.fresh_of @@ Level1 ty)
        | Annotation tv, Annotation ty ->
            Some (subst_of (Annotation tv) @@ Types.fresh_of @@ Annotation ty)
        | Classifier tv, Classifier ty ->
            Some (subst_of (Classifier tv) @@ Types.fresh_of @@ Classifier ty)
        | _ -> None
      in
      let () =
        info "[Substitution.Subst.refresh_and_gen_subst] %s, %s ===> %s"
          (AllVarList.show vars) (Types.show ty) (show result)
      in
      result
  end

  module type S = sig
    type t

    val subst : Subst.t -> t -> t
  end

  module type S1 = sig
    type 'a t

    val subst : Subst.t -> 'a t -> 'a t
  end
end

module Types = struct
  include Aux.Types

  module type Level0 = sig
    include Types.Level0
    include FV with type t := t
    include Substitution.S with type t := t
  end

  module type Level1 = sig
    include Aux.Types.Level1
    include FV with type t := t
    include Substitution.S with type t := t
  end

  module type Classifier = sig
    include Aux.Types.Classifier
    include Substitution.S with type t := t
    include FV with type t := t
  end

  module type Annotation = sig
    include Aux.Types.Annotation
    include FV with type t := t
    include Substitution.S with type t := t
  end

  module Classifier : Classifier = struct
    include Aux.Types.Classifier

    let subst sbt ty =
      List.fold_right
        (fun (Substitution.Subst.Pack (tv, ty_sub)) ty ->
          Aux.Types.Classifier.subst_map tv ty_sub ty)
        sbt ty

    let fv cls = Aux.AllVarList.singleton (Classifier cls)
  end

  module Level1 : Level1 = struct
    open Aux
    include Aux.Types.Level1

    let rec fv : t -> AllVarList.t = function
      | Var x -> AllVarList.singleton (Level1 x)
      | Const _ -> AllVarList.empty
      | Fun (ty1, ty2) -> AllVarList.(fv ty1 @ fv ty2)

    let subst sbt ty =
      List.fold_right
        (fun (Substitution.Subst.Pack (tv, ty_sub)) ty ->
          Aux.Types.Level1.subst_map tv ty_sub ty)
        sbt ty
  end

  module rec Level0 : Level0 = struct
    include Aux.Types.Level0

    let rec fv : t -> AllVarList.t = function
      | Var x -> AllVarList.singleton (Level0 x)
      | Const _ -> AllVarList.empty
      | Fun (ty1, ty2, ann) -> AllVarList.(fv ty1 @ fv ty2 @ Annotation.fv ann)
      | ContFun (ty1, ty2, ann) ->
          AllVarList.(fv ty1 @ fv ty2 @ Annotation.fv ann)
      | Code (lv1, cls) -> AllVarList.(Level1.fv lv1 @ Classifier.fv cls)

    let subst sbt ty =
      List.fold_right
        (fun (Substitution.Subst.Pack (tv, ty_sub)) ty ->
          Aux.Types.Level0.subst_map tv ty_sub ty)
        sbt ty
  end

  and Annotation : Annotation = struct
    include Aux.Types.Annotation

    let rec fv : t -> AllVarList.t = function
      | Empty -> AllVarList.empty
      | Var x -> AllVarList.singleton (Annotation x)
      | Seq (ty0, ann) -> AllVarList.(Level0.fv ty0 @ fv ann)

    let subst sbt ty =
      List.fold_right
        (fun (Substitution.Subst.Pack (tv, ty_sub)) ty ->
          Aux.Types.Annotation.subst_map tv ty_sub ty)
        sbt ty
  end

  let fv : type a. a Syntax.Types.t -> AllVarList.t =
   fun ty ->
    match ty with
    | Level0 ty0 -> Level0.fv ty0
    | Level1 ty1 -> Level1.fv ty1
    | Classifier cls -> Classifier.fv cls
    | Annotation ann -> Annotation.fv ann

  let subst sbt ty =
    List.fold_left
      (fun ty (Substitution.Subst.Pack (tv, ty_sub)) ->
        Aux.Types.subst_map tv ty_sub ty)
      ty sbt
end

module Constraint = struct
  open Substitution
  include Constraint

  let fv_of : ('a -> AllVarList.t) -> 'a t -> AllVarList.t =
   fun f -> function
    | { body = Le (x, y); _ } -> AllVarList.(f x @ f y)
    | { body = NotIn (cls, x); _ } -> AllVarList.(Types.Classifier.fv cls @ f x)

  let subst_of : (Subst.t -> 'a -> 'a) -> Subst.t -> 'a t -> 'a t =
   fun f sbt -> function
    | { body = Le (x, y); attr } -> { body = Le (f sbt x, f sbt y); attr }
    | { body = NotIn (cls, x); attr } -> { body = NotIn (cls, f sbt x); attr }
end

module Assumption = struct
  include Assumption

  let fv l =
    List.fold_left
      (fun acc (cls1, cls2) ->
        AllVarList.(Types.Classifier.fv cls1 @ Types.Classifier.fv cls2 @ acc))
      AllVarList.empty l

  let subst sbt l =
    List.map
      (fun (cls1, cls2) ->
        (Types.Classifier.subst sbt cls1, Types.Classifier.subst sbt cls2))
      l
end

module AllVarList = struct
  include AllVarList
  open Substitution

  let refresh : t -> t * Subst.t =
   fun t ->
    S.fold
      (fun (Pack v) (acc_vars, acc_sbt) ->
        let v' = Aux.Types.Var.fresh_of v in
        let sbt = Subst.subst_of v @@ Aux.Types.varof v' in
        (v' @:: acc_vars, Subst.(sbt **. acc_sbt)))
      t (empty, Subst.id)
end

module AllConstraintList = struct
  include AllConstraintList

  let fv l =
    List.fold_left
      (fun acc (Pack x) -> AllVarList.(acc @ Constraint.fv_of Types.fv x))
      AllVarList.empty l

  let subst sbt l =
    List.map (fun (Pack c) -> Pack (Constraint.subst_of Types.subst sbt c)) l
end

module TypeScheme = struct
  include TypeScheme
  open Substitution

  let fv (t : t) : AllVarList.t =
    AllVarList.diff
      AllVarList.(
        Types.Level0.fv t.ty
        @ Assumption.fv t.assumptions
        @ AllConstraintList.fv t.all_constraints)
      t.bound_vars

  let subst sbt (t : t) : t =
    let sbt = Subst.elim t.bound_vars sbt in
    {
      bound_vars = t.bound_vars;
      assumptions = Assumption.subst sbt t.assumptions;
      all_constraints = AllConstraintList.subst sbt t.all_constraints;
      ty = Types.Level0.subst sbt t.ty;
    }
end
