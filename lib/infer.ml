open Common.All
open Format
open Subst

module TypeContext = struct
  type t = {
    context : (Exp.Level0.Var.t * Types.Level0.t) list;
    constraints : Assumption.t;
  }

  let pp fmt t =
    fprintf fmt "@[<hov 1>%a,@ %a@]"
      ( pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
      @@ fun fmt (var, ty) ->
        fprintf fmt "%a : %a" Exp.Level0.Var.pp var Types.Level0.pp ty )
      (List.rev t.context) Assumption.pp (List.rev t.constraints)

  let show t = asprintf "%a" pp t
  let empty = { context = []; constraints = [] }

  let add var ty t : t =
    let t = { t with context = (var, ty) :: t.context } in
    info "[Infer.TypeContext.add] %s" @@ show t;
    t

  let add_cstr var1 var2 t : t =
    let t = { t with constraints = (var1, var2) :: t.constraints } in
    info "[Infer.TypeContext.add_cstr] %s" @@ show t;
    t

  let lookup var t : Types.Level0.t option =
    info "[Infer.TypeContext.lookup] Looking up %s in %s"
      (Exp.Level0.Var.show var) (show t);
    List.assoc_opt var t.context

  let to_assumption : t -> Assumption.t = fun t -> t.constraints

  let fv : t -> AllVarList.t =
   fun t ->
    let open AllVarList in
    let vars =
      List.fold_left ( @ ) empty @@ List.map (Types.Level0.fv **. snd) t.context
    in
    let cls_vars =
      List.fold_left ( @ ) empty
      @@ flip List.map t.constraints
      @@ fun (c1, c2) -> Classifier c1 @:: Classifier c2 @:: empty
    in
    vars @ cls_vars

  let subst : Substitution.Subst.t -> t -> t =
   fun sbt t ->
    {
      context =
        List.map (fun (var, ty) -> (var, Types.Level0.subst sbt ty)) t.context;
      constraints = Assumption.subst sbt t.constraints;
    }
end

module PolyContext = struct
  type t = (Exp.Level0.Var.t * TypeScheme.t) list

  let from_list l = l

  let pp fmt t =
    fprintf fmt "@[<hov 1>%a@]"
      ( pp_list_comma @@ fun fmt (var, scheme) ->
        fprintf fmt "%a : %a" Exp.Level0.Var.pp var TypeScheme.pp scheme )
      t

  let show t = asprintf "%a" pp t
  let empty : t = []

  let add t var scheme : t =
    info "[Infer.PolyContext.add] %s"
    @@ asprintf "%a, %a :: %a" Exp.Level0.Var.pp var TypeScheme.pp scheme pp t;
    (var, scheme) :: t

  let lookup var t : TypeScheme.t option =
    info "[Infer.PolyContext.lookup] Looking up %s in %s"
      (Exp.Level0.Var.show var) (show t);
    List.assoc_opt var t
end

module ConstraintGraph = struct
  module type Elem = sig
    type t = Pack : 'a Syntax.Types.Var.t Constraint.t -> t

    include Show.S with type t := t
    include Set.I with type t := t
  end

  module Elem = struct
    module PackVar = struct
      type t = Pack : 'a Syntax.Types.Var.t -> t
    end

    type t = Pack : 'a Syntax.Types.Var.t Constraint.t -> t

    let pp fmt (Pack cstr) =
      fprintf fmt "%a" (Constraint.pp_of Types.Var.pp) cstr

    let show x = asprintf "%a" pp x
    let compare p1 p2 = compare p1 p2
    let ( = ) p1 p2 = compare p1 p2 = 0

    let less_eq : type a. a Syntax.Types.Var.t -> t -> bool =
     fun var (Pack cstr) ->
      match cstr with
      | { body = Constraint.Le (v1, _); _ } -> PackVar.(Pack var = Pack v1)
      | _ -> false

    let gret_eq : type a. a Syntax.Types.Var.t -> t -> bool =
     fun var (Pack cstr) ->
      match cstr with
      | { body = Constraint.Le (_, v2); _ } -> PackVar.(Pack var = Pack v2)
      | _ -> false

    let less_nonrec : type a. a Syntax.Types.Var.t -> t -> bool =
     fun var (Pack cstr) ->
      match cstr with
      | { body = Constraint.NotIn (_, v2); _ } -> PackVar.(Pack var = Pack v2)
      | _ -> false

    let safe_fst : t -> AllVarList.t =
     fun (Pack cstr) ->
      match cstr with
      | { body = Constraint.Le (v1, _); _ } -> AllVarList.singleton v1
      | _ -> AllVarList.empty

    let safe_snd : t -> AllVarList.t =
     fun (Pack cstr) ->
      match cstr with
      | { body = Constraint.Le (_, v2); _ } -> AllVarList.singleton v2
      | _ -> AllVarList.empty

    let prod : AllVarList.t -> AllVarList.t -> t list =
     fun tv1 tv2 ->
      let open Constraint in
      let result =
        List.concat_option @@ List.concat
        @@ flip List.map (AllVarList.S.to_list tv1)
        @@ fun (Pack v1) ->
        flip List.map (AllVarList.S.to_list tv2) @@ fun (Pack v2) ->
        match (v1, v2) with
        | Level0 v1, Level0 v2 ->
            Some (Pack (Types.Var.Level0 v1 *<= Level0 v2))
        | Level1 v1, Level1 v2 ->
            Some (Pack (Types.Var.Level1 v1 *<= Level1 v2))
        | Classifier v1, Classifier v2 ->
            Some (Pack (Types.Var.Classifier v1 *<= Classifier v2))
        | Annotation v1, Annotation v2 ->
            Some (Pack (Types.Var.Annotation v1 *<= Annotation v2))
        | _ -> None
      in
      let () =
        info "[Infer.ConstraintGraph.Elem.prod] %s, %s ===> %s"
          (AllVarList.show tv1) (AllVarList.show tv2)
        @@ asprintf "%a" (pp_list pp) result
      in
      result

    let non_related : Types.Classifier.t -> AllVarList.t -> t list =
     fun cls tv ->
      let open Constraint in
      flip List.map (AllVarList.S.to_list tv) @@ fun (Pack v) -> Pack (cls */= v)
  end

  module S : Set.S with type elt = Elem.t = Set.Make (Elem)

  type t = { assumption : Assumption.t; graph : S.t }

  let pp fmt t =
    fprintf fmt "@[<hov 1>Assumptions: %a@ Constraints: %a@]" Assumption.pp
      t.assumption S.pp t.graph

  let show t = asprintf "%a" pp t

  let init tycxt =
    { assumption = tycxt.TypeContext.constraints; graph = S.empty }

  let filter_lesseq : type a. t -> a Syntax.Types.Var.t -> AllVarList.t =
   fun t var ->
    List.fold_left AllVarList.( @ ) (AllVarList.singleton var)
    @@ List.map Elem.safe_snd @@ S.to_list
    @@ S.filter (Elem.less_eq var) t.graph

  let filter_greteq : type a. t -> a Syntax.Types.Var.t -> AllVarList.t =
   fun t var ->
    List.fold_left AllVarList.( @ ) (AllVarList.singleton var)
    @@ List.map Elem.safe_fst @@ S.to_list
    @@ S.filter (Elem.gret_eq var) t.graph

  let filter_less_nonrec : type a. t -> a Syntax.Types.Var.t -> AllVarList.t =
   fun t var ->
    List.fold_left AllVarList.( @ ) (AllVarList.singleton var)
    @@ List.map Elem.safe_snd @@ S.to_list
    @@ S.filter (Elem.less_nonrec var) t.graph

  let filter_sim : type a. t -> a Syntax.Types.Var.t -> AllVarList.t =
   fun t var ->
    let vars = AllVarList.(filter_lesseq t var @ filter_greteq t var) in
    AllVarList.S.fold
      (fun (Pack var) vars ->
        AllVarList.(filter_lesseq t var @ filter_greteq t var @ vars))
      vars AllVarList.empty

  let add : type a. a Syntax.Types.Var.t Constraint.t -> t -> t =
   fun cstr t ->
    match cstr with
    | { body = Le (v1, v2); _ } ->
        let less = AllVarList.(v1 @:: filter_greteq t v1) in
        let gret = AllVarList.(v2 @:: filter_lesseq t v2) in
        let result =
          { t with graph = List.fold_right S.add (Elem.prod less gret) t.graph }
        in
        let () =
          info "[Infer.ConstraintGraph.add] Adding %s: result = %s"
            (asprintf "%a <= %a" Aux.Types.Var.pp v1 Aux.Types.Var.pp v2)
            (show result)
        in
        result
    | { body = NotIn (cls, v); _ } ->
        let nonrecv = AllVarList.(v @:: filter_less_nonrec t v) in
        let result =
          {
            t with
            graph = List.fold_right S.add (Elem.non_related cls nonrecv) t.graph;
          }
        in
        let () =
          info "[Infer.ConstraintGraph.add] Adding %s: result = %s"
            (asprintf "%a ⋢ %a" Types.Classifier.Var.pp cls Aux.Types.Var.pp v)
            (show result)
        in
        result

  let contains : type a. t -> a Syntax.Types.Var.t Constraint.t -> bool =
   fun t cstr -> S.mem (Elem.Pack cstr) t.graph

  let get_sim : type a. t -> a Syntax.Types.Var.t -> t =
   fun t var ->
    {
      t with
      graph =
        S.filter
          (fun cstr ->
            Elem.less_eq var cstr || Elem.gret_eq var cstr
            || Elem.less_nonrec var cstr)
          t.graph;
    }

  let elim_sim : type a. a Syntax.Types.Var.t -> t -> t =
   fun var t ->
    let () =
      info "[Infer.ConstraintGraph.elim_sim] Eliminating variable: %s"
      @@ asprintf "%a" Aux.Types.Var.pp var
    in
    {
      t with
      graph =
        S.filter
          (fun cstr ->
            not
              (Elem.less_eq var cstr || Elem.gret_eq var cstr
             || Elem.less_nonrec var cstr))
          t.graph;
    }

  let occurs_check : type a.
      t -> a Syntax.Types.Var.t -> a Syntax.Types.t -> bool =
   fun t var ty ->
    let vars = filter_sim t var in
    let result = not @@ AllVarList.is_disjoint vars @@ Types.fv ty in
    let () =
      info
        "[Infer.ConstraintGraph.occurs_check] Occurs check: {%s} (= {v| v ~ \
         %s} ) ⊄ FV(%s) === %b"
        (AllVarList.show vars) (Types.Var.show var) (Types.show ty) result
    in
    result

  let subst_var =
   fun sbt t ->
    let open Constraint in
    flip List.map (S.to_list t.graph) @@ fun (Elem.Pack cstr) ->
    match cstr with
    | { body = Constraint.Le (v1, v2); _ } ->
        AllConstraintList.Pack
          (Subst.Types.subst sbt (Aux.Types.varof v1)
          *<= Subst.Types.subst sbt (Aux.Types.varof v2))
    | { body = Constraint.NotIn (cls, v); _ } ->
        AllConstraintList.Pack
          (Types.Classifier.subst sbt cls
          */= Subst.Types.subst sbt (Aux.Types.varof v))
end

(* Constraint generation *)

module CGenInput = struct
  type t = {
    ty_cxt : TypeContext.t;
    poly_cxt : PolyContext.t;
    exp : Exp.Level0.t;
  }

  let pp fmt input =
    fprintf fmt "@[<hov 1>%a@ ;@ %a@ |-@ %a@]" TypeContext.pp input.ty_cxt
      PolyContext.pp input.poly_cxt Exp.Level0.pp input.exp

  let show = asprintf "%a" pp

  let init exp =
    { ty_cxt = TypeContext.empty; poly_cxt = PolyContext.empty; exp }
end

module CGenOutput = struct
  type t = {
    ty : Types.Level0.t;
    annotation : Types.Annotation.t;
    all_vars : AllVarList.t;
    assumptions : Assumption.t;
    constraints : AllConstraintList.t;
  }

  let pp fmt t =
    fprintf fmt "@[<hov 1>%a@ ;@ %a@ |_(%a)@ %a@ =>@ %a @]" Types.Level0.pp t.ty
      Types.Annotation.pp t.annotation AllVarList.pp t.all_vars Assumption.pp
      t.assumptions AllConstraintList.pp t.constraints

  let show t = asprintf "%a" pp t

  let ann_default ty all_vars assumptions constraints =
    let ann_var = Types.Annotation.Var.fresh () in
    let annotation = Types.Annotation.varof ann_var in
    {
      ty;
      annotation;
      all_vars = AllVarList.(Annotation ann_var @:: all_vars);
      assumptions;
      constraints;
    }

  let bottom_of (input : CGenInput.t) ty =
    ann_default ty AllVarList.empty
      (TypeContext.to_assumption input.ty_cxt)
      AllConstraintList.empty
end

module InferError = struct
  type t =
    | UnboundVariable of Exp.Level0.Var.t * SourcePosition.t
    | UnificationErrorLevel0 of
        Types.Level0.t * Types.Level0.t * SourcePosition.t
    | UnificationErrorLevel1 of
        Types.Level1.t * Types.Level1.t * SourcePosition.t
    | UnificationErrorClassifier of
        Types.Classifier.t * Types.Classifier.t * SourcePosition.t
    | UnificationErrorAnnotation of
        Types.Annotation.t * Types.Annotation.t * SourcePosition.t
    | ScopeExtrusionError of
        Types.Classifier.t * Types.Classifier.t * SourcePosition.t
    | AmbiguousVarError of Exp.Level0.Var.t * SourcePosition.t
    | NotFrontTerm of Exp.Level0.t * SourcePosition.t

  let pp fmt = function
    | UnboundVariable (v, pos) ->
        fprintf fmt "Unbound variable: %a\n%a" Exp.Level0.Var.pp v
          SourcePosition.pp pos
    | UnificationErrorLevel0 (ty1, ty2, pos) ->
        fprintf fmt "Unification error (Level0): %a and %a\n%a" Types.Level0.pp
          ty1 Types.Level0.pp ty2 SourcePosition.pp pos
    | UnificationErrorLevel1 (ty1, ty2, pos) ->
        fprintf fmt "Unification error (Level1): %a and %a\n%a" Types.Level1.pp
          ty1 Types.Level1.pp ty2 SourcePosition.pp pos
    | UnificationErrorClassifier (cls1, cls2, pos) ->
        fprintf fmt "Scope extrusion error: %a and %a\n%a" Types.Classifier.pp
          cls1 Types.Classifier.pp cls2 SourcePosition.pp pos
    | UnificationErrorAnnotation (ann1, ann2, pos) ->
        fprintf fmt "Unification error (Annotation): %a and %a\n%a"
          Types.Annotation.pp ann1 Types.Annotation.pp ann2 SourcePosition.pp
          pos
    | ScopeExtrusionError (cls1, cls2, pos) ->
        fprintf fmt "Scope extrusion error: %a and %a\n%a" Types.Classifier.pp
          cls1 Types.Classifier.pp cls2 SourcePosition.pp pos
    | AmbiguousVarError (v, pos) ->
        fprintf fmt "Ambiguous variable: %a\n%a" Exp.Level0.Var.pp v
          SourcePosition.pp pos
    | NotFrontTerm (e, pos) ->
        fprintf fmt "Not a front term: %a\n%a" Exp.Level0.pp e SourcePosition.pp
          pos

  let show e = asprintf "%a" pp e
end

exception InferError of InferError.t

module TypeScheme = struct
  include TypeScheme

  let inst :
      t -> Types.Level0.t * Assumption.t * AllConstraintList.t * AllVarList.t =
   fun scheme ->
    let vars, sbt = AllVarList.refresh scheme.bound_vars in
    ( Types.Level0.subst sbt scheme.ty,
      Assumption.subst sbt scheme.assumptions,
      AllConstraintList.subst sbt scheme.all_constraints,
      vars )

  let generalize : CGenOutput.t -> TypeScheme.t =
   fun output ->
    {
      bound_vars = output.all_vars;
      assumptions = output.assumptions;
      all_constraints = output.constraints;
      ty = output.ty;
    }
end

module CGenDeriv = struct
  type t = { input : CGenInput.t; result : CGenOutput.t }

  let pp fmt deriv =
    fprintf fmt "@[<hov 1>%a@ ===>@ %a@]" CGenInput.pp deriv.input CGenOutput.pp
      deriv.result

  let show = asprintf "%a" pp

  let trace input output =
    let () =
      info "[Infer.CGenDeriv.trace] %s"
      @@ asprintf "%a" pp { input; result = output }
    in
    output
end

let cgen x =
  let rec aux (x : CGenInput.t) : CGenOutput.t =
    let pos_opt = x.exp.attr in
    let open CGenInput in
    let open AllConstraintList in
    let open Constraint in
    let open Syntax.Types in
    CGenDeriv.trace x
    @@
    match x.exp.body with
    | Bracket _ | ContLam _ | ULam _ ->
        raise (InferError (NotFrontTerm (x.exp, pos_opt)))
    | Const c -> CGenOutput.bottom_of x @@ Const (Exp.Const.ty c)
    | Var v -> (
        match
          (PolyContext.lookup v x.poly_cxt, TypeContext.lookup v x.ty_cxt)
        with
        | Some scheme, None ->
            let ty, assumptions, constraints, all_vars =
              TypeScheme.inst scheme
            in
            let ann_var = Types.Annotation.Var.fresh () in
            CGenOutput.
              {
                ty;
                annotation = Types.Annotation.varof ann_var;
                all_vars = AllVarList.(Annotation ann_var @:: all_vars);
                assumptions;
                constraints;
              }
        | None, Some ty -> CGenOutput.bottom_of x ty
        | Some _, Some _ -> raise (InferError (AmbiguousVarError (v, pos_opt)))
        | None, None -> raise (InferError (UnboundVariable (v, pos_opt))))
    | Lam (v, e1) ->
        let level0_var = Types.Level0.Var.fresh () in
        let level0 = Types.Level0.varof level0_var in
        let output1 =
          aux
            {
              ty_cxt = TypeContext.add v level0 @@ x.ty_cxt;
              poly_cxt = x.poly_cxt;
              exp = e1;
            }
        in
        CGenOutput.ann_default
          (Fun (level0, output1.ty, output1.annotation))
          AllVarList.(Level0 level0_var @:: output1.all_vars)
          output1.assumptions output1.constraints
    | App (e1, e2) ->
        let output1 = aux { x with exp = e1 } in
        let output2 = aux { x with exp = e2 } in
        let level0_var = Types.Level0.Var.fresh () in
        let level0 = Types.Level0.varof level0_var in
        let annotation_var = Types.Annotation.Var.fresh () in
        let annotation = Types.Annotation.varof annotation_var in
        {
          ty = level0;
          annotation;
          all_vars =
            AllVarList.(
              Level0 level0_var @:: Annotation annotation_var
              @:: output1.all_vars @ output2.all_vars);
          assumptions = Assumption.(output1.assumptions @ output2.assumptions);
          constraints =
            (Level0 output1.ty
             *<=@ Level0 (Fun (output2.ty, level0, annotation))
            @@ x.exp.attr)
            @:: ((Annotation output1.annotation *<=@ Annotation annotation)
                @@ x.exp.attr)
            @:: ((Annotation output2.annotation *<=@ Annotation annotation)
                @@ x.exp.attr)
            @:: output1.constraints @ output2.constraints;
        }
    | If (e1, e2, e3) ->
        let output1 = aux { x with exp = e1 } in
        let output2 = aux { x with exp = e2 } in
        let output3 = aux { x with exp = e3 } in
        let annotation_var = Types.Annotation.Var.fresh () in
        let annotation = Types.Annotation.varof annotation_var in
        let level0_var = Types.Level0.Var.fresh () in
        let level0 = Types.Level0.varof level0_var in
        {
          ty = output2.ty;
          annotation;
          all_vars =
            AllVarList.(
              Level0 level0_var @:: Annotation annotation_var
              @:: output1.all_vars @ output2.all_vars @ output3.all_vars);
          assumptions =
            Assumption.(
              output1.assumptions @ output2.assumptions @ output3.assumptions);
          constraints =
            AllConstraintList.(
              ((Level0 output1.ty *<=@ Level0 (Const Bool)) @@ x.exp.attr)
              @:: ((Level0 output2.ty *<=@ Level0 level0) @@ x.exp.attr)
              @:: ((Level0 output3.ty *<=@ Level0 level0) @@ x.exp.attr)
              @:: ((Annotation output1.annotation *<=@ Annotation annotation)
                  @@ x.exp.attr)
              @:: ((Annotation output2.annotation *<=@ Annotation annotation)
                  @@ x.exp.attr)
              @:: ((Annotation output3.annotation *<=@ Annotation annotation)
                  @@ x.exp.attr)
              @:: output1.constraints @ output2.constraints
              @ output3.constraints);
        }
    | Let (v, e1, e2) when Exp.Level0.is_value e1 ->
        let output2 =
          aux
            {
              ty_cxt = x.ty_cxt;
              poly_cxt =
                PolyContext.add x.poly_cxt v
                @@ TypeScheme.generalize
                @@ aux { x with exp = e1 };
              exp = e2;
            }
        in
        {
          ty = output2.ty;
          annotation = output2.annotation;
          all_vars = output2.all_vars;
          assumptions = output2.assumptions;
          constraints = output2.constraints;
        }
    | Let (v, e1, e2) ->
        let output1 = aux { x with exp = e1 } in
        let level0_var = Types.Level0.Var.fresh () in
        let level0 = Types.Level0.varof level0_var in
        let annotation_var = Types.Annotation.Var.fresh () in
        let annotation = Types.Annotation.varof annotation_var in
        let output2 =
          aux { x with exp = e2; ty_cxt = TypeContext.add v level0 x.ty_cxt }
        in
        {
          ty = output2.ty;
          annotation;
          all_vars =
            AllVarList.(
              Level0 level0_var @:: Annotation annotation_var
              @:: output1.all_vars @ output2.all_vars);
          assumptions = Assumption.(output1.assumptions @ output2.assumptions);
          constraints =
            AllConstraintList.(
              ((Level0 output1.ty *<=@ Level0 level0) @@ x.exp.attr)
              @:: ((Annotation output1.annotation *<=@ Annotation annotation)
                  @@ x.exp.attr)
              @:: ((Annotation output2.annotation *<=@ Annotation annotation)
                  @@ x.exp.attr)
              @:: output1.constraints @ output2.constraints);
        }
    | CLam (v, e1) ->
        let cls_var0 = Types.Classifier.Var.fresh () in
        let cls0 = Types.Classifier.varof cls_var0 in
        let cls_var1 = Types.Classifier.Var.fresh () in
        let cls1 = Types.Classifier.varof cls_var1 in
        let level1_var0 = Types.Level1.Var.fresh () in
        let level10 = Types.Level1.varof level1_var0 in
        let level1_var1 = Types.Level1.Var.fresh () in
        let level11 = Types.Level1.varof level1_var1 in
        let ann_var = Types.Annotation.Var.fresh () in
        let ann = Types.Annotation.varof ann_var in
        let output1 =
          aux
            {
              ty_cxt =
                TypeContext.add_cstr cls_var0 cls_var1
                @@ TypeContext.add v (Code (level10, cls1))
                @@ x.ty_cxt;
              poly_cxt = x.poly_cxt;
              exp = e1;
            }
        in
        {
          ty = Code (Fun (level10, level11), cls0);
          annotation = ann;
          all_vars =
            AllVarList.(
              Classifier cls_var0 @:: Classifier cls_var1 @:: Level1 level1_var0
              @:: Level1 level1_var1 @:: Annotation ann_var @:: output1.all_vars);
          assumptions = output1.assumptions;
          constraints =
            AllConstraintList.(
              ((Classifier cls0 *<=@ Classifier cls1) @@ x.exp.attr)
              @:: ((Level0 output1.ty *<=@ Level0 (Code (level11, cls1)))
                  @@ x.exp.attr)
              @:: ((Annotation output1.annotation *<=@ Annotation ann)
                  @@ x.exp.attr)
              @:: ((cls1 */=@ Annotation ann) @@ x.exp.attr)
              @:: ((cls1 */=@ Classifier cls0) @@ x.exp.attr)
              @:: ((cls1 */=*@ TypeContext.fv x.ty_cxt) @@ x.exp.attr)
              @ output1.constraints);
        }
    | Reset0 e1 ->
        let cls_var = Types.Classifier.Var.fresh () in
        let cls = Types.Classifier.varof cls_var in
        let level1_var = Types.Level1.Var.fresh () in
        let level1 = Types.Level1.varof level1_var in
        let ann_var = Types.Annotation.Var.fresh () in
        let ann = Types.Annotation.varof ann_var in
        let output1 = aux { x with exp = e1 } in
        {
          ty = Code (level1, cls);
          annotation = ann;
          all_vars =
            AllVarList.(
              Classifier cls_var @:: Level1 level1_var @:: Annotation ann_var
              @:: output1.all_vars);
          assumptions = output1.assumptions;
          constraints =
            AllConstraintList.(
              ((Level0 output1.ty *<=@ Level0 (Code (level1, cls)))
              @@ x.exp.attr)
              @:: (Annotation output1.annotation
                   *<=@ Annotation (Seq (Code (level1, cls), ann))
                  @@ x.exp.attr)
              @:: output1.constraints);
        }
    | Shift0 (v, e1) ->
        let cls_var0 = Types.Classifier.Var.fresh () in
        let cls0 = Types.Classifier.varof cls_var0 in
        let cls_var1 = Types.Classifier.Var.fresh () in
        let cls1 = Types.Classifier.varof cls_var1 in
        let level1_var0 = Types.Level1.Var.fresh () in
        let level10 = Types.Level1.varof level1_var0 in
        let level1_var1 = Types.Level1.Var.fresh () in
        let level11 = Types.Level1.varof level1_var1 in
        let ann_var = Types.Annotation.Var.fresh () in
        let ann = Types.Annotation.varof ann_var in
        let output1 =
          aux
            {
              x with
              exp = e1;
              ty_cxt =
                TypeContext.add v
                  (ContFun (Code (level10, cls1), Code (level11, cls0), ann))
                  x.ty_cxt;
            }
        in
        {
          ty = Code (level10, cls1);
          annotation = Seq (Code (level11, cls0), ann);
          all_vars =
            AllVarList.(
              Classifier cls_var0 @:: Classifier cls_var1 @:: Level1 level1_var0
              @:: Level1 level1_var1 @:: Annotation ann_var @:: output1.all_vars);
          assumptions = output1.assumptions;
          constraints =
            AllConstraintList.(
              ((Level0 output1.ty *<=@ Level0 (Code (level11, cls0)))
              @@ x.exp.attr)
              @:: ((Annotation output1.annotation *<=@ Annotation ann)
                  @@ x.exp.attr)
              @:: ((Classifier cls0 *<=@ Classifier cls1) @@ x.exp.attr)
              @:: output1.constraints);
        }
    | Throw (e1, e2) ->
        let level1_var0 = Types.Level1.Var.fresh () in
        let level10 = Types.Level1.varof level1_var0 in
        let level1_var1 = Types.Level1.Var.fresh () in
        let level11 = Types.Level1.varof level1_var1 in
        let ann_var = Types.Annotation.Var.fresh () in
        let ann = Types.Annotation.varof ann_var in
        let cls_var0 = Types.Classifier.Var.fresh () in
        let cls0 = Types.Classifier.varof cls_var0 in
        let cls_var1 = Types.Classifier.Var.fresh () in
        let cls1 = Types.Classifier.varof cls_var1 in
        let cls_var2 = Types.Classifier.Var.fresh () in
        let cls2 = Types.Classifier.varof cls_var2 in
        let cls_var3 = Types.Classifier.Var.fresh () in
        let cls3 = Types.Classifier.varof cls_var3 in
        let output1 = aux { x with exp = e1 } in
        let output2 =
          aux
            {
              x with
              exp = e2;
              ty_cxt =
                TypeContext.add_cstr cls_var1 cls_var3
                @@ TypeContext.add_cstr cls_var2 cls_var3 x.ty_cxt;
            }
        in
        {
          ty = Code (level11, cls2);
          annotation = ann;
          all_vars =
            AllVarList.(
              Level1 level1_var0 @:: Level1 level1_var1 @:: Classifier cls_var0
              @:: Classifier cls_var1 @:: Classifier cls_var2
              @:: Classifier cls_var3 @:: Annotation ann_var
              @:: output1.all_vars @ output2.all_vars);
          assumptions = Assumption.(output1.assumptions @ output2.assumptions);
          constraints =
            AllConstraintList.(
              ((Annotation output1.annotation *<=@ Annotation ann) @@ x.exp.attr)
              @:: ((Annotation output2.annotation *<=@ Annotation ann)
                  @@ x.exp.attr)
              @:: (Level0 output1.ty
                   *<=@ Level0
                          (ContFun
                             (Code (level11, cls1), Code (level10, cls0), ann))
                  @@ x.exp.attr)
              @:: ((Level0 output2.ty *<=@ Level0 (Code (level11, cls3)))
                  @@ x.exp.attr)
              @:: ((Classifier cls0 *<=@ Classifier cls2) @@ x.exp.attr)
              @:: ((cls3 */=@ Classifier cls1) @@ x.exp.attr)
              @:: ((cls3 */=@ Classifier cls2) @@ x.exp.attr)
              @:: ((cls3 */=@ Annotation ann) @@ x.exp.attr)
              @:: ((cls3 */=*@ TypeContext.fv x.ty_cxt) @@ x.exp.attr)
              @ output1.constraints @ output2.constraints);
        }
  in
  try Result.ok @@ aux x with InferError err -> Result.error err

(* Constraint-solving *)

module CSolveOutput = struct
  type t = {
    constraints : AllConstraintList.t;
    subst : Substitution.Subst.t;
    cgraph : ConstraintGraph.t;
  }

  let pp fmt t =
    fprintf fmt
      "@[<hov 1>Constraints: %a@ Substitution: %a@ Constraint Graph: %a@]"
      AllConstraintList.pp t.constraints Substitution.Subst.pp t.subst
      ConstraintGraph.pp t.cgraph

  let show t = asprintf "%a" pp t
end

module CSolveConfiguration = struct
  type t = {
    constraints : AllConstraintList.t;
    cgraph : ConstraintGraph.t;
    subst : Substitution.Subst.t;
  }

  let pp fmt t =
    fprintf fmt
      "@[<hov 1>Constraints: %a@ Substitution: %a@ Constraint Graph: %a@]"
      AllConstraintList.pp t.constraints Substitution.Subst.pp t.subst
      ConstraintGraph.pp t.cgraph

  let show t = asprintf "%a" pp t

  let init tycxt constraints =
    {
      constraints;
      cgraph = ConstraintGraph.init tycxt;
      subst = Substitution.Subst.id;
    }
end

let imitate : type a.
    CSolveConfiguration.t ->
    a Syntax.Types.Var.t ->
    a Syntax.Types.t ->
    CSolveConfiguration.t =
 fun config var ty ->
  let tvl = ConstraintGraph.filter_sim config.cgraph @@ var in
  let sbt = Substitution.Subst.refresh_and_gen_subst tvl ty in
  let elim_cstr =
    ConstraintGraph.subst_var sbt
    @@ ConstraintGraph.get_sim config.cgraph
    @@ var
  in
  let new_constraints = AllConstraintList.subst sbt config.constraints in
  {
    constraints = new_constraints @ elim_cstr;
    subst = Substitution.Subst.(sbt **. config.subst);
    cgraph = ConstraintGraph.elim_sim var config.cgraph;
  }

let csolve assumptions allcstr =
  let open Constraint in
  let open Substitution.Subst in
  let open AllConstraintList in
  let open ConstraintGraph in
  let rec aux config =
    let () = info "[Infer.csolve] %s" @@ CSolveConfiguration.show config in
    let info_case str = info "[Infer.csolve] %s" str in
    match config.CSolveConfiguration.constraints with
    | [] ->
        info_case "All constraints solved.";
        CSolveOutput.
          {
            constraints = AllConstraintList.empty;
            subst = config.subst;
            cgraph = config.cgraph;
          }
    (* U-Refl *)
    | Pack { body = Le (ty1, ty2); _ } :: tl when Types.(ty1 = ty2) ->
        info_case "U-Refl";
        aux { config with constraints = tl }
    (* U-Var *)
    | Pack { body = Le (Level0 (Var tv1), Level0 (Var tv2)); _ } :: tl ->
        info_case "U-Var (Level0)";
        aux
          {
            config with
            constraints = tl;
            cgraph =
              ConstraintGraph.add
                (Types.Var.Level0 tv1 *<= Level0 tv2)
                config.cgraph;
          }
    | Pack { body = Le (Level1 (Var tv1), Level1 (Var tv2)); _ } :: tl ->
        info_case "U-Var (Level1)";
        aux
          {
            config with
            constraints = tl;
            cgraph =
              ConstraintGraph.add
                (Types.Var.Level1 tv1 *<= Level1 tv2)
                config.cgraph;
          }
    | Pack { body = Le (Annotation (Var tv1), Annotation (Var tv2)); _ } :: tl
      ->
        info_case "U-Var (Annotation)";
        aux
          {
            config with
            constraints = tl;
            cgraph =
              ConstraintGraph.add
                (Types.Var.Annotation tv1 *<= Annotation tv2)
                config.cgraph;
          }
    (* U-LVar *)
    | Pack { body = Le (Level0 (Var tv), Level0 ty2); _ } :: tl
      when not @@ occurs_check config.cgraph (Level0 tv) (Level0 ty2) ->
        info_case "U-LVar (Level0)";
        let config =
          imitate { config with constraints = tl } (Level0 tv) (Level0 ty2)
        in
        aux
          {
            config with
            constraints =
              Syntax.Types.Level0 (Types.Level0.subst config.subst (Var tv))
              *<= Level0 ty2
              @:: config.constraints;
          }
    | Pack { body = Le (Level1 (Var tv), Level1 ty2); _ } :: tl
      when not @@ occurs_check config.cgraph (Level1 tv) (Level1 ty2) ->
        info_case "U-LVar (Level1)";
        let config =
          imitate { config with constraints = tl } (Level1 tv) (Level1 ty2)
        in
        aux
          {
            config with
            constraints =
              Syntax.Types.Level1 (Types.Level1.subst config.subst (Var tv))
              *<= Level1 ty2
              @:: config.constraints;
          }
    | Pack { body = Le (Annotation (Var tv), Annotation ty2); _ } :: tl
      when not @@ occurs_check config.cgraph (Annotation tv) (Annotation ty2) ->
        info_case "U-LVar (Annotation)";
        let config =
          imitate
            { config with constraints = tl }
            (Annotation tv) (Annotation ty2)
        in
        aux
          {
            config with
            constraints =
              Syntax.Types.Annotation
                (Types.Annotation.subst config.subst (Var tv))
              *<= Annotation ty2
              @:: config.constraints;
          }
    (* U-RVar *)
    | Pack { body = Le (Level0 ty, Level0 (Var tv)); _ } :: tl
      when not @@ occurs_check config.cgraph (Level0 tv) (Level0 ty) ->
        info_case "U-RVar (Level0)";
        let config =
          imitate { config with constraints = tl } (Level0 tv) (Level0 ty)
        in
        aux
          {
            config with
            constraints =
              Syntax.Types.Level0 ty
              *<= Level0 (Types.Level0.subst config.subst (Var tv))
              @:: config.constraints;
          }
    | Pack { body = Le (Level1 ty, Level1 (Var tv)); _ } :: tl
      when not @@ occurs_check config.cgraph (Level1 tv) (Level1 ty) ->
        info_case "U-RVar (Level1)";
        let config =
          imitate { config with constraints = tl } (Level1 tv) (Level1 ty)
        in
        aux
          {
            config with
            constraints =
              Syntax.Types.Level1 ty
              *<= Level1 (Types.Level1.subst config.subst (Var tv))
              @:: config.constraints;
          }
    | Pack { body = Le (Annotation ty, Annotation (Var tv)); _ } :: tl
      when not @@ occurs_check config.cgraph (Annotation tv) (Annotation ty) ->
        info_case "U-RVar (Annotation)";
        let config =
          imitate
            { config with constraints = tl }
            (Annotation tv) (Annotation ty)
        in
        aux
          {
            config with
            constraints =
              Syntax.Types.Annotation ty
              *<= Annotation (Types.Annotation.subst config.subst (Var tv))
              @:: config.constraints;
          }
    (* U-TArrow *)
    | Pack
        {
          body =
            Le (Level0 (Fun (ty11, ty12, ann1)), Level0 (Fun (ty21, ty22, ann2)));
          attr;
        }
      :: tl ->
        info_case "U-TArrow";
        aux
          {
            config with
            constraints =
              ((Syntax.Types.Level0 ty21 *<=@ Level0 ty11) @@ attr)
              @:: ((Syntax.Types.Level0 ty12 *<=@ Level0 ty22) @@ attr)
              @:: ((Syntax.Types.Annotation ann1 *<=@ Annotation ann2) @@ attr)
              @:: tl;
          }
        (* U-TContArrow *)
    | Pack
        {
          body =
            Le
              ( Level0 (ContFun (ty11, ty12, ann1)),
                Level0 (ContFun (ty21, ty22, ann2)) );
          attr;
        }
      :: tl ->
        info_case "U-TContArrow";
        aux
          {
            config with
            constraints =
              ((Syntax.Types.Level0 ty21 *<=@ Level0 ty11) @@ attr)
              @:: ((Syntax.Types.Level0 ty12 *<=@ Level0 ty22) @@ attr)
              @:: ((Syntax.Types.Annotation ann1 *<=@ Annotation ann2) @@ attr)
              @:: tl;
          }
        (* U-TCode *)
    | Pack
        {
          body = Le (Level0 (Code (ty1, cls1)), Level0 (Code (ty2, cls2)));
          attr;
        }
      :: tl ->
        info_case "U-TCode";
        aux
          {
            config with
            constraints =
              ((Syntax.Types.Level1 ty1 *<=@ Syntax.Types.Level1 ty2) @@ attr)
              @:: (Syntax.Types.Classifier cls1
                   *<=@ Syntax.Types.Classifier cls2
                  @@ attr)
              @:: tl;
          }
        (* U-T1Arrow *)
    | Pack
        {
          body = Le (Level1 (Fun (ty11, ty12)), Level1 (Fun (ty21, ty22)));
          attr;
        }
      :: tl ->
        info_case "U-T1Arrow";
        aux
          {
            config with
            constraints =
              ((Syntax.Types.Level1 ty21 *<=@ Syntax.Types.Level1 ty11) @@ attr)
              @:: ((Syntax.Types.Level1 ty12 *<=@ Syntax.Types.Level1 ty22)
                  @@ attr)
              @:: tl;
          }
    (* U-AnnSeq *)
    | Pack
        {
          body =
            Le (Annotation (Seq (ty11, ann1)), Annotation (Seq (ty21, ann2)));
          attr;
        }
      :: tl ->
        info_case "AnnSeq";
        aux
          {
            config with
            constraints =
              ((Syntax.Types.Level0 ty11 *<=@ Syntax.Types.Level0 ty21) @@ attr)
              @:: (Syntax.Types.Annotation ann1
                   *<=@ Syntax.Types.Annotation ann2
                  @@ attr)
              @:: tl;
          }
    (* U-Clas *)
    | Pack { body = Le (Classifier cls1, Classifier cls2); attr } :: tl
      when not
           @@ ConstraintGraph.contains config.cgraph
           @@ (cls1 */=@ Syntax.Types.Var.Classifier cls2)
           @@ attr ->
        info_case "U-Clas";
        aux
          {
            config with
            constraints = tl;
            cgraph =
              ConstraintGraph.add
                (Types.Var.Classifier cls1 *<= Classifier cls2)
                config.cgraph;
          }
    (* U-NRClas *)
    | Pack { body = NotIn (cls1, Classifier cls2); attr } :: tl
      when not
           @@ ConstraintGraph.contains config.cgraph
           @@ (Types.Var.Classifier cls1 *<=@ Classifier cls2)
           @@ attr ->
        info_case "U-NRClas";
        aux
          {
            config with
            constraints = tl;
            cgraph =
              ConstraintGraph.add
                ((cls1 */=@ Types.Var.Classifier cls2) @@ attr)
                config.cgraph;
          }
    (* U-NRVar *)
    | Pack { body = NotIn (cls, Level0 (Var ty)); attr } :: tl ->
        info_case "U-NRVar (Level0)";
        aux
          {
            config with
            constraints = tl;
            cgraph =
              ConstraintGraph.add
                ((cls */=@ Types.Var.Level0 ty) @@ attr)
                config.cgraph;
          }
    | Pack { body = NotIn (_, Level1 (Var _)); _ } :: tl ->
        info_case "U-NRT1";
        aux { config with constraints = tl }
    | Pack { body = NotIn (cls, Annotation (Var ty)); attr } :: tl ->
        info_case "U-NRVar (Annotation)";
        aux
          {
            config with
            constraints = tl;
            cgraph =
              ConstraintGraph.add
                ((cls */=@ Types.Var.Annotation ty) @@ attr)
                config.cgraph;
          }
    (* U-NRTArrow  *)
    | Pack { body = NotIn (cls, Level0 (Fun (ty1, ty2, ann))); attr } :: tl ->
        info_case "U-NRTArrow";
        aux
          {
            config with
            constraints =
              ((cls */=@ Syntax.Types.Level0 ty1) @@ attr)
              @:: ((cls */=@ Syntax.Types.Level0 ty2) @@ attr)
              @:: ((cls */=@ Syntax.Types.Annotation ann) @@ attr)
              @:: tl;
          }
        (* U-NRTContArrow  *)
    | Pack { body = NotIn (cls, Level0 (ContFun (ty1, ty2, ann))); attr } :: tl
      ->
        info_case "U-NRTContArrow";
        aux
          {
            config with
            constraints =
              ((cls */=@ Syntax.Types.Level0 ty1) @@ attr)
              @:: ((cls */=@ Syntax.Types.Level0 ty2) @@ attr)
              @:: ((cls */=@ Syntax.Types.Annotation ann) @@ attr)
              @:: tl;
          }
        (* U-NRTCode  *)
    | Pack { body = NotIn (cls, Level0 (Code (ty1, cls2))); attr } :: tl ->
        info_case "U-NRTCode";
        aux
          {
            config with
            constraints =
              ((cls */=@ Syntax.Types.Level1 ty1) @@ attr)
              @:: ((cls */=@ Syntax.Types.Classifier cls2) @@ attr)
              @:: tl;
          }
        (* U-NRT1Arrow  *)
    | Pack { body = NotIn (_, Level1 _); _ } :: tl ->
        info_case "U-NRT1";
        aux { config with constraints = tl }
    (* U-AnnEmpty *)
    | Pack { body = NotIn (_, Annotation Empty); _ } :: tl ->
        info_case "U-NRT1";
        aux { config with constraints = tl }
    (* U-AnnSeq *)
    | Pack { body = NotIn (cls, Annotation (Seq (ty, ann1))); attr } :: tl ->
        info_case "U-NRAnnSeq";
        aux
          {
            config with
            constraints =
              ((cls */=@ Syntax.Types.Level0 ty) @@ attr)
              @:: ((cls */=@ Syntax.Types.Annotation ann1) @@ attr)
              @:: tl;
          }
        (* U-NRConst *)
    | Pack { body = NotIn (_, Level0 (Const _)); _ } :: tl ->
        info_case "U-NRConst";
        aux { config with constraints = tl }
    (* Failure case *)
    | Pack { body = NotIn (cls, Classifier cls2); attr } :: _ ->
        raise (InferError (ScopeExtrusionError (cls, cls2, attr)))
    | Pack { body = Le (Level0 ty1, Level0 ty2); attr } :: _ ->
        raise (InferError (UnificationErrorLevel0 (ty1, ty2, attr)))
    | Pack { body = Le (Level1 ty1, Level1 ty2); attr } :: _ ->
        raise (InferError (UnificationErrorLevel1 (ty1, ty2, attr)))
    | Pack { body = Le (Annotation ty1, Annotation ty2); attr } :: _ ->
        raise (InferError (UnificationErrorAnnotation (ty1, ty2, attr)))
    | Pack { body = Le (Classifier ty1, Classifier ty2); attr } :: _ ->
        raise (InferError (UnificationErrorClassifier (ty1, ty2, attr)))
  in
  try Result.ok @@ aux (CSolveConfiguration.init assumptions allcstr)
  with InferError err -> Result.error err

let csimpl cgraph = cgraph

module InferResult = struct
  type t = {
    type_ctx : TypeContext.t;
    type_ : Types.Level0.t;
    annotation : Types.Annotation.t;
  }

  let pp fmt t =
    fprintf fmt "@[<hov 1>Type Context: %a@ Inferred Type: %a@ Annotation: %a@]"
      TypeContext.pp t.type_ctx Types.Level0.pp t.type_ Types.Annotation.pp
      t.annotation

  let show t = asprintf "%a" pp t
end

let infer cgeninput =
  Result.bind (cgen cgeninput) @@ fun cgenoutput ->
  Result.bind (csolve cgeninput.ty_cxt cgenoutput.constraints)
  @@ fun csolveoutput ->
  Result.ok
    InferResult.
      {
        type_ctx = TypeContext.subst csolveoutput.subst cgeninput.ty_cxt;
        type_ = Types.Level0.subst csolveoutput.subst cgenoutput.ty;
        annotation =
          Types.Annotation.subst csolveoutput.subst cgenoutput.annotation;
      }
