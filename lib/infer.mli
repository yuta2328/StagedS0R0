open Common.All
open Subst

module TypeContext : sig
  type t

  val empty : t
  val add : Exp.Level0.Var.t -> Types.Level0.t -> t -> t
  val add_cstr : Types.Classifier.Var.t -> Types.Classifier.Var.t -> t -> t
  val lookup : Exp.Level0.Var.t -> t -> Types.Level0.t option

  include Show.S with type t := t
end

module PolyContext : sig
  type t

  val empty : t
  val add : t -> Exp.Level0.Var.t -> TypeScheme.t -> t
  val lookup : Exp.Level0.Var.t -> t -> TypeScheme.t option

  include Show.S with type t := t
end

module ConstraintGraph : sig
  type t

  val init : TypeContext.t -> t
  val add : 'a Syntax.Types.Var.t Constraint.t -> t -> t

  include Show.S with type t := t
end

module CGenInput : sig
  type t = {
    ty_cxt : TypeContext.t;
    poly_cxt : PolyContext.t;
    exp : Exp.Level0.t;
  }

  val init : Exp.Level0.t -> t

  include Show.S with type t := t
end

module CGenOutput : sig
  type t = {
    ty : Types.Level0.t;
    annotation : Types.Annotation.t;
    all_vars : AllVarList.t;
    assumptions : Assumption.t;
    constraints : AllConstraintList.t;
  }

  include Show.S with type t := t
end

module InferError : sig
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

  include Show.S with type t := t
end

val cgen : CGenInput.t -> (CGenOutput.t, InferError.t) result

module CSolveOutput : sig
  type t = {
    constraints : AllConstraintList.t;
    subst : Substitution.Subst.t;
    cgraph : ConstraintGraph.t;
  }

  include Show.S with type t := t
end

module InferResult : sig
  type t = {
    type_ctx : TypeContext.t;
    type_ : Types.Level0.t;
    annotation : Types.Annotation.t;
  }

  include Show.S with type t := t
end

val csolve :
  TypeContext.t -> AllConstraintList.t -> (CSolveOutput.t, InferError.t) result

val csimpl : ConstraintGraph.t -> ConstraintGraph.t
val infer : CGenInput.t -> (InferResult.t, InferError.t) result
