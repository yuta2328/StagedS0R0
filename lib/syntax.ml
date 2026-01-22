open Common.All

module Exp = struct
  module Const = struct
    type t = Int of int | Bool of bool | String of string
  end

  module Level1 = struct
    module Var = struct
      type t = string [@@deriving show]
    end

    type t =
      | Const of Const.t
      | Var of Var.t
      | Lam of Var.t * t
      | App of t * t
      | If of t * t * t
      | Let of Var.t * t * t
  end

  module Level0 = struct
    module Var = struct
      type t = Var of string

      let pp fmt (Var x) = Format.fprintf fmt "%s" x
      let show (Var x) = x
    end

    type t = (_exp, SourcePosition.t) Annotated.t

    and _exp =
      | Var of Var.t
      | Const of Const.t
      | Lam of Var.t * t
      | Bracket of Level1.t
      | ContLam of Var.t * t
      | App of t * t
      | CLam of Var.t * t
      | ULam of Level1.Var.t * t
      | If of t * t * t
      | Let of Var.t * t * t
      | Reset0 of t
      | Shift0 of Var.t * t
      | Throw of t * t
  end

  module Value = struct
    type t =
      | Var of Level0.Var.t
      | Const of Const.t
      | Lam of Level0.Var.t * Level0.t
      | ContLam of Level0.Var.t * Level0.t
      | Bracket of Level1.t
      | BuiltIn of (t -> t)
  end
end

module Types = struct
  module Const = struct
    type t = Int | Bool | String
  end

  module Classifier = struct
    module Var = struct
      type t = Var of int
    end

    type t = Var.t
  end

  module Level1 = struct
    module Var = struct
      type t = Var of int
    end

    type t = Const of Const.t | Var of Var.t | Fun of t * t
  end

  module rec Level0 : sig
    module Var : sig
      type t = Var of int
    end

    type t =
      | Const of Const.t
      | Var of Var.t
      | Fun of t * t * Annotation.t
      | ContFun of t * t * Annotation.t
      | Code of Level1.t * Classifier.t
  end = struct
    module Var = struct
      type t = Var of int
    end

    type t =
      | Const of Const.t
      | Var of Var.t
      | Fun of t * t * Annotation.t
      | ContFun of t * t * Annotation.t
      | Code of Level1.t * Classifier.t
  end

  and Annotation : sig
    module Var : sig
      type t = Var of int
    end

    type t = Empty | Var of Var.t | Seq of Level0.t * t
  end = struct
    module Var = struct
      type t = Var of int
    end

    type t = Empty | Var of Var.t | Seq of Level0.t * t
  end

  type 'a t =
    | Level0 : Level0.t -> Level0.t t
    | Level1 : Level1.t -> Level1.t t
    | Classifier : Classifier.t -> Classifier.t t
    | Annotation : Annotation.t -> Annotation.t t

  module Var = struct
    type 'a t =
      | Level0 : Level0.Var.t -> Level0.t t
      | Level1 : Level1.Var.t -> Level1.t t
      | Classifier : Classifier.Var.t -> Classifier.t t
      | Annotation : Annotation.Var.t -> Annotation.t t
  end
end

module Constraint = struct
  type 'a _t =
    | Le : 'a * 'a -> 'a _t
    | NotIn : Types.Classifier.t * 'a -> 'a _t

  type 'a t = ('a _t, SourcePosition.t) Annotated.t
end

module Assumption = struct
  type t = (Types.Classifier.t * Types.Classifier.t) list
end
