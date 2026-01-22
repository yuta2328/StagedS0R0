open Common.All
open Format
open Syntax

module SubstitutionExp = struct
  module VarSet = Set.Make (struct
    type t = Exp.Level0.Var.t

    let pp : Format.formatter -> t -> unit =
     fun fmt (Var x) -> fprintf fmt "%s" x

    let compare : t -> t -> int = fun (Var x) (Var y) -> compare x y
  end)

  module type S = sig
    type t

    val fv : t -> VarSet.t
    val subst : Exp.Level0.Var.t -> Exp.Level0.t -> t -> t
  end
end

module Exp = struct
  open Exp

  module Const = struct
    include Const

    let pp fmt = function
      | Int n -> fprintf fmt "%d" n
      | Bool b -> fprintf fmt "%b" b
      | String s -> fprintf fmt "\"%s\"" s

    let show x = asprintf "%a" pp x

    let ty = function
      | Int _ -> Types.Const.Int
      | Bool _ -> Types.Const.Bool
      | String _ -> Types.Const.String
  end

  module Level1 = struct
    module I = struct
      include Level1

      module Var = struct
        include Level1.Var

        let ( = ) v1 v2 = v1 = v2
        let compare v1 v2 = compare v1 v2
        let _counter = ref 0

        let fresh : unit -> t =
         fun () ->
          let v = !_counter in
          _counter := v + 1;
          "x" ^ string_of_int v
      end

      open ShowAST

      let assoc_level : t -> Level.t = function
        | Const _ -> L1
        | Var _ -> L1
        | Lam (_, _) -> L18
        | App (_, _) -> L4
        | If (_, _, _) -> L16
        | Let (_, _, _) -> L18

      let assoc_type = function
        | App (_, _) -> AssocType.Left
        | _ -> AssocType.None

      let pp_neg f fmt = function
        | Const c -> fprintf fmt "%s" (Const.show c)
        | Var v -> fprintf fmt "%s" (Var.show v)
        | Lam (v, body) ->
            fprintf fmt "fun %s -> %a" (Var.show v) (f Pos.None) body
        | App (e1, e2) -> fprintf fmt "%a %a" (f Pos.Left) e1 (f Pos.Right) e2
        | If (cond, then_e, else_e) ->
            fprintf fmt "if %a then %a else %a" (f Pos.None) cond (f Pos.None)
              then_e (f Pos.None) else_e
        | Let (v, e1, e2) ->
            fprintf fmt "let %s = %a in %a" (Var.show v) (f Pos.None) e1
              (f Pos.None) e2
    end

    include ShowAST.Make (I)
    include I
  end

  module Level0 = struct
    open ShowAST

    module I = struct
      include Level0

      module Var = struct
        include Level0.Var

        let ( = ) (Var v1) (Var v2) = v1 = v2
        let compare (Var v1) (Var v2) = compare v1 v2
        let _counter = ref 0

        let fresh : unit -> t =
         fun () ->
          let v = !_counter in
          _counter := v + 1;
          Var ("x" ^ string_of_int v)
      end

      let assoc_level : t -> Level.t =
       fun x ->
        match x.body with
        | Var _ | Const _ | Bracket _ | Reset0 _ -> L1
        | If (_, _, _) -> L16
        | App (_, _) -> L4
        | _ -> L18

      let assoc_type : t -> AssocType.t =
       fun x ->
        match x.body with App (_, _) -> AssocType.Left | _ -> AssocType.None

      let pp_neg f fmt (x : t) =
        match x.body with
        | Var v -> fprintf fmt "%a" Var.pp v
        | Const c -> fprintf fmt "%a" Const.pp c
        | Bracket e -> fprintf fmt "<%a>" Level1.pp e
        | Lam (v, body) ->
            fprintf fmt "@[<hov 1>fun %a ->@ %a@]" Var.pp v (f Pos.None) body
        | ContLam (v, body) ->
            fprintf fmt "@[<hov 1>contfun %a ->@ %a@]" Var.pp v (f Pos.None)
              body
        | App (e1, e2) ->
            fprintf fmt "@[<hov 1>%a@ %a@]" (f Pos.Left) e1 (f Pos.Right) e2
        | CLam (v, body) ->
            fprintf fmt "@[<hov 1>cfun %a ->@ %a@]" Var.pp v (f Pos.None) body
        | ULam (v, body) ->
            fprintf fmt "@[<hov 1>ulam %a ->@ %a@]" Level1.Var.pp v (f Pos.None)
              body
        | If (e1, e2, e3) ->
            fprintf fmt "@[<hov 1>if %a@ then %a@ else %a@]" (f Pos.None) e1
              (f Pos.None) e2 (f Pos.None) e3
        | Let (v, e1, e2) ->
            fprintf fmt "@[<hov 1>let %a = %a in@ %a@]" Var.pp v (f Pos.None) e1
              (f Pos.None) e2
        | Reset0 e -> fprintf fmt "@[<hov 1>{%a}@]" (f Pos.None) e
        | Shift0 (v, e) ->
            fprintf fmt "@[<hov 1>shift0 %a in@ %a@]" Var.pp v (f Pos.None) e
        | Throw (e1, e2) ->
            fprintf fmt "@[<hov 1>throw %a to@ %a@]" (f Pos.None) e1
              (f Pos.None) e2
    end

    include ShowAST.Make (I)
    include I

    let rec fv (e : t) : SubstitutionExp.VarSet.t =
      match e.body with
      | Var v -> SubstitutionExp.VarSet.singleton v
      | Const _ -> SubstitutionExp.VarSet.empty
      | Bracket _ -> SubstitutionExp.VarSet.empty
      | Lam (v, body) -> SubstitutionExp.VarSet.remove v (fv body)
      | ContLam (v, body) -> SubstitutionExp.VarSet.remove v (fv body)
      | App (e1, e2) -> SubstitutionExp.VarSet.union (fv e1) (fv e2)
      | CLam (v, body) -> SubstitutionExp.VarSet.remove v (fv body)
      | ULam (_, body) -> fv body
      | If (e1, e2, e3) ->
          SubstitutionExp.VarSet.union (fv e1)
            (SubstitutionExp.VarSet.union (fv e2) (fv e3))
      | Let (v, e1, e2) ->
          SubstitutionExp.VarSet.union (fv e1)
            (SubstitutionExp.VarSet.remove v (fv e2))
      | Reset0 e -> fv e
      | Shift0 (v, e) -> SubstitutionExp.VarSet.remove v (fv e)
      | Throw (e1, e2) -> SubstitutionExp.VarSet.union (fv e1) (fv e2)

    let rec subst v v_e (e : t) : t =
      match e.body with
      | Var v1 when v = v1 -> v_e
      | Var _ | Const _ | Bracket _ -> e
      | Lam (v1, body) ->
          if v <> v1 && not (SubstitutionExp.VarSet.mem v1 @@ fv v_e) then
            { e with body = Lam (v1, subst v v_e body) }
          else e
      | ContLam (v1, body) ->
          if v <> v1 && not (SubstitutionExp.VarSet.mem v1 @@ fv v_e) then
            { e with body = ContLam (v1, subst v v_e body) }
          else e
      | App (e1, e2) -> { e with body = App (subst v v_e e1, subst v v_e e2) }
      | CLam (v1, body) ->
          if v <> v1 && not (SubstitutionExp.VarSet.mem v1 @@ fv v_e) then
            { e with body = CLam (v1, subst v v_e body) }
          else e
      | ULam (v1, body) -> { e with body = ULam (v1, subst v v_e body) }
      | If (e1, e2, e3) ->
          { e with body = If (subst v v_e e1, subst v v_e e2, subst v v_e e3) }
      | Let (v1, e1, e2) ->
          if v <> v1 && not (SubstitutionExp.VarSet.mem v1 @@ fv v_e) then
            { e with body = Let (v1, subst v v_e e1, subst v v_e e2) }
          else { e with body = Let (v1, subst v v_e e1, e2) }
      | Reset0 e1 -> { e with body = Reset0 (subst v v_e e1) }
      | Shift0 (v1, e1) ->
          if v <> v1 && not (SubstitutionExp.VarSet.mem v1 @@ fv v_e) then
            { e with body = Shift0 (v1, subst v v_e e1) }
          else e
      | Throw (e1, e2) ->
          { e with body = Throw (subst v v_e e1, subst v v_e e2) }

    let return : _exp -> t = fun body -> { body; attr = SourcePosition.dummy }

    let is_value (e : t) : bool =
      match e.body with
      | Var _ | Const _ | Lam _ | ContLam _ | Bracket _ -> true
      | _ -> false
  end

  module Value = struct
    include Value

    let pp fmt : t -> unit = function
      | Var v -> fprintf fmt "%a" Exp.Level0.Var.pp v
      | Const c -> fprintf fmt "%a" Const.pp c
      | Lam (v, body) ->
          fprintf fmt "@[<hov 1>fun %a ->@ %a@]" Exp.Level0.Var.pp v Level0.pp
            body
      | ContLam (v, body) ->
          fprintf fmt "@[<hov 1>contfun %a ->@ %a@]" Exp.Level0.Var.pp v
            Level0.pp body
      | Bracket e -> fprintf fmt "<%a>" Level1.pp e
      | _ -> fprintf fmt "<builtin-fun>"

    let show v = asprintf "%a" pp v

    let embed : t -> Level0.t = function
      | Var x -> Level0.return @@ Var x
      | Const c -> Level0.return @@ Const c
      | Lam (x, e) -> Level0.return @@ Lam (x, e)
      | ContLam (x, e) -> Level0.return @@ ContLam (x, e)
      | Bracket e -> Level0.return @@ Bracket e
      | _ -> failwith "[Value.embed]: Cannot embed non-value to Level0"

    let cast : Level0.t -> t =
     fun e ->
      match e.body with
      | Var x -> Var x
      | Const c -> Const c
      | Lam (x, body) -> Lam (x, body)
      | ContLam (x, body) -> ContLam (x, body)
      | Bracket e1 -> Bracket e1
      | _ -> failwith "[Value.cast]: Cannot cast non-value to Value"
  end
end

module Types = struct
  module type Level0 = sig
    module Var : sig
      type t = Types.Level0.Var.t

      include Show.S with type t := t
      include Order.I with type t := t

      val fresh : unit -> t
    end

    type t = Types.Level0.t

    include Show.S with type t := t

    val fresh : unit -> t
    val fresh_of : t -> t
    val varof : Var.t -> t
    val subst_map : 'a Types.Var.t -> 'a Types.t -> t -> t
  end

  module type Level1 = sig
    module Var : sig
      type t = Types.Level1.Var.t

      include Show.S with type t := t
      include Order.I with type t := t

      val fresh : unit -> t
    end

    type t = Types.Level1.t

    include Show.S with type t := t

    val fresh : unit -> t
    val fresh_of : t -> t
    val varof : Var.t -> t
    val subst_map : 'a Types.Var.t -> 'a Types.t -> t -> t
  end

  module type Classifier = sig
    module Var : sig
      type t = Types.Classifier.Var.t

      include Show.S with type t := t
      include Order.I with type t := t

      val fresh : unit -> t
    end

    type t = Types.Classifier.t

    include Show.S with type t := t

    val fresh : unit -> t
    val fresh_of : t -> t
    val varof : Var.t -> t
    val subst_map : 'a Types.Var.t -> 'a Types.t -> t -> t
  end

  module type Annotation = sig
    module Var : sig
      type t = Types.Annotation.Var.t

      include Show.S with type t := t
      include Order.I with type t := t

      val fresh : unit -> t
    end

    type t = Types.Annotation.t

    include Show.S with type t := t

    val fresh : unit -> t
    val fresh_of : t -> t
    val varof : Var.t -> t
    val subst_map : 'a Types.Var.t -> 'a Types.t -> t -> t
  end

  open Types

  module Const = struct
    include Const

    let pp fmt = function
      | Int -> fprintf fmt "int"
      | Bool -> fprintf fmt "bool"
      | String -> fprintf fmt "string"

    let show x = asprintf "%a" pp x
  end

  module Classifier : Classifier = struct
    include Classifier

    module Var = struct
      include Var

      let pp fmt (Var v) = fprintf fmt "γ_%d" v
      let show = asprintf "%a" pp
      let _gen = ref 0

      let fresh () =
        let v = !_gen in
        _gen := v + 1;
        Var v

      let compare (Var v1) (Var v2) = compare v1 v2
    end

    let pp = Var.pp
    let show = Var.show
    let fresh () = Var.fresh ()
    let varof v = v

    let subst_map : type a. a Types.Var.t -> a Types.t -> t -> t =
     fun tv ty cls ->
      match (tv, ty) with
      | Classifier fr, Classifier tr -> if cls = fr then tr else cls
      | _ -> cls

    let fresh_of _ = Var.fresh ()
  end

  module Level1 : Level1 = struct
    include Level1

    module Var = struct
      include Level1.Var

      let pp fmt (Var v) = fprintf fmt "β_%d" v
      let show = asprintf "%a" pp
      let _gen = ref 0

      let fresh () =
        let v = !_gen in
        _gen := v + 1;
        Var v

      let compare (Var v1) (Var v2) = compare v1 v2
    end

    module I = struct
      open ShowAST

      type t = Level1.t

      let assoc_level : Level1.t -> Level.t = function
        | Const _ -> L1
        | Var _ -> L1
        | Fun _ -> L4

      let assoc_type : t -> AssocType.t = function
        | Fun _ -> AssocType.Right
        | _ -> AssocType.None

      let pp_neg f fmt : Level1.t -> unit = function
        | Const c -> fprintf fmt "%a" Const.pp c
        | Var v -> fprintf fmt "%a" Var.pp v
        | Fun (ty1, ty2) ->
            fprintf fmt "%a -> %a" (f Pos.Left) ty1 (f Pos.Right) ty2
    end

    module M : sig
      include Show.S with type t := Level1.t
    end =
      ShowAST.Make (I)

    include M

    let fresh () = Var (Var.fresh ())

    let rec fresh_of : t -> t = function
      | Const c -> Const c
      | Var _ -> Var (Var.fresh ())
      | Fun (ty1, ty2) -> Fun (fresh_of ty1, fresh_of ty2)

    let varof v = Var v

    let rec subst_map : type a. a Types.Var.t -> a Types.t -> t -> t =
     fun tv ty t ->
      match (tv, ty, t) with
      | Level1 v, Level1 ty, Var x -> if v = x then ty else t
      | _, _, Fun (ty1, ty2) -> Fun (subst_map tv ty ty1, subst_map tv ty ty2)
      | _ -> t
  end

  module rec Level0Aux : Level0 = struct
    open ShowAST

    module Var = struct
      include Level0.Var

      let pp fmt (Var v) = fprintf fmt "α_%d" v
      let show = asprintf "%a" pp
      let _gen = ref 0

      let fresh () =
        let v = !_gen in
        _gen := v + 1;
        Var v

      let compare (Var v1) (Var v2) = compare v1 v2
    end

    module I = struct
      type t = Level0.t

      let assoc_level : t -> Level.t = function
        | Fun _ | ContFun _ -> L4
        | _ -> L1

      let assoc_type : t -> AssocType.t = function
        | Fun _ | ContFun _ -> AssocType.Right
        | _ -> AssocType.None

      let pp_neg f fmt : t -> unit = function
        | Const c -> fprintf fmt "%a" Const.pp c
        | Var v -> fprintf fmt "%a" Var.pp v
        | Fun (ty1, ty2, ann) ->
            fprintf fmt "%a -[%a]-> %a" (f Pos.Left) ty1 AnnotationAux.pp ann
              (f Pos.Right) ty2
        | ContFun (ty1, ty2, ann) ->
            fprintf fmt "%a >-{%a}-> %a" (f Pos.Left) ty1 AnnotationAux.pp ann
              (f Pos.Right) ty2
        | Code (ty, cls) -> fprintf fmt "<%a>^%a" Level1.pp ty Classifier.pp cls
    end

    module M : sig
      include Show.S with type t := Level0.t
    end =
      ShowAST.Make (I)

    include M

    type t = Level0.t

    let fresh () = Level0.Var (Var.fresh ())

    let rec fresh_of : t -> t = function
      | Const c -> Const c
      | Var _ -> Var (Var.fresh ())
      | Fun (ty1, ty2, ann) ->
          Fun (fresh_of ty1, fresh_of ty2, AnnotationAux.fresh_of ann)
      | ContFun (ty1, ty2, ann) ->
          ContFun (fresh_of ty1, fresh_of ty2, AnnotationAux.fresh_of ann)
      | Code (ty1, cls) -> Code (Level1.fresh_of ty1, Classifier.fresh_of cls)

    let varof v = Level0.Var v

    let rec subst_map : type a. a Types.Var.t -> a Types.t -> t -> t =
     fun tv ty t ->
      match (tv, ty, t) with
      | Level0 v, Level0 ty, Var x when v = x -> ty
      | _, _, Fun (ty1, ty2, ann) ->
          Fun
            ( subst_map tv ty ty1,
              subst_map tv ty ty2,
              AnnotationAux.subst_map tv ty ann )
      | _, _, ContFun (ty1, ty2, ann) ->
          ContFun
            ( subst_map tv ty ty1,
              subst_map tv ty ty2,
              AnnotationAux.subst_map tv ty ann )
      | _, _, Code (ty1, cls) ->
          Code (Level1.subst_map tv ty ty1, Classifier.subst_map tv ty cls)
      | _ -> t
  end

  and AnnotationAux : Annotation = struct
    module Var = struct
      include Annotation.Var

      let pp fmt (Var v) = fprintf fmt "δ_%d" v
      let show = asprintf "%a" pp
      let _gen = ref 0

      let fresh () =
        let v = !_gen in
        _gen := v + 1;
        Var v

      let compare (Var v1) (Var v2) = compare v1 v2
    end

    type t = Annotation.t

    let rec pp fmt : Annotation.t -> unit = function
      | Empty -> fprintf fmt "ε"
      | Var v -> fprintf fmt "%a" Var.pp v
      | Seq (ty, ann) -> fprintf fmt "[%a, %a]" Level0Aux.pp ty pp ann

    let show ann = asprintf "%a" pp ann
    let fresh () = Annotation.Var (Var.fresh ())

    let rec fresh_of : t -> t = function
      | Empty -> Empty
      | Var _ -> Var (Var.fresh ())
      | Seq (ty, ann) -> Seq (Level0Aux.fresh_of ty, fresh_of ann)

    let varof v = Annotation.Var v

    let rec subst_map : type a. a Types.Var.t -> a Types.t -> t -> t =
     fun tv ty ann ->
      match (tv, ty, ann) with
      | Annotation v, Annotation ty, Var x -> if v = x then ty else ann
      | _, _, Seq (ty1, ann1) ->
          Seq (Level0Aux.subst_map tv ty ty1, subst_map tv ty ann1)
      | _ -> ann
  end

  module Level0 = Level0Aux
  module Annotation = AnnotationAux

  let pp : type a. formatter -> a t -> unit =
   fun fmt ty ->
    match ty with
    | Level0 ty -> Level0.pp fmt ty
    | Level1 ty -> Level1.pp fmt ty
    | Annotation ty -> Annotation.pp fmt ty
    | Classifier ty -> Classifier.pp fmt ty

  let show ty = asprintf "%a" pp ty

  module Var = struct
    include Var

    let pp : type a. formatter -> a Var.t -> unit =
     fun fmt var ->
      match var with
      | Level0 v -> Level0.Var.pp fmt v
      | Level1 v -> Level1.Var.pp fmt v
      | Annotation v -> Annotation.Var.pp fmt v
      | Classifier v -> Classifier.Var.pp fmt v

    let show var = asprintf "%a" pp var

    let fresh_of : type a. a Var.t -> a Var.t =
     fun var ->
      match var with
      | Level0 _ -> Level0 (Level0.Var.fresh ())
      | Level1 _ -> Level1 (Level1.Var.fresh ())
      | Annotation _ -> Annotation (Annotation.Var.fresh ())
      | Classifier _ -> Classifier (Classifier.Var.fresh ())

    let ( = ) : type a. a Var.t -> a Var.t -> bool =
     fun var1 var2 ->
      match (var1, var2) with
      | Level0 v1, Level0 v2 -> v1 = v2
      | Level1 v1, Level1 v2 -> v1 = v2
      | Annotation v1, Annotation v2 -> v1 = v2
      | Classifier v1, Classifier v2 -> v1 = v2

    let compare : type a. a Var.t -> a Var.t -> int =
     fun var1 var2 ->
      match (var1, var2) with
      | Level0 v1, Level0 v2 -> Level0.Var.compare v1 v2
      | Level1 v1, Level1 v2 -> Level1.Var.compare v1 v2
      | Annotation v1, Annotation v2 -> Annotation.Var.compare v1 v2
      | Classifier v1, Classifier v2 -> Classifier.Var.compare v1 v2
  end

  let ( = ) : type a. a t -> a t -> bool =
   fun ty1 ty2 ->
    match (ty1, ty2) with
    | Level0 t1, Level0 t2 -> t1 = t2
    | Level1 t1, Level1 t2 -> t1 = t2
    | Annotation t1, Annotation t2 -> t1 = t2
    | Classifier t1, Classifier t2 -> t1 = t2

  let fresh_of : type a. a t -> a t = function
    | Level0 ty -> Level0 (Level0.fresh_of ty)
    | Level1 ty -> Level1 (Level1.fresh_of ty)
    | Annotation ty -> Annotation (Annotation.fresh_of ty)
    | Classifier ty -> Classifier (Classifier.fresh_of ty)

  let varof : type a. a Var.t -> a t =
   fun var ->
    match var with
    | Level0 v -> Level0 (Level0.varof v)
    | Level1 v -> Level1 (Level1.varof v)
    | Annotation v -> Annotation (Annotation.varof v)
    | Classifier v -> Classifier (Classifier.varof v)

  let subst_map : type a b. a Var.t -> a t -> b t -> b t =
   fun tv ty t ->
    match t with
    | Level0 ty0 -> Level0 (Level0.subst_map tv ty ty0)
    | Level1 ty1 -> Level1 (Level1.subst_map tv ty ty1)
    | Annotation ty2 -> Annotation (Annotation.subst_map tv ty ty2)
    | Classifier ty3 -> Classifier (Classifier.subst_map tv ty ty3)
end

module Constraint = struct
  include Constraint
  open Annotated

  let less = function { body = Le (x, _); _ } -> Some x | _ -> None
  let gret = function { body = Le (_, y); _ } -> Some y | _ -> None

  let less_equal var = function
    | { body = Le (x, _); _ } -> x = var
    | _ -> false

  let greater_equal var = function
    | { body = Le (_, y); _ } -> y = var
    | _ -> false

  let product l1 l2 =
    flip List.concat_map l1 @@ fun x ->
    flip List.map l2 @@ fun y ->
    { body = Le (x, y); attr = SourcePosition.dummy }

  let pp_of pp_ty fmt = function
    | { body = Le (ty1, ty2); _ } -> fprintf fmt "%a ≤ %a" pp_ty ty1 pp_ty ty2
    | { body = NotIn (cls, ann); _ } ->
        fprintf fmt "%a ⋢ %a" Types.Classifier.pp cls pp_ty ann

  let ( *<= ) x y = { body = Le (x, y); attr = SourcePosition.dummy }
  let ( *<=@ ) x y spos = { body = Le (x, y); attr = spos }
  let ( */= ) x y = { body = NotIn (x, y); attr = SourcePosition.dummy }
  let ( */=@ ) x y spos = { body = NotIn (x, y); attr = spos }
end

module Assumption = struct
  include Assumption

  let empty = []
  let ( @ ) : t -> t -> t = fun a1 a2 -> a1 @ a2

  let pp =
    pp_tuple @@ fun fmt (cls1, cls2) ->
    fprintf fmt "%a ≤ %a" Types.Classifier.pp cls1 Types.Classifier.pp cls2

  let show asm = asprintf "%a" (pp_list pp) asm
end

module AllVarList = struct
  type pack = Pack : 'a Types.Var.t -> pack

  module S = Set.Make (struct
    type t = pack

    let compare x y = compare x y
    let pp fmt (Pack x) = fprintf fmt "%a" Types.Var.pp x
    let show x = asprintf "%a" pp x
  end)

  type t = S.t

  let empty : t = S.empty
  let pp fmt (vars : t) = S.pp fmt vars
  let show : t -> string = asprintf "%a" pp
  let ( @ ) : t -> t -> t = S.union

  let ( @:: ) : type a. a Types.Var.t -> t -> t =
   fun var vars -> S.add (Pack var) vars

  let fresh_of : t -> t =
    S.of_list
    **. List.map (fun (Pack tv) -> Pack (Types.Var.fresh_of tv))
    **. S.to_list

  let singleton : type a. a Types.Var.t -> t = fun tv -> S.singleton @@ Pack tv
  let mem : type a. a Types.Var.t -> t -> bool = fun tv t -> S.mem (Pack tv) t
  let diff : t -> t -> t = S.diff
  let is_disjoint : t -> t -> bool = S.disjoint
end

module AllConstraintList = struct
  type pack = Pack : 'a Syntax.Types.t Constraint.t -> pack
  type t = pack list

  let empty = []
  let is_empty t = t = []

  let pp =
    pp_list_comma @@ fun fmt (Pack pack) ->
    fprintf fmt "%a" (Constraint.pp_of Types.pp) pack

  let show cls = asprintf "%a" pp cls
  let ( @ ) : t -> t -> t = ( @ )

  let ( @:: ) : type a. a Syntax.Types.t Constraint.t -> t -> t =
   fun cls0 clsl -> Pack cls0 :: clsl

  let ( @::! ) = fun cls_list clsl -> List.fold_right ( @:: ) cls_list clsl

  let ( */=* ) cls (allvar : AllVarList.t) : t =
    flip List.map (AllVarList.S.to_list allvar) @@ fun (Pack tv) ->
    Pack Constraint.(cls */= Types.varof tv)

  let ( */=*@ ) cls (allvar : AllVarList.t) spos : t =
    flip List.map (AllVarList.S.to_list allvar) @@ fun (Pack tv) ->
    Pack (Constraint.(cls */=@ Types.varof tv) @@ spos)
end

module TypeScheme = struct
  type t = {
    bound_vars : AllVarList.t;
    assumptions : Assumption.t;
    all_constraints : AllConstraintList.t;
    ty : Types.Level0.t;
  }

  let pp fmt (ts : t) =
    fprintf fmt "@[<hov 1> ∀%a.@ (@[%a@]@ => @[%a@])@ =>@ %a@]" AllVarList.pp
      ts.bound_vars Assumption.pp ts.assumptions AllConstraintList.pp
      ts.all_constraints Types.Level0.pp ts.ty

  let show ts = asprintf "%a" pp ts
end

module Env = struct
  module Value = struct
    type t = { ty : TypeScheme.t; value : Exp.Value.t }
  end

  type t = (Exp.Level0.Var.t * Value.t) list

  let lookup key t = List.assoc_opt key t

  let poly_ctx : t -> (Exp.Level0.Var.t * TypeScheme.t) list =
    List.map @@ fun (k, v) -> (k, v.Value.ty)
end
