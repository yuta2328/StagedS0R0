open Format

let id x = x
let const x _ = x
let compose f g x = f @@ g x
let ( **. ) f g = fun x -> f (g x)
let flip f x y = f y x
let bin_compose f g = fun x y -> f (g x y)
let bin_compose2 f g = fun x y -> f (g x) (g y)

module type T = sig
  type t
end

module type T1 = sig
  type 'a t
end

module type T2 = sig
  type ('a, 'b) t
end

module type F = functor () -> sig end

(* Show *)
module Show = struct
  module type I = sig
    type t

    val pp : Format.formatter -> t -> unit
  end

  module type S = sig
    type t

    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Make (PP : I) : S with type t = PP.t = struct
    include PP

    let show x = Format.asprintf "%a" pp x
  end
end

module Show1 = struct
  module type I = sig
    type 'a t

    val pp : formatter -> 'a t -> unit
  end

  module type S = sig
    type 'a t

    val show : 'a t -> string
  end

  module Make (I : I) : S = struct
    include I

    let show x = asprintf "%a" I.pp x
  end
end

module ShowAST = struct
  module AssocType = struct
    type t = Left | Right | None
  end

  module Pos = struct
    type t = Left | Right | None
  end

  module Level = struct
    type t =
      | L1
      | L2
      | L3
      | L4
      | L5
      | L6
      | L7
      | L8
      | L9
      | L10
      | L11
      | L12
      | L13
      | L14
      | L15
      | L16
      | L17
      | L18

    let to_int = function
      | L1 -> 1
      | L2 -> 2
      | L3 -> 3
      | L4 -> 4
      | L5 -> 5
      | L6 -> 6
      | L7 -> 7
      | L8 -> 8
      | L9 -> 9
      | L10 -> 10
      | L11 -> 11
      | L12 -> 12
      | L13 -> 13
      | L14 -> 14
      | L15 -> 15
      | L16 -> 16
      | L17 -> 17
      | L18 -> 18

    let t_of_max = L18
    let compare l1 l2 = Int.compare (to_int l1) (to_int l2)
  end

  module type I = sig
    type t

    val assoc_level : t -> Level.t
    val assoc_type : t -> AssocType.t
    val pp_neg : (Pos.t -> formatter -> t -> unit) -> formatter -> t -> unit
  end

  module type S = Show.S

  module Make (I : I) : S with type t := I.t = struct
    module Base = struct
      type t = I.t

      let rec y_pp f =
        f @@ fun pos lvl fmt x ->
        match (compare lvl (I.assoc_level x), I.assoc_type x, pos) with
        | -1, _, _ -> fprintf fmt "(%a)" (y_pp f pos lvl) x
        | 0, AssocType.Left, Pos.Right -> fprintf fmt "(%a)" (y_pp f pos lvl) x
        | 0, AssocType.Right, Pos.Left -> fprintf fmt "(%a)" (y_pp f pos lvl) x
        | _ -> y_pp f pos lvl fmt x

      let pp fmt x =
        y_pp
          (fun f _ _ fmt x ->
            I.pp_neg (fun pos fmt x -> f pos Level.L18 fmt x) fmt x)
          Pos.None Level.t_of_max fmt x
    end

    include Show.Make (Base)
  end
end

module Ex = struct
  module Base : sig
    type t =
      | Var of string
      | Lam of string * t
      | App of t * t
      | Int of int
      | Add of t * t

    include ShowAST.I with type t := t
  end = struct
    type t =
      | Var of string
      | Lam of string * t
      | App of t * t
      | Int of int
      | Add of t * t

    let assoc_level : t -> ShowAST.Level.t = function
      | Var _ | Int _ -> L1
      | App _ -> L4
      | Add _ -> L8
      | Lam _ -> L18

    let assoc_type = function
      | Var _ | Int _ | Lam _ -> ShowAST.AssocType.None
      | App _ | Add _ -> ShowAST.AssocType.Left

    let pp_neg f fmt e =
      match e with
      | Var x -> fprintf fmt "%s" x
      | Int n -> fprintf fmt "%d" n
      | Lam (x, e) -> fprintf fmt "λ%s. %a" x (f ShowAST.Pos.None) e
      | App (e1, e2) ->
          fprintf fmt "%a %a" (f ShowAST.Pos.Left) e1 (f ShowAST.Pos.Right) e2
      | Add (e1, e2) ->
          fprintf fmt "%a + %a" (f ShowAST.Pos.Left) e1 (f ShowAST.Pos.Right) e2
  end

  open ShowAST.Make (Base)
  open Base

  (* let () = *)
  (*   printf "%a\n" pp *)
  (*     (Lam ("x", App (Lam ("y", Var "y"), App (Lam ("y", Var "y"), Var "x")))) *)
end

(* Eq *)

module Eq = struct
  module type I = sig
    type t

    val ( = ) : t -> t -> bool
  end

  module type S = sig
    include I

    val ( <> ) : t -> t -> bool
  end

  module Make (E : I) = struct
    include E

    let ( <> ) x y = not (x = y)
  end
end

module Eq1 = struct
  module type I = sig
    type 'a t

    val ( = ) : 'a t -> 'a t -> bool
  end

  module type S = sig
    include I

    val ( <> ) : 'a t -> 'a t -> bool
  end

  module Make (E : I) = struct
    include E

    let ( <> ) x y = not (x = y)
  end
end

(* Order *)
module Order = struct
  module type I = sig
    type t

    val compare : t -> t -> int
  end

  module type S = sig
    include I

    val ( <= ) : t -> t -> bool
    val ( < ) : t -> t -> bool
    val ( > ) : t -> t -> bool
    val ( >= ) : t -> t -> bool

    include Eq.S with type t := t
  end

  module Make (O : I) = struct
    include O

    let ( <= ) x y = compare x y <= 0
    let ( < ) x y = compare x y < 0
    let ( > ) x y = compare x y > 0
    let ( >= ) x y = compare x y >= 0

    include Eq.Make (struct
      type t = O.t

      let ( = ) = fun x y -> compare x y = 0
    end)
  end
end

module type ShowEqOrder = sig
  include Show.S
  include Eq.S with type t := t
  include Order.S with type t := t
end
