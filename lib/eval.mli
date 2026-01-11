open Common.All
open Subst
open Exp

module ValueEnv : sig
  type t

  val empty : t
  val extend : t -> Level0.Var.t -> Value.t -> t
  val lookup : Level0.Var.t -> t -> Value.t option
end

module RuntimeError : sig
  type t = Msg of string

  include Show.S with type t := t
end

val eval : ValueEnv.t -> Level0.t -> (Value.t, RuntimeError.t) result
