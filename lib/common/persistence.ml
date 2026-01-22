module type I = sig
  type t

  val name : string
  val default : t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  val to_yojson : t -> Yojson.Safe.t
end

module type S = sig
  type t

  val get : unit -> t
  val save : t -> unit
end

module Make (I : I) : S with type t = I.t = struct
  include I

  let rootpath = "config"

  let () =
    if not @@ Sys.file_exists rootpath then Sys.mkdir rootpath 0o755 else ()

  let filepath = Printf.sprintf "%s/%s.json" rootpath I.name
  let save x = Yojson.Safe.to_file filepath @@ I.to_yojson x
  let () = if not @@ Sys.file_exists filepath then save I.default else ()
  let get () = Result.get_ok @@ I.of_yojson @@ Yojson.Safe.from_file filepath
end
