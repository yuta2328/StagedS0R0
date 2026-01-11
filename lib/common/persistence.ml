module type I = sig
  type t

  val name: string
  val default: t
  val of_yojson: Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  val to_yojson: t -> Yojson.Safe.t
end

exception Config_parse_error of Yojson.Safe.t * string

module type S = sig
  type t

  val get: unit -> t
  val save: t -> unit
  val run_from_persistence: (unit -> 'w) -> 'w
  val run: t -> (unit -> 'w) -> 'w
end

module MakeFromPY(PY: I): S = struct
  open Effect
  open Effect.Deep

  type _ Effect.t += GetConfig : PY.t Effect.t

  type t = PY.t

  let rootpath = "config"
  let () = if not @@ Sys.file_exists rootpath then
      Sys.mkdir rootpath 0o755
    else
      ()
  
  let filepath = Printf.sprintf "%s/%s.json" rootpath PY.name

  let () = if not @@ Sys.file_exists filepath then
      let () = Yojson.Safe.to_file filepath @@ PY.to_yojson PY.default in
      ()
    else
      ()

  let get () = perform GetConfig

  let save cfg = Yojson.Safe.to_file filepath @@ PY.to_yojson cfg

  let run_from_persistence f =
    try f () with
    | effect GetConfig, k ->
      let cfg =
        if Sys.file_exists filepath then
          let json = Yojson.Safe.from_file filepath in
          match PY.of_yojson json with
          | Ok x -> x
          | Error msg -> raise (Config_parse_error (json, msg))
        else
          failwith @@ Printf.sprintf "Config file %s does not exist" filepath
      in continue k cfg

  let run (cfg: t) f =
    try f () with
    | effect GetConfig, k -> continue k cfg
end
