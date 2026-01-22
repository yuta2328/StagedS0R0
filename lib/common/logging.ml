open Format
open Myutil
module Log = Dolog.Log

(* let filepath = "logging" *)
(* let () = if not (Sys.file_exists filepath) then Unix.mkdir filepath 0o755 *)

(* let timestamp_filename () = *)
(*   let tm = Unix.localtime @@ Unix.time () in *)
(*   Printf.sprintf "%04d%02d%02d%02d%02d%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) *)
(*     tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec *)

(* let _timestamp = timestamp_filename () *)
(* let log_filename = Printf.sprintf "%s/%s.log" filepath _timestamp *)

let () = Log.set_output stdout
let () = Log.set_log_level Log.INFO
let info fmt = Log.info fmt
let warn fmt = Log.warn fmt
let error fmt = Log.error fmt

(* with BER MetaOCaml *)
(* let print_code s c = *)
(*   let fmt = match InnerLogConfig.config.out with *)
(*     | LogConfig.Stdout -> info "print_code is called and the code is generated to stdout" ; Format.std_formatter *)
(*     | LogConfig.Dir fp -> info "print_code is called and the code is generated to %s" fp; Format.formatter_of_out_channel @@ log_output_format s "ml" *)
(*   in *)
(*   Codelib.print_code fmt c *)

(* let print_c_format s k = *)
(*   match InnerLogConfig.config.out with *)
(*   | LogConfig.Stdout -> info "print_c_format is called and the code is generated to stdout" ; k Format.std_formatter *)
(*   | LogConfig.Dir fp -> *)
(*     info "print_c_format is called and the code is generated to %s" fp; *)
(*     let oc = log_output_format s "c" in  *)
(*     k (Format.formatter_of_out_channel oc); *)
(*     close_out oc *)

let trace (type f t) name (module In : Show.S with type t = f)
    (module Out : Show.S with type t = t) f x =
  let y = f x in
  let () =
    info "%s: %s" name @@ asprintf "@[<hov 1>%a@ ->@ %a@]" In.pp x Out.pp y
  in
  y
