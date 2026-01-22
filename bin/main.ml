open Common.All
open Format
open StagedS0R0

(* Usage:
   - <exec> -repl: start REPL
   - <exec> <filename>: run from file
*)

let () =
  let args = Sys.argv in
  if Array.length args = 2 && args.(1) = "-repl" then (
    info "Starting REPL mode";
    Top.run_repl ())
  else if Array.length args = 2 then (
    let filename = args.(1) in
    info "Running from file: %s" filename;
    Top.run_from_file filename)
  else (
    eprintf "Usage:\n";
    eprintf "  %s -repl          Start REPL mode\n" args.(0);
    eprintf "  %s <filename>    Run from file\n" args.(0);
    exit 1)
