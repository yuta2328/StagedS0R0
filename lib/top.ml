open Common.All
open Aux
open Infer
open Eval
open Format

type state = {
  type_ctx : TypeContext.t;
  poly_ctx : PolyContext.t;
  eval_env : ValueEnv.t;
}

type result = {
  inferred_type : Types.Level0.t;
  annotation : Types.Annotation.t;
  value : Exp.Value.t;
}

let initial_state : state =
  {
    type_ctx = TypeContext.empty;
    poly_ctx = PolyContext.empty;
    eval_env = ValueEnv.empty;
  }

let process state lexbuf =
  Result.get_ok (Parser_wrap.parse lexbuf) |> fun exp ->
  let exp = Trans.Exp.trans exp in
  Result.get_ok
    (infer { ty_cxt = state.type_ctx; poly_cxt = state.poly_ctx; exp })
  |> fun infer_result ->
  Result.get_ok (eval state.eval_env exp) |> fun value ->
  {
    inferred_type = infer_result.type_;
    annotation = infer_result.annotation;
    value;
  }

let run_repl () =
  let rec loop state =
    printf "> ";
    flush stdout;
    let line = read_line () in
    let lexbuf = Lexing.from_string line in
    match process state lexbuf with
    | exception Failure msg ->
        printf "Error: %s\n" msg;
        loop state
    | result ->
        printf "Inferred type: %s\n" (Types.Level0.show result.inferred_type);
        printf "Evaluated value: %s\n" (Exp.Value.show result.value);
        loop state
  in
  loop initial_state

let run_from_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let state = initial_state in
  match process state lexbuf with
  | exception Failure msg ->
      eprintf "Error: %s\n" msg;
      close_in ic
  | result ->
      printf "Inferred type: %s\n" (Types.Level0.show result.inferred_type);
      printf "Evaluated value: %s\n" (Exp.Value.show result.value);
      close_in ic
