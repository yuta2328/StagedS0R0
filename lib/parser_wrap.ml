open Common.All
open SourcePosition

let parse lexbuf =
  try Result.ok @@ Parser.main_exp Lexer.token lexbuf
  with _ ->
    let sp =
      {
        start = Lexing.lexeme_start_p lexbuf;
        end_ = Lexing.lexeme_end_p lexbuf;
      }
    in
    Result.error @@ show sp
