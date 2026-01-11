let parse lexbuf =
  try Result.ok @@ Parser.main_exp Lexer.token lexbuf
  with Lexer.LexerError _ -> Result.error "parse_error"
