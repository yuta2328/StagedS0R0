{
open Parser

exception LexerError of Lexing.position
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let alpha = ['A'-'Z' 'a'-'z']
let alnum = digit | alpha | '_'
let escapes = ['\\' '"' '\'' 'n' 't' 'r' 'b']

rule token = parse
    | '"' ([^'"''\\'] | '\\'['\x00'-'\xff'])+ '"' {
            let lexeme = Bytes.sub_string lexbuf.lex_buffer (lexbuf.lex_start_pos + 1) (lexbuf.lex_curr_pos - lexbuf.lex_start_pos - 2) in
            STRING ( Scanf.unescaped lexeme )
            }
    (* separators *)
    | '=' { DEF } | "->" { ARROW }
    (* operations *)
    | '+' { PLUS }  | '-' { MINUS }  | '*' { TIMES }  | '/' { DIV }
    | "==" { EQ } | "!=" { NEQ }
    | "<" { LT }  | ">" { GT } | "<=" { LE }  | ">=" { GE }
    | "&&" { AND }  | "||" { OR }
    (* delimiters *)
    | '(' { LPAREN }  | ')' { RPAREN }
    | '{' { LBRACE }  | '}' { RBRACE }
    (* keywords *)
    | "true" { TRUE }  | "false" { FALSE }
    | "cfun" { CFUN } | "fun" { FUN } | "shift0" { SHIFT }
    | "let" { LET } | "in" { IN }
    | "if" { IF } | "then" { THEN } | "else" { ELSE }
    | "throw" { THROW } | "to" { TO }
    (* base *)
    | alpha alnum* { LVAR (Lexing.lexeme lexbuf) }
    | space+ { token lexbuf }
    | digit+ as i { INT (int_of_string i) }
    | eof { EOF }
    | _ { raise ( LexerError (Lexing.lexeme_start_p lexbuf) ) }
