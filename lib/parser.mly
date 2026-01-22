%{ module Msp_eff = struct end (* omajinai (https://github.com/ocaml/dune/issues/2450) *)   
   open Common.All.SourcePosition
   open Common.Ext.Annotated
   open Front %}

%token <string> LVAR
%token <int> INT
%token <string> STRING

%token PLUS    // '+'
%token MINUS    // '-'
%token TIMES    // '*'
%token DIV  // '/'
%token EQ   // '='
%token NEQ  // '!='
%token LT   // '<'
%token GT   // '>'
%token LE   // '<='
%token GE   // '>='
%token AND  // '&&'
%token OR   // '||'

%token LPAREN // '('
%token RPAREN // ')'
%token LBRACE // '{'
%token RBRACE // '}'

%token DEF // '='
%token ARROW // '->'

// Keyword

%token TRUE
%token FALSE
%token FUN
%token CFUN
%token IF
%token THEN
%token ELSE
%token LET
%token CLET
%token IN
%token SHIFT  (* shift0 *)
%token THROW
%token TO

%token EOF

%nonassoc IN ARROW TO ELSE
%left OR
%left AND
%nonassoc EQ NEQ LT GT LE GE
%left PLUS MINUS
%left TIMES DIV
%left TRUE FALSE LVAR INT STRING LPAREN LBRACE

%start <Front.Exp.t> main_exp

%%

main_exp:
  | e = exp EOF { e }

(* exp *)

var:
  | s = LVAR { { body = s ; attr = { start = $startpos ; end_ = $endpos } } }

base_exp:
  | x = STRING { Exp.Const.String x }
  | x = INT { Exp.Const.Int x }
  | TRUE { Exp.Const.Bool true }
  | FALSE { Exp.Const.Bool false }

arg_exp:
  | e = base_exp { { body = Front.Exp.Const e ; attr = { start = $startpos ; end_ = $endpos } } }
  | v = var { { body = Var v ; attr = { start = $startpos ; end_ = $endpos } } }
  | LBRACE e = exp RBRACE { { body = Reset0 e ; attr = { start = $startpos ; end_ = $endpos }} }
  | LPAREN e = exp RPAREN { { body = e.body ; attr = { start = $startpos ; end_ = $endpos } } }

exp:
  | e = arg_exp { e }
  | e1 = exp e2 = arg_exp { { body = App (e1, e2) ; attr = { start = $startpos; end_ = $endpos } } }
  | e1 = exp PLUS e2 = exp { { body = OpApp (OP "+", e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | e1 = exp MINUS e2 = exp { { body = OpApp (OP "-", e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | e1 = exp TIMES e2 = exp { { body = OpApp (OP "*", e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | e1 = exp DIV e2 = exp { { body = OpApp (OP "/", e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | e1 = exp EQ e2 = exp { { body = OpApp (OP "==", e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | e1 = exp NEQ e2 = exp { { body = OpApp (OP "<>", e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | e1 = exp LT e2 = exp { { body = OpApp (OP "<", e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | e1 = exp GT e2 = exp { { body = OpApp (OP ">=", e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | e1 = exp LE e2 = exp { { body = OpApp (OP "<", e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | e1 = exp GE e2 = exp { { body = OpApp (OP "<=", e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | e1 = exp AND e2 = exp { { body = OpApp (OP "&&", e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | e1 = exp OR e2 = exp { { body = OpApp (OP "||", e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | IF e1 = exp THEN e2 = exp ELSE e3 = exp { { body = If (e1, e2, e3); attr = { start = $startpos ; end_ = $endpos } } }
  | LET x = var DEF e1 = exp IN e2 = exp { { body = Let (x, e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | CLET x = var DEF e1 = exp IN e2 = exp { { body = CLet (x, e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | SHIFT v = var IN e = exp { { body = Shift0 (v, e) ; attr = { start = $startpos ; end_ = $endpos } } }
  | THROW e1 = exp TO e2 = exp { { body = Throw (e1, e2) ; attr = { start = $startpos ; end_ = $endpos } } }
  | FUN x = var ARROW e = exp { { body = Lam (x, e) ; attr = { start = $startpos ; end_ = $endpos } } }
  | CFUN x = var ARROW e = exp { { body = CLam (x, e) ; attr = { start = $startpos ; end_ = $endpos } } }
