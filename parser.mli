type token =
  | IDENT of (string)
  | FUN
  | LET
  | IN
  | FORALL
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | ARROW
  | EQUALS
  | COMMA
  | EOF

val expr_eof :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.expr
val ty_eof :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.ty
val ty_forall_eof :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.ty
