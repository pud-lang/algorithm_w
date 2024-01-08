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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

open Expr
open Infer

let replace_ty_constants_with_vars var_name_list ty =
  let env = List.fold_left
    (fun env var_name -> Env.extend env var_name (new_gen_var ()))
    Env.empty var_name_list
  in
  let rec f ty = match ty with
    | TConst name -> begin
        try
          Env.lookup env name
        with Not_found -> ty
      end
    | TVar _ -> ty
    | TApp(ty, ty_arg_list) ->
        TApp(f ty, List.map f ty_arg_list)
    | TArrow(param_ty_list, return_ty) ->
        TArrow(List.map f param_ty_list, f return_ty)
  in
  f ty

# 43 "parser.ml"
let yytransl_const = [|
  258 (* FUN *);
  259 (* LET *);
  260 (* IN *);
  261 (* FORALL *);
  262 (* LPAREN *);
  263 (* RPAREN *);
  264 (* LBRACKET *);
  265 (* RBRACKET *);
  266 (* ARROW *);
  267 (* EQUALS *);
  268 (* COMMA *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\004\000\004\000\004\000\007\000\007\000\
\007\000\007\000\008\000\008\000\009\000\009\000\006\000\006\000\
\005\000\005\000\005\000\005\000\010\000\010\000\010\000\011\000\
\011\000\000\000\000\000\000\000"

let yylen = "\002\000\
\002\000\002\000\002\000\001\000\006\000\004\000\001\000\003\000\
\004\000\003\000\001\000\002\000\001\000\003\000\001\000\005\000\
\001\000\004\000\003\000\007\000\001\000\004\000\003\000\001\000\
\003\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\007\000\000\000\000\000\000\000\
\026\000\000\000\000\000\021\000\000\000\027\000\000\000\000\000\
\000\000\028\000\015\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\003\000\012\000\000\000\000\000\008\000\010\000\000\000\000\000\
\000\000\023\000\000\000\000\000\000\000\019\000\000\000\006\000\
\000\000\000\000\009\000\018\000\000\000\000\000\022\000\000\000\
\000\000\014\000\000\000\025\000\016\000\005\000\000\000\020\000"

let yydgoto = "\004\000\
\009\000\014\000\018\000\039\000\044\000\020\000\011\000\022\000\
\040\000\016\000\045\000"

let yysindex = "\035\000\
\023\255\027\255\026\255\000\000\000\000\012\255\029\255\023\255\
\000\000\039\000\040\255\000\000\002\255\000\000\047\000\035\255\
\041\255\000\000\000\000\048\000\012\255\042\255\039\255\044\255\
\000\000\016\255\043\255\028\255\000\000\027\255\027\255\012\255\
\000\000\000\000\023\255\023\255\000\000\000\000\045\255\047\255\
\027\255\000\000\027\255\046\255\051\255\000\000\052\255\000\000\
\058\255\023\255\000\000\000\000\048\255\027\255\000\000\027\255\
\023\255\000\000\054\255\000\000\000\000\000\000\027\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\002\000\
\000\000\000\000\000\000\000\000\011\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\255\000\000\
\000\000\000\000\000\000\003\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\006\000\003\000\000\000\000\000\235\255\
\015\000\000\000\217\255"

let yytablesize = 270
let yytable = "\034\000\
\004\000\017\000\012\000\053\000\015\000\019\000\010\000\013\000\
\027\000\024\000\047\000\024\000\021\000\024\000\060\000\028\000\
\005\000\006\000\007\000\011\000\011\000\008\000\038\000\005\000\
\006\000\007\000\012\000\012\000\008\000\023\000\017\000\013\000\
\013\000\046\000\042\000\001\000\002\000\003\000\025\000\043\000\
\048\000\049\000\030\000\052\000\031\000\026\000\029\000\033\000\
\032\000\036\000\037\000\035\000\041\000\051\000\059\000\013\000\
\050\000\054\000\061\000\055\000\056\000\057\000\062\000\063\000\
\058\000\064\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\000\000\000\000\004\000\
\017\000\000\000\017\000\000\000\004\000\017\000"

let yycheck = "\021\000\
\000\000\000\000\001\001\043\000\002\000\003\000\001\000\006\001\
\007\001\007\001\032\000\009\001\001\001\008\000\054\000\013\000\
\001\001\002\001\003\001\009\001\010\001\006\001\007\001\001\001\
\002\001\003\001\001\001\001\001\006\001\001\001\005\001\006\001\
\006\001\031\000\007\001\001\000\002\000\003\000\000\000\012\001\
\035\000\036\000\008\001\041\000\010\001\006\001\000\000\000\000\
\008\001\011\001\007\001\010\001\010\001\007\001\007\001\007\001\
\012\001\012\001\056\000\009\001\009\001\004\001\057\000\010\001\
\050\000\063\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\255\255\255\255\007\001\
\007\001\255\255\009\001\255\255\012\001\012\001"

let yynames_const = "\
  FUN\000\
  LET\000\
  IN\000\
  FORALL\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  ARROW\000\
  EQUALS\000\
  COMMA\000\
  EOF\000\
  "

let yynames_block = "\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                    ( _1 )
# 212 "parser.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 46 "parser.mly"
                    ( _1 )
# 219 "parser.ml"
               : Expr.ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty_forall) in
    Obj.repr(
# 49 "parser.mly"
                    ( _1 )
# 226 "parser.ml"
               : Expr.ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expr) in
    Obj.repr(
# 52 "parser.mly"
                                        ( _1 )
# 233 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                                        ( Let(_2, _4, _6) )
# 242 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                                        ( Fun(_2, _4) )
# 250 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                                                      ( Var _1 )
# 257 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                                                      ( _2 )
# 264 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'simple_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_comma_list) in
    Obj.repr(
# 59 "parser.mly"
                                                      ( Call(_1, _3) )
# 272 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple_expr) in
    Obj.repr(
# 60 "parser.mly"
                                                      ( Call(_1, []) )
# 279 "parser.ml"
               : 'simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
                        ( [_1] )
# 286 "parser.ml"
               : 'ident_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ident_list) in
    Obj.repr(
# 64 "parser.mly"
                        ( _1 :: _2 )
# 294 "parser.ml"
               : 'ident_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                                  ( [_1] )
# 301 "parser.ml"
               : 'expr_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_comma_list) in
    Obj.repr(
# 68 "parser.mly"
                                  ( _1 :: _3 )
# 309 "parser.ml"
               : 'expr_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 71 "parser.mly"
                                              ( _1 )
# 316 "parser.ml"
               : 'ty_forall))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 72 "parser.mly"
                                              ( replace_ty_constants_with_vars _3 _5 )
# 324 "parser.ml"
               : 'ty_forall))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_ty) in
    Obj.repr(
# 75 "parser.mly"
                                                      ( _1 )
# 331 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 76 "parser.mly"
                                                      ( TArrow([], _4) )
# 338 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple_ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 77 "parser.mly"
                                                      ( TArrow([_1], _3) )
# 346 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'ty_comma_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 78 "parser.mly"
                                                      ( TArrow(_2 :: _4, _7) )
# 355 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
                                                  ( TConst _1 )
# 362 "parser.ml"
               : 'simple_ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'simple_ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty_comma_list) in
    Obj.repr(
# 82 "parser.mly"
                                                  ( TApp(_1, _3) )
# 370 "parser.ml"
               : 'simple_ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 83 "parser.mly"
                                                  ( _2 )
# 377 "parser.ml"
               : 'simple_ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 86 "parser.mly"
                              ( [_1] )
# 384 "parser.ml"
               : 'ty_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty_comma_list) in
    Obj.repr(
# 87 "parser.mly"
                              ( _1 :: _3 )
# 392 "parser.ml"
               : 'ty_comma_list))
(* Entry expr_eof *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry ty_eof *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry ty_forall_eof *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr_eof (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Expr.expr)
let ty_eof (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Expr.ty)
let ty_forall_eof (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : Expr.ty)
