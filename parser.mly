%{

open Expr
open Infer

(* 在给定的类型表达式 ty 中替换所有的类型常量（如 int, bool 等）
   为新生成的类型变量。这通常在类型推导的某些阶段使用，例如泛型
   函数的实例化。*)
let replace_ty_constants_with_vars var_name_list ty =
  (* 这里使用 List.fold_left 函数构建一个环境，它将 var_name_list 
     中的每个名称映射到一个新生成的泛型类型变量（new_gen_var ()）。
     初始环境是空的（Env.empty）。 *)
  let env = List.fold_left
    (fun env var_name -> Env.extend env var_name (new_gen_var ()))
    Env.empty var_name_list
  in
  (* 这是一个递归函数，用于遍历和替换类型表达式中的类型常量。
     对于类型常量 TConst，如果它的名称在环境中找到了对应的类型变量，
     则替换它；否则保留原样。对于复合类型（如 TApp 和 TArrow），
     递归地应用这个函数到它们的子类型上。 *)
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

%}

%token <string> IDENT
%token FUN LET IN FORALL
%token LPAREN RPAREN LBRACKET RBRACKET
%token ARROW EQUALS COMMA
%token EOF

%start expr_eof
%type <Expr.expr> expr_eof
%start ty_eof
%type <Expr.ty> ty_eof
%start ty_forall_eof
%type <Expr.ty> ty_forall_eof

%%

expr_eof:
  | expr EOF        { $1 }

ty_eof:
  | ty EOF          { $1 }

ty_forall_eof:
  | ty_forall EOF   { $1 }

expr:
  | simple_expr                         { $1 }
  | LET IDENT EQUALS expr IN expr       { Let($2, $4, $6) }
  | FUN ident_list ARROW expr           { Fun($2, $4) }

simple_expr:
  | IDENT                                             { Var $1 }
  | LPAREN expr RPAREN                                { $2 }
  | simple_expr LPAREN expr_comma_list RPAREN         { Call($1, $3) }
  | simple_expr LPAREN RPAREN                         { Call($1, []) }

ident_list:
  | IDENT               { [$1] }
  | IDENT ident_list    { $1 :: $2 }

expr_comma_list:
  | expr                          { [$1] }
  | expr COMMA expr_comma_list    { $1 :: $3 }

ty_forall:
  | ty                                        { $1 }
  | FORALL LBRACKET ident_list RBRACKET ty    { replace_ty_constants_with_vars $3 $5 }

ty:
  | simple_ty                                         { $1 }
  | LPAREN RPAREN ARROW ty                            { TArrow([], $4) }
  | simple_ty ARROW ty                                { TArrow([$1], $3) }
  | LPAREN ty COMMA ty_comma_list RPAREN ARROW ty     { TArrow($2 :: $4, $7) }

simple_ty:
  | IDENT                                         { TConst $1 }
  | simple_ty LBRACKET ty_comma_list RBRACKET     { TApp($1, $3) }
  | LPAREN ty RPAREN                              { $2 }
  
ty_comma_list:
  | ty                        { [$1] }
  | ty COMMA ty_comma_list    { $1 :: $3 }