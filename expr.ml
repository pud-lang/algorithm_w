type name = string

type expr =
  | Var of name                (* 变量 *)
  | Call of expr * expr list   (* 函数应用，由一个表达式和一个表达式列表组成，表达函数调用。 *)
  | Fun of name list * expr    (* 函数抽象，表示匿名函数定义。由一个名字列表（参数）和一个表达式（函数体）组成。 *)
  | Let of name * expr * expr  (* 局部绑定，允许你定义局部变量。由变量名、变量的表达式和使用该变量的另一个表达式组成。 *)

type id = int
type level = int

type ty =
  | TConst of name             (* 类型常量，例如 int 或 bool。 *)
  | TApp of ty * ty list       (* 类型应用，类似于泛型。由一个类型和一个类型列表组成，例如 list[int]。 *)
  | TArrow of ty list * ty     (* 函数类型，由参数类型列表和返回类型组成，例如 (int, int) -> int。 *)
  | TVar of tvar ref           (* 类型变量，用于表示未知或可变的类型。这是 Hindley-Milner 类型系统中的核心概念之一。 *)

and tvar =
  | Unbound of id * level      (* 未绑定的类型变量，带有一个唯一标识符（id）和一个级别（level）。 *)
  | Link of ty                 (* 链接到另一个类型。当一个类型变量被确定为特定类型时，它可以链接到这个类型。 *)
  | Generic of id              (* 泛型类型变量，带有一个唯一标识符。 *)

(* 将 expr 类型的值转换为字符串。它定义了一个递归辅助函数 f 来处理不同的 expr 构造器。 *)
let string_of_expr expr : string =
  (* 定义了一个名为 f 的递归函数。
     is_simple 是一个布尔参数，用来确定表达式是否需要用括号包围。
     function 关键字用于模式匹配不同的 expr 构造器。 *)
  let rec f is_simple = function
      (* 处理 Var 构造器，直接返回变量名。 *)
    | Var name -> name
      (* 处理 Call 构造器，递归地处理函数表达式和参数列表，
         然后将它们组合成函数调用的字符串表示形式。*)
    | Call(fn_expr, arg_list) ->
        f true fn_expr ^ "(" ^ String.concat ", " (List.map (f false) arg_list) ^ ")"
      (* 处理 Fun 构造器，创建一个表示函数的字符串，递归地处理函数体。 *)
    | Fun(param_list, body_expr) ->
        let fun_str =
          "fun " ^ String.concat " " param_list ^ " -> " ^ f false body_expr
        in
        if is_simple then "(" ^ fun_str ^ ")" else fun_str
      (* 处理 Let 构造器，创建一个表示 let 绑定的字符串。 *)
    | Let(var_name, value_expr, body_expr) ->
        let let_str =
          "let " ^ var_name ^ " = " ^ f false value_expr ^ " in " ^ f false body_expr
        in
        if is_simple then "(" ^ let_str ^ ")" else let_str
  in
  f false expr

(* 将 ty 类型的值转换为字符串。 *)
let string_of_ty ty : string =
  (* 使用 Hashtbl.create 创建一个哈希表来存储泛型类型变量的名字。 *)
  let id_name_map = Hashtbl.create 10 in
  let count = ref 0 in
  (* next_name 函数生成新的泛型变量名。 *)
  let next_name () =
    let i = !count in
    incr count ;
    let name = String.make 1 (Char.chr (97 + i mod 26)) ^
      if i >= 26 then string_of_int (i / 26) else ""
    in
    name
  in
  (* 定义了一个递归函数 f 来处理 ty 的不同构造器。 *)
  let rec f is_simple = function
    | TConst name -> name
    | TApp(ty, ty_arg_list) ->
        f true ty ^ "[" ^ String.concat ", " (List.map (f false) ty_arg_list) ^ "]"
    | TArrow(param_ty_list, return_ty) ->
        let arrow_ty_str = match param_ty_list with
          | [param_ty] ->
              let param_ty_str = f true param_ty in
              let return_ty_str = f false return_ty in
              param_ty_str ^ " -> " ^ return_ty_str
          | _ ->
              let param_ty_list_str = String.concat ", " (List.map (f false) param_ty_list) in
              let return_ty_str = f false return_ty in
              "(" ^ param_ty_list_str ^ ") -> " ^ return_ty_str
        in
        if is_simple then "(" ^ arrow_ty_str ^ ")" else arrow_ty_str
    | TVar {contents = Generic id} -> begin
          try
            Hashtbl.find id_name_map id
          with Not_found ->
            let name = next_name () in
            Hashtbl.add id_name_map id name ;
            name
        end
    | TVar {contents = Unbound(id, _)} -> "_" ^ string_of_int id
    | TVar {contents = Link ty} -> f is_simple ty
  in
  let ty_str = f false ty in
  if !count > 0 then
    let var_names = Hashtbl.fold (fun _ value acc -> value :: acc) id_name_map [] in
    "forall[" ^ String.concat " " (List.sort String.compare var_names) ^ "] " ^ ty_str
  else
    ty_str
