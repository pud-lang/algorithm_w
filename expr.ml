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


let string_of_expr expr : string =
  let rec f is_simple = function
    | Var name -> name
    | Call(fn_expr, arg_list) ->
        f true fn_expr ^ "(" ^ String.concat ", " (List.map (f false) arg_list) ^ ")"
    | Fun(param_list, body_expr) ->
        let fun_str =
          "fun " ^ String.concat " " param_list ^ " -> " ^ f false body_expr
        in
        if is_simple then "(" ^ fun_str ^ ")" else fun_str
    | Let(var_name, value_expr, body_expr) ->
        let let_str =
          "let " ^ var_name ^ " = " ^ f false value_expr ^ " in " ^ f false body_expr
        in
        if is_simple then "(" ^ let_str ^ ")" else let_str
  in
  f false expr

let string_of_ty ty : string =
  let id_name_map = Hashtbl.create 10 in
  let count = ref 0 in
  let next_name () =
    let i = !count in
    incr count ;
    let name = String.make 1 (Char.chr (97 + i mod 26)) ^
      if i >= 26 then string_of_int (i / 26) else ""
    in
    name
  in
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
