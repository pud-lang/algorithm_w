open Expr

let current_id = ref 0

let next_id () =
  let id = !current_id in
  current_id := id + 1 ;
  id

let reset_id () = current_id := 0


let new_var level = TVar (ref (Unbound(next_id (), level)))
let new_gen_var () = TVar (ref (Generic(next_id ())))

exception Error of string
let error msg = raise (Error msg)

(* 提供了一个类型环境，用于跟踪类型变量和表达式的类型。 *)
(* 定义了一个名为 Env 的模块，封装了与类型环境相关的函数和类型。 *)
module Env = struct
  module StringMap = Map.Make (String)
  (* 定义了一个 env 类型，它是一个映射，将字符串（变量名）映射到 ty 类型。 *)
  type env = ty StringMap.t
  (* 定义了一个空的类型环境。 *)
  let empty : env = StringMap.empty
  (* 定义了一个函数 extend，用于在环境中添加新的名称和类型绑定。 *)
  let extend env name ty = StringMap.add name ty env
  (* 定义了一个函数 lookup，用于在环境中查找给定名称的类型。 *)
  let lookup env name = StringMap.find name env
end

(* 用于进行所谓的“发生检查”（occurs check），这是类型推断中防止类型
   变量递归引用自己的一个重要步骤。
   定义了一个函数 occurs_check_adjust_levels，它检查给定类型 ty 是否
   包含特定的类型变量 tvar_id。如果是，这表示类型定义递归，会引发错误。
   同时，该函数还会调整类型变量的级别（level），以便更好地处理类型推断
   中的泛型和作用域。*)
let occurs_check_adjust_levels tvar_id tvar_level ty =
  let rec f = function
      (* 如果类型变量已链接到另一个类型，递归地检查该类型。 *)
    | TVar {contents = Link ty} -> f ty
      (* 泛型类型变量不应出现在这里。 *)
    | TVar {contents = Generic _} -> assert false
      (* 如果未绑定类型变量的 ID 与给定的 tvar_id 相同，
         则表示发生了递归类型引用，这是错误的。
         否则，调整该类型变量的级别。 *)
    | TVar ({contents = Unbound(other_id, other_level)} as other_tvar) ->
        if other_id = tvar_id then
          error "recursive types"
        else
          if other_level > tvar_level then
            other_tvar := Unbound(other_id, tvar_level)
          else
            ()
      (* 递归地检查这些复合类型的组成部分。 *)
    | TApp(ty, ty_arg_list) ->
        f ty ;
        List.iter f ty_arg_list
    | TArrow(param_ty_list, return_ty) ->
        List.iter f param_ty_list ;
        f return_ty
      (* 基本类型常量无需进一步检查。 *)
    | TConst _ -> ()
  in
  f ty

(* 实现 Hindley-Milner 类型推断算法中的核心部分之一，
   即类型的统一（unification）算法。
   统一算法的目的是确定两个类型是否可以一致，
   如果可以，它会相应地修改类型变量以反映这种一致性。 *)
(* 统一算法 unify 接受两个类型 ty1 和 ty2，并尝试确定这两个
   类型是否可以“统一”（即它们是否可以表示相同的类型）。如果两者可以统一，
   该算法可能会修改类型变量的引用，使它们引用相同的具体类型。 *)
let rec unify ty1 ty2 =
  (* 如果两个类型引用相同的对象，则它们已经统一，无需进一步操作。 *)
  if ty1 == ty2 then () else
  (* 对类型对进行模式匹配，根据类型的结构采取不同的行动。 *)
  match (ty1, ty2) with
      (* 如果两个类型都是相同的类型常量（如两个 int），则它们已经统一。 *)
    | TConst name1, TConst name2 when name1 = name2 -> ()
      (* 如果两个类型都是类型应用（如两个列表类型 list[int] 和 list[string]），
         则递归地统一它们的主体和类型参数列表。 *)
    | TApp(ty1, ty_arg_list1), TApp(ty2, ty_arg_list2) ->
        unify ty1 ty2 ;
        List.iter2 unify ty_arg_list1 ty_arg_list2
      (* 如果两个类型都是函数类型，递归地统一它们的参数类型列表和返回类型。 *)
    | TArrow(param_ty_list1, return_ty1), TArrow(param_ty_list2, return_ty2) ->
        List.iter2 unify param_ty_list1 param_ty_list2 ;
        unify return_ty1 return_ty2
      (* 如果其中一个类型是一个已经被绑定的类型变量，则递归地统一引用的类型。 *)
    | TVar {contents = Link ty1}, ty2 | ty1, TVar {contents = Link ty2} -> unify ty1 ty2
      (* 如果两个类型都是未绑定的类型变量且具有相同的 ID，则它们是同一个变量的两个引用，
         此时应该触发错误，因为这表示递归类型。 *)
    | TVar {contents = Unbound(id1, _)}, TVar {contents = Unbound(id2, _)} when id1 = id2 ->
        assert false
      (* 如果其中一个类型是未绑定的类型变量，另一个是具体类型或另一个未绑定的变量，
         则进行 occurs check 调整级别，如果通过，则将该类型变量绑定到具体类型上。 *)
    | TVar ({contents = Unbound(id, level)} as tvar), ty
    | ty, TVar ({contents = Unbound(id, level)} as tvar) ->
        occurs_check_adjust_levels id level ty ;
        tvar := Link ty
    | _, _ -> error ("cannot unify types " ^ string_of_ty ty1 ^ " and " ^ string_of_ty ty2)


(* generalize 函数的作用是将一个类型中的未绑定的类型变量泛化为泛型类型变量。
   这通常在将局部变量的类型添加到环境时进行，这时局部变量的类型可能包含了只
   在局部作用域有效的类型变量。 *)
(* 接受一个“级别”（level）和一个类型（ty），并将该类型中所有高于这个级别的未
   绑定的类型变量转换为泛型类型变量。*)
let rec generalize level = function
    (* 如果是一个未绑定的类型变量且其级别高于给定的级别，则将其转换为泛型类型变量。 *)
  | TVar {contents = Unbound(id, other_level)} when other_level > level ->
      TVar (ref (Generic id))
    (* 对于类型应用和函数类型，递归地泛化它们的部分（主体、参数、返回类型等）。 *)
  | TApp(ty, ty_arg_list) ->
      TApp(generalize level ty, List.map (generalize level) ty_arg_list)
  | TArrow(param_ty_list, return_ty) ->
      TArrow(List.map (generalize level) param_ty_list, generalize level return_ty)
    (* 如果类型变量已经被绑定到另一个类型，则递归地泛化这个绑定的类型。 *)
  | TVar {contents = Link ty} -> generalize level ty
    (* 对于其他类型（如类型常量、已经是泛型或其他未绑定类型变量），保持不变。 *)
  | TVar {contents = Generic _} | TVar {contents = Unbound _} | TConst _ as ty -> ty

(* instantiate 函数用于将一个泛型类型具体化为一个特定的类型实例。
   这通常发生在将一个泛型函数应用到具体参数时。*)
(* 接受一个级别和一个类型，并返回一个新的类型，
   其中所有的泛型类型变量被替换为新的未绑定的类型变量。*)
let instantiate level ty =
  (* 用于记录哪些泛型类型变量被替换为哪些新的类型变量。 *)
  let id_var_map = Hashtbl.create 10 in
  let rec f ty = match ty with
    | TConst _ -> ty
      (* 如果是泛型类型变量，则检查是否已经在 id_var_map 中有对应的新变量，
         如果没有，则创建一个新的未绑定类型变量并记录下来。 *)
    | TVar {contents = Link ty} -> f ty
    | TVar {contents = Generic id} -> begin
        try
          Hashtbl.find id_var_map id
        with Not_found ->
          let var = new_var level in
          Hashtbl.add id_var_map id var ;
          var
      end
    | TVar {contents = Unbound _} -> ty
      (* 对于类型应用和函数类型，递归地具体化它们的部分。 *)
    | TApp(ty, ty_arg_list) ->
        TApp(f ty, List.map f ty_arg_list)
    | TArrow(param_ty_list, return_ty) ->
        TArrow(List.map f param_ty_list, f return_ty)
  in
  f ty


(* 用于匹配函数类型，并提取函数参数的类型列表和返回类型。
   输入：num_params（参数的数量）和一个类型 ty。
   确保 ty 是一个函数类型（TArrow）且其参数数量与 num_params 匹配。*)
let rec match_fun_ty num_params = function
    (* 如果 ty 是一个箭头类型（TArrow），并且参数数量正确，则返回参数类型列表和返回类型。 *)
  | TArrow(param_ty_list, return_ty) ->
      if List.length param_ty_list <> num_params then
        error "unexpected number of arguments"
      else
        param_ty_list, return_ty
    (* 如果 ty 是一个链接到另一个类型的类型变量，则递归地匹配该类型。 *)
  | TVar {contents = Link ty} -> match_fun_ty num_params ty
    (* 如果 ty 是一个未绑定的类型变量，则创建一个新的函数类型（带有正确数量的参数），
       并将该类型变量链接到此新创建的函数类型。 *)
  | TVar ({contents = Unbound(id, level)} as tvar) ->
      let param_ty_list = 
        let rec f = function
          | 0 -> []
          | n -> new_var level :: f (n - 1)
        in
        f num_params
      in
      let return_ty = new_var level in
      tvar := Link (TArrow(param_ty_list, return_ty)) ;
      param_ty_list, return_ty
    (* 其他情况，抛出错误，表示期待了一个函数类型。 *)
  | _ -> error "expected a function"

(* 类型推断算法的核心，用于推断表达式的类型。
   输入：环境 env（包含变量的类型绑定），级别 level，和表达式 expr。
   根据表达式的结构和环境中的类型信息，推断出表达式的类型。 *)
let rec infer env level = function
    (* 查找变量在环境中的类型，并具体化（instantiate）它。 *)
  | Var name -> begin
      try
        instantiate level (Env.lookup env name)
      with Not_found -> error ("variable " ^ name ^ " not found")
    end
    (* 对于函数抽象，为每个参数创建一个新的类型变量，并在新的环境中推断
       函数体的类型，最后返回函数类型（TArrow）。 *)
  | Fun(param_list, body_expr) ->
      let param_ty_list = List.map (fun _ -> new_var level) param_list in
      let fn_env = List.fold_left2
        (fun env param_name param_ty -> Env.extend env param_name param_ty)
        env param_list param_ty_list
      in
      let return_ty = infer fn_env level body_expr in
      TArrow(param_ty_list, return_ty)
    (* 推断 value_expr 的类型，泛化化（generalize）它，然后在添加了这个
       新绑定的环境中推断 body_expr 的类型。 *)
  | Let(var_name, value_expr, body_expr) ->
      let var_ty = infer env (level + 1) value_expr in
      let generalized_ty = generalize level var_ty in
      infer (Env.extend env var_name generalized_ty) level body_expr
    (* 推断函数表达式的类型，并确保它是一个函数类型，其参数数量与实际参
       数列表匹配。然后对每个实际参数进行类型推断，并与函数参数的类型进
       行统一（unify）。最后返回函数的返回类型。*)
  | Call(fn_expr, arg_list) ->
      let param_ty_list, return_ty =
        match_fun_ty (List.length arg_list) (infer env level fn_expr)
      in
      List.iter2
        (fun param_ty arg_expr -> unify param_ty (infer env level arg_expr))
        param_ty_list arg_list
      ;
      return_ty