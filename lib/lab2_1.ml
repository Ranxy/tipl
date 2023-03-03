exception ExprError of string

module Naive = struct
  type expr =
    | Cst of int
    | Add of expr * expr
    | Mul of expr * expr
    | Var of string
    | Let of string * expr * expr
    | Fn of string list * expr (* function(parameters, body)*)
    | App of expr * expr list (*call function arguments*)

  type value = Vint of int | Vclosure of env * string list * expr
  and env = (string * value) list

  let get_value v =
    match v with Vint vv -> vv | _ -> raise (ExprError "Value Not Int")

  let vadd v1 v2 =
    match (v1, v2) with
    | Vint a, Vint b -> Vint (a + b)
    | _ -> raise (ExprError "vadd failed")

  let vmul v1 v2 =
    match (v1, v2) with
    | Vint a, Vint b -> Vint (a * b)
    | _ -> raise (ExprError "vadd failed")

  let rec eval expr env =
    match expr with
    | Cst a -> Vint a
    | Add (e1, e2) -> vadd (eval e1 env) (eval e2 env)
    | Mul (e1, e2) -> vmul (eval e1 env) (eval e2 env)
    | Var name -> List.assoc name env
    | Let (name, e1, e2) -> eval e2 ((name, eval e1 env) :: env)
    | Fn (xs, e) -> Vclosure (env, xs, e)
    | App (fn, args) ->
        let env_closure, params, body =
          match eval fn env with
          | Vclosure (env_closure, params, body) -> (env_closure, params, body)
          | Vint _ -> raise (ExprError "eval response failed")
        in
        let vs = List.map (fun e -> eval e env) args in
        let vs =
          match Base.List.zip params vs with
          | Ok vs -> vs
          | _ -> raise (ExprError "arguments and parameters not match")
        in
        let fun_env = List.concat [ vs; env_closure ] in
        eval body fun_env
end

module Nameless = struct
  type expr =
    | Cst of int
    | Add of expr * expr
    | Mul of expr * expr
    | Var of int
    | Let of expr * expr
    | Fn of expr
    | App of expr * expr list

  type value = Vint of int | Vclosure of env * expr
  and env = value list

  let get_value v =
    match v with Vint vv -> vv | _ -> raise (ExprError "Value Not Int")

  let vadd v1 v2 =
    match (v1, v2) with
    | Vint a, Vint b -> Vint (a + b)
    | _ -> raise (ExprError "vadd failed")

  let vmul v1 v2 =
    match (v1, v2) with
    | Vint a, Vint b -> Vint (a * b)
    | _ -> raise (ExprError "vadd failed")

  let rec eval expr env =
    match expr with
    | Cst i -> Vint i
    | Add (e1, e2) -> vadd (eval e1 env) (eval e2 env)
    | Mul (e1, e2) -> vmul (eval e1 env) (eval e2 env)
    | Var i -> List.nth env i
    | Let (e1, e2) -> eval e2 (eval e1 env :: env)
    | Fn body -> Vclosure (env, body)
    | App (fn, args) ->
        let env_closure, expr =
          match eval fn env with
          | Vclosure (env_closure, expr) -> (env_closure, expr)
          | Vint _ -> raise (ExprError "eval response failed")
        in
        let vs = List.map (fun e -> eval e env) args in
        let fn_env = List.concat [ vs; env_closure ] in
        eval expr fn_env

  let index_of_cenv cenv x =
    let rec go cenv n : int =
      match cenv with
      | [] -> raise (ExprError "Notfound")
      | a :: rest -> if a == x then n else go rest n + 1
    in

    go cenv 0

  let rec from_naive (from : Naive.expr) cenv =
    match from with
    | Naive.Cst i -> Cst i
    | Naive.Add (e1, e2) -> Add (from_naive e1 cenv, from_naive e2 cenv)
    | Naive.Mul (e1, e2) -> Mul (from_naive e1 cenv, from_naive e2 cenv)
    | Naive.Var s -> Var (index_of_cenv cenv s)
    | Naive.Let (x, e1, e2) ->
        Let (from_naive e1 cenv, from_naive e2 (x :: cenv))
    | Naive.Fn (parameters, body) ->
        Fn (from_naive body (List.concat [ parameters; cenv ]))
    | Naive.App (fn, arguments) ->
        App (from_naive fn cenv, List.map (fun e -> from_naive e cenv) arguments)
end
