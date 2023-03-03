open Lab4_vm

type prim = Add | Mul | Self

type expr =
  | Cst of int
  | Var of string
  | Let of string * expr * expr
  | Letfn of string * string list * expr * expr
  | App of string * expr list
  | Prim of prim * expr list
  | If of expr * expr * expr

module Flat = struct
  type expr =
    | Cst of int
    | Var of string
    | Let of string * expr * expr
    | App of string * expr list
    | If of expr * expr * expr
    | Prim of prim * expr list

  type ffun = string * string list * expr
end

type var = Para of string | Local of string | Temp
type venv = expr list


let rec remove_funs expr : Flat.expr =
  match expr with
  | Cst i -> Cst i
  | Prim (p, es) -> Prim (p, List.map remove_funs es)
  | If (e1, e2, e3) -> If (remove_funs e1, remove_funs e2, remove_funs e3)
  | Var s -> Var s
  | Let (s, e1, e2) -> Let (s, remove_funs e1, remove_funs e2)
  | Letfn (_, _, _, scope) -> remove_funs scope
  | App (s, es) -> App (s, List.map remove_funs es)

let rec collect_funs expr : Flat.ffun list =
  match expr with
  | Cst _ | Var _ -> []
  | Prim (_, es) -> List.concat (List.map collect_funs es)
  | Let (_, e1, e2) -> List.concat [ collect_funs e1; collect_funs e2 ]
  | Letfn (name, args, body, scope) ->
      List.concat
        [
          [ (name, args, remove_funs body) ];
          collect_funs body;
          collect_funs scope;
        ]
  | App (_, args) -> List.concat (List.map collect_funs args)
  | If (e1, e2, e3) ->
      List.concat [ collect_funs e1; collect_funs e2; collect_funs e3 ]

let generate_label_fresher (prefix : string) =
  let counter = ref 0 in
  fun () ->
    counter := counter.contents + 1;
    prefix ^ string_of_int counter.contents

let else_fresh = generate_label_fresher "__else__"
let exit_fresh = generate_label_fresher "__exit__"

let vindex venv x =
  let rec vindex_aux vvenv x acc =
    match vvenv with
    | [] -> raise Not_found
    | Temp :: rest -> vindex_aux rest x (acc + 1)
    | Local y :: rest -> if x == y then acc else vindex_aux rest x (acc + 1)
    | Para y :: rest -> if x == y then acc else vindex_aux rest x (acc + 1)
  in
  vindex_aux venv x 0

let rec compile_exprs venv (exprs : Flat.expr list) name num =
  let rec compile_exprs_aux venv exprs acc =
    match exprs with
    | [] -> acc
    | expr :: rest ->
        let expr_code = compile_expr venv expr name num in
        compile_exprs_aux (Temp :: venv) rest (acc @ expr_code)
  in
  compile_exprs_aux venv exprs []

and compile_expr venv expr name num : instr list =
  match expr with
  | Cst i -> [ Cst i ]
  | Prim (op, es) -> (
      match op with
      | Add | Mul | Self ->
          let es_code = compile_exprs venv es name num in
          let op_code : instr =
            match op with Add -> Add | Mul -> Mul | Self -> Call (name, num)
          in
          es_code @ [ op_code ])
  | Var x -> [ Var (vindex venv x) ]
  | Let (x, e1, e2) ->
      List.concat
        [
          compile_expr venv e1 name num;
          compile_expr (Local x :: venv) e2 name num;
          [ Swap; Pop ];
        ]
  | App (fn, args) ->
      let n = List.length args in
      let args_code = compile_exprs venv args name num in
      args_code @ [ Call (fn, n) ]
  | If (cond, ifso, ifelse) ->
      let elseLabel = else_fresh () in
      let exitLabel = exit_fresh () in
      List.concat
        [
          compile_expr venv cond name num;
          [ IFZERO elseLabel ];
          compile_expr venv ifso name num;
          [ GOTO exitLabel ];
          [ Label elseLabel ];
          compile_expr venv ifelse name num;
          [ Label exitLabel ];
        ]

let compile_fun (ffun : Flat.ffun) =
  let name, args, body = ffun in
  let n = List.length args in
  let venv = List.rev (List.map (fun a -> Para a) args) in
  List.concat [ [ Label name ]; compile_expr venv body name n; [ Ret n ] ]

let pre_process expr =
  let main = ("main", [], remove_funs expr) in
  let rest = collect_funs expr in
  main :: rest

let compile funs =
  let funs_code = List.concat (List.map compile_fun funs) in
  List.concat [ [ Call ("main", 0); Exit ]; funs_code ]

let compile_to_vm prog =
  let vmcode = prog |> pre_process |> compile in
  initVm (Array.of_list vmcode)

let exec_vm vm = run vm
let compile_and_exec prog = prog |> compile_to_vm |> exec_vm
