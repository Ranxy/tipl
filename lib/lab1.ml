
exception ExprError of string


module Naive  = struct
  
type expr =
| Cst of int
| Add of expr * expr
| Mul of expr * expr
| Var of string
| Let of string * expr * expr

let rec eval expr env = 
  match expr with
  | Cst a -> a
  | Add(e1,e2) -> eval e1 env  + eval e2 env
  | Mul(e1,e2) -> eval e1 env  * eval e2 env
  | Var(name) -> List.assoc name env
  | Let(name,e1,e2) -> eval e2 ((name,(eval e1 env))::env)
;;

end



module Nameless = struct

type expr = 
| Cst of int
| Add of expr * expr
| Mul of expr * expr
| Var of int
| Let of expr * expr


let rec eval expr s = 
  match expr with
  | Cst(i) -> i
  | Add(e1,e2) -> eval e1 s  + eval e2 s
  | Mul(e1,e2) -> eval e1 s * eval e2 s
  | Var(i) -> List.nth s i 
  | Let(e1,e2) -> eval e2 ((eval e1 s)::s)

  
;;

let index_of_cenv cenv x = 
  let rec go cenv n:int = 
    match cenv with
      | [] -> raise(ExprError "Notfound");
      | a::rest -> if a == x then n  else go rest n+1

  in go cenv 0
;;

let rec from_naive (from:Naive.expr) cenv =
  match from with
  | Naive.Cst(i) -> Cst(i)
  | Naive.Add(e1,e2) -> Add((from_naive e1 cenv),(from_naive e2 cenv))
  | Naive.Mul(e1,e2) -> Mul((from_naive e1 cenv),(from_naive e2 cenv))
  | Naive.Var(s) -> Var((index_of_cenv cenv s))
  | Naive.Let(x,e1,e2) -> Let((from_naive e1 cenv),(from_naive e2 (x::cenv)))
;;
end


module Instr = struct

type instr = 
| Cst of int 
| Add 
| Mul 
| Var of int 
| Pop 
| Swap

type oprand =  int
let rec eval (instrs:instr list) (stk:oprand list) = 
  match instrs,stk with
  | Cst(i)::rest,stk -> eval rest (i::stk)
  | Add::rest, a::b::stk -> eval rest (a+b::stk)
  | Mul::rest, a::b::stk -> eval rest (a*b::stk)
  | Var(i)::rest, stk -> eval rest ((List.nth stk i)::stk)
  | Swap::rest, a::b::stk -> eval rest (b::a::stk)
  | Pop::rest, _::stk -> eval rest stk
  | [],v::_ -> v
  | _ -> raise(ExprError "UNEXCEPTED")
;;

end

module NamelessStackVM = struct

type sv = Slocal | Stmp

let sindex senv i = 
  let rec go senv i acc = 
    match senv with
      | [] -> raise(ExprError "Not found")
      | Slocal::rest -> if i ==0 then acc else go rest (i-1) (acc+1)
      | Stmp::rest -> go rest i acc+1
  in
  go senv i 0
;;

let complie expr = 
  let rec go (expr:Nameless.expr) (senv:sv list) :Instr.instr list = 
    match expr with
      | Nameless.Cst(i) -> [Cst(i)]
      | Nameless.Var(i) -> [Var(sindex senv i )]
      | Nameless.Add(e1,e2) -> List.concat [(go e1 senv); (go e2 (Stmp::senv)); [Add]]
      | Nameless.Mul(e1,e2) -> List.concat [(go e1 senv); (go e2 (Stmp::senv)); [Mul]]
      | Nameless.Let(e1,e2) -> List.concat [(go e1 senv); (go e2 (Slocal::senv)); [Swap; Pop]]
  in
  go expr []
;;



end


module NaiveStackVM = struct

type sv = Slocal of string | Stmp

let sindex senv x = 
  let rec go senv x acc = 
    match senv with
      | [] -> raise(ExprError "Not found")
      | Slocal(v)::rest -> if x ==v then acc else go rest x (acc+1)
      | Stmp::rest -> go rest x acc+1
  in
  go senv x 0
;;

let complie expr = 
  let rec go (expr:Naive.expr) (senv:sv list) :Instr.instr list = 
    match expr with
      | Naive.Cst(i) -> [Cst(i)]
      | Naive.Var(i) -> [Var(sindex senv i )]
      | Naive.Add(e1,e2) -> List.concat [(go e1 senv); (go e2 (Stmp::senv)); [Add]]
      | Naive.Mul(e1,e2) -> List.concat [(go e1 senv); (go e2 (Stmp::senv)); [Mul]]
      | Naive.Let(x,e1,e2) -> List.concat [(go e1 senv); (go e2 (Slocal(x)::senv)); [Swap; Pop]]
  in
  go expr []
;;

end