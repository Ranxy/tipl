open OUnit2;;
open Tipl.Lab1;;


let test_naive_eval _ = 
  let v = Naive.eval (Naive.Let("x",Naive.Cst(3), Naive.Add(Naive.Cst(2),Naive.Let("y",Naive.Add(Naive.Cst(1),Naive.Cst(3)),Naive.Mul(Naive.Var("y"),Naive.Var("x")))))) [] in 
  assert_equal v  14

let test_nameless_eval _ = 
  let v = Nameless.eval (Nameless.Let(Nameless.Cst(3),Nameless.Add(Nameless.Cst(2),Nameless.Mul(Nameless.Var(0),Nameless.Cst(3))))) [] in
  assert_equal v 11


let test_naive_to_nameless _ = 
  let naive_expr = Naive.Let("x",Naive.Cst(3), Naive.Add(Naive.Cst(2),Naive.Let("y",Naive.Add(Naive.Cst(1),Naive.Cst(3)),Naive.Mul(Naive.Var("y"),Naive.Var("x"))))) in
  let nameless_expr = Nameless.from_naive naive_expr [] in
  let naive_v = Naive.eval naive_expr [] in
  let nameless_v = Nameless.eval nameless_expr [] in
  assert_equal naive_v nameless_v;
  assert_equal nameless_v 14


let test_stack_machine_eval _ = 
  let stack_expr = [
    Instr.Cst(10);
    Instr.Cst(3);
    Instr.Var(0);
    Instr.Var(2);
    Instr.Mul;
    Instr.Swap;
    Instr.Pop;
    Instr.Add] in
  let v = Instr.eval stack_expr [] in
  assert_equal v 40


let test_nameless_to_stack _ = 
  let expr = Nameless.Let(Nameless.Cst(3),Nameless.Add(Nameless.Cst(2),Nameless.Mul(Nameless.Var(0),Nameless.Cst(3)))) in 
  let instrs = NamelessStackVM.complie expr in
  let nameless_v = Nameless.eval expr [] in
  let stack_v = Instr.eval instrs [] in
  assert_equal nameless_v stack_v;
  assert_equal stack_v 11


let test_naive_nameless_stack _ = 
  let expr = Naive.Let(
    "x",
    Naive.Cst(3), 
    Naive.Add(
      Naive.Cst(2),
      Naive.Let(
        "y",
        Naive.Add(Naive.Cst(1),Naive.Cst(3)),
        Naive.Mul(Naive.Var("y"),Naive.Var("x"))
      )
    )
  ) in
  let expr_nameless = Nameless.from_naive expr [] in
  let naive_instrs = NaiveStackVM.complie expr in
  let nameless_instrs = NamelessStackVM.complie expr_nameless in 
  let value_naive = Naive.eval expr [] in 
  let value_nameless = Nameless.eval expr_nameless [] in 
  let value_naive_stack = Instr.eval naive_instrs [] in
  let value_nameless_stack = Instr.eval nameless_instrs [] in 
  assert_equal value_naive value_nameless;
  assert_equal value_nameless value_nameless_stack;
  assert_equal value_nameless_stack value_naive_stack;
  assert_equal value_naive_stack 14

let suite = 
  "TestList" >:::[
    "test_naive_eval" >:: test_naive_eval;
    "test_nameless_eval" >::test_nameless_eval;
    "test_naive_to_nameless" >:: test_naive_to_nameless;
    "test_stack_machine_eval" >:: test_stack_machine_eval;
    "test_nameless_to_stack" >:: test_nameless_to_stack;
    "test_naive_nameless_stack" >:: test_naive_nameless_stack
  ]

let () = run_test_tt_main suite