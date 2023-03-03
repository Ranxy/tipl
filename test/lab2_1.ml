open OUnit2
open Tipl.Lab2_1

let test_naive_eval_fn_simple _ =
  let son =
    Naive.App
      ( Naive.Fn ([ "bar" ], Naive.Add (Naive.Var "bar", Naive.Var "foo")),
        [ Naive.Cst 2 ] )
  in

  let v =
    Naive.eval (Naive.App (Naive.Fn ([ "foo" ], son), [ Naive.Cst 3 ])) []
  in
  assert_equal v (Naive.Vint 5)

let test_naive_eval_fn_two _ =
  let v =
    Naive.eval
      (Naive.Let
         ( "x",
           Naive.App
             ( Naive.Fn ([ "x" ], Naive.Var "x"),
               [
                 Naive.Add
                   ( Naive.App
                       ( Naive.Fn
                           ([ "x" ], Naive.Mul (Naive.Cst 2, Naive.Var "x")),
                         [ Naive.Cst 3 ] ),
                     Naive.Cst 4 );
               ] ),
           Naive.Add
             ( Naive.Cst 2,
               Naive.Let
                 ( "y",
                   Naive.Add (Naive.Cst 1, Naive.Cst 3),
                   Naive.Mul (Naive.Var "y", Naive.Var "x") ) ) ))
      []
  in
  assert_equal v (Naive.Vint 42)

let test_nameless_eval_fn_simple _ =
  let v =
    Nameless.eval
      (Nameless.App
         ( Nameless.Fn (Nameless.Add (Nameless.Cst 2, Nameless.Var 0)),
           [ Nameless.Cst 3 ] ))
      []
  in
  assert_equal v (Nameless.Vint 5)

let test_naive_to_nameless _ =
  let son =
    Naive.App
      ( Naive.Fn ([ "bar" ], Naive.Add (Naive.Var "bar", Naive.Var "foo")),
        [ Naive.Cst 2 ] )
  in

  let naive_expr = Naive.App (Naive.Fn ([ "foo" ], son), [ Naive.Cst 3 ]) in
  let nameless_expr = Nameless.from_naive naive_expr [] in
  let naive_v = Naive.eval naive_expr [] in
  let nameless_v = Nameless.eval nameless_expr [] in
  assert_equal (Naive.get_value naive_v) (Nameless.get_value nameless_v);
  assert_equal (Nameless.get_value nameless_v) 5

let suite =
  "TestList"
  >::: [
         "test_naive_eval_fn" >:: test_naive_eval_fn_simple;
         "test_naive_eval_fn_two" >:: test_naive_eval_fn_two;
         "test_nameless_eval_fn_simple" >:: test_nameless_eval_fn_simple;
         "test_naive_to_nameless" >:: test_naive_to_nameless;
       ]

let () = run_test_tt_main suite
