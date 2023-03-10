open OUnit2
open Tipl.Lab4
open Tipl.Lab4_vm

let fact e =
  Letfn
    ( "fact",
      [ "n" ],
      If
        ( Var "n",
          Prim
            ( Mul,
              [ Var "n"; Prim (Self, [ Prim (Add, [ Var "n"; Cst (-1) ]) ]) ] ),
          Cst 1 ),
      e )

let sum e =
  Letfn
    ( "sum",
      [ "n" ],
      If
        ( Var "n",
          Prim
            ( Add,
              [ Var "n"; Prim (Self, [ Prim (Add, [ Var "n"; Cst (-1) ]) ]) ] ),
          Cst 0 ),
      e )

let fib e =
  Letfn
    ( "fib",
      [ "n" ],
      If
        ( Var "n",
          If
            ( Prim (Add, [ Var "n"; Cst (-1) ]),
              Prim
                ( Add,
                  [
                    Prim (Self, [ Prim (Add, [ Var "n"; Cst (-1) ]) ]);
                    Prim (Self, [ Prim (Add, [ Var "n"; Cst (-2) ]) ]);
                  ] ),
              Cst 1 ),
          Cst 1 ),
      e )

let fact_tail e =
  Letfn
    ( "fact_tail",
      [ "n"; "acc" ],
      If
        ( Var "n",
          Prim
            ( Self,
              [
                Prim (Add, [ Var "n"; Cst (-1) ]);
                Prim (Mul, [ Var "n"; Var "acc" ]);
              ] ),
          Var "acc" ),
      Letfn ("fact", [ "n" ], App ("fact_tail", [ Var "n"; Cst 1 ]), e) )

let test_fact_tail =
  fact_tail
    (Let
       ( "a",
         Cst 4,
         Letfn
           ("id", [ "x" ], Var "x", App ("id", [ App ("fact", [ Var "a" ]) ]))
       ))

let test_cube_square =
  Let
    ( "a",
      Cst 2,
      Letfn
        ( "cube",
          [ "x" ],
          Letfn
            ( "square",
              [ "x" ],
              Prim (Mul, [ Var "x"; Var "x" ]),
              Prim (Mul, [ App ("square", [ Var "x" ]); Var "x" ]) ),
          App ("cube", [ Var "a" ]) ) )

let test_fact = fact (App ("fact", [ Cst 5 ]))
let test_sum = sum (App ("sum", [ Cst 5 ]))
let test_fib = fib (App ("fib", [ Cst 7 ]))

let assert_exec_one fn expr value =
  assert_equal value (fn expr) ~printer:string_of_int;
  ()

let assert_exec fn =
  assert_exec_one fn test_sum 15;
  assert_exec_one fn test_fact 120;
  assert_exec_one fn test_fib 21;
  assert_exec_one fn test_fact_tail 24;
  assert_exec_one fn test_cube_square 8;
  ()

let test_exec _ = assert_exec compile_and_exec
let test_encode_exec _ = assert_exec compile_encode_and_exec

let test_insertToArrayInPlace_basic _ =
  let arr = [| 1; 2; 3; 4; 5; 0; 0; 0 |] in
  let pos = 2 in
  let add = [| 10; 20; 30 |] in
  let expected = [| 1; 2; 10; 20; 30; 3; 4; 5 |] in
  insertToArrayInPlace pos add arr;
  assert_equal expected arr

let test_write _ = compile_encode_and_write test_sum "testfile.txt"

let suite =
  "suite"
  >::: [
         "test_insertToArrayInPlace_basic" >:: test_insertToArrayInPlace_basic;
         "test_exec_sum" >:: test_exec;
         "test_encode" >:: test_encode_exec;
         "test_write" >:: test_write;
       ]

let () = run_test_tt_main suite
