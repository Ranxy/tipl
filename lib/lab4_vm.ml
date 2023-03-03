type instr =
  | Cst of int
  | Add
  | Mul
  | Var of int
  | Pop
  | Swap
  | Label of string
  | Call of string * int
  | Ret of int (*num of args*)
  | IFZERO of string
  | GOTO of string
  | Exit

let instr_to_string instr =
  match instr with
  | Cst i -> Printf.sprintf "Cst(%d)" i
  | Add -> "Add"
  | Mul -> "Mul"
  | Var i -> Printf.sprintf "Var(%d)" i
  | Pop -> "Pop"
  | Swap -> "Swap"
  | Label s -> Printf.sprintf "Label(%s)" s
  | Call (s, n) -> Printf.sprintf "Call %s %d" s n
  | Ret n -> Printf.sprintf "Ret %d" n
  | IFZERO s -> Printf.sprintf "IFZERO %s" s
  | GOTO s -> Printf.sprintf "GOTO %s" s
  | Exit -> "Exit"

type instrs = instr array
type operand = int
type stack = operand array
type vm = { code : instrs; stack : stack; mutable pc : int; mutable sp : int }

let print_vm vm =
  print_endline "";
  print_endline "VM Stack:";
  Array.iter (fun x -> Printf.printf "%i:" x) vm.stack;
  print_endline "";
  print_endline "VM Codes:";
  Array.iter (fun x -> print_endline (instr_to_string x)) vm.code;
  print_endline "";
  Printf.printf "VM PC %d" vm.pc;
  ()

let debug = false
let unwrap_option v = match v with Some x -> x | None -> raise Not_found

let find_index arr x =
  let n = Array.length arr in
  let i = ref 0 in
  let found = ref false in
  while (not !found) && !i < n do
    if arr.(!i) = x then found := true else i := !i + 1
  done;
  if !found then Some !i else None

let getlabel (code : instrs) (label : string) =
  unwrap_option (find_index code (Label label))

let initStack length = Array.make length 0
let getInitPc code = unwrap_option (find_index code (Call ("main", 0)))

let push (vm : vm) (x : operand) =
  vm.stack.(vm.sp) <- x;
  vm.sp <- vm.sp + 1

let pop (vm : vm) : operand =
  vm.sp <- vm.sp - 1;
  let tt = vm.stack.(vm.sp) in
  if debug then vm.stack.(vm.sp) <- 0;
  tt

let initVm code = { code; stack = initStack 40; pc = getInitPc code; sp = 0 }

let insertToArrayInPlace pos add arr =
  let len = Array.length add in
  let add_len = Array.length add in
  let arr_len = Array.length arr in
  for i = arr_len - 1 - add_len downto pos do
    arr.(i + len) <- arr.(i)
  done;
  for i = 0 to len - 1 do
    arr.(pos + i) <- add.(i)
  done

let run vm =
  let break = ref false in

  while not break.contents do
    if debug then print_vm vm;

    let ins = vm.code.(vm.pc) in
    vm.pc <- vm.pc + 1;
    match ins with
    | Cst i -> push vm i
    | Add ->
        let a = pop vm in
        let b = pop vm in
        push vm (a + b)
    | Mul ->
        let a = pop vm in
        let b = pop vm in
        push vm (a * b)
    | Var i ->
        let var = vm.stack.(vm.sp - i - 1) in
        push vm var
    | Pop ->
        let _ = pop vm in
        ()
    | Swap ->
        let a = pop vm in
        let b = pop vm in
        let _ = push vm a in
        let _ = push vm b in
        ()
    | Label _ -> ()
    | Call (f, n) ->
        let next_pc = getlabel vm.code f in
        let _ = insertToArrayInPlace (vm.sp - n) [| vm.pc |] vm.stack in
        vm.sp <- vm.sp + 1;
        vm.pc <- next_pc;
        ()
    | Ret n ->
        let res = pop vm in
        vm.sp <- vm.sp - n;
        let next_pc = pop vm in
        let _ = push vm res in
        vm.pc <- next_pc;
        ()
    | IFZERO t ->
        let cond = pop vm in
        if cond == 0 then vm.pc <- getlabel vm.code t
    | GOTO t -> vm.pc <- getlabel vm.code t
    | Exit ->
        break := true;
        ()
  done;
  pop vm
