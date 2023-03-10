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

let instr_length instr =
  match instr with
  | Label _ -> 0
  | Add | Mul | Pop | Swap | Exit -> 1
  | Cst _ | Var _ | Ret _ | IFZERO _ | GOTO _ -> 2
  | Call (_, _) -> 3

let encode instrs =
  let get_offset label_index cur =
    let start = min label_index cur in
    let end_ = max label_index cur in
    let segment = Array.sub instrs start (end_ - start) in
    let offset =
      Array.fold_left (fun acc instr -> acc + instr_length instr) 0 segment
    in
    if label_index > cur then offset else 0 - offset
  in
  let code_seg = Array.make 0xff 0 in
  let p = ref 0 in
  if debug then Printf.printf "Instr len %d .\n" (Array.length instrs) else ();
  for cur = 0 to Array.length instrs - 1 do
    let instr = Array.get instrs cur in
    if debug then Printf.printf "Exec %d Got %s \n" cur (instr_to_string instr)
    else ();
    match instr with
    | Cst i ->
        code_seg.(p.contents) <- 0;
        code_seg.(p.contents + 1) <- i;
        p := p.contents + 2
    | Add ->
        code_seg.(p.contents) <- 1;
        p := p.contents + 1
    | Mul ->
        code_seg.(p.contents) <- 2;
        p := p.contents + 1
    | Var i ->
        code_seg.(p.contents) <- 3;
        code_seg.(p.contents + 1) <- i;
        p := p.contents + 2
    | Pop ->
        code_seg.(p.contents) <- 4;
        p := p.contents + 1
    | Swap ->
        code_seg.(p.contents) <- 5;
        p := p.contents + 1
    | Label _ -> ()
    | Call (s, i) ->
        let label_index = getlabel instrs s in
        let offset = get_offset label_index cur in
        code_seg.(p.contents) <- 6;
        code_seg.(p.contents + 1) <- offset;
        code_seg.(p.contents + 2) <- i;
        p := p.contents + 3
    | Ret i ->
        code_seg.(p.contents) <- 7;
        code_seg.(p.contents + 1) <- i;
        p := p.contents + 2
    | IFZERO s ->
        let label_index = getlabel instrs s in
        let offset = get_offset label_index cur in
        code_seg.(p.contents) <- 8;
        code_seg.(p.contents + 1) <- offset;
        p := p.contents + 2
    | GOTO s ->
        let label_index = getlabel instrs s in
        let offset = get_offset label_index cur in
        code_seg.(p.contents) <- 9;
        code_seg.(p.contents + 1) <- offset;
        p := p.contents + 2
    | Exit ->
        code_seg.(p.contents) <- 10;
        p := p.contents + 1
  done;
  (code_seg, p.contents)

module RealVm = struct
  type vm = {
    code : int array;
    stack : int array;
    mutable pc : int;
    mutable sp : int;
  }

  let print_stack vm =
    for x = 0 to Array.length vm.stack - 1 do
      Printf.printf "%d," vm.stack.(x)
    done;
    print_newline ()

  let initVm code pc = { code; stack = initStack 40; pc; sp = 0 }

  let push vm x =
    vm.stack.(vm.sp) <- x;
    vm.sp <- vm.sp + 1

  let pop vm =
    vm.sp <- vm.sp - 1;
    vm.stack.(vm.sp)

  let run vm =
    let break = ref false in
    while not break.contents do
      let op_code = Array.unsafe_get vm.code vm.pc in
      (* Printf.printf "@EXEC PC %i SP: %i OP  %d \n" vm.pc vm.sp op_code; *)
      (* print_stack vm; *)
      match op_code with
      | 0 ->
          (*Cst(i)*)
          let i = Array.unsafe_get vm.code (vm.pc + 1) in
          let _ = push vm i in
          vm.pc <- vm.pc + 2
      | 1 ->
          (*Add*)
          let a = pop vm in
          let b = pop vm in
          push vm (a + b);
          vm.pc <- vm.pc + 1
      | 2 ->
          (*Mul*)
          let a = pop vm in
          let b = pop vm in
          push vm (a * b);
          vm.pc <- vm.pc + 1
      | 3 ->
          (*Var*)
          let i = Array.unsafe_get vm.code (vm.pc + 1) in
          let var = vm.stack.(vm.sp - i - 1) in
          push vm var;
          vm.pc <- vm.pc + 2
      | 4 ->
          (*Pop*)
          let _ = pop vm in
          vm.pc <- vm.pc + 1
      | 5 ->
          (*Swap*)
          let a = pop vm in
          let b = pop vm in
          let _ = push vm a in
          let _ = push vm b in
          vm.pc <- vm.pc + 1
      | 6 ->
          (*Call (offset, arity)*)
          let offset = Array.unsafe_get vm.code (vm.pc + 1) in
          let arity = Array.unsafe_get vm.code (vm.pc + 2) in
          let next_pc = vm.pc + offset in
          let _ =
            insertToArrayInPlace (vm.sp - arity) [| vm.pc + 3 |] vm.stack
          in
          vm.sp <- vm.sp + 1;
          vm.pc <- next_pc
      | 7 ->
          (*Ret(arity)*)
          let arity = Array.unsafe_get vm.code (vm.pc + 1) in
          let res = pop vm in
          vm.sp <- vm.sp - arity;
          let next_pc = pop vm in
          let _ = push vm res in
          vm.pc <- next_pc
      | 8 ->
          (*IFZERO(offset)*)
          let offset = Array.unsafe_get vm.code (vm.pc + 1) in
          let to_ = vm.pc + offset in
          let cond = pop vm in
          if cond == 0 then vm.pc <- to_ else vm.pc <- vm.pc + 2
      | 9 ->
          (*Goto(offset)*)
          let offset = Array.unsafe_get vm.code (vm.pc + 1) in
          vm.pc <- vm.pc + offset
      | 10 -> break := true
      | _ -> assert false
    done;
    pop vm
end
