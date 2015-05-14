open Cil
module E = Errormsg

(* and raiseNullExExp (e : exp) (v : lval) : bool = *)
(* (\* returns true iff evaluating e raises a NullEx cased by v *\) *)
(* and raiseNullExInstr (i : instr) (v : lval) :  bool = *)
(* (\* returns true iff executing i raises a NullEx caused by v *\) *)
(*     match e with *)
(*     Set(l, e, _) ->raiseNullExLval l v || raiseNullExExp e v *)
(*     | _ -> failwith "implement here." *)
(* and raiseNullExInstrs (is : instr list) ( v : lval) : bool = *)
(* and raiseNullExStmt (s : stmt) (v : lval) : bool = *)
(*   match s.skind with *)
(*   | If(e, b1, b2, _) -> *)
(*      raiseNullExExp e v || (raiseNullExBlock b1 v && raiseNullExBlock b2 v) *)
(*   | Break _ -> false *)
(*   | Loop(b, _, _, _) -> raiseNullExBlock b v *)

let isFree = ref false

let rec callFree pname  exp  =
  match exp with
  | Lval a ->
     (
       match a with
         (l, offset) ->
         (
           match l with
             Var a  when a.vname = "free" -> print_string " free\n"; true
           | Var b -> (print_string b.vname); print_string " call func\n"; false
           | Mem _ -> print_string "  call mem\n"; false
         )
     )
  | Const _ -> print_string " cconst\n"; false
  | SizeOf _ -> print_string " csizeof\n"; false
  | SizeOfE _ -> print_string " csizeofe\n"; false
  | SizeOfStr _ -> print_string " csizeofstr\n"; false
  | AlignOf _ -> print_string " calignof\n"; false
  | AlignOfE _ -> print_string " calignofe\n"; false
  | UnOp _ -> print_string " cunop\n"; false
  | BinOp  _ -> print_string " cbinop\n"; false
  | Question _ -> print_string " cquestion\n"; false
  | CastE _ -> print_string " ccaste\n"; false
  | AddrOf _ -> print_string " caddof\n"; false
  | AddrOfLabel _ -> print_string " caddroflabel\n"; false
  | StartOf _ -> print_string " cstartof\n"; false

let rec findFreeInstr instr pname =
  match instr with
  | Set _ -> print_string " Set\n"; false
  | Call (_,exp,exps,location) -> (callFree pname exp )
  | Asm _ -> print_string " asm\n"; false

and  findFreeInstrs instrs pname =
  match instrs with
    i :: rest -> let b1 =  (findFreeInstr i pname) in
                 let b2 = (findFreeInstrs rest pname) in
                 b1 || b2
  | [] -> false

and  findFreeFun pname stm =
  match stm.skind with
  | Instr il -> let b = (findFreeInstrs il pname) in
                isFree := b
  | Return _ -> print_string " return\n"
  | Goto _ -> print_string " Goto\n"
  | ComputedGoto _ -> print_string "ComputedGoto\n"
  | Break _ -> print_string " Break\n"
  | Continue _ -> print_string " Continue\n"
  | If(pr,tb,fb,_) -> hasFree tb pr
  | Switch(_,b,_,_) -> print_string " switch \n"
  | Loop(b,_,_,_) -> print_string " loop\n"
  | Block b -> print_string " Block\n"
  | TryFinally(b1, b2, _) -> print_string " TryFinally\n"
  | TryExcept(b1,_,b2,_) -> print_string " TryExcept\n"

and hasFree (blk:block) pname =
  List.iter (findFreeFun pname) blk.bstmts

and  isPointer (e:exp) =
  match e with
  | Lval a -> ( match a with
                  (lhost, offset) -> match lhost with
                                       Var info -> print_string info.vname;
                                                   ( match info.vtype with
                                                       TVoid _   -> print_string " tvoid\n"; false
                                                     | TInt _  -> print_string " tint\n"; false
                                                     | TFloat  _ -> print_string " tfloat\n"; false
                                                     | TPtr _ -> true
                                                     |  TArray _ -> print_string " tvoid\n"; false
                                                     |  TFun _ -> print_string " tvoid\n"; false
                                                     | TNamed _  -> print_string " tnamed\n"; false
                                                     | TComp _ -> print_string " tcomp\n"; false
                                                     | TEnum _ -> print_string " tenum\n"; false
                                                     | TBuiltin_va_list  _ -> print_string " tbuiltin_va_list\n"; false
                                                   )

                                   | Mem exp -> isPointer exp )
  | Const _ -> print_string " Const\n"; false
  | SizeOf _ -> print_string " sizeof\n"; false
  | SizeOfE _ -> print_string "sizeofe\n"; false
  | SizeOfStr _-> print_string "sizeofstr\n"; false
  | AlignOf _ -> print_string " alignof\n"; false
  | AlignOfE _ -> print_string "alignofe\n"; false
  | UnOp _ -> print_string "unop\n"; false
  | BinOp  _ -> print_string "binop\n"; false
  | Question _  -> print_string "question\n"; false
  | CastE _ -> print_string " casee\n"; false
  | AddrOf _ -> print_string " addof\n"; false
  | AddrOfLabel _ -> print_string " addroflabel\n"; false
  | StartOf _ -> print_string " startof\n"; false

and analyStmts (s : stmt) : unit =
  match s.skind with
  | Instr il -> print_string " instr\n";
  | Return _ -> print_string " return\n"
  | Goto _ -> print_string " Goto\n"
  | ComputedGoto _ -> print_string "ComputedGoto\n"
  | Break _ -> print_string " Break\n"
  | Continue _ -> print_string " Continue\n"
  | If(pr,tb,fb,_) -> let flag = (isPointer pr) in  (* if(exp, block ,block, location)*)
                      (
                        match flag with
                          false -> print_string " not pointer \n"
                        | true ->  (hasFree tb pr);
                                   let b = !isFree in
                                   match b with
                                     true -> analyBlock tb
                                   | false -> print_string " isFree is false\n"
                      )

  | Switch(_,b,_,_) -> print_string " switch\n "
  | Loop(b,_,_,_) -> print_string " loop\n"
  | Block b -> print_string " Block\n"
  | TryFinally(b1, b2, _) -> print_string " TryFinally\n"
  | TryExcept(b1,_,b2,_) -> print_string " TryExcept\n"

and analyBlock (b : block) : unit = List.iter analyStmts b.bstmts

let analyFuns (func : fundec) : unit = analyBlock func.sbody

let tut1 (f : file) : unit =
  ( List.iter
      (fun g -> match g with
                        | GFun (func, loc) -> analyFuns func
                        | _ -> ()
      )
      f.globals (*global list :  functions*)
  )
