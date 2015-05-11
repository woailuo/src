open Cil
module E = Errormsg


let rec tut1FixInstrs ins : bool =
  match ins with
    i :: rest -> (fixinstr i) && (tut1FixInstrs rest)
  | [] -> true

and fixcallnone c =
  match c with
  | Lval a -> true
  | Const _
  | SizeOf _  | SizeOfE _ | SizeOfStr _  | AlignOf _
  | AlignOfE _ | UnOp _  | BinOp  _  | Question _
  | CastE _  | AddrOf _  | AddrOfLabel _  | StartOf _ -> false

and isPointer c =
  match c with
  | Lval a -> true
  | Const _
  | SizeOf _  | SizeOfE _ | SizeOfStr _  | AlignOf _
  | AlignOfE _ | UnOp _  | BinOp  _  | Question _
  | CastE _  | AddrOf _  | AddrOfLabel _  | StartOf _ -> false

and findFreePointer pr =


and  fixinstr (i: instr) =
  match i with
  | Set((Var vi, NoOffset), _, loc) -> true
  | Call (_, exp, _ ,location)   ->  fixcallnone exp
  | Asm _-> false
  |  _ -> false

and tut1FixStmt (s : stmt) : unit =
  match s.skind with
  | Instr il ->
    if  (  tut1FixInstrs il ) then (print_string " \n test \n";)
  | If(pr,tb,fb,_) ->
     let b1 = isPointer  pr in
     match b1 with
       true ->
     | false -> tut1FixBlock tb


  | Switch(_,b,_,_) ->
    tut1FixBlock b
  | Loop(b,_,_,_) ->
    tut1FixBlock b
  | Block b ->
    tut1FixBlock b
  | TryFinally(b1, b2, _) ->
    tut1FixBlock b1;
    tut1FixBlock b2
  | TryExcept(b1,_,b2,_) ->
    tut1FixBlock b1;
    tut1FixBlock b2

  | _ -> ()

and tut1FixBlock (b : block) : unit = List.iter tut1FixStmt b.bstmts

let tut1FixFunction (fd : fundec) : unit = tut1FixBlock fd.sbody

let tut1 (f : file) : unit =
 ( List.iter (fun g ->
    match g with
    | GFun (fd, loc) ->
       tut1FixFunction fd
    | _ -> () )
  f.globals )
