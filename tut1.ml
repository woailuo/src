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

and findFreeP (pr:exp) (b:block) =
  let stms = b.stmts in (* stmt list*)
  match stms.skind with
    Instr il -> fixinstrs il
  |  If(_, tb,fb,_) ->

and fixinstrs instrs  pr =
    match ins with
    i :: rest -> (fixInstr  i) ^  (fixinstrs rest)
  | [] -> false

and  fixinstr (i: instr)  pr =
  match i with
  | Set((Var vi, NoOffset), _, loc) -> true
  | Call (_, exp, _ ,location)   ->  fixcallnone exp
  | Asm _-> false
  |  _ -> false

and raiseNullExExp (e : exp) (v : lval) : bool =
(* returns true iff evaluating e raises a NullEx cased by v *)
and raiseNullExInstr (i : instr) (v : lval) :  bool =
(* returns true iff executing i raises a NullEx caused by v *)
    match e with
    Set(l, e, _) ->raiseNullExLval l v || raiseNullExExp e v
    | _ -> failwith "implement here."
and raiseNullExInstrs (is : instr list) ( v : lval) : bool =
and raiseNullExStmt (s : stmt) (v : lval) : bool =
  match s.skind with
  | If(e, b1, b2, _) ->
     raiseNullExExp e v || (raiseNullExBlock b1 v && raiseNullExBlock b2 v)
  | Break _ -> false
  | Loop(b, _, _, _) -> raiseNullExBlock b v

and tut1FixStmt (s : stmt) : unit =
  match s.skind with
  (* | Instr il -> *)

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
