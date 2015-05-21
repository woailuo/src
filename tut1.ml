open Cil
module E = Errormsg

(* and raiseNullExExpr (pt : lval) exp = *)

(* and raiseNullExLval lval pt = *)

(* and iterRaiseExps pt exps = (\* free(exps) or printf("%d", *p)*\) *)

(* and raiseNullExInstr ins pt = *)
(*    match ins with *)
(*    | Set (lval, exp, loc) -> (print_string " \n set lval exp : \n" ); (\*  int m = *p; or *p = *q; or ... *\) *)
(*                              let b1 =  ( raiseNullExLval lval pt) in *)
(*                             ( print_string " lval end \n" ); *)
(*                             let b2 = (raiseNullExExpr pt exp) in *)
(*                             print_string " expr end \n" ;     b1 || b2 *)
(*    | Call(_, Lval(a,NoOffset), [e], loc) when a.vname = "free" && raiseNullExExpr pt e -> *)
(*       true *)
(*    (\* TODO: Doesn't work, maybe because isSameP is wrong. *\) *)
(*    | Call (_,exp,exps,location) -> (print_string " call exp exps: \n "); (raiseNullExExpr pt exp); *)
(*                                    print_string " end of the call exp \n Start call exps \n"; *)
(*                                    let b = (iterRaiseExps pt exps) in *)
(*                                    print_string " end of the call exps \n";  freeCall := false; b *)
(*    | Asm _ -> print_string " raise asm\n"; false *)

(* and raiseNullExInstrs inss pt = *)
(*   match inss with *)
(*     [] -> false *)
(*   | i :: rest -> let b = raiseNullExInstr i pt in *)
(*                  if b then true else raiseNullExInstrs rest pt *)

let rec raiseNullExStmt vi stm =
  match stm.skind with
  | Instr ins ->  print_string " instructions \n" (* raiseNullExInstrs ins pt *); false
  | If(guard ,tb,fb,_) -> let b1 =  (raiseNullExStmts vi tb.bstmts ) in
                      let b2  = (raiseNullExStmts  vi fb.bstmts ) in
                      b1 && b2
  | Loop (b, loc,_,_ ) ->  false
  | Return _   | Goto _   | ComputedGoto _   | Break _   | Continue _
  | Switch _   | Block _  | TryFinally _
  | TryExcept _ -> print_string " other statements \n" ; false

and raiseNullExStmts (vi:varinfo) (stmts : stmt list) : bool =
  match stmts with
  [] -> false (* *)
  | s :: rest -> let b = raiseNullExStmt vi s in
                     if b then true else (raiseNullExStmts vi rest)

and isPointer (vi : varinfo) =
  match vi.vtype with
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

and analyStmts (s : stmt) : unit =
  match s.skind with
  | Instr il -> print_string " instr\n";
  | Return _ -> print_string " return\n"
  | Goto _ -> print_string " Goto\n"
  | ComputedGoto _ -> print_string "ComputedGoto\n"
  | Break _ -> print_string " Break\n"
  | Continue _ -> print_string " Continue\n"
  | If(Lval(Var (vi:varinfo),NoOffset),tb,eb,loc) when eb.bstmts = [] ->
     let ispointer = isPointer vi in
     (
       match ispointer with
       false -> print_string " if-guard is not a pointer \n"
       | true -> raiseNullExStmts vi tb.bstmts; ()
     )
  | If _ -> print_string " if \n"
  | Switch(_,b,_,_) -> print_string " switch\n "
  | Loop(b,_,_,_) -> analyBlock b
  | Block b -> print_string " Block\n"
  | TryFinally(b1, b2, _) -> print_string " TryFinally\n"
  | TryExcept(b1,_,b2,_) -> print_string " TryExcept\n"

and analyBlock (b : block) : unit = List.iter analyStmts b.bstmts

and analyFuns (func : fundec) : unit = analyBlock func.sbody

and tut1 (f : file) : unit =
  ( List.iter
      (fun g -> match g with
                        | GFun (func, loc) -> analyFuns func
                        | _ -> ()
      )
      f.globals (*global list :  functions*)
  )
