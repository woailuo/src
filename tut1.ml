open Cil
module E = Errormsg

let beforeNullExTrue = ref false

let rec getVarinfoName (e : exp) : string =
  match e with
    Lval ( Var a, _ ) -> ( print_string (a.vname ^ "\n") );  a.vname
  | Lval(Mem e ,_) -> (print_string " get info name : : ") ;  getVarinfoName e
  | _ -> ""

and raiseNullExExpr (vi : varinfo) (e : exp) : bool =
  match e with
    Lval (Var info, _ ) ->  print_string (" ex expr var info: "^ info.vname ^" \n" ) ; false
  | Lval (Mem expr, Field(fieldinfo, offset)) ->
     print_string (" raise mem : p->f :  " ^ vi.vname ^ " ; " ^ "\n");
     if (vi.vname = getVarinfoName expr ) then true else false
  | Lval (Mem expr, NoOffset) ->
     print_string ( " raise mem : *p :"  ^ vi.vname^ " ; " ^ "\n" );
     if (vi.vname = getVarinfoName expr ) then true else false
  | Lval (Mem expr, _) ->
    ( print_string( " raise mem no offset :  " ^ vi.vname ^ " ; "^" \n" )) ; false

  | Const c  ->  (match c with
                    CInt64 _ -> print_string " cint 64\n";false
                  | CStr s -> print_string (" cstr s : " ^ s ^ " \n");false
                  | CWStr _ -> print_string " cwstr \n";false
                  | CChr _ -> print_string " cchr  \n";false
                  | CReal _ -> print_string " creal \n";false
                  | CEnum _ -> print_string " cenum \n";false )
  (* print_string " rasise err const \n";false *)
  | SizeOf _ -> print_string " rasise err sizeof\n";false
  | SizeOfE _ -> print_string " rasise err sizeofe \n";false
  | SizeOfStr _-> print_string " rasise err sizeofstr\n";false
  | AlignOf _ -> print_string " rasise err alignof \n";false
  | AlignOfE _ -> print_string " rasise err alignofe \n";false
  | UnOp _  -> print_string " rasise err unop \n";false
  | BinOp  (binop, e1, e2,typ) ->  let b1 = raiseNullExExpr vi e1 in
                                   let b2 = raiseNullExExpr vi e2 in
                                   b1 || b2
  | Question _-> print_string " rasise err question \n";false
  | CastE _-> print_string " rasise err caste \n";false
  | AddrOf _  -> print_string " rasise err addrof \n";false
  | AddrOfLabel _ -> print_string " rasise err addroflabel \n";false
  | StartOf _ -> print_string " raise err  startof \n"; false


and raiseNullExLval (vi : varinfo) (va :lval) : bool =
  match va with
    (Var a, _) ->  print_string (" ex lval var: " ^ a.vname ^ "\n" ); false
  | (Mem exp,Field(fieldinfo, offset) ) ->
      print_string " raise mem in ex lval  : p->f  \n";
     if (vi.vname = getVarinfoName exp ) then true else false
  | (Mem exp, NoOffset) ->
      print_string " raise mem in ex lval : *p \n";
     if (vi.vname = getVarinfoName exp ) then true else false
  | (Mem exp, _) ->  print_string " raise mem  in lval : _   \n"; false

and iterRaiseExps (vi:varinfo) (exps : exp list) =
  match exps with
    [] -> false
  | e :: rest -> let b1 = (raiseNullExExpr vi e) in
                 (print_string "    end iter rasi eexp \n ");
                 let b2 =  (iterRaiseExps vi rest) in
                 b1 || b2

and raiseFreeNullEx (vi:varinfo) (e : exp) : bool =
  match e with
    Lval (Var a, NoOffset) -> a = vi
  | _ -> false

and  raiseNullExInstr (vi:varinfo)  (ins : instr) : bool =
  match ins with
    Set ((Var a,_), _ ,_)  when a.vname = vi.vname -> (* p = q; *)
    print_string "  instruction changes value of pointer : p = q :   \n";
    (beforeNullExTrue := true); false
   | Set (lval, exp, loc) -> (print_string " \n set lval exp : \n" ); (*  int m = *p; or *p = *q; or ... *)
                             let b1 =  ( raiseNullExLval vi lval) in
                            ( print_string " lval end \n" );
                            let b2 = (raiseNullExExpr vi exp) in
                            print_string " expr end \n" ;     b1 || b2
   | Call(_, Lval(Var a,NoOffset), [e], loc) when a.vname = "free" ->
      (print_string " start free null exce  \n ");
     ( print_string " call var , no offset , free \n ");
      let b1 = (raiseFreeNullEx vi e) in
      let b2 =  (raiseNullExExpr vi e) in
      (print_string (" raise free null ex : "^ string_of_bool b1 ^" \n"));
       (print_string (" raise null ex  when free : "^ string_of_bool b2 ^" \n"));
      ( print_string " end free null exception \n ");
       b1 || b2
   | Call (Some lval, _, exps, loc) ->
      (print_string "  start: changes value of pointer : ( p = func();) \n");
      let b1 = ( vi.vname = getVarinfoName (Lval lval)) in
      (print_string (" change the pointer  : " ^ string_of_bool b1 ^"\n"));
      let b2 = ( iterRaiseExps vi exps) in
      (print_string (" the expression raise null exception  : " ^ string_of_bool b2 ^"\n"));
      (print_string " end :changes value of pointer : (p = func(); ) \n" ); b1 || b2
    | Call (None ,exp,exps,location) ->
                                   print_string " Start call exps \n";
                                   let b = (iterRaiseExps vi exps) in
                                   print_string " End of the call exps \n";   b
   | Asm _ -> print_string " raise asm\n"; false

and raiseNullExInstrs (vi:varinfo) (inss: instr list) : bool  =
  match inss with
    [] -> print_string " instructions nullex : false \n" ; false
  | i :: rest -> let b = raiseNullExInstr vi i  in
                 if b then true else raiseNullExInstrs vi rest

and  raiseNullExStmt (vi:varinfo) (stm: stmt) : bool =
  match stm.skind with
  | Instr ins ->  (print_string "  \n Start instructions:  \n");
                  let b =  (raiseNullExInstrs vi ins) in
                   (print_string " \n  End instructions:  \n"); b
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
     print_string " Start Analysis if (_,_,_): \n \n";
     let ispointer = isPointer vi in
     (
       match ispointer with
       false -> print_string " if-guard is not a pointer \n"
       | true -> let b = raiseNullExStmts vi tb.bstmts in
                 match b with
                   true -> print_string " both raise null exception \n"
                 | false -> print_string " one or two branches do not raise null exception  \n"
     );
     print_string " End Analysis if(_,_,_) \n"
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
