open Cil
module E = Errormsg

let isFree = ref false
let isMem = ref false
let freeCall = ref false

let rec  getPointerName exp =
           match exp with
       | Lval a ->
          (
            match a with
              (l, offset) ->
              (
                match l with
                  Var a  -> print_string (" get pointer name var : " ^ a.vname ^ "\n"); a.vname
                | Mem  exp ->  print_string " get pointer name mem: \n";  getPointerName exp
              )
          )
       | Const a -> ( match a with
                        CStr a ->  print_string a ;" gcconst\n"; ""
                      | _ -> print_string " other contents\n" ;""
       )
       | SizeOf _ -> print_string " gcsizeof\n"; ""
       | SizeOfE _ -> print_string " gcsizeofe\n"; ""
       | SizeOfStr _ -> print_string " gcsizeofstr\n"; ""
       | AlignOf _ -> print_string " gcalignof\n"; ""
       | AlignOfE _ -> print_string " gcalignofe\n"; ""
       | UnOp _ -> print_string " gcunop\n"; ""
       | BinOp (_, exp1,exp2,typ) ->(* ( match typ with *)
         (*                                               TVoid _   -> print_string " tvoid\n"; false *)
         (*                                             | TInt _  -> print_string " tint\n"; false *)
         (*                                             | TFloat  _ -> print_string " tfloat\n"; false *)
         (*                                             | TPtr _ -> true *)
         (*                                             |  TArray _ -> print_string " tvoid\n"; false *)
         (*                                             |  TFun _ -> print_string " tvoid\n"; false *)
         (*                                             | TNamed _  -> print_string " tnamed\n"; false *)
         (*                                             | TComp _ -> print_string " tcomp\n"; false *)
         (*                                             | TEnum _ -> print_string " tenum\n"; false *)
         (*                                             | TBuiltin_va_list  _ -> print_string " tbuiltin_va_list\n"; false *)
         (*                                           ); *)
         (* (getPointerName exp1); (getPointerName exp2); *)
         print_string " gcbinop\n"; ""
       | Question _ -> print_string " gcquestion\n"; ""
       | CastE _ -> print_string " gccaste\n"; ""
       | AddrOf _ -> print_string " gcaddof\n"; ""
       | AddrOfLabel _ -> print_string " gcaddroflabel\n"; ""
       | StartOf _ -> print_string " gcstartof\n"; ""


and callFree pname  exp  =
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

and  isSameP pname exps =
  match exps with
    [] -> false
  | exp :: rest -> let e = getPointerName  exp in
                  let p = getPointerName pname in
                  if (e == p) then (print_string " same pointer\n"; true)
                  else isSameP pname rest

and findFreeInstr instr pname =
  match instr with
  | Set _ -> print_string " Set\n"; false
  | Call (_,exp,exps,location) -> let b = (callFree pname exp ) in (* there is free ? *)
                                  if b then (isSameP pname exps) else false
                                 (* the p of free(p) is same to if(p) ? *)
  | Asm _ -> print_string " asm\n"; false

and  findFreeInstrs instrs pname =
  (*  to find  whether there is free(p) in instructions and check whether p is same if(p); if found,  returning true otherwise false *)
  match instrs with
    i :: rest -> let b1 =  (findFreeInstr i pname) in
                 if b1 then true else  (findFreeInstrs rest pname)
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
  | If(pr,tb,fb,_) -> (hasFree tb pname);
                      let b1 = !isFree in print_string (string_of_bool !isFree); print_newline ();
                       (hasFree fb pname); print_string (string_of_bool !isFree); print_newline(); let b2 = !isFree in
                                                         isFree := b1||b2
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
                                       Var info -> print_string (info.vname ^ "\n");
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


and getOffsetInfo offset =
  match offset with
    NoOffset -> print_string " no offset \n"
  | Field (fdinfo, offset) ->  print_string "  offset field\n "
  | Index _ -> print_string "  offset index\n"

and raiseNullExExpr pt exp =
  match exp with
  | Lval a ->
     (
       match a with
         (l, offset) ->
         (
           match l, offset with
             (* TODO: Consider offset *)
             Var a,  _  ->
             (
               try
                 print_string ( " \n raise null expr , var a  \n" ^ a.vname ^ ";\n");
                ( if (a.vname = "free") then freeCall := true);
                 let b1 = !isMem in
                 let b2 = (exp = pt) in
                 isMem := false;
                 print_string (" var a,  issame pointer : " ^ (string_of_bool !freeCall)^ "  ,  "^ (string_of_bool b1) ^ " , "^ (string_of_bool b2)); print_newline ();
                 if (!freeCall) then  (if (b1 = false) then b2 else false) else (b1 && b2)
               with
                 Out_of_memory ->  print_string " out memory  -1 \n"; false
               | _ ->  print_string " out memory  0:  \n"; false
             )
           | Mem e , Field (fieldinfo, offset2)->
              (
              try
              print_string "  raise null expr mem e, p->f : \n";
              let b = (exp = pt) in
              let b2 = (raiseNullExExpr pt e) in
              print_string ((string_of_bool b) ^" , " ^(string_of_bool b2));   b || b2
              with
                Out_of_memory ->  print_string " out memory1 \n"; false
              | _ ->  print_string " out memory 2:  \n"; false
                )
           | Mem e, _ ->
              try
                print_string " raise null expr mem e \n";
              isMem := true;
              let b = ( (e = pt) || (raiseNullExExpr pt e) ) in
              print_string ((string_of_bool (e = pt)) ^ " , " ^ (string_of_bool (raiseNullExExpr pt e)));
              print_string ("  raise null expr mem e, *p :  " ^string_of_bool b ^ " \n");  b
              with
                Out_of_memory ->  print_string " out memory 3 \n"; false
              | _ ->  print_string " out memory 4:  \n"; false
         )
     )
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
  | BinOp  _ -> print_string " rasise err binof \n";false
  | Question _-> print_string " rasise err question \n";false
  | CastE _-> print_string " rasise err caste \n";false
  | AddrOf _  -> print_string " rasise err addrof \n";false
  | AddrOfLabel _ -> print_string " rasise err addroflabel \n";false
  | StartOf _ -> print_string " raise err  startof \n"; false

and raiseNullExLval lval pt =
  ( match lval with
                  (lhost, offset) -> match lhost with
                                       Var info -> print_string (info.vname ^ ": raise var info  \n");
                                                   (
                                                      match offset with
                                                        NoOffset -> print_string " no offset \n"
                                                      | Field _ ->  print_string "  offset field\n "
                                                      | Index _ -> print_string "  offset index\n"
                                                   );
                                                   ( match info.vtype with
                                                     | TPtr _ -> let ptname = getPointerName pt in
                                                                 if (info.vname = ptname) then true else false
                                                     | TVoid _   | TInt _  | TFloat  _  |  TArray _
                                                     |  TFun _   | TNamed _  | TComp _  | TEnum _
                                                     | TBuiltin_va_list  _ -> print_string " tbuiltin_va_list\n"; false
                                                   )
                                     | Mem exp ->
                                                   (
                                                      match offset with
                                                        NoOffset -> print_string " no offset \n"
                                                      | Field _ ->  print_string "  offset field\n "
                                                      | Index _ -> print_string "  offset index\n"
                                                   );

                                                   (print_string " \n raise null lval mem \n");  raiseNullExExpr pt exp)
and iterRaiseExps pt exps =
  match exps with
    [] -> false
  | e :: rest -> (raiseNullExExpr pt e); (print_string "    end iterrasieexp \n "); (iterRaiseExps pt rest);


and raiseNullExInstr ins pt =
   match ins with
   | Set (lval, exp, loc) -> (print_string " \n set lval exp : \n" ); (*  int m = *p; or *p = *q; or ... *)
                             let b1 =  ( raiseNullExLval lval pt) in
                            ( print_string " lval end \n" );
                            let b2 = (raiseNullExExpr pt exp) in
                            print_string " expr end \n" ;     b1 || b2
   (* TODO: Doesn't work, maybe because isSameP is wrong. *)
   | Call (_,exp,exps,location) -> (print_string " call exp exps: \n "); (raiseNullExExpr pt exp);
                                   print_string " end of the call exp \n Start call exps \n";
                                   let b = (iterRaiseExps pt exps) in
                                   print_string " end of the call exps \n";  freeCall := false; b

      (* let raisenull = isSameP pt exps in *)
      (*                             print_string (" raise call func : "^ (string_of_bool raisenull) ^ "\n"); raisenull *)
  | Asm _ -> print_string " raise asm\n"; false

and raiseNullExInstrs inss pt =
  match inss with
    [] -> false
  | i :: rest -> let b = raiseNullExInstr i pt in
                 if b then true else raiseNullExInstrs rest pt

and raiseNullExStmt stm pt =
  match stm.skind with
  |Instr ins -> raiseNullExInstrs ins pt
  | Return _   | Goto _   | ComputedGoto _   | Break _   | Continue _ ->
                                                            print_string " raise error\n"; false
  | If(pr,tb,fb,_) -> let b1 =  (raiseNullExStmts tb.bstmts pt) in
                      let b2  = (raiseNullExStmts fb.bstmts pt) in
                      b1 && b2
  (* TODO: false *)
  | Loop (b, loc,_,_ ) ->  false; (* raiseNullExStmts b.bstmts pt *)
  | Switch _   | Block _  | TryFinally _
  | TryExcept _ -> print_string " raise error2 \n" ; false

and raiseNullExStmts (stmts : stmt list) pt = match stmts with
  [] -> false (* *)
  | s :: rest -> let b = raiseNullExStmt s pt in
                     if b then true else (raiseNullExStmts rest pt)

and analyStmts (s : stmt) : unit =
  match s.skind with
  | Instr il -> print_string " instr\n";
  | Return _ -> print_string " return\n"
  | Goto _ -> print_string " Goto\n"
  | ComputedGoto _ -> print_string "ComputedGoto\n"
  | Break _ -> print_string " Break\n"
  | Continue _ -> print_string " Continue\n"
  | If(pr,tb,fb,_) -> (
    match fb.bstmts with
                        [] ->   let flag = (isPointer pr) in  (* if(exp, block ,block, location)*)
                      (
                        match flag with
                          false -> print_string " not pointer \n"                         (*TODO: also check that fb contains some instructions *)
                        | true ->  (hasFree tb pr);
                                   let b = !isFree in
                                   match b with
                                     true ->  (* has this form:  if(p) {...free(p);..}*)
                                     (* then, to check whether each barch cause NullEx *)
                                     print_string " \n Start \n";
                                     let raisenull = raiseNullExStmts tb.bstmts pr in

                                     if raisenull then  (print_string " \n raise exp, can remove \n") else
                                       (print_string " \n not raise exp, cannot remove \n");

                                     (s.skind <- Block tb );
                                   | false -> print_string " isFree is false\n"
                      )

                      | _ -> print_string " fb is not empty \n "

  )
  | Switch(_,b,_,_) -> print_string " switch\n "
  | Loop(b,_,_,_) -> analyBlock b
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
