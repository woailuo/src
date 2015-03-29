open Cil

module E = Errormsg

let str = ref ""
let funclist = ref (("","") :: [])

let rec fixcall c =
  match c with
    (l, offset) ->match l with
                  | Var a when a.vname = "malloc" || a.vname = "free"
                       ->   a.vname ^ ";"
                  | Mem exp -> (fixcallnone exp) ^ "\n"
                  | Var a ->try 
                             (List.assoc a.vname funclist.contents) ^ ";"
                           with
                             (* ( *)
                             (*   try *)
                             (*     let val = findOrCreateFunc a.vname  *)
                             (* ) *)
                             _ -> ""

and fixcallnone c =
  match c with 
  | Lval a -> fixcall a
  | Const _ 
  | SizeOf _  | SizeOfE _ | SizeOfStr _  | AlignOf _ 
  | AlignOfE _ | UnOp _  | BinOp  _  | Question _ 
  | CastE _  | AddrOf _  | AddrOfLabel _  | StartOf _ -> ""

and  fixinstrs ins =
  match ins with
    i :: rest -> (fixInstr  i) ^  (fixinstrs rest)
  | [] -> ""

and fixInstr (i : instr)  =
  match i with
  | Set((Var vi, NoOffset), _, loc) -> ""
  | Call (_, exp, _ ,location)   ->  fixcallnone exp
  | Asm _-> ""
  |  _ -> ""


and findMorF s =
  let b =  (String.contains s 'm') || (String.contains s 'f') in
  b

and fixStmt (s : stmt) =
  match s.skind with
  | Instr il ->
     fixinstrs il
  | If(_,tb,fb,_) ->
     let sb1 = fixBlock tb in
     let sb2 = fixBlock fb in
     let b1 = findMorF sb1 in
     let b2 = findMorF sb2 in
     begin
     match (b1,b2) with
       (true, true) ->  " ( " ^ sb1 ^  " + "  ^ sb2  ^ " ); "
     | (true, false) ->  " ( " ^ sb1 ^  " + "  ^ "0"  ^ " ); "
     | (false, true) -> " ( " ^ "0" ^  " + "  ^ sb2  ^ " ); "
     | (false, false) -> " ( " ^ "0" ^  " + "  ^ "0"  ^ " ); "
     end
  | Switch(_,b,_,_) ->
     let sb = fixBlock b in
     let b= findMorF sb in
     begin
       match b with
         true ->   " ( " ^ sb ^ " ); "
       | false -> "0;"
     end
  | Loop(b,_,_,_) ->
     let sb = fixBlock b in
     let b = findMorF sb in
     begin
       match b with
         true ->  "(ua. " ^ sb ^ ";a)"
       | false -> "(ua. " ^ "0" ^ ";a)"
     end
  | Block b ->
     fixBlock b
  | TryFinally(b1, b2, _) ->
    "(" ^ (fixBlock b1) ^ " f+ " ^ (fixBlock b2 )^ " ); "
  | TryExcept(b1,_,b2,_) ->
     " ( " ^  (fixBlock b1 ) ^  " try+ "  ^ (fixBlock b2 )  ^ " ); "
  | Return _ -> ""
  | Goto _ -> ""
  | Break _ -> ""
  | Continue _ -> ""
  | _ -> print_string " stms unknown "; ""

and fixstmts stms =
  match stms with
    [] -> ""
  | s :: rest -> ( fixStmt s) ^ (fixstmts rest)

and fixBlock (b : block)  = fixstmts b.bstmts

and fixFunction (fd : fundec)  = fixBlock fd.sbody

and printfuns flists =
  match flists with
    (a,b) :: rest ->( if ( a = "") then ()
                      else
                        begin
                        print_string a; print_string ":  "; print_string b ; print_newline ();
                        printfuns rest
                        end)
  | [] -> ()


let tut1 (f : file) : unit =
  try
    (  List.iter (fun g ->
                  match g with
                  | GFun (fd, loc)  ->
                     if (fd.svar.vname = "main") then
                       ()
                     else funclist :=  (fd.svar.vname, fixFunction fd) :: !funclist
                  | _ -> () )
                 f.globals );

    (  List.iter (fun g ->
                  match g with
                  | GFun (fd, loc) ->
                     if (fd.svar.vname = "main") then
                        funclist :=  (fd.svar.vname, fixFunction fd) :: !funclist
                     else ()
                  | _ -> () )
                 f.globals );

    printfuns funclist.contents; print_newline ();

      with
    _ -> print_string "Error: tut1.ml"; print_newline ()
