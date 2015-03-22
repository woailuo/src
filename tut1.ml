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
                             (List.assoc a.vname funclist.contents)
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
  |  _ -> print_string "other instr "; print_newline (); ""

(* let t_file = { fileName = ""; *)
(*               globinitcalled = true; *)
(*               globinit = t_fundec t_option; *)
(*               globals = t_global :: []} *)

(* let t_global = { *)

(* } *)

(* let t_fundec = { svar = t_varinfo; *)
(*                  sformals = t_varinfo :: []; *)
(*                  slocals = t_varinfo :: []; *)
(*                  smaxid = 1; *)
(*                  sbody = t_block; *)
(*                  smaxstmtid = 1 t_option; *)
(*                  sallstmts = t_stmt  :: []} *)

(* let  *)

(* let find_malloc_free (f:file) (name:string) (t:typ) : varinfo =  *)
(*   let rec search glist =  *)
(*     match glist with *)
(* 	GVarDecl(vi,_) :: rest | GFun ({svar = vi},_) :: rest when vi.vname = name -> *)
(*           if not (isFunctionType vi.vtype) then  *)
(*             E.s (error ("findOrCreateFunc: can't create %s because another " *)
(*                         ^^"global exists with that name.") name); *)
(*           vi *)
(*       | _ :: rest -> search rest (\* tail recursive *\) *)
(*       | [] -> (\*not found, so create one *\) *)
(*           let t' = unrollTypeDeep t in *)
(* 	  let new_decl = makeGlobalVar name t' in *)
(* 	  f.globals <- GVarDecl(new_decl, locUnknown) :: f.globals; *)
(* 	  new_decl *)
(*   in *)
(*   search f.globals            *)

and fixStmt (s : stmt) =
  match s.skind with
  | Instr il ->
     fixinstrs il
  | If(_,tb,fb,_) ->
     " ( " ^ (fixBlock tb) ^  " + "  ^ ( fixBlock fb)  ^ " ); "
  | Switch(_,b,_,_) ->
     " ( " ^ ( fixBlock b) ^ " ); "
  | Loop(b,_,_,_) ->
      "(ua. " ^ ( fixBlock b) ^ ";a)"
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

(* let tut1 (f : file) : unit = *)
(*   let malloc = findOrCreateFunc f  "malloc" intType in *)
(*   print_string malloc.vname; print_newline (); *)
(*   match malloc.vtype with *)
(*     TVoid _ -> print_string "TVoid"; print_newline () *)
(*   | TInt _ -> print_string "TInt"; print_newline () *)
(*   | TFloat _  -> print_string "float"; print_newline () *)
(*   | TPtr _-> print_string "TPtr"; print_newline () *)
(*   | TArray _ -> print_string "array"; print_newline () *)
(*   | TFun _ -> print_string "fun"; print_newline () *)
(*   | TNamed _ -> print_string "name"; print_newline () *)
(*   | TComp _ -> print_string "come"; print_newline () *)
(*   | TEnum _ -> print_string "enum"; print_newline () *)
(*   | TBuiltin_va_list _ -> print_string "builtin"; print_newline () *)
 
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
