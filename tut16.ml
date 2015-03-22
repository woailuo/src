open Cil
module E = Errormsg

type allocation = Atomic of Cil.typ | Array of Cil.typ | Struct of compinfo;;

let filter pred (lst : 'a list) =
   let rec blah x_ =
      match x_ with
      | x :: xs ->
            if (pred x) then
               x :: (blah xs)
            else
               (blah xs)
      | [] -> []
   in
   blah lst;;

let rec removeDuplicates (lst : 'a list) =
   match lst with
   | x :: xs ->
         if List.mem x xs then
            removeDuplicates xs
         else
            x :: removeDuplicates xs
   | [] -> []

let rec cilTypeToString () t =
   match t with
   | TPtr(t,_) -> Printf.sprintf "pointer of %a" cilTypeToString t
   | TInt(_,_) -> "int"
   | TComp(_,_) -> "struct/union"
   | TVoid(_) -> "void"
   | _ -> "??"

let typeName t =
   match t with
   | Atomic t -> Printf.sprintf "Atomic %a" cilTypeToString t
   | Array t -> "Array"
   | Struct c -> "Struct"

let isAllocator name =
   let allocators = ["malloc";"calloc";"realloc";"kmalloc"] in
   List.mem name allocators;;

(* cil converts 'int * x = malloc(...)' into
 * void * tmp = malloc(...);
 * int * x = tmp;
 * So this structure keeps track of that.
 * get = tmp
 * call = malloc
 * set = x
 *)
type site = {
   mutable get : varinfo;
   mutable call : varinfo;
   mutable set : varinfo;
}

let guessType set get call =
   match set.vtype with
   | TPtr(TPtr(_,_),_) -> Array set.vtype
   | TPtr(TComp(comp,_),_) -> Struct comp
   | _ -> Atomic get.vtype
;;

let gc_struct_tag_name st =
   "__gc_struct_" ^ st.cname ^ "_tag"
;;

class analyzer file gc_malloc = object(self)
   inherit nopCilVisitor

   val mutable allocated_structs = []

   method getAllocatedStructs =
      allocated_structs

   (*
   method vstmt t =
      match t.skind with
      | Block b ->
            E.log "Block\n";
            DoChildren
   | _ -> DoChildren
   *)

   (*
   method guessType alloc =
      E.log "Guess type for %s = %s (%s)\n" alloc.set.vname alloc.get.vname
      alloc.call.vname;
      let ultimate =
         match alloc.set.vtype with
         | TPtr(t,attrs) ->
               (*E.log "%s is a pointer of type %a\n" alloc.set.vname Cil.d_type t;*)
               Atomic alloc.set.vtype
         | _ -> Atomic alloc.set.vtype
   in
   E.log "Final type is %s\n" (typeName ultimate)
   *)

   method replaceAllocations allocations func (typer : (allocation -> varinfo)) =
      let replace_allocations = object(self)
         inherit nopCilVisitor

         method isAllocated var =
            List.exists (fun v -> v.get = var) allocations

         method getAllocation var =
            List.find (fun v -> v.get = var) allocations

         method guessTag site =
            let t = typer (guessType site.set site.get site.call) in
            Lval((Var t), NoOffset)

         method vinst i =
            match i with
            | Call(Some(Var vi,vi1), Lval(Var f, b), args, loc) ->
                  (* E.log "replace %s\n" f.vname; *)
                  if self#isAllocated vi then
                     ChangeTo [Call(Some(Var vi,vi1), Lval(Var gc_malloc, b),
                     (self#guessTag (self#getAllocation vi)) :: args, loc)]
                  else
                     SkipChildren
            | _ -> SkipChildren
      end in
      Cil.visitCilFunction (replace_allocations :> nopCilVisitor) func

   method vfunc vi =
      let allocator = object(self)
         inherit nopCilVisitor

         val mutable allocations = []
         val mutable allocated_vars = []

         method addAllocation var site =
            E.log "Add allocation variable %s\n" var.vname;
            allocations <- { get = var; call = site; set = var } :: allocations

         method addAllocatedVar var stuff =
            (* allocated_vars <- (var,stuff) :: allocated_vars *)
            stuff.set <- var

         method getAllocatedVars =
            (* allocated_vars *)
            allocations

         method isAllocation var =
            List.exists (fun v -> v.get = var)
            allocations

         method getAllocation var =
            List.find (fun v -> v.get = var)
            allocations

         method dumpAllocations =
            E.log "--Dump allocations--\n";
            let dump x =
               E.log "Allocate %s id %d\n" x.get.vname x.get.vid;
            in
            List.iter dump allocations;
            let dump2 x =
               match x with
               | (set,(get,func)) -> E.log "Set %s to %s\n" set.vname get.vname
            in
            List.iter dump2 allocated_vars

         method replaceAllocator (i : Cil.instr) =
            match i with
            | Call(a, Lval(Var func,b), args, loc) ->
                  Call(a, Lval(Var gc_malloc, b), args, loc)
            | _ -> i

         method vinst i =
            match i with
            | Call(Some(Var vi,_), Lval(Var func,_), args, loc) ->
                  E.log "Calling %s. Set %s\n" func.vname vi.vname;
                  if isAllocator func.vname then begin
                     self#addAllocation vi func;
                     (* ChangeTo (self#replaceAllocator i) *)
                     (* ChangeTo [(self#replaceAllocator i)] *)
                     SkipChildren
                  end else
                     SkipChildren
            | Set((Var set,_), CastE(_,Lval(Var get, _)),loc) ->
                  E.log "Set %s to %s\n" set.vname get.vname;
                  if self#isAllocation get then
                     self#addAllocatedVar set (self#getAllocation get);
                     SkipChildren
                     (*
            | Set((Var set, NoOffset), Lval(Var get, NoOffset), loc ) ->
                  E.log "Setting %s to %s\n" set.vname get.vname;
                  SkipChildren
                  *)
                     (*
            | Set((Var set,NoOffset), CastE(ti, Lval(Var get, NoOffset)), loc ) ->
                  E.log "Setting %s to %s\n" set.vname get.vname;
                  SkipChildren
                  *)
            | _ -> SkipChildren

                  end in
      E.log "Function %s\n" vi.svar.vname;
      let find_allocations = allocator in
      ignore (Cil.visitCilFunction (find_allocations :> nopCilVisitor) vi);
      let makeTag name =
         let var = Cil.makeLocalVar vi name (Formatcil.cType "struct %c:name *"
         ["name", Fc(Cil.mkCompInfo true "gctag" (fun c -> []) [])]) in
         var.vstorage <- Extern;
         var
      in
      let structs = removeDuplicates (filter (fun v ->
         match v with
         | Struct c -> true
         | _ -> false)
      (List.map (fun v -> guessType v.set v.get v.call)
      find_allocations#getAllocatedVars))
      in
      let (defs : Cil.varinfo list) = List.map makeTag
                                      ([ "__gcstandard_atomic_tag";
                                         "__gcstandard_array_tag"; ] @
                                       (List.map (fun v ->
                                          match v with
                                          | Struct c -> gc_struct_tag_name c
                                          (* should raise an exception here *)
                                          | _ -> "bogus")
                                       structs))
      in
      let typer t =
         match t with
         | Atomic _ -> List.find (fun v -> v.vname = "__gcstandard_atomic_tag")
         defs
         | Array _ -> List.find (fun v -> v.vname = "__gcstandard_array_tag")
         defs
         | Struct comp -> List.find (fun v -> v.vname = gc_struct_tag_name comp) defs
      in
      allocated_structs <- structs @ allocated_structs;
      ignore(self#replaceAllocations find_allocations#getAllocatedVars vi typer);
      (*
      let all = find_allocations#getAllocatedVars in
      let replace_allocations = object(self)
         method vinst i =
            match i with
            | Call(Some(Var vi,_), Lval(Var func,_), args, loc) ->

      end in
      (* find_allocations#dumpAllocations; *)
      (* List.iter self#guessType find_allocations#getAllocatedVars; *)
      Cil.visitCilFunction (replace_allocations :> nopCilVisitor) vi;
      *)
      SkipChildren

      end;;

let gc_malloc_type =
   Formatcil.cType "void * () (struct %c:gc * tag, int size)"
   ["gc", Fc(Cil.mkCompInfo true "gctag" (fun c -> []) [])]
;;
   (* (TFun ((TPtr ((TVoid []), [])), Some([("GC_malloc", (TVoid []), [])]), false, [])) *)

(*
let foobar = object(self)
   inherit nopCilVisitor
end;;
*)

let add_function f g =
   f.globals <- f.globals @ [g]
;;

let cast_to_void_ptr exp =
   let void_ptr = (TPtr ((TVoid []), [])) in
   (CastE (void_ptr, exp))
;;

let gc_mark_name comp =
   "_gc_mark_" ^ comp.cname
;;

let gc_repair_name comp =
   "_gc_repair_" ^ comp.cname
;;

module Markfuncs =
   struct
      let makeFunction func_namer name file comp =
         let f = emptyFunction (func_namer comp) in
         let arg = makeFormalVar f "x" (Formatcil.cType "void *" []) in
         let casted = makeTempVar f ~name:"mark" (Formatcil.cType "struct %c:gc *" ["gc",Fc(comp)]) in
         let mark = findOrCreateFunc file name (Formatcil.cType "void ()(void * x)" []) in
         let var_stmt =
            (Formatcil.cStmt
            ("%v:c = (struct %c:gc *) x_;")
            (fun n t -> makeTempVar f ~name:n t)
            locUnknown
            ["gc",Fc(comp);
            "c", Fv casted;
            "x_",Fv arg])
         in
         let marks =
            (List.map (fun x ->
               let void_ptr =
                  (cast_to_void_ptr (Formatcil.cExp "( * %v:arg )
                  %o:offset" ["arg",Fv casted;"offset", Fo( Field( x, NoOffset))]))
               in
               Formatcil.cStmt
               (* This should work, but cil complains about it *)
               (* "%v:mark( ( void * ) ( * %v:arg ) %o:offset ) ;" *)
               "%v:mark( %e:void_ptr );"
               (fun n t -> makeTempVar f ~name:n t)
               locUnknown
               ["void_ptr", Fe void_ptr;
               "mark", Fv mark;])
            comp.cfields)
         in
         let return =
            (Formatcil.cStmt
            ("return;")
            (fun n t -> makeTempVar f ~name:n t)
            locUnknown
            [])
         in
         let body = var_stmt :: marks @ [return] in
         f.sbody <- {battrs = []; bstmts = body};
         f;
   end;;

let makeMarkFunction = Markfuncs.makeFunction gc_mark_name "gcMARK";;
let makeRepairFunction = Markfuncs.makeFunction gc_repair_name "gcREPAIR";;

      (*
let makeMarkFunction file comp =
   let f = emptyFunction (gc_mark_name comp) in
   let arg = makeFormalVar f "x" (Formatcil.cType "void *" []) in
   let casted = makeTempVar f ~name:"mark" (Formatcil.cType "struct %c:gc *" ["gc",Fc(comp)]) in
   let mark = findOrCreateFunc file "gcMARK" (Formatcil.cType "void ()(void * x)" []) in
   let var_stmt =
      (Formatcil.cStmt
      ("%v:c = (struct %c:gc * ) x_;")
      (fun n t -> makeTempVar f ~name:n t)
      locUnknown
      ["gc",Fc(comp);
      "c", Fv casted;
      "x_",Fv arg])
   in
   let marks =
      (List.map (fun x ->
         let void_ptr =
            (cast_to_void_ptr (Formatcil.cExp "( * %v:arg )
            %o:offset" ["arg",Fv casted;"offset", Fo( Field( x, NoOffset))]))
         in
         Formatcil.cStmt
         (* This should work, but cil complains about it *)
         (* "%v:mark( ( void * ) ( * %v:arg ) %o:offset ) ;" *)
         "%v:mark( %e:void_ptr );"
         (fun n t -> makeTempVar f ~name:n t)
         locUnknown
         ["void_ptr", Fe void_ptr;
         "mark", Fv mark;])
      comp.cfields)
   in
   let return =
      (Formatcil.cStmt
      ("return;")
      (fun n t -> makeTempVar f ~name:n t)
      locUnknown
      [])
   in
   let body = var_stmt :: marks @ [return] in
   f.sbody <- {battrs = []; bstmts = body};
   f
;;
*)

let make_init_func file structs =
   let gc_func name =
      findOrCreateFunc file name (Formatcil.cType "void ()(void * x)" [])
   in
   let gc_comp_info = (Cil.mkCompInfo true "gctag" (fun c -> []) []) in
   let gc_init =
      findOrCreateFunc file "__gc_init_tag" (Formatcil.cType "struct %c:name *
      ()( %t:tag a, %t:tag b )" ["name", Fc(gc_comp_info); "tag",
      Ft (Formatcil.cType "void ()( void * x )" [])])
   in
   let f = emptyFunction ("_gc_init_all") in
   let body =
      List.map (fun s ->
         match s with
         | Struct comp ->
               let makeTag name =
                  let var = Cil.makeLocalVar f name (Formatcil.cType "struct %c:name *"
                  ["name", Fc(gc_comp_info)]) in
                  var.vstorage <- Extern;
                  var
               in
               let v = makeTag (gc_struct_tag_name comp) in
               let mark = gc_func (gc_mark_name comp) in
               let repair = gc_func (gc_repair_name comp) in
               Formatcil.cStmt
               "%v:var = %v:init ( %v:mark, %v:repair ) ;"
               (fun n t -> makeTempVar f ~name:n t)
               locUnknown
               ["var", Fv v;
               "mark", Fv mark;
               "repair", Fv repair;
               "init", Fv gc_init;]
         | _ -> raise (Failure ""))
      structs
   in
   f.sbody <- {battrs = []; bstmts = body};
   f
;;

(* let allocation_analysis (f:Cil.file) = *)
(*    E.log "Allocation analysis\n"; *)
(*    let gc_malloc = findOrCreateFunc f "GC_malloc" gc_malloc_type in *)
(*    let a = (new analyzer f gc_malloc) in *)
(*    visitCilFileSameGlobals (a :> cilVisitor) f; *)
(*    (List.iter (fun v -> *)
(*       match v with *)
(*       | Struct c -> *)
(*             E.log "Adding function for struct %s\n" c.cname; *)
(*             add_function f (GFun((makeMarkFunction f c),locUnknown)); *)
(*             add_function f (GFun((makeRepairFunction f c),locUnknown)) *)
(*       | _ -> ()) *)
(*    a#getAllocatedStructs); *)
(*    add_function f (GFun((make_init_func f a#getAllocatedStructs),locUnknown)) *)
(* ;; *)
let tut16 (f:Cil.file) =
   E.log "Allocation analysis\n";
   let gc_malloc = findOrCreateFunc f "GC_malloc" gc_malloc_type in
   let a = (new analyzer f gc_malloc) in
   visitCilFileSameGlobals (a :> cilVisitor) f;
   (List.iter (fun v ->
      match v with
      | Struct c ->
            E.log "Adding function for struct %s\n" c.cname;
            add_function f (GFun((makeMarkFunction f c),locUnknown));
            add_function f (GFun((makeRepairFunction f c),locUnknown))
      | _ -> ())
   a#getAllocatedStructs);
   add_function f (GFun((make_init_func f a#getAllocatedStructs),locUnknown))
;;

(* let feature : Cil.featureDescr = *)
(*    { fd_name = "allocanalysis"; *)
(*    fd_enabled = ref false; *)
(*    fd_description = "magpie allocation analysis phase"; *)
(*    fd_extraopt = [ *)
(*       (\* *)
(*       ("--suck", Arg.String (fun s -> someGlobal := s), *)
(*       " some ridiculous option"); *)
(*       *\) *)
(*         ]; *)
(*         fd_doit = allocation_analysis; *)
(*         fd_post_check = true *)
(*    } *)
