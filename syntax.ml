(** The Abstract Syntax of CIL *)


(** The top-level representation of a CIL source file. Its main contents is
    the list of global declarations and definitions. *)
type file =
    { mutable fileName: string;   (** The complete file name *)
      mutable globals: global list; (** List of globals as they will appear
                                        in the printed file *)
      mutable globinit: fundec option;
      (** An optional global initializer function. This is a function where
       * you can put stuff that must be executed before the program is
       * started. This function, is conceptually at the end of the file,
       * although it is not part of the globals list. Use {!Cil.getGlobInit}
       * to create/get one. *)
      mutable globinitcalled: bool;
      (** Whether the global initialization function is called in main. This
          should always be false if there is no global initializer. When
          you create a global initialization CIL will try to insert code in
          main to call it. *)
    }

and comment = location * string

(** The main type for representing global declarations and definitions. A list
    of these form a CIL file. The order of globals in the file is generally
    important. *)
and global =
  | GType of typeinfo * location
    (** A typedef. All uses of type names (through the [TNamed] constructor)
        must be preceeded in the file by a definition of the name. The string
        is the defined name and always not-empty. *)

  | GCompTag of compinfo * location
    (** Defines a struct/union tag with some fields. There must be one of
        these for each struct/union tag that you use (through the [TComp]
        constructor) since this is the only context in which the fields are
        printed. Consequently nested structure tag definitions must be
        broken into individual definitions with the innermost structure
        defined first. *)

  | GCompTagDecl of compinfo * location
    (** Declares a struct/union tag. Use as a forward declaration. This is
      * printed without the fields.  *)

  | GEnumTag of enuminfo * location
   (** Declares an enumeration tag with some fields. There must be one of
      these for each enumeration tag that you use (through the [TEnum]
      constructor) since this is the only context in which the items are
      printed. *)

  | GEnumTagDecl of enuminfo * location
    (** Declares an enumeration tag. Use as a forward declaration. This is
      * printed without the items.  *)

  | GVarDecl of varinfo * location
   (** A variable declaration (not a definition). If the variable has a
       function type then this is a prototype. There can be several
       declarations and at most one definition for a given variable. If both
       forms appear then they must share the same varinfo structure. A
       prototype shares the varinfo with the fundec of the definition. Either
       has storage Extern or there must be a definition in this file *)

  | GVar  of varinfo * initinfo * location
     (** A variable definition. Can have an initializer. The initializer is
      * updateable so that you can change it without requiring to recreate
      * the list of globals. There can be at most one definition for a
      * variable in an entire program. Cannot have storage Extern or function
      * type. *)


  | GFun of fundec * location
     (** A function definition. *)

  | GAsm of string * location           (** Global asm statement. These ones
                                            can contain only a template *)
  | GPragma of attribute * location     (** Pragmas at top level. Use the same
                                            syntax as attributes *)
  | GText of string                     (** Some text (printed verbatim) at
                                            top level. E.g., this way you can
                                            put comments in the output.  *)


(** The various types available. Every type is associated with a list of
 * attributes, which are always kept in sorted order. Use {!Cil.addAttribute}
 * and {!Cil.addAttributes} to construct list of attributes. If you want to
 * inspect a type, you should use {!Cil.unrollType} to see through the uses
 * of named types. *)
and typ =
    TVoid of attributes   (** Void type *)
  | TInt of ikind * attributes (** An integer type. The kind specifies
                                       the sign and width. *)
  | TFloat of fkind * attributes (** A floating-point type. The kind
                                         specifies the precision. *)

  | TPtr of typ * attributes
           (** Pointer type. *)

  | TArray of typ * exp option * attributes
           (** Array type. It indicates the base type and the array length. *)

  | TFun of typ * (string * typ * attributes) list option * bool * attributes
          (** Function type. Indicates the type of the result, the name, type
           * and name attributes of the formal arguments ([None] if no
           * arguments were specified, as in a function whose definition or
           * prototype we have not seen; [Some \[\]] means void). Use
           * {!Cil.argsToList} to obtain a list of arguments. The boolean
           * indicates if it is a variable-argument function. If this is the
           * type of a varinfo for which we have a function declaration then
           * the information for the formals must match that in the
           * function's sformals. *)

  | TNamed of typeinfo * attributes
          (** The use of a named type. All uses of the same type name must
           * share the typeinfo. Each such type name must be preceeded
           * in the file by a [GType] global. This is printed as just the
           * type name. The actual referred type is not printed here and is
           * carried only to simplify processing. To see through a sequence
           * of named type references, use {!Cil.unrollType}. The attributes
           * are in addition to those given when the type name was defined. *)

  | TComp of compinfo * attributes
          (** A reference to a struct or a union type. All references to the
             same struct or union must share the same compinfo among them and
             with a [GCompTag] global that preceeds all uses (except maybe
             those that are pointers to the composite type). The attributes
             given are those pertaining to this use of the type and are in
             addition to the attributes that were given at the definition of
             the type and which are stored in the compinfo.  *)

  | TEnum of enuminfo * attributes
           (** A reference to an enumeration type. All such references must
               share the enuminfo among them and with a [GEnumTag] global that
               preceeds all uses. The attributes refer to this use of the
               enumeration and are in addition to the attributes of the
               enumeration itself, which are stored inside the enuminfo  *)



  | TBuiltin_va_list of attributes
            (** This is the same as the gcc's type with the same name *)

(** Various kinds of integers *)
and ikind =
    IChar       (** [char] *)
  | ISChar      (** [signed char] *)
  | IUChar      (** [unsigned char] *)
  | IBool       (** [_Bool (C99)] *)
  | IInt        (** [int] *)
  | IUInt       (** [unsigned int] *)
  | IShort      (** [short] *)
  | IUShort     (** [unsigned short] *)
  | ILong       (** [long] *)
  | IULong      (** [unsigned long] *)
  | ILongLong   (** [long long] (or [_int64] on Microsoft Visual C) *)
  | IULongLong  (** [unsigned long long] (or [unsigned _int64] on Microsoft
                    Visual C) *)

(** Various kinds of floating-point numbers*)
and fkind =
    FFloat      (** [float] *)
  | FDouble     (** [double] *)
  | FLongDouble (** [long double] *)

(** An attribute has a name and some optional parameters *)
and attribute = Attr of string * attrparam list

(** Attributes are lists sorted by the attribute name *)
and attributes = attribute list

(** The type of parameters in attributes *)
and attrparam =
  | AInt of int                          (** An integer constant *)
  | AStr of string                       (** A string constant *)
  | ACons of string * attrparam list       (** Constructed attributes. These
                                             are printed [foo(a1,a2,...,an)].
                                             The list of parameters can be
                                             empty and in that case the
                                             parentheses are not printed. *)
  | ASizeOf of typ                       (** A way to talk about types *)
  | ASizeOfE of attrparam
  | ASizeOfS of typsig                   (** Replacement for ASizeOf in type
                                             signatures.  Only used for
                                             attributes inside typsigs.*)
  | AAlignOf of typ
  | AAlignOfE of attrparam
  | AAlignOfS of typsig
  | AUnOp of unop * attrparam
  | ABinOp of binop * attrparam * attrparam
  | ADot of attrparam * string           (** a.foo **)
  | AStar of attrparam                   (** * a *)
  | AAddrOf of attrparam                 (** & a **)
  | AIndex of attrparam * attrparam      (** a1[a2] *)
  | AQuestion of attrparam * attrparam * attrparam (** a1 ? a2 : a3 **)


(** Information about a composite type (a struct or a union). Use
    {!Cil.mkCompInfo}
    to create non-recursive or (potentially) recursive versions of this. Make
    sure you have a [GCompTag] for each one of these.  *)
and compinfo = {
    mutable cstruct: bool;              (** True if struct, False if union *)
    mutable cname: string;              (** The name. Always non-empty. Use
                                         * {!Cil.compFullName} to get the
                                         * full name of a comp (along with
                                         * the struct or union) *)
    mutable ckey: int;                  (** A unique integer constructed from
                                         * the name. Use {!Hashtbl.hash} on
                                         * the string returned by
                                         * {!Cil.compFullName}. All compinfo
                                         * for a given key are shared. *)
    mutable cfields: fieldinfo list;    (** Information about the fields *)
    mutable cattr:   attributes;        (** The attributes that are defined at
                                            the same time as the composite
                                            type *)
    mutable cdefined: bool;             (** Whether this is a defined
                                         * compinfo. *)
    mutable creferenced: bool;          (** True if used. Initially set to
                                         * false *)
  }

(** Information about a struct/union field *)
and fieldinfo = {
    mutable fcomp: compinfo;            (** The compinfo of the host. Note
                                            that this must be shared with the
                                            host since there can be only one
                                            compinfo for a given id *)
    mutable fname: string;              (** The name of the field. Might be
                                         * the value of
                                         * {!Cil.missingFieldName} in which
                                         * case it must be a bitfield and is
                                         * not printed and it does not
                                         * participate in initialization *)
    mutable ftype: typ;                 (** The type *)
    mutable fbitfield: int option;      (** If a bitfield then ftype should be
                                            an integer type *)
    mutable fattr: attributes;          (** The attributes for this field
                                          * (not for its type) *)
    mutable floc: location;             (** The location where this field
                                          * is defined *)
}



(** Information about an enumeration. This is shared by all references to an
    enumeration. Make sure you have a [GEnumTag] for each of of these.   *)
and enuminfo = {
    mutable ename: string;              (** The name. Always non-empty *)
    mutable eitems: (string * exp * location) list; (** Items with names
                                                      and values. This list
                                                      should be
                                                      non-empty. The item
                                                      values must be
                                                      compile-time
                                                      constants. *)
    mutable eattr: attributes;         (** Attributes *)
    mutable ereferenced: bool;         (** True if used. Initially set to false*)
    mutable ekind: ikind;
    (** The integer kind used to represent this enum. Per ANSI-C, this
      * should always be IInt, but gcc allows other integer kinds *)
}

(** Information about a defined type *)
and typeinfo = {
    mutable tname: string;
    (** The name. Can be empty only in a [GType] when introducing a composite
     * or enumeration tag. If empty cannot be refered to from the file *)
    mutable ttype: typ;
    (** The actual type. *)
    mutable treferenced: bool;
    (** True if used. Initially set to false*)
}


(** Information about a variable. These structures are shared by all
 * references to the variable. So, you can change the name easily, for
 * example. Use one of the {!Cil.makeLocalVar}, {!Cil.makeTempVar} or
 * {!Cil.makeGlobalVar} to create instances of this data structure. *)
and varinfo = {
    mutable vname: string;		(** The name of the variable. Cannot
                                          * be empty. *)
    mutable vtype: typ;                 (** The declared type of the
                                          * variable. *)
    mutable vattr: attributes;          (** A list of attributes associated
                                          * with the variable. *)
    mutable vstorage: storage;          (** The storage-class *)
    (* The other fields are not used in varinfo when they appear in the formal
     * argument list in a [TFun] type *)


    mutable vglob: bool;                (** True if this is a global variable*)

    (** Whether this varinfo is for an inline function. *)
    mutable vinline: bool;

    mutable vdecl: location;            (** Location of variable declaration *)

    vinit: initinfo;
    (** Optional initializer.  Only used for static and global variables.
     * Initializers for other types of local variables are turned into
     * assignments. *)

    mutable vid: int;  (** A unique integer identifier.  *)
    mutable vaddrof: bool;              (** True if the address of this
                                            variable is taken. CIL will set
                                         * these flags when it parses C, but
                                         * you should make sure to set the
                                         * flag whenever your transformation
                                         * create [AddrOf] expression. *)

    mutable vreferenced: bool;          (** True if this variable is ever
                                            referenced. This is computed by
                                            [removeUnusedVars]. It is safe to
                                            just initialize this to False *)

    mutable vdescr: doc;                (** For most temporary variables, a
                                            description of what the var holds.
                                            (e.g. for temporaries used for
                                            function call results, this string
                                            is a representation of the function
                                            call.) *)

    mutable vdescrpure: bool;           (** Indicates whether the vdescr above
                                            is a pure expression or call.
                                            True for all CIL expressions and
                                            Lvals, but false for e.g. function
                                            calls.
                                            Printing a non-pure vdescr more
                                            than once may yield incorrect
                                            results. *)
}

(** Storage-class information *)
and storage =
    NoStorage |                         (** The default storage. Nothing is
                                         * printed  *)
    Static |
    Register |
    Extern


(** Expressions (Side-effect free)*)
and exp =
    Const      of constant              (** Constant *)
  | Lval       of lval                  (** Lvalue *)
  | SizeOf     of typ                   (** sizeof(<type>). Has [unsigned
                                         * int] type (ISO 6.5.3.4). This is
                                         * not turned into a constant because
                                         * some transformations might want to
                                         * change types *)

  | SizeOfE    of exp                   (** sizeof(<expression>) *)
  | SizeOfStr  of string
    (** sizeof(string_literal). We separate this case out because this is the
      * only instance in which a string literal should not be treated as
      * having type pointer to character. *)

  | AlignOf    of typ                   (** Has [unsigned int] type *)
  | AlignOfE   of exp


  | UnOp       of unop * exp * typ      (** Unary operation. Includes
                                            the type of the result *)

  | BinOp      of binop * exp * exp * typ
                                        (** Binary operation. Includes the
                                            type of the result. The arithemtic
                                            conversions are made  explicit
                                            for the arguments *)
  | Question   of exp * exp * exp * typ
                                        (** (a ? b : c) operation. Includes
                                            the type of the result *)
  | CastE      of typ * exp            (** Use {!Cil.mkCast} to make casts *)

  | AddrOf     of lval                 (** Always use {!Cil.mkAddrOf} to
                                        * construct one of these. Apply to an
                                        * lvalue of type [T] yields an
                                        * expression of type [TPtr(T)] *)
  | AddrOfLabel of stmt ref

  | StartOf    of lval   (** There is no C correspondent for this. C has
                          * implicit coercions from an array to the address
                          * of the first element. [StartOf] is used in CIL to
                          * simplify type checking and is just an explicit
                          * form of the above mentioned implicit conversion.
                          * It is not printed. Given an lval of type
                          * [TArray(T)] produces an expression of type
                          * [TPtr(T)]. *)


(** Literal constants *)
and constant =
  | CInt64 of int64 * ikind * string option
                 (** Integer constant. Give the ikind (see ISO9899 6.1.3.2)
                  * and the textual representation, if available. Use
                  * {!Cil.integer} or {!Cil.kinteger} to create these. Watch
                  * out for integers that cannot be represented on 64 bits.
                  * OCAML does not give Overflow exceptions. *)
  | CStr of string (** String constant (of pointer type) *)
  | CWStr of int64 list (** Wide string constant (of type "wchar_t *") *)
  | CChr of char (** Character constant.  This has type int, so use
                  *  charConstToInt to read the value in case
                  *  sign-extension is needed. *)
  | CReal of float * fkind * string option (** Floating point constant. Give
                                               the fkind (see ISO 6.4.4.2) and
                                               also the textual representation,
                                               if available *)
  | CEnum of exp * string * enuminfo
     (** An enumeration constant with the given value, name, from the given
      * enuminfo. This is not used if {!Cil.lowerEnum} is false (default).
      * Use {!Cillower.lowerEnumVisitor} to replace these with integer
      * constants. *)

(** Unary operators *)
and unop =
    Neg                                 (** Unary minus *)
  | BNot                                (** Bitwise complement (~) *)
  | LNot                                (** Logical Not (!) *)

(** Binary operations *)
and binop =
    PlusA                               (** arithmetic + *)
  | PlusPI                              (** pointer + integer *)
  | IndexPI                             (** pointer + integer but only when
                                         * it arises from an expression
                                         * [e\[i\]] when [e] is a pointer and
                                         * not an array. This is semantically
                                         * the same as PlusPI but CCured uses
                                         * this as a hint that the integer is
                                         * probably positive. *)
  | MinusA                              (** arithmetic - *)
  | MinusPI                             (** pointer - integer *)
  | MinusPP                             (** pointer - pointer *)
  | Mult                                (** * *)
  | Div                                 (** / *)
  | Mod                                 (** % *)
  | Shiftlt                             (** shift left *)
  | Shiftrt                             (** shift right *)

  | Lt                                  (** <  (arithmetic comparison) *)
  | Gt                                  (** >  (arithmetic comparison) *)
  | Le                                  (** <= (arithmetic comparison) *)
  | Ge                                  (** >  (arithmetic comparison) *)
  | Eq                                  (** == (arithmetic comparison) *)
  | Ne                                  (** != (arithmetic comparison) *)
  | BAnd                                (** bitwise and *)
  | BXor                                (** exclusive-or *)
  | BOr                                 (** inclusive-or *)

  | LAnd                                (** logical and *)
  | LOr                                 (** logical or *)




(** An lvalue denotes the contents of a range of memory addresses. This range
 * is denoted as a host object along with an offset within the object. The
 * host object can be of two kinds: a local or global variable, or an object
 * whose address is in a pointer expression. We distinguish the two cases so
 * that we can tell quickly whether we are accessing some component of a
 * variable directly or we are accessing a memory location through a pointer.*)
and lval =
    lhost * offset

(** The host part of an {!Cil.lval}. *)
and lhost =
  | Var        of varinfo
    (** The host is a variable. *)

  | Mem        of exp
    (** The host is an object of type [T] when the expression has pointer
     * [TPtr(T)]. *)


(** The offset part of an {!Cil.lval}. Each offset can be applied to certain
  * kinds of lvalues and its effect is that it advances the starting address
  * of the lvalue and changes the denoted type, essentially focussing to some
  * smaller lvalue that is contained in the original one. *)
and offset =
  | NoOffset          (** No offset. Can be applied to any lvalue and does
                        * not change either the starting address or the type.
                        * This is used when the lval consists of just a host
                        * or as a terminator in a list of other kinds of
                        * offsets. *)

  | Field      of fieldinfo * offset
                      (** A field offset. Can be applied only to an lvalue
                       * that denotes a structure or a union that contains
                       * the mentioned field. This advances the offset to the
                       * beginning of the mentioned field and changes the
                       * type to the type of the mentioned field. *)

  | Index    of exp * offset
                     (** An array index offset. Can be applied only to an
                       * lvalue that denotes an array. This advances the
                       * starting address of the lval to the beginning of the
                       * mentioned array element and changes the denoted type
                       * to be the type of the array element *)



(* The following equivalences hold *)
(* Mem(AddrOf(Mem a, aoff)), off   = Mem a, aoff + off                *)
(* Mem(AddrOf(Var v, aoff)), off   = Var v, aoff + off                *)
(* AddrOf (Mem a, NoOffset)        = a                                *)

(** Initializers for global variables.  You can create an initializer with
 * {!Cil.makeZeroInit}. *)
and init =
  | SingleInit   of exp   (** A single initializer *)
  | CompoundInit   of typ * (offset * init) list
            (** Used only for initializers of structures, unions and arrays.
             * The offsets are all of the form [Field(f, NoOffset)] or
             * [Index(i, NoOffset)] and specify the field or the index being
             * initialized. For structures all fields
             * must have an initializer (except the unnamed bitfields), in
             * the proper order. This is necessary since the offsets are not
             * printed. For arrays the list must contain a prefix of the
             * initializers; the rest are 0-initialized.
             * For unions there must be exactly one initializer. If
             * the initializer is not for the first field then a field
             * designator is printed, so you better be on GCC since MSVC does
             * not understand this. You can scan an initializer list with
             * {!Cil.foldLeftCompound}. *)

(** We want to be able to update an initializer in a global variable, so we
 * define it as a mutable field *)
and initinfo = {
    mutable init : init option;
  }


(** Function definitions. *)
and fundec =
    { mutable svar:     varinfo;
         (** Holds the name and type as a variable, so we can refer to it
          * easily from the program. All references to this function either
          * in a function call or in a prototype must point to the same
          * varinfo. *)
      mutable sformals: varinfo list;
        (** Formals. These must be shared with the formals that appear in the
         * type of the function. Use {!Cil.setFormals} or
         * {!Cil.setFunctionType} to set these
         * formals and ensure that they are reflected in the function type.
         * Do not make copies of these because the body refers to them. *)
      mutable slocals: varinfo list;
        (** Locals. Does not include the sformals. Do not make copies of
         * these because the body refers to them. *)
      mutable smaxid: int;           (** Max local id. Starts at 0. *)
      mutable sbody: block;          (** The function body. *)
      mutable smaxstmtid: int option;  (** max id of a (reachable) statement
                                        * in this function, if we have
                                        * computed it. range = 0 ...
                                        * (smaxstmtid-1). This is computed by
                                        * {!Cil.computeCFGInfo}. *)
      mutable sallstmts: stmt list;   (** After you call {!Cil.computeCFGInfo}
                                      * this field is set to contain all
                                      * statements in the function *)
    }


(** A block is a sequence of statements with the control falling through from
    one element to the next *)
and block =
   { mutable battrs: attributes;      (** Attributes for the block *)
     mutable bstmts: stmt list;       (** The statements comprising the block*)
   }


(** Statements.
    The statement is the structural unit in the control flow graph. Use mkStmt
    to make a statement and then fill in the fields. *)
and stmt = {
    mutable labels: label list;        (** Whether the statement starts with
                                           some labels, case statements or
                                           default statement *)
    mutable skind: stmtkind;           (** The kind of statement *)

    (* Now some additional control flow information. Initially this is not
     * filled in. *)
    mutable sid: int;                  (** A number (>= 0) that is unique
                                           in a function. *)
    mutable succs: stmt list;          (** The successor statements. They can
                                           always be computed from the skind
                                           and the context in which this
                                           statement appears *)
    mutable preds: stmt list;          (** The inverse of the succs function*)
  }

(** Labels *)
and label =
    Label of string * location * bool
          (** A real label. If the bool is "true", the label is from the
           * input source program. If the bool is "false", the label was
           * created by CIL or some other transformation *)
  | Case of exp * location              (** A case statement *)
  | CaseRange of exp * exp * location   (** A case statement corresponding to a
                                            range of values *)
  | Default of location                 (** A default statement *)



(* The various kinds of statements *)
and stmtkind =
  | Instr  of instr list               (** A group of instructions that do not
                                           contain control flow. Control
                                           implicitly falls through. *)
  | Return of exp option * location     (** The return statement. This is a
                                            leaf in the CFG. *)

  | Goto of stmt ref * location         (** A goto statement. Appears from
                                            actual goto's in the code. *)

  | ComputedGoto of exp * location

  | Break of location                   (** A break to the end of the nearest
                                             enclosing Loop or Switch *)
  | Continue of location                (** A continue to the start of the
                                            nearest enclosing [Loop] *)
  | If of exp * block * block * location (** A conditional.
                                             Two successors, the "then" and
                                             the "else" branches. Both
                                             branches  fall-through to the
                                             successor of the If statement *)
  | Switch of exp * block * (stmt list) * location
                                       (** A switch statement. The block
                                           contains within all of the cases.
                                           We also have direct pointers to the
                                           statements that implement the
                                           cases. Which cases they implement
                                           you can get from the labels of the
                                           statement *)

  | Loop of block * location * (stmt option) * (stmt option)
                                           (** A [while(1)] loop. The
                                            * termination test is implemented
                                            * in the body of a loop using a
                                            * [Break] statement. If
                                            * prepareCFG has been called, the
                                            * first stmt option will point to
                                            * the stmt containing the
                                            * continue label for this loop
                                            * and the second will point to
                                            * the stmt containing the break
                                            * label for this loop. *)

  | Block of block                      (** Just a block of statements. Use it
                                            as a way to keep some attributes
                                            local *)
    (** On MSVC we support structured exception handling. This is what you
     * might expect. Control can get into the finally block either from the
     * end of the body block, or if an exception is thrown. The location
     * corresponds to the try keyword. *)
  | TryFinally of block * block * location

    (** On MSVC we support structured exception handling. The try/except
     * statement is a bit tricky:
         __try { blk }
         __except (e) {
            handler
         }

         The argument to __except  must be an expression. However, we keep a
         list of instructions AND an expression in case you need to make
         function calls. We'll print those as a comma expression. The control
         can get to the __except expression only if an exception is thrown.
         After that, depending on the value of the expression the control
         goes to the handler, propagates the exception, or retries the
         exception !!! The location corresponds to the try keyword.
     *)
  | TryExcept of block * (instr list * exp) * block * location


(** Instructions. They may cause effects directly but may not have control
    flow.*)
and instr =
    Set        of lval * exp * location  (** An assignment. A cast is present
                                             if the exp has different type
                                             from lval *)
  | Call       of lval option * exp * exp list * location
                         (** optional: result is an lval. A cast might be
                             necessary if the declared result type of the
                             function is not the same as that of the
                             destination. If the function is declared then
                             casts are inserted for those arguments that
                             correspond to declared formals. (The actual
                             number of arguments might be smaller or larger
                             than the declared number of arguments. C allows
                             this.) If the type of the result variable is not
                             the same as the declared type of the function
                             result then an implicit cast exists. *)

                         (* See the GCC specification for the meaning of ASM.
                          * If the source is MS VC then only the templates
                          * are used *)
                         (* sm: I've added a notes.txt file which contains more
                          * information on interpreting Asm instructions *)
  | Asm        of attributes * (* Really only const and volatile can appear
                               * here *)
                  string list *         (* templates (CR-separated) *)
                  (string option * string * lval) list *
                                          (* outputs must be lvals with
                                           * optional names and constraints.
                                           * I would like these
                                           * to be actually variables, but I
                                           * run into some trouble with ASMs
                                           * in the Linux sources  *)
                  (string option * string * exp) list *
                                        (* inputs with optional names and constraints *)
                  string list *         (* register clobbers *)
                  location
        (** An inline assembly instruction. The arguments are (1) a list of
            attributes (only const and volatile can appear here and only for
            GCC), (2) templates (CR-separated), (3) a list of
            outputs, each of which is an lvalue with a constraint, (4) a list
            of input expressions along with constraints, (5) clobbered
            registers, and (5) location information *)



(** Describes a location in a source file *)
and location = {
    line: int;             (** The line number. -1 means "do not know" *)
    file: string;          (** The name of the source file*)
    byte: int;             (** The byte position in the source file *)
}

(* Type signatures. Two types are identical iff they have identical
 * signatures *)
and typsig =
    TSArray of typsig * int64 option * attribute list
  | TSPtr of typsig * attribute list
  | TSComp of bool * string * attribute list
  | TSFun of typsig * typsig list option * bool * attribute list
  | TSEnum of string * attribute list
  | TSBase of typ

let locUnknown = { line = -1;
                   file = "";
                   byte = -1;}

(* A reference to the current location *)
let currentLoc : location ref = ref locUnknown

(* A reference to the current global being visited *)
let currentGlobal: global ref = ref (GText "dummy")


let compareLoc (a: location) (b: location) : int =
  let namecmp = compare a.file b.file in
  if namecmp != 0
  then namecmp
  else
    let linecmp = a.line - b.line in
    if linecmp != 0
    then linecmp
    else a.byte - b.byte

let argsToList : (string * typ * attributes) list option
                  -> (string * typ * attributes) list
    = function
    None -> []
  | Some al -> al


(* A hack to allow forward reference of d_exp *)
let pd_exp : (unit -> exp -> doc) ref =
  ref (fun _ -> E.s (E.bug "pd_exp not initialized"))
let pd_type : (unit -> typ -> doc) ref =
  ref (fun _ -> E.s (E.bug "pd_type not initialized"))
let pd_attr : (unit -> attribute -> doc) ref =
  ref (fun _ -> E.s (E.bug "pd_attr not initialized"))

(** Different visiting actions. 'a will be instantiated with [exp], [instr],
    etc. *)
type 'a visitAction =
    SkipChildren                        (** Do not visit the children. Return
                                            the node as it is. *)
  | DoChildren                          (** Continue with the children of this
                                            node. Rebuild the node on return
                                            if any of the children changes
                                            (use == test) *)
  | ChangeTo of 'a                      (** Replace the expression with the
                                            given one *)
  | ChangeDoChildrenPost of 'a * ('a -> 'a) (** First consider that the entire
                                           exp is replaced by the first
                                           parameter. Then continue with
                                           the children. On return rebuild
                                           the node if any of the children
                                           has changed and then apply the
                                           function on the node *)

                                                               (* let tut1 (f : file) : unit = *)
                                                               (*   List.iter (fun g -> *)
                                                               (*     match g with *)
                                                               (*     | GFun (fd, loc) when fd.svar.vname = "target" -> *)
                                                                                                                    (*       tut1FixFunction fd *)
                                                                                                                    (*     | _ -> ()) *)
                                                                                                                    (*   f.globals *)
