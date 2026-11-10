(** Dependency analysis of the R side of a package.

    Walks the surface AST Rsem produces, so that we see exactly the program the
    type-checker will see -- no second parse, and no regex over the source. *)

open Lang

module StrSet = Set.Make(String)
module StrMap = Map.Make(String)

(* The four ways R reaches into a native library. The symbol is the *R-visible*
   name (before [Pkg.native_symbol] strips the [.fixes] prefix). *)
type native_call = { symbol : string ; convention : R_c_typing.Package.calling_convention }

let convention_of_name = function
  | ".C" -> Some R_c_typing.Package.C
  | ".Call" -> Some R_c_typing.Package.Call
  | ".Fortran" -> Some R_c_typing.Package.Fortran
  | ".External" -> Some R_c_typing.Package.External
  | _ -> None

(* [.Call(foo, ...)] names the symbol with a variable, [.Call("foo", ...)] with
   a string; both are used in the wild. *)
let symbol_of_arg = function
  | Some (PAst.Unnamed (_, PAst.Id str)) -> Some str
  | Some (PAst.Unnamed (_, PAst.Const (PAst.CStr str))) -> Some str
  | _ -> None

(* Every identifier and native call occurring in [e], nested functions
   included. Over-approximating (locals are included too) is harmless here: the
   result is only used to order definitions, and a local shadowing a top-level
   name just adds an edge that was already implied.

   Also the functions [e] calls by name ([f(...)], or [ns::f(...)] with its
   namespace), and the names local to it: the parameters of its functions, the
   variables of its [for] loops, and the names it assigns with [<-], [=] or
   [->] ([<<-] assigns a name outside). *)
let scan_e e =
  let ids = ref StrSet.empty and natives = ref [] in
  let calls = ref [] and locals = ref StrSet.empty in
  let add_id str = ids := StrSet.add str !ids in
  let add_local str = locals := StrSet.add str !locals in
  let rec aux (_, e) =
    match e with
    | PAst.Id str -> add_id str
    (* [pkg::name] may name one of the package's own definitions: keep the
       edge, in line with the over-approximation above. *)
    | PAst.Ns (_, str) -> add_id str
    | PAst.Const _ | PAst.Dots | PAst.DotsN _
    | PAst.Return | PAst.Break | PAst.Next -> ()
    | PAst.Unop (str, e) -> add_id str ; aux e
    | PAst.Binop (str, (e1, e2)) ->
      (match str, e1, e2 with
       | ("<-" | "="), (_, PAst.Id x), _ | "->", _, (_, PAst.Id x) -> add_local x
       | _ -> ()) ;
      add_id str ; aux e1 ; aux e2
    | PAst.Dollar (e, _) | PAst.At (e, _) -> aux e
    | PAst.Call (f, args) ->
      (match f with
       | _, PAst.Id name -> calls := (None, name) :: !calls
       | _, PAst.Ns (ns, name) -> calls := (Some ns, name) :: !calls
       | _ -> ()) ;
      (match f, args with
       | (_, PAst.Id name), first :: _ ->
         (match convention_of_name name, symbol_of_arg first with
          | Some convention, Some symbol ->
            natives := { symbol ; convention } :: !natives
          | _ -> ())
       | _ -> ()) ;
      aux f ; aux_args args
    | PAst.Subset (e, args) | PAst.Subset2 (e, args) -> aux e ; aux_args args
    | PAst.Function (_, params, e) ->
      Option.iter (List.iter (function
        | PAst.Default (a, e) -> local_arg a ; aux e
        | PAst.NoDefault a -> local_arg a)) params ;
      aux e
    | PAst.Ite (e, e1, e2) -> aux e ; aux e1 ; Option.iter aux e2
    | PAst.For (a, e1, e2) -> local_arg a ; aux e1 ; aux e2
    | PAst.While (e1, e2) -> aux e1 ; aux e2
    | PAst.Repeat e -> aux e
    | PAst.Braced es -> List.iter aux es
  and local_arg = function PAst.ArgId x -> add_local x | _ -> ()
  and aux_args args =
    args |> List.iter (function
      | None | Some (PAst.Named (_, None)) -> ()
      | Some (PAst.Unnamed e) | Some (PAst.Named (_, Some e)) -> aux e)
  in
  aux e ; !ids, List.rev !natives, List.sort_uniq compare !calls, !locals

(* One top-level definition, with what it needs. [name] is [None] for a
   top-level statement that binds nothing. *)
type def = {
  name : string option ;
  uses : StrSet.t ;
  natives : native_call list ;
  calls : (string option * string) list ;  (** called by name, with the namespace *)
  locals : StrSet.t ;                      (** cf. [scan_e] *)
}

let def_of_past past =
  let uses, natives, calls, locals = scan_e past in
  { name = Driver.toplevel_name past ; uses ; natives ; calls ; locals }

let defs_of_program (prog : PAst.t) = List.map def_of_past prog

(* All the native symbols an R program reaches, R-visible name first. *)
let natives_of_defs defs =
  defs |> List.concat_map (fun d -> d.natives)
  |> List.fold_left (fun acc n -> StrMap.add n.symbol n.convention acc) StrMap.empty
