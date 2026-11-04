(** Linking the two sides.

    NativeSem types a routine reached through [.Call] as a function over a C
    argument *tuple* -- [[T1, T2] -> R]. On the R side the same routine is
    called as an ordinary R closure, whose domain is an R argument record. This
    module bridges the two: it rewrites the calls, and adapts the types. *)

open Lang
open Mlsem.Types

module StrMap = Map.Make(String)

(* ===== Types ===== *)

(* [Mlsem.Types.Ty] and [Rstt.Ty] denote the same [Sstt.Ty.t], but only the
   latter exposes the optional/field constructors. *)
let field ty = Rstt.(Ty.O.required ty |> Ty.F.mk_descr)
let absent = Rstt.(Ty.F.mk_descr Ty.O.absent)

(* [x1: T1, ..., xn: Tn] as a definition-site R argument: exactly [n]
   positional parameters, no [...]. *)
let arg_of_components tys =
  Rstt.Arg.mk
    { pos_named = List.mapi (fun i ty -> (Printf.sprintf "x%d" (i + 1), field ty)) tys ;
      pos_tl = absent ; named = [] ; named_tl = absent }

(* The R argument type matching a C argument tuple. A domain that is a union of
   tuples (of possibly different arities) yields the union of the corresponding
   argument types. *)
let arg_of_tuple dom =
  let comps, _ = Tuple.decompose dom in
  comps
  |> List.concat_map (fun (_arity, lines) -> List.map arg_of_components lines)
  |> Ty.disj

let fun_ty = Arrow.any |> Rstt.Attr.mk_content

(* Why a native routine gets no R binding. [No_native_type] is decided by the
   caller (nothing was inferred at all); the other three are decided here. *)
type reject =
  | No_native_type          (* NativeSem produced no type for the symbol *)
  | Empty_native_type       (* it produced [empty] *)
  | Not_a_function          (* a global, or inference left a non-arrow *)
  | Empty_after_conversion  (* every arrow's domain had no R counterpart *)
  | Not_a_dotcode_routine   (* [.C]/[.Fortran]: not a single arrow over the
                               pointers they pass (cf. [dotcode_kinds_of_native]) *)

let string_of_reject = function
  | No_native_type -> "no native type"
  | Empty_native_type -> "empty native type"
  | Not_a_function -> "not a function type"
  | Empty_after_conversion -> "empty after conversion"
  | Not_a_dotcode_routine -> "not a .C routine"

(* [r_type_of_native' ty] is the type of the R binding that stands for a native
   routine whose inferred type is [ty], or the reason there is none. *)
let r_type_of_native' ty =
  (* A routine inferred from its body carries R attributes ([... -> ...]);
     one typed from its C signature does not ([... --> ...]). *)
  let content =
    if Ty.leq ty (Rstt.Attr.any) then Rstt.Attr.proj_content ty else ty in
  if Ty.is_empty content then Error Empty_native_type
  else if not (Ty.leq content Arrow.any) then Error Not_a_function
  else
    let arrows =
      Arrow.dnf content
      |> List.map (List.map (fun (dom, codom) -> (arg_of_tuple dom, codom)))
      |> Arrow.of_dnf
    in
    if Ty.is_empty arrows then Error Empty_after_conversion
    else Ok (Rstt.Attr.mk_content arrows)

let r_type_of_native ty = Result.to_option (r_type_of_native' ty)

let is_fun_ty ty = Ty.leq ty fun_ty && not (Ty.is_empty ty)

(* ===== .C and .Fortran ===== *)

(* [.C] and [.Fortran] pass each argument as a pointer to a copy of its data,
   and return the list of these copies as the routine left them: named after
   the tags of the arguments, with the attributes of the arguments, and with
   the same storage mode and length (R's [do_dotCode]). So the R type of such a
   call follows from the kinds of pointers the routine takes, whatever its body
   does, and the calls are rewritten into lists of applications of helpers,
   one per argument (cf. [dotcode_call]). *)
type dotcode_kind = Int | Dbl | Chr | Raw

(* The C type of each kind, as [C_interface.dotC_typeof] translates it. *)
let dotcode_ctypes =
  let module A = R_c_typing.Ast in
  [ Int, A.Ptr A.Int ; Dbl, A.Ptr A.Float ; Chr, A.Ptr (A.Ptr A.Char) ; Raw, A.Ptr A.Char ]

let string_of_dotcode_kind = function
  | Int -> "int*" | Dbl -> "double*" | Chr -> "char**" | Raw -> "unsigned char*"

let kind_of_dotcode_ty ty =
  dotcode_ctypes |> List.find_map (fun (k, ct) ->
    let kty = R_c_typing.C_interface.dotC_typeof R_c_typing.Ast.DeclMap.empty ct in
    if Ty.leq ty kty && Ty.leq kty ty then Some k else None)

(* The kinds of the parameters of a routine typed for [.C], i.e. by
   [C_interface.infer_dotC]: a single arrow from a tuple of R vectors. *)
let dotcode_kinds_of_native ty =
  match Arrow.dnf ty with
  | [ [ (dom, _) ] ] ->
    (match Tuple.decompose dom with
     | [ (_, [ tys ]) ], _ ->
       let kinds = List.filter_map kind_of_dotcode_ty tys in
       if List.length kinds = List.length tys then Some kinds else None
     | _ -> None)
  | _ -> None

(* The routine [sym], called through [conv] ([".C"] or [".Fortran"]), is
   represented by the bindings [<conv>.<sym>[i]], one per parameter, and by
   [<conv>.<sym>] for the whole routine. Neither can be the name of an R
   binding, and they show in the error messages. *)
type dotcode = { name : string ; kinds : dotcode_kind list }

let dotcode_name conv sym = conv ^ "." ^ sym
let param_helper d i = Printf.sprintf "%s[%d]" d.name (i + 1)

let modes = function
  | Int -> [ "INT" ; "LGL" ] | Dbl -> [ "DBL" ] | Chr -> [ "CHR" ] | Raw -> [ "RAW" ]

(* [param_helper d i] gives back its argument with its attributes, its storage
   mode and its length, but with any value. A logical goes to an [int*] and
   comes back logical. The domains do not exclude [NA], though [.C] refuses it
   unless [NAOK = TRUE]: rstt only tells NA-free vectors apart for scalars.
   The whole routine takes exactly its parameters, and its result does not
   matter: it is only applied to the arguments of a call of the wrong arity,
   to report it. *)
let dotcode_signatures d =
  let params =
    d.kinds |> List.mapi (fun i k ->
      modes k |> List.concat_map (fun m ->
        [ Printf.sprintf "%s : (x: %s1<`a>) -> %s1<`a>" (param_helper d i) m m ;
          Printf.sprintf "%s : (x: %s<`a>) -> %s<`a>" (param_helper d i) m m ]))
    |> List.concat in
  let whole =
    d.kinds |> List.mapi (fun i k ->
      modes k |> List.map (fun m -> Printf.sprintf "%s<`a%d>" m (i + 1)) |> String.concat " | "
      |> Printf.sprintf "x%d: %s" (i + 1))
    |> String.concat ", " |> Printf.sprintf "%s : (%s) -> null" d.name in
  whole :: params

let add_dotcode_helpers ctx d =
  List.fold_left (fun ctx s -> Driver.add_def ctx 0 (R_types.IO.parse_def s))
    ctx (dotcode_signatures d)

(* The arguments of [.C] and [.Fortran] that are not passed to the routine. *)
let dotcode_options = [ "NAOK" ; "DUP" ; "PACKAGE" ; "ENCODING" ]

(* [.C(sym, a1, n2 = a2, NAOK = TRUE)] as [base::list(h1(a1), n2 = h2(a2))].
   With a number of arguments that is not the arity of the routine (which R
   refuses for a registered routine), it is an application of the whole
   routine instead, which fails. [None] when the arguments cannot be told
   apart: forwarded [...], or an empty argument.
   The unnamed elements come first: Rsem does not encode a positional argument
   after a named one, and the order of the two does not show in the type of a
   list (its named fields, and a tail for the others). *)
let dotcode_call pos d args =
  let args = args |> List.filter (function
    | Some (PAst.Named (PAst.ArgId name, _)) -> not (List.mem name dotcode_options)
    | _ -> true) in
  let simple = args |> List.for_all (function
    | Some (PAst.Unnamed ((_, (PAst.Dots | PAst.DotsN _)))) -> false
    | Some (PAst.Unnamed _) | Some (PAst.Named (PAst.ArgId _, Some _)) -> true
    | _ -> false) in
  let call f args = PAst.Call ((pos, PAst.Id f), args) in
  if not simple then None
  else if List.length args <> List.length d.kinds then Some (call d.name args)
  else
    let wrap i e = (pos, call (param_helper d i) [ Some (PAst.Unnamed e) ]) in
    let args = args |> List.mapi (fun i -> function
      | Some (PAst.Unnamed e) -> Some (PAst.Unnamed (wrap i e))
      | Some (PAst.Named (id, Some e)) -> Some (PAst.Named (id, Some (wrap i e)))
      | a -> a) in
    let unnamed, named = args |> List.partition (function
      | Some (PAst.Unnamed _) -> true
      | _ -> false) in
    Some (PAst.Call ((pos, PAst.Ns ("base", "list")), unnamed @ named))

(* ===== Calls ===== *)

(* [.Call(foo, a, b)] is an application of the routine [foo] to [a] and [b].
   Rewriting it to [foo(a, b)] is what lets the R type-checker use the type
   NativeSem inferred for [foo] -- it has no built-in knowledge of [.Call].
   Only symbols [known] to the native side are rewritten; the others are left
   alone, so they keep failing visibly rather than silently changing meaning.

   [.C] and [.Fortran] copy their arguments and return the (possibly modified)
   argument list: a call of a routine whose parameters have [dotcode] kinds
   becomes that list (cf. [dotcode_call]). [.External] packs everything into a
   single LANGSXP, so the R-level type of such a call is not the type of the C
   routine: it is left alone. *)
let rewrite_native_calls ?(dotcode = fun _ -> None) known (prog : PAst.t) : PAst.t =
  let rec aux (pos, e) =
    let e =
      match e with
      | PAst.Call (((_, PAst.Id name) as f), (first :: rest as args)) ->
        let default () = PAst.Call (aux f, List.map aux_arg args) in
        (match R_deps.convention_of_name name, R_deps.symbol_of_arg first with
         | Some R_c_typing.Package.Call, Some symbol when known symbol ->
           PAst.Call ((pos, PAst.Id symbol), List.map aux_arg rest)
         | Some (R_c_typing.Package.C | R_c_typing.Package.Fortran), Some symbol ->
           (match Option.bind (dotcode symbol) (fun d ->
                    dotcode_call pos d (List.map aux_arg rest)) with
            | Some e -> e
            | None -> default ())
         | _ -> default ())
      | PAst.Call (f, args) -> PAst.Call (aux f, List.map aux_arg args)
      | PAst.Unop (str, e) -> PAst.Unop (str, aux e)
      | PAst.Binop (str, (e1, e2)) -> PAst.Binop (str, (aux e1, aux e2))
      | PAst.Dollar (e, a) -> PAst.Dollar (aux e, a)
      | PAst.At (e, a) -> PAst.At (aux e, a)
      | PAst.Subset (e, args) -> PAst.Subset (aux e, List.map aux_arg args)
      | PAst.Subset2 (e, args) -> PAst.Subset2 (aux e, List.map aux_arg args)
      | PAst.Function (l, params, e) ->
        PAst.Function (l, Option.map (List.map aux_param) params, aux e)
      | PAst.Ite (e, e1, e2) -> PAst.Ite (aux e, aux e1, Option.map aux e2)
      | PAst.While (e1, e2) -> PAst.While (aux e1, aux e2)
      | PAst.For (i, e1, e2) -> PAst.For (i, aux e1, aux e2)
      | PAst.Repeat e -> PAst.Repeat (aux e)
      | PAst.Braced es -> PAst.Braced (List.map aux es)
      | (PAst.Const _ | PAst.Id _ | PAst.Ns _ | PAst.Dots | PAst.DotsN _
        | PAst.Return | PAst.Break | PAst.Next) as e -> e
    in
    (pos, e)
  and aux_arg = function
    | None -> None
    | Some (PAst.Unnamed e) -> Some (PAst.Unnamed (aux e))
    | Some (PAst.Named (i, e)) -> Some (PAst.Named (i, Option.map aux e))
  and aux_param = function
    | PAst.NoDefault i -> PAst.NoDefault i
    | PAst.Default (i, e) -> PAst.Default (i, aux e)
  in
  List.map aux prog
