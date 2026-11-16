(** Writes the types of the R functions of a package as [#|] signatures.
    contractr checks the calls of the package against this file. tyr reads
    the same syntax (see Rsem's [r-types]), so the file is also a prelude for
    a package that imports this package.

    Each line is one arrow of the type of a function:
    {[ #| pkg::name : (params) -> result ]}
    The type of a function is the intersection of its lines. The scope of a
    type variable is one line. This is correct because [∀] distributes over
    [∩]. If the domain of an arrow is a union of argument lists, tyr writes
    one line for each argument list: [(D1 | D2) -> R] is
    [(D1 -> R) & (D2 -> R)].

    If tyr cannot write the type of a function in this form, the file gives
    the reason in a [# skipped] comment. *)

module Defs = Lang.Defs

open Rstt

module GTy = Mlsem.Types.GTy

(* The name of the variable number [k], after [sigil]: [a] to [z], then [a1],
   [b1], and so on. *)
let var_name sigil k =
  Printf.sprintf "%s%c%s" sigil (Char.chr (Char.code 'a' + (k mod 26)))
    (if k < 26 then "" else string_of_int (k / 26))

(* A substitution that gives the variables of [tys] the names ['a], ['b], and
   so on. It does not change the variables that rstt uses for polymorphic
   argument lists. *)
let renaming tys =
  let poly = Arg.polymorphic_vars () in
  let vars = List.fold_left (fun acc t -> MixVarSet.union acc (Ty.all_vars t)) MixVarSet.empty tys
             |> MixVarSet.filter1 (fun v -> not (VarSet.mem v poly)) in
  let k1 = ref 0 and k2 = ref 0 in
  let next r sigil = let k = !r in incr r ; var_name sigil k in
  Subst.refresh ~names1:(fun _ -> next k1 "'") ~names2:(fun _ -> next k2 "`") vars
  |> fst

let print ty = Report.one_line Pp.ty ty

(* [name] as a [#|] declaration writes it: unchanged for an identifier, in
   parentheses for an operator. The two regular expressions are copies of
   [id] and [sym] in Rsem's [r-types/lexer.mll]. *)
let ident = Str.regexp {|^[a-zA-Z_.][]a-zA-Z0-9_'?!+[$@<>.-]*$|}
let operator = Str.regexp {|^\([][+$@<>:=*%/|&!^~-]+\|%[^%]*%\)$|}
let spell name =
  if Str.string_match ident name 0 then Some name
  else if Str.string_match operator name 0 then Some ("(" ^ name ^ ")")
  else None

exception Skip of string

(* The signatures of [dom -> cod], one for each argument list of [dom]. *)
let signatures (dom, cod) =
  let atoms = try Arg.destruct dom with Invalid_argument _ | Not_found -> [] in
  if atoms = [] then raise (Skip "a domain is not a union of argument lists") ;
  atoms |> List.map (function
    | Arg.CallSite _ -> raise (Skip "a domain is not a union of argument lists")
    | Arg.DefSite a ->
      let d = Arg.mk a in
      let s = Subst.apply (renaming [ d ; cod ]) in
      print (s d) ^ " -> " ^ print (s cod))

(* True if tyr reads [sg] as the signature of a function in a [#|]
   declaration, and rstt reads [sg] as a type. contractr sends [sg] to rstt. *)
let reads_back ~package spelled sg =
  (match R_types.IO.parse_def (Printf.sprintf " %s::%s : %s" package spelled sg) with
   | R_types.Sigs.NsSig (_, _, R_types.Sigs.AFun _) -> true
   | _ -> false
   | exception _ -> false)
  && (match Defs.ty sg with _ -> true | exception _ -> false)

(* The [#|] lines of [name], or the reason to skip it. *)
let lines ~package name gty =
  let ty = GTy.ub gty in
  let content = Gen.content ty in
  match spell name with
  | None -> Error "name cannot be spelled in a #| declaration"
  | Some _ when not (Ty.equiv (GTy.lb gty) ty) -> Error "gradual type"
  | Some _ when Ty.is_empty content || not (Ty.leq content Mlsem.Types.Arrow.any) ->
    Error "not a function"
  | Some spelled ->
    match Mlsem.Types.Arrow.dnf content with
    | [ arrows ] ->
      (* Arrows that differ only in their variables give the same text. *)
      (match Fuzz.dedup Fun.id (List.concat_map signatures arrows) with
       | exception Skip reason -> Error reason
       | sigs ->
         match List.find_opt (fun s -> not (reads_back ~package spelled s)) sigs with
         | Some s -> Error ("printed signature does not parse back: " ^ s)
         | None -> Ok (List.map (Printf.sprintf "#| %s::%s : %s" package spelled) sigs))
    | _ -> Error "a union of function types"

(* Writes [file] and returns the number of written and skipped functions.
   [targets] are the typed definitions of the package, in order. If a name
   has more than one definition, the last definition applies. *)
let write file ~package (targets : Fuzz.target list) =
  let targets =
    List.filter (fun (t : Fuzz.target) -> t.symbol = None) targets   (* not a native entry point *)
    |> List.rev |> Fuzz.dedup (fun (t : Fuzz.target) -> t.name) |> List.rev in
  let oc = open_out file in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    Printf.fprintf oc "# tyr: inferred types of %s, for contractr::insert_tyr_contract\n" package ;
    List.fold_left (fun (written, skipped) (t : Fuzz.target) ->
      match lines ~package t.name t.gty with
      | Ok ls -> List.iter (Printf.fprintf oc "%s\n") ls ; (written + 1, skipped)
      | Error reason ->
        Printf.fprintf oc "# skipped %s: %s\n" t.name reason ; (written, skipped + 1))
      (0, 0) targets)
