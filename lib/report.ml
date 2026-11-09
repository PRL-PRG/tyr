(** The machine-readable account of a run: one JSON record per line (JSONL),
    written as the run goes and flushed at every record, so that a run killed
    half-way still leaves everything up to that point readable. The consumer is
    r-typing's [parse_output.R], through [jsonlite::stream_in].

    Records are flat -- scalar fields only, list-valued fields [;]-joined -- so
    that [stream_in(..., simplifyDataFrame = TRUE)] yields a data frame with no
    unnesting. Absent values are emitted as [null] rather than omitted, so the
    column set is stable across the records of one kind. *)

open Mlsem.Types

(* [None]: reporting is disabled and every [emit] is a no-op. *)
type t = out_channel option

let create = function
  | None -> None
  | Some path -> Some (open_out path)

let close t = Option.iter close_out t

type v =
  | S of string
  | I of int
  | F of float
  | B of bool
  | N                   (* null *)
  | L of string list    (* one [;]-joined string *)

(* Yojson writes a non-finite float as [NaN] / [Infinity], which is not JSON
   and which jsonlite rejects: those become [null]. *)
let json_of_v : v -> Yojson.Safe.t = function
  | S s -> `String s
  | I i -> `Int i
  | F f -> if Float.is_finite f then `Float f else `Null
  | B b -> `Bool b
  | N -> `Null
  | L l -> `String (String.concat ";" l)

(* [emit t kind fields] writes [{"k": kind, ...}] on one line. Yojson escapes
   the newlines a multi-line type signature contains, which is what keeps the
   one-record-per-line invariant. *)
let emit t kind fields =
  t |> Option.iter (fun oc ->
    let record =
      `Assoc (("k", `String kind) :: List.map (fun (k, v) -> (k, json_of_v v)) fields) in
    output_string oc (Yojson.Safe.to_string record) ;
    output_char oc '\n' ;
    flush oc)

(* ===== Types ===== *)

(* A type printed on one line. [Format.asprintf] wraps at 78 columns, and a
   wrapped signature is an awkward value for a table cell or a CSV field. *)
let one_line pp x =
  (* An [h] box turns every break hint into a space, as NativeSem's own
     printing does; a forced newline still gets through, hence the pass. *)
  let s = Format.asprintf "@[<h>%a@]" pp x in
  String.split_on_char '\n' s |> List.map String.trim
  |> List.filter (fun l -> l <> "") |> String.concat " "

(* A function type that can never return: every arrow has an empty codomain.
   Inference concluding this about a definition is worth a column of its own.
   Same unwrapping of the R attributes as [Link.r_type_of_native']. *)
let degenerate ty =
  let content = if Ty.leq ty Rstt.Attr.any then Rstt.Attr.proj_content ty else ty in
  not (Ty.is_empty content) && Ty.leq content Arrow.any
  && Arrow.dnf content
     |> List.for_all (List.for_all (fun (_dom, codom) -> Ty.is_empty codom))

(* The parameters of a function type that are typed [any] in at least one of
   its arrows, in order: by name for an R function, whose domain is an
   argument record, and by 1-based position for a native one, whose domain is
   a C argument tuple (cf. [Link.arg_of_tuple]). Inference learned nothing
   about them: often a smell, though a parameter the body never uses, or one
   a function really accepts whatever it is, is rightly [any]. A type variable
   is not [any], and neither is the [...] of an R function, which types the
   extra arguments. *)
let any_args ty =
  let content = if Ty.leq ty Rstt.Attr.any then Rstt.Attr.proj_content ty else ty in
  if Ty.is_empty content || not (Ty.leq content Arrow.any) then []
  else
    let is_any t = Ty.leq Ty.any t in
    let field_any f =
      Rstt.Ty.F.get_descr f |> Rstt.Ty.O.get |> Rstt.Ty.O.Atom.get |> is_any in
    let of_dom dom =
      if Ty.leq dom Rstt.Arg.any then
        (try Rstt.Arg.destruct dom with Invalid_argument _ -> [])
        |> List.concat_map (function
          | Rstt.Arg.DefSite a ->
            a.pos_named @ a.named
            |> List.filter_map (fun (n, f) -> if field_any f then Some n else None)
          | Rstt.Arg.CallSite _ -> [])
      else
        Tuple.decompose dom |> fst
        |> List.concat_map (fun (_arity, lines) -> lines |> List.concat_map (fun comps ->
          comps |> List.mapi (fun i t -> if is_any t then Some (string_of_int (i + 1)) else None)
          |> List.filter_map Fun.id))
    in
    let names =
      Arrow.dnf content |> List.concat_map (List.concat_map (fun (dom, _) -> of_dom dom)) in
    List.fold_left (fun acc n -> if List.mem n acc then acc else n :: acc) [] names |> List.rev

(* ===== The R side's errors ===== *)

(* A definition Rsem could not type, from the error its [Driver.on_outcome]
   hook passes: the title, the position as (line, first and last character),
   when it has one, and the description. *)
type untypeable = { title : string ; loc : (int * int * int) option ; descr : string option }

(* The description is normalized as it reads once printed: lines trimmed,
   blank ones dropped. *)
let untypeable_of_error (err : Mlsem.System.Error.t) pos =
  let open Mlsem.Common in
  let loc =
    if pos = Position.dummy then None
    else
      let start = Position.start_of_position pos in
      let c1, c2 = Position.characters start (Position.end_of_position pos) in
      Some (Position.line start, c1, c2)
  in
  let descr =
    Option.bind (Lang.Error.descr err) (fun d ->
      match String.split_on_char '\n' d |> List.map String.trim |> List.filter (( <> ) "") with
      | [] -> None
      | ls -> Some (String.concat "\n" ls))
  in
  { title = Lang.Error.title err ; loc ; descr }

(* The name an [unbound variable] error is about: its descr is [name: <id>]. *)
let unbound_name (u : untypeable) =
  match u.title, u.descr with
  | "unbound variable", Some d when String.starts_with ~prefix:"name: " d ->
    Some (String.trim (String.sub d 6 (String.length d - 6)))
  | _ -> None
