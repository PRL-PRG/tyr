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

(* ===== Smells read off the printed type ===== *)

(* These look at a type as it prints on one line ([one_line]), not at the
   type: going through the arrows of a big type and destructing their
   argument records costs time and memory (enough for vctrs to exhaust a CI
   runner), and a smell need not be exact. A type equivalent to [any] that
   prints otherwise is missed, and a complemented domain [~[...]] is not
   read.
   TODO: the better version is to make sure the type parsers can parse what
   is printed, and then to traverse the parsed representation, matching its
   nodes (still without any type operation) rather than scanning characters.
   Today they cannot: [R_types.IO.parse_def] rejects the [∀] quantifier,
   labels with spaces or [$] and bare attributes such as [<connection, ...>],
   and NativeSem's C types ([*struct s], [^T], [-->]) are not its syntax at
   all. *)
module Printed = struct
  (* Brackets are ( [ { and the < > of attributes -- but not the > of an
     arrow -> or -->; string literals are skipped. *)

  let is_open = function '(' | '[' | '{' | '<' -> true | _ -> false

  let is_close s k =
    match s.[k] with
    | ')' | ']' | '}' -> true
    | '>' -> k = 0 || s.[k - 1] <> '-'
    | _ -> false

  (* The index after the string literal that starts at [k]. *)
  let skip_string s k =
    let n = String.length s in
    let rec go k = if k >= n then n else match s.[k] with
      | '\\' -> go (k + 2) | '"' -> k + 1 | _ -> go (k + 1) in
    go (k + 1)

  (* The indices in [i, j) of [s] where [p] holds at bracket depth 0. *)
  let top_level s i j p =
    let rec go k depth acc =
      if k >= j then List.rev acc
      else if s.[k] = '"' then go (skip_string s k) depth acc
      else if is_open s.[k] then go (k + 1) (depth + 1) acc
      else if is_close s k then go (k + 1) (depth - 1) acc
      else go (k + 1) depth (if depth = 0 && p k then k :: acc else acc)
    in
    go i 0 []

  (* The closing bracket of the one that opens at [k]. *)
  let closing s k =
    let n = String.length s in
    let rec go k depth =
      if k >= n then n - 1
      else if s.[k] = '"' then go (skip_string s k) depth
      else if is_open s.[k] then go (k + 1) (depth + 1)
      else if is_close s k then (if depth = 1 then k else go (k + 1) (depth - 1))
      else go (k + 1) depth
    in
    go k 0

  let trim s i j =
    let blank c = c = ' ' || c = '\t' || c = '\n' in
    let rec l i = if i < j && blank s.[i] then l (i + 1) else i in
    let i = l i in
    let rec r j = if j > i && blank s.[j - 1] then r (j - 1) else j in
    i, r j

  let sub s (i, j) = String.sub s i (j - i)

  (* [s] in [i, j), cut at the top-level indices [cuts], each separator being
     one character wide. *)
  let split s i j cuts =
    let rec go i = function
      | [] -> [ trim s i j ]
      | k :: ks -> trim s i k :: go (k + 1) ks in
    go i cuts

  (* The parameters of a domain: by name for an R argument list, [(x: T, ...)],
     by 1-based position for a C argument tuple, [[T, ...]]; each with whether
     it prints as [any] ([any?] when optional). The dots of an R function are
     not a parameter, and [tupleN] is the tuple of [N] [any]s. [None] when the
     domain is none of these. *)
  let params s (i, j) =
    let d = sub s (i, j) in
    match int_of_string_opt (if String.starts_with ~prefix:"tuple" d
                             then String.sub d 5 (String.length d - 5) else "") with
    | Some n -> Some (List.init n (fun k -> (string_of_int (k + 1), true)))
    | None ->
    if j - i < 2 || closing s i <> j - 1 then None
    else
      let items = split s (i + 1) (j - 1) (top_level s (i + 1) (j - 1) (fun k -> s.[k] = ',')) in
      match s.[i] with
      | '(' ->
        Some (items |> List.filter_map (fun (a, b) ->
          match top_level s a b (fun k -> s.[k] = ':') with
          | [] -> None
          | c :: _ ->
            let name = sub s (trim s a c) and ty = sub s (trim s (c + 1) b) in
            if name = "..." || name = "" then None
            else Some (name, ty = "any" || ty = "any?")))
      | '[' ->
        Some (items |> List.filter (fun (a, b) -> b > a)
              |> List.mapi (fun n it -> (string_of_int (n + 1), sub s it = "any")))
      | _ -> None

  (* The arrows of a printed function type, each as its parameters (cf.
     [params]) and whether its result prints as [any]: the arrows of each
     intersection and of each union of them, past a [∀...] quantifier and the
     brackets and attributes around them and before the [where] that defines
     its recursive types. A domain that is a union of argument
     lists gives one arrow per list. [] if the type is not a function type. *)
  let arrows printed =
    let s = printed in
    let rec go (i, j) =
      if i >= j then []
      else if String.length s - i >= 3 && String.sub s i 3 = "\xe2\x88\x80" (* ∀ *) then
        match top_level s i j (fun k -> s.[k] = '.') with
        | k :: _ -> go (trim s (k + 1) j)
        | [] -> []
      else
        match top_level s i j (fun k -> s.[k] = '-' && k + 1 < j && s.[k + 1] = '>') with
        | k :: _ ->
          let a = if k > i && s.[k - 1] = '-' then k - 1 else k in
          let result = sub s (trim s (k + 2) j) = "any" in
          split s i a (top_level s i a (fun k -> s.[k] = '|'))
          |> List.filter_map (params s)
          |> List.map (fun ps -> (ps, result))
        | [] ->
          match top_level s i j (fun k -> s.[k] = '&' || s.[k] = '|') with
          | _ :: _ as cuts -> split s i j cuts |> List.concat_map go
          | [] ->
            (* [(T)], or [(T)<attributes>] *)
            if s.[i] <> '(' then []
            else
              let c = closing s i in
              let rest = trim s (c + 1) j in
              if fst rest = snd rest || (s.[fst rest] = '<' && closing s (fst rest) = snd rest - 1)
              then go (trim s (i + 1) c) else []
    in
    let n = String.length s in
    let where k = k + 7 <= n && String.sub s k 7 = " where " in
    go (trim s 0 (match top_level s 0 n where with k :: _ -> k | [] -> n))
end

(* The parameters of a function type that print as [any] in at least one of
   its arrows, in order (cf. [Printed.arrows]), from the type printed by
   [one_line]. Inference learned nothing about them: often a smell, though a
   parameter the body never uses, or one a function really accepts whatever it
   is, is rightly [any]. A type variable is not [any], and neither is the
   [...] of an R function, which types the extra arguments. *)
let any_args printed =
  Printed.arrows printed
  |> List.concat_map (fun (ps, _) -> List.filter_map (fun (n, a) -> if a then Some n else None) ps)
  |> List.fold_left (fun acc n -> if List.mem n acc then acc else n :: acc) []
  |> List.rev

(* A printed function type that says nothing: in every arrow, every parameter
   is [any] and so is the result. The LLM fallback refuses such signatures. *)
let trivial printed =
  match Printed.arrows printed with
  | [] -> false
  | arrows -> arrows |> List.for_all (fun (ps, result) -> result && List.for_all snd ps)

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
