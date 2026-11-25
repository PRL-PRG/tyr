(** The S3 methods of the package being typed.

    Rsem dispatches on the class of the first argument of the generics declared
    with [method g], to the class-overloads declared with [new g @ cls], whose
    signature is the type of the function [g.cls]. Packages do not carry these
    annotations: this module finds their methods, as R does, so that they are
    declared before the package is typed.

    - The methods registered in NAMESPACE, with [S3method(g, cls)].
      Registration is what makes a method reachable from other packages,
      exported or not.
    - The functions named [g.cls] for a known generic [g] that are not
      registered: R still finds them when the package itself calls [g]
      ([UseMethod] looks in the environment of the call first), and
      [R CMD check] reports them as apparent S3 methods. The generic is the
      longest prefix of the name that is a known generic, and the names of
      [tools:::nonS3methods] (e.g. [t.test]) are not methods.

    The known generics are the package's own (the functions that call
    [UseMethod]), declared here, and those the prelude (or r-typeshed) declares
    with [method]: the prelude declares the generics of R's attached packages. *)

open Lang

module StrSet = Set.Make (String)
module StrMap = Map.Make (String)

(* The functions whose name looks like a method but that are not, for every
   package R knows of ([tools:::nonS3methods(NULL)], in data/non_s3_methods.txt:
   one name per line, [#] starting a comment). The list is by package, but only
   the name matters here. *)
let non_methods =
  String.split_on_char '\n' Non_s3_methods_data.text
  |> List.filter_map (fun l ->
    let l = String.trim l in
    if l = "" || l.[0] = '#' then None else Some l)
  |> StrSet.of_list

(* The group generics: a method [Ops.cls] implements every operator of the
   group, which Rsem has no way to declare (the class-overloads are per
   operator, and their function is [op.cls]). *)
let groups = [ "Ops"; "Math"; "Summary"; "Complex"; "matrixOps" ]

(* Rsem's name for the generic R names [g] (cf. [MetaEnv.r_name]). *)
let rsem_name g =
  match g with
  | "[" -> "[]" | "[[" -> "[[]]" | "[<-" -> "[]<-" | "[[<-" -> "[[]]<-" | g -> g

(* A method of the package: its generic (with its namespace, [None] for the
   package's own or for one found on the search path), its class, whether
   NAMESPACE registers it, and the name of its function. *)
type t = {
  generic : string option * string ;
  cls : string ;
  registered : bool ;
  fn : string ;
}

(* The [S3method(g, cls)] and [S3method(g, cls, fn)] directives of NAMESPACE:
   the generic (with its namespace, for [S3method(pkg::g, cls)]), the class and
   the function, [g.cls] unless given. *)
let registrations path =
  let namespace = Filename.concat path "NAMESPACE" in
  if not (Sys.file_exists namespace) then []
  else
    let text = In_channel.with_open_text namespace In_channel.input_all |> Pkg.strip_comments in
    Pkg.calls "S3method" text |> List.filter_map (fun (_, args) ->
      let args = List.filter (fun a -> not (String.contains a '=')) args |> List.map Pkg.unquote in
      let split g = match String.index_opt g ':' with
        | Some i when i + 1 < String.length g && g.[i+1] = ':' ->
          let rest = String.sub g (i+2) (String.length g - i - 2) in
          let rest = if String.length rest > 0 && rest.[0] = ':' then String.sub rest 1 (String.length rest - 1) else rest in
          (Some (String.sub g 0 i), Pkg.unquote rest)
        | _ -> (None, g) in
      match args with
      | [ g ; cls ] -> let (ns, g) = split g in Some ((ns, g), cls, g ^ "." ^ cls)
      | g :: cls :: fn :: _ -> let (ns, g) = split g in Some ((ns, g), cls, fn)
      | _ -> None)

(* The generics the prelude declares, [#| method ns::g] or [#| method g]: their
   name (as Rsem names them, e.g. [[]] for R's [\[]) and their namespace.
   Operators are written [(op)]. *)
let prelude_generics files =
  let re = Str.regexp "^#|[ \t]*method[ \t]+\\(\\([A-Za-z.][A-Za-z0-9._]*\\)::\\)?\\([^ \t:]+\\)" in
  let unparen g =
    if String.length g >= 2 && g.[0] = '(' && g.[String.length g - 1] = ')'
    then String.sub g 1 (String.length g - 2) else g in
  files |> List.concat_map (fun f ->
    try In_channel.with_open_text f In_channel.input_lines
        |> List.filter_map (fun l ->
          if Str.string_match re l 0 then
            let ns = try Some (Str.matched_group 2 l) with Not_found -> None in
            Some (unparen (Str.matched_group 3 l), ns)
          else None)
    with Sys_error _ -> [])
  |> List.fold_left (fun m (g, ns) -> if StrMap.mem g m then m else StrMap.add g ns m) StrMap.empty

(* The top-level functions of a program: their name, and whether they call
   [UseMethod] (a generic). *)
let functions (prog : PAst.t) =
  prog |> List.filter_map (fun ((_, e) as past) ->
    match e with
    | PAst.Binop (("<-" | "="), ((_, PAst.Id name), (_, PAst.Function _)))
    | PAst.Binop ("->", ((_, PAst.Function _), (_, PAst.Id name))) ->
      let d = R_deps.def_of_past past in
      Some (name, List.mem (None, "UseMethod") d.R_deps.calls)
    | _ -> None)

(* The methods of a package whose NAMESPACE is in [root] and whose programs
   are [progs], given the generics the prelude declares ([prelude], by Rsem
   name, cf. [prelude_generics]). A method needs its function in the package
   (Rsem types the class-overload with it), and a generic the package's
   functions reach: the package's own, or one of the prelude's. Left out, and
   returned with the reason for the report:
   - the methods of the generics of other packages ([S3method(pkg::g, cls)],
     or an imported [g]): Rsem would look the method up as [pkg::g.cls];
   - the methods of the group generics (cf. [groups]);
   - a method registered under another name ([S3method(g, cls, fn)]): Rsem
     types the class-overload with [g.cls].
   Also returns the package's generics that have a method: they are declared
   ([method g]) before the class-overloads. *)
let methods ~root ~prelude progs =
  let fns = List.concat_map functions progs in
  let defined = StrSet.of_list (List.map fst fns) in
  let own = fns |> List.filter snd |> List.map fst |> StrSet.of_list in
  let r_name g = match g with
    | "[]" -> "[" | "[[]]" -> "[[" | "[]<-" -> "[<-" | "[[]]<-" -> "[[<-" | g -> g in
  let declared = StrMap.fold (fun g _ acc -> StrSet.add (r_name g) acc) prelude StrSet.empty in
  let known g = StrSet.mem g own || StrSet.mem g declared in
  let skip reason r = (r, reason) in
  let registered, skipped =
    registrations root |> List.partition_map (fun (((ns, g), cls, fn) as r) ->
      if ns <> None then Right (skip "generic of another package" r)
      else if List.mem g groups then Right (skip "group generic" r)
      else if not (known g) then Right (skip "unknown generic (another package's?)" r)
      else if fn <> g ^ "." ^ cls then Right (skip "registered under another name" r)
      else if not (StrSet.mem fn defined) then Right (skip "no such function" r)
      else Left { generic = (None, rsem_name g) ; cls ; registered = true ; fn }) in
  let taken = StrSet.of_list (List.map (fun m -> m.fn) registered) in
  (* The longest prefix [g] of [name] that is a known generic, with a non-empty
     class after it. *)
  let split name =
    let n = String.length name in
    let rec go i best =
      match String.index_from_opt name i '.' with
      | None -> best
      | Some j ->
        let g = String.sub name 0 j in
        let best = if j > 0 && j + 1 < n && known g then Some (g, String.sub name (j+1) (n-j-1)) else best in
        go (j + 1) best in
    if n = 0 then None else go 0 None in
  let unregistered =
    fns |> List.filter_map (fun (name, _) ->
      if StrSet.mem name taken || StrSet.mem name non_methods then None
      else match split name with
        | Some (g, _) when List.mem g groups -> None
        | Some (g, cls) -> Some { generic = (None, rsem_name g) ; cls ; registered = false ; fn = name }
        | None -> None) in
  let ms = registered @ unregistered in
  let generics =
    ms |> List.filter_map (fun m ->
      let g = snd m.generic in
      if StrSet.mem g own then Some (None, g) else None)
    |> List.sort_uniq compare in
  ms, generics, skipped
