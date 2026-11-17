(** Layout of an R package: which sources belong to the R side, which to the
    native side, and how R names native symbols. *)

(* The C side is exactly what NativeSem already collects (headers first, and
   the transitive closure of [src/], since packages vendor helper libraries in
   subdirectories). *)
let get_c_files = R_c_typing.Package.get_c_files

let r_extensions = [ ".R" ; ".r" ; ".S" ; ".s" ; ".q" ]

let get_r_files path =
  let r_dir = Filename.concat path "R" in
  if not (Sys.file_exists r_dir && Sys.is_directory r_dir) then []
  else
    Sys.readdir r_dir |> Array.to_list
    |> List.filter (fun f -> List.exists (Filename.check_suffix f) r_extensions)
    (* R itself collates alphabetically unless the DESCRIPTION says otherwise;
       the dependency ordering computed later refines this. *)
    |> List.sort String.compare
    |> List.map (Filename.concat r_dir)

(* [useDynLib(lib, .registration = TRUE, .fixes = "PREFIX_")] prefixes the R
   variables bound to the registered routines, while the native symbols keep
   their bare name. Returns "" when there is no such declaration. *)
let native_prefix path =
  let namespace = Filename.concat path "NAMESPACE" in
  if not (Sys.file_exists namespace) then ""
  else
    try
      let content =
        In_channel.with_open_text namespace In_channel.input_lines
        |> String.concat "\n"
      in
      let re = Str.regexp "useDynLib.*\\.fixes[ \t]*=[ \t]*\"\\([^\"]+\\)\"" in
      ignore (Str.search_forward re content 0) ;
      Str.matched_group 1 content
    with _ -> ""

(* ===== Imports ===== *)

(* A directive of NAMESPACE: [import(pkg, except = ...)] or
   [importFrom(pkg, names...)]. *)
type import = Import of string * string list | ImportFrom of string * string list

(* NAMESPACE is R code, but its directives are plain calls whose arguments are
   names or strings: they are read from the text. [#] starts a comment outside
   strings. *)
let strip_comments text =
  let b = Buffer.create (String.length text) in
  let quote = ref None and comment = ref false in
  String.iter (fun c ->
    if !comment then (if c = '\n' then (comment := false ; Buffer.add_char b c))
    else match !quote with
      | Some q -> Buffer.add_char b c ; if c = q then quote := None
      | None ->
        if c = '#' then comment := true
        else (if c = '"' || c = '\'' || c = '`' then quote := Some c ; Buffer.add_char b c))
    text ;
  Buffer.contents b

(* Each call of [name] in [text]: its position, and its top-level arguments,
   as written. *)
let calls name text =
  let n = String.length text and l = String.length name in
  let is_ident c = c = '.' || c = '_' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') in
  let rec args i depth quote cur acc =
    if i >= n then List.rev (String.trim cur :: acc), n
    else let c = text.[i] in
      match quote with
      | Some q -> args (i+1) depth (if c = q then None else quote) (cur ^ String.make 1 c) acc
      | None when c = '"' || c = '\'' || c = '`' -> args (i+1) depth (Some c) (cur ^ String.make 1 c) acc
      | None when c = '(' -> args (i+1) (depth+1) None (cur ^ "(") acc
      | None when c = ')' && depth = 0 -> List.rev (String.trim cur :: acc), i+1
      | None when c = ')' -> args (i+1) (depth-1) None (cur ^ ")") acc
      | None when c = ',' && depth = 0 -> args (i+1) depth None "" (String.trim cur :: acc)
      | None -> args (i+1) depth None (cur ^ String.make 1 c) acc
  in
  let rec find i acc =
    match String.index_from_opt text i name.[0] with
    | None -> List.rev acc
    | Some j when j + l < n && String.sub text j l = name
                  && (j = 0 || not (is_ident text.[j-1])) ->
      let k = ref (j + l) in
      while !k < n && (text.[!k] = ' ' || text.[!k] = '\t' || text.[!k] = '\n') do incr k done ;
      if !k < n && text.[!k] = '(' then
        let a, next = args (!k + 1) 0 None "" [] in
        find next ((j, List.filter (( <> ) "") a) :: acc)
      else find (j + 1) acc
    | Some j -> find (j + 1) acc
  in
  if n = 0 then [] else find 0 []

let unquote s =
  let s = String.trim s in
  let n = String.length s in
  if n >= 2 && (s.[0] = '"' || s.[0] = '\'' || s.[0] = '`') && s.[n-1] = s.[0]
  then String.sub s 1 (n - 2) else s

(* [c(a, "b")] or a single name. *)
let names_of s =
  let s = String.trim s in
  match calls "c" s with
  | [ (0, args) ] -> List.map unquote args
  | _ -> [ unquote s ]

(* The [import] and [importFrom] directives of NAMESPACE, in its order. An
   [import] may name several packages, and exclude names with [except]. *)
let imports path =
  let namespace = Filename.concat path "NAMESPACE" in
  if not (Sys.file_exists namespace) then []
  else
    let text = In_channel.with_open_text namespace In_channel.input_all |> strip_comments in
    let named arg = match String.index_opt arg '=' with
      | Some i -> Some (String.trim (String.sub arg 0 i), String.sub arg (i+1) (String.length arg - i - 1))
      | None -> None in
    let import args =
      let except = args |> List.find_map (fun a -> match named a with
          | Some ("except", v) -> Some (names_of v) | _ -> None) |> Option.value ~default:[] in
      args |> List.filter (fun a -> named a = None)
      |> List.map (fun p -> Import (unquote p, except)) in
    let import_from args =
      match List.filter (fun a -> named a = None) args with
      | p :: names -> [ ImportFrom (unquote p, List.map unquote names) ]
      | [] -> [] in
    (* Both directives, in the order of the file. *)
    let at name f = calls name text |> List.map (fun (j, a) -> j, f a) in
    at "import" import @ at "importFrom" import_from
    |> List.sort (fun (a, _) (b, _) -> compare a b) |> List.concat_map snd

(* The packages an R file reaches with [pkg::name] or [pkg:::name]. *)
let qualified_packages files =
  let re = Str.regexp "\\([A-Za-z][A-Za-z0-9._]*\\):::?" in
  files |> List.concat_map (fun f ->
    let text = try In_channel.with_open_text f In_channel.input_all with _ -> "" in
    let rec go i acc = match Str.search_forward re text i with
      | _ -> go (Str.match_end ()) (Str.matched_group 1 text :: acc)
      | exception Not_found -> acc in
    go 0 [])
  |> List.sort_uniq String.compare

(* ===== r-typeshed ===== *)

(* The version an r-typeshed directory's signatures are written for, from its
   METADATA file (DCF: [Version: 1.2.0]). *)
let typeshed_version dir =
  let f = Filename.concat dir "METADATA" in
  if not (Sys.file_exists f) then None
  else In_channel.with_open_text f In_channel.input_lines
       |> List.find_map (fun l ->
         match String.index_opt l ':' with
         | Some i when String.trim (String.sub l 0 i) = "Version" ->
           Some (String.trim (String.sub l (i+1) (String.length l - i - 1)))
         | _ -> None)

type t = {
  root : string ;
  r_files : string list ;
  c_files : string list ;
  prefix : string ;      (* [.fixes] prefix of the R-visible symbol names *)
  registered : R_c_typing.Package.registered list ;   (* cf. [native_symbol] *)
  imports : import list ;   (* NAMESPACE's, in its order *)
}

let imported_packages pkg =
  pkg.imports |> List.map (function Import (p, _) | ImportFrom (p, _) -> p)

(* The signature files of r-typeshed (the checkout [dir]) for the packages
   [pkg] imports or reaches qualified, other than itself: (package, file,
   version), for the packages that have one. *)
let typeshed_files dir pkg =
  let self = Filename.basename pkg.root in
  imported_packages pkg @ qualified_packages pkg.r_files
  |> List.sort_uniq String.compare
  |> List.filter (fun p -> p <> self)
  |> List.filter_map (fun p ->
    let d = Filename.concat dir p in
    let f = Filename.concat d (p ^ ".R") in
    if Sys.file_exists f then Some (p, f, typeshed_version d) else None)

(* The native symbol an R-visible name refers to, called through [conv]: the
   name without the [.fixes] prefix, mapped by the registration tables, whose
   entries can name a function differently ([{"vctrs_split", (DL_FUNC)
   &vec_split, 2}]). R looks an unregistered [.Fortran] routine [x] up as the
   symbol [x_]. *)
let native_symbol pkg conv name =
  let prefix = pkg.prefix in
  let name =
    if prefix <> "" && String.starts_with ~prefix name
    then String.sub name (String.length prefix) (String.length name - String.length prefix)
    else name in
  let module P = R_c_typing.Package in
  match List.find_opt (fun (r : P.registered) -> r.r_name = name && r.convention = conv)
          pkg.registered with
  | Some r -> r.c_name
  | None when conv = P.Fortran -> String.lowercase_ascii name ^ "_"
  | None -> name

(* The types a [.C]/[.Fortran] routine is registered with, if any. *)
let registered_arg_types pkg c_name =
  let module P = R_c_typing.Package in
  pkg.registered |> List.find_map (fun (r : P.registered) ->
    match r.convention with
    | P.C | P.Fortran when r.c_name = c_name -> r.arg_types
    | _ -> None)

let scan root =
  if not (Sys.file_exists root && Sys.is_directory root) then
    failwith (Printf.sprintf "Not a package directory: %s" root) ;
  { root ; r_files = get_r_files root ; c_files = get_c_files root ;
    prefix = native_prefix root ;
    registered = R_c_typing.Package.registered_routines root ;
    imports = imports root }
