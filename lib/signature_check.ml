(** Checking an R definition against a signature given as text.

    This is the tool the LLM fallback gives the model: the model proposes a
    signature, written as in a [#|] annotation, and the definition is checked
    against it as Rsem checks an annotated definition. The typing context is
    an immutable record, so each candidate starts from the same context: a
    rejected one leaves no trace, an accepted one gives the context the
    definition is then typed in.

    A check prints nothing: Rsem prints its results with [Format.printf], and
    so does the timeout guard, so the standard formatter is muted meanwhile. *)

open Lang
open Mlsem.Types

type accepted = {
  ctx : Driver.typing_ctx ;  (** the context with the definition typed *)
  ty : TyScheme.t ;          (** its type: the signature's *)
  printed : string ;         (** the type on one line *)
  any_args : string list ;   (** its parameters typed [any] (cf. {!Report.any_args}) *)
  any_result : bool ;        (** its result is [any] (cf. {!Report.any_result}) *)
  degenerate : bool ;        (** it can never return (cf. {!Report.degenerate}) *)
}

type outcome =
  | Accepted of accepted
  | Invalid of string              (** not a signature: syntax, unknown type, parameters
                                       other than the definition's, ... *)
  | Rejected of Report.untypeable  (** the definition does not have this type *)
  | Timeout of float               (** checking it took longer than this *)
  | Failed of string               (** the checker raised *)

(* Runs [f] with the standard formatter writing nowhere. What [f] left in the
   formatter is flushed away before the output is restored. *)
let quietly f =
  Format.pp_print_flush Format.std_formatter () ;
  let out = Format.pp_get_formatter_out_functions Format.std_formatter () in
  Format.pp_set_formatter_out_functions Format.std_formatter
    { out_string = (fun _ _ _ -> ()) ; out_flush = ignore ; out_newline = ignore ;
      out_spaces = ignore ; out_indent = ignore } ;
  Fun.protect f ~finally:(fun () ->
    Format.pp_print_flush Format.std_formatter () ;
    Format.pp_set_formatter_out_functions Format.std_formatter out)

(* The parameters of the definition [past], when it is [name <- function(...)]:
   their names, ["..."] for the dots, and whether each has a default. *)
let formals past =
  let name = function
    | PAst.ArgId s -> Some s | PAst.EllipsisId -> Some "..." | _ -> None in
  match snd past with
  | PAst.Binop (("<-" | "="), (_, (_, PAst.Function (_, ps, _))))
  | PAst.Binop ("->", ((_, PAst.Function (_, ps, _)), _)) ->
    Some (Option.value ps ~default:[] |> List.filter_map (function
      | PAst.NoDefault id -> Option.map (fun n -> (n, false)) (name id)
      | PAst.Default (id, _) -> Option.map (fun n -> (n, true)) (name id)))
  | _ -> None

(* The parameters of one arrow of a signature, as [formals] gives them, a
   parameter being optional when its type is [T?] (or a union with one).
   [None] when a label is not a constant (label variables, groups). *)
let sig_params (a : (_, _, _) Rstt.FunSig.arrow) =
  let rec optional : (_, _, _) Rstt.Builder.t -> bool = function
    | TOption _ -> true
    | TCup (t1, t2) -> optional t1 || optional t2
    | _ -> false in
  let param (l, (t : (_, _, _) Rstt.FunSig.ty)) =
    match l with
    | Rstt.FunSig.LConst n -> Some (n, match t with FRegular b -> optional b | _ -> false)
    | _ -> None in
  let all f l = List.fold_right (fun x acc ->
    match f x, acc with Some y, Some acc -> Some (y :: acc) | _ -> None) l (Some []) in
  let dots = match a.dom.pos_tl with
    | FRegular (TOption TEmpty) -> []
    | _ -> [ ("...", true) ] in
  match all param a.dom.pos_named, all param a.dom.named with
  | Some before, Some after -> Some (before @ dots @ after)
  | _ -> None

(* Why the signature [annot] does not fit the parameters of [past], if it
   does not: every arrow must have the definition's parameters, in its
   order, and those with a default must be optional. Inference does not
   require it, but a signature that misses them describes another function:
   one that cannot be called as the package calls it. *)
let mismatch past (annot : R_types.Sigs.annot) =
  match formals past, annot with
  | Some formals, AFun arrows ->
    let names l = String.concat ", " (List.map fst l) in
    arrows |> List.find_map (fun a ->
      match sig_params a with
      | None -> None
      | Some ps when List.map fst ps <> List.map fst formals ->
        Some (Printf.sprintf "the parameters must be the definition's, in its order: (%s), \
                              not (%s)" (names formals) (names ps))
      | Some ps ->
        let required = List.filter (fun (n, d) -> d && not (List.assoc n ps)) formals in
        if required = [] then None
        else
          Some (Printf.sprintf "%s %s a default in the definition, so the signature must \
                                make %s optional, as in `%s: <type>?`"
                  (String.concat ", " (List.map (fun (n, _) -> "`" ^ n ^ "`") required))
                  (if List.length required = 1 then "has" else "have")
                  (if List.length required = 1 then "it" else "them")
                  (fst (List.hd required))))
  | _ -> None

(* The signature [signature] of [name], registered in [ctx]: the context
   [treat_def] then checks the definition [past] of [name] in. *)
let declare ctx past ~name signature =
  match R_types.IO.parse_def (name ^ " : " ^ signature) with
  | exception (Rstt_repl.IO.SyntaxError (_, m) | R_types.IO.SyntaxError (_, m)) ->
    Error (if m = "syntax error" then m else "syntax error: " ^ m)
  | exception Invalid_argument m -> Error m
  | exception e -> Error (Printexc.to_string e)
  | R_types.Sigs.Sig (n, annot) as def when n = name ->
    (match mismatch past annot with
     | Some m -> Error m
     | None ->
       try Ok (quietly (fun () -> Driver.add_def ctx 0 def))
       with e -> Error (match e with Failure m | Invalid_argument m -> m | e -> Printexc.to_string e))
  | _ -> Error "not a signature of the definition"

let check ?timeout ctx past ~name signature =
  match declare ctx past ~name signature with
  | Error m -> Invalid m
  | Ok ctx' ->
    let errors = ref [] in
    let saved = !Driver.on_outcome in
    Driver.on_outcome := (fun v o ->
      match o with
      | Driver.Untypeable (err, pos) when Mlsem.Common.Variable.get_name v = Some name ->
        errors := (err, pos) :: !errors
      | _ -> ()) ;
    let run () =
      quietly (fun () ->
        Timeout.guard' timeout ~name ~unchanged:ctx' (fun () -> Driver.treat_def ctx' past)) in
    match Fun.protect run ~finally:(fun () -> Driver.on_outcome := saved) with
    | exception e -> Failed (Printexc.to_string e)
    | _, true -> Timeout (Option.value timeout ~default:0.)
    | ctx'', false ->
      match List.rev !errors, Driver.find ctx'' name with
      | (err, pos) :: _, _ -> Rejected (Report.untypeable_of_error err pos)
      | [], None -> Failed "the definition was checked, but has no type"
      | [], Some ty ->
        let ub = TyScheme.get ty |> snd |> GTy.ub in
        let printed = Report.one_line TyScheme.pp_short ty in
        Accepted { ctx = ctx'' ; ty ; printed ;
                   any_args = Report.any_args printed ; any_result = Report.any_result printed ;
                   degenerate = Report.degenerate ub }
