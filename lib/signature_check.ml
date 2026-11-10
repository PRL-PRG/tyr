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
  degenerate : bool ;        (** it can never return (cf. {!Report.degenerate}) *)
}

type outcome =
  | Accepted of accepted
  | Invalid of string              (** not a signature: syntax, unknown type, ... *)
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

(* The signature [signature] of [name], registered in [ctx]: the context
   [treat_def] then checks the definition of [name] in. *)
let declare ctx ~name signature =
  match R_types.IO.parse_def (name ^ " : " ^ signature) with
  | exception (Rstt_repl.IO.SyntaxError (_, m) | R_types.IO.SyntaxError (_, m)) ->
    Error (if m = "syntax error" then m else "syntax error: " ^ m)
  | exception Invalid_argument m -> Error m
  | exception e -> Error (Printexc.to_string e)
  | R_types.Sigs.Sig (n, _) as def when n = name ->
    (try Ok (quietly (fun () -> Driver.add_def ctx 0 def))
     with e -> Error (match e with Failure m | Invalid_argument m -> m | e -> Printexc.to_string e))
  | _ -> Error "not a signature of the definition"

let check ?timeout ctx past ~name signature =
  match declare ctx ~name signature with
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
                   any_args = Report.any_args printed ; degenerate = Report.degenerate ub }
