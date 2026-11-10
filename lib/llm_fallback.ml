(** The LLM fallback: when inference fails on an R definition, ask a language
    model for a signature, and check it.

    The model gets the definition and what the context options select
    ({!part}), and one tool, [check_type], which checks the definition against
    a signature ({!Signature_check}). It has a few turns. Only signatures the
    tool accepted count: the model's final message either accepts (the last
    accepted signature is kept, or the one it names if that one was accepted)
    or gives up with a short explanation. Out of turns with nothing accepted,
    the model is asked once more to conclude, so that it says why; out of
    turns otherwise, or when the API fails, the last accepted signature is
    kept. A signature whose parameters and result are all [any] is refused: it
    says nothing. A rejection that repeats inference's error says so: no
    signature can then help, and the model is told to give up.

    Inference's result is not touched: the pipeline reports the fallback's
    outcome next to it, and binds an accepted type for the definitions that
    follow when [bind] is set. *)

open Llm
open Conversation

(* ===== Configuration ===== *)

(** What the prompt contains besides the instructions:
    - [Source]: the definition's code;
    - [Error]: why inference failed (its error, or its time limit);
    - [Callees]: the types of the names the definition uses;
    - [Docs]: the package's documentation of the function ([man/*.Rd]);
    - [Callsites]: the lines of the package that call it;
    - [Examples]: definitions of the same file that did type, with their
      types. *)
type part = Source | Error | Callees | Docs | Callsites | Examples

let part_names =
  [ ("source", Source) ; ("error", Error) ; ("callees", Callees) ; ("docs", Docs) ;
    ("callsites", Callsites) ; ("examples", Examples) ]

let string_of_part p = fst (List.find (fun (_, q) -> q = p) part_names)

let parts_of_string s =
  String.split_on_char ',' s |> List.map String.trim |> List.filter (( <> ) "")
  |> List.map (fun n ->
    match List.assoc_opt n part_names with
    | Some p -> p
    | None ->
      failwith (Printf.sprintf "unknown context part %s (one of: %s)" n
                  (String.concat ", " (List.map fst part_names))))

let default_context = [ Source ; Error ; Callees ]

(** How the type language is explained: a short reference written for the
    fallback, or r-typing-pipeline's two documents in full. *)
type syntax = Short | Full

type config = {
  client : Client.config ;
  context : part list ;
  syntax : syntax ;
  max_turns : int ;
  max_tokens : int ;                  (** output tokens per turn, reasoning included *)
  bind : bool ;                       (** bind an accepted type for what follows *)
  cache : string option ;             (** cf. {!Llm.Client.complete} *)
  transcript : out_channel option ;   (** the conversations, as JSONL *)
}

(* ===== What the prompt is made of ===== *)

type failure = Untypeable of Report.untypeable | Timed_out of float

(** Gathered by the pipeline; only the parts of [config.context] are sent. *)
type input = {
  name : string ;
  file : string ;
  source : string ;
  failure : failure ;
  callees : (string * string) list ;    (** name, type *)
  docs : (string * string) option ;     (** file, text *)
  callsites : string list ;
  examples : (string * string) list ;   (** source, type *)
}

let replace what by s = Str.global_replace (Str.regexp_string what) (String.trim by) s

let system_prompt cfg =
  let syntax = match cfg.syntax with
    | Short -> Prompts.syntax_short
    | Full -> Prompts.type_system ^ "\n\n" ^ Prompts.annotation_guide in
  Prompts.system
  |> replace "{{max_turns}}" (string_of_int cfg.max_turns)
  |> replace "{{syntax}}" syntax
  |> String.trim

let user_prompt cfg (i : input) =
  let b = Buffer.create 4096 in
  let add fmt = Printf.bprintf b fmt in
  add "Find a signature of `%s`.\n" i.name ;
  cfg.context |> List.iter (function
    | Source -> add "\n## The definition (%s)\n\n```r\n%s\n```\n" i.file (String.trim i.source)
    | Error ->
      add "\n## Why inference failed\n\n" ;
      (match i.failure with
       | Untypeable u ->
         add "%s\n" u.title ;
         Option.iter (add "%s\n") u.descr
       | Timed_out s -> add "It took longer than %g s.\n" s)
    | Callees ->
      if i.callees <> [] then begin
        add "\n## The types of the names it uses\n\n" ;
        List.iter (fun (n, t) -> add "- `%s`: `%s`\n" n t) i.callees
      end
    | Docs ->
      Option.iter (fun (f, t) -> add "\n## Its documentation (%s)\n\n```\n%s\n```\n" f (String.trim t))
        i.docs
    | Callsites ->
      if i.callsites <> [] then begin
        add "\n## Where the package calls it\n\n```r\n" ;
        List.iter (add "%s\n") i.callsites ;
        add "```\n"
      end
    | Examples ->
      if i.examples <> [] then begin
        add "\n## Definitions of the same file, with the types the checker found\n" ;
        List.iter (fun (src, ty) -> add "\n```r\n%s\n```\nType: `%s`\n" (String.trim src) ty)
          i.examples
      end) ;
  Buffer.contents b

let check_type = {
  name = "check_type" ;
  description =
    "Check the definition against a signature. Answers \"accepted: <type>\", \
     \"rejected: <reason>\" (often with a witness: a value or call for which the \
     definition fails) or \"invalid signature: <reason>\"." ;
  parameters = Yojson.Safe.from_string {|{
    "type": "object",
    "properties": {"signature": {"type": "string",
                                 "description": "The type only, without the function's name, e.g. (x: dbl, n: int1?) -> CHR"}},
    "required": ["signature"]}|} ;
}

(* ===== The conversation ===== *)

type outcome =
  | Typed of { signature : string ; accepted : Signature_check.accepted }
  | Gave_up of string
  | Failed of string  (** the API failed *)

type result = {
  outcome : outcome ;
  turns : int ;
  checks : int ;
  input_tokens : int ;
  output_tokens : int ;
  elapsed : float ;
}

let log cfg kind fields =
  cfg.transcript |> Option.iter (fun oc ->
    output_string oc (Yojson.Safe.to_string (`Assoc (("k", `String kind) :: fields))) ;
    output_char oc '\n' ;
    flush oc)

(* The system prompt goes to the transcript once per run. *)
let system_logged = ref false

let trivial (a : Signature_check.accepted) =
  Report.trivial (Mlsem.Types.TyScheme.get a.ty |> snd |> Mlsem.Types.GTy.ub)

(* Whether a rejection is the error inference reported: same title, same
   first line of description (the witness below may differ). *)
let same_error (u : Report.untypeable) = function
  | Some (i : Report.untypeable) ->
    let first d = Option.map (fun d -> List.hd (String.split_on_char '\n' d)) d in
    u.title = i.title && first u.descr = first i.descr
  | None -> false

(* The answer to one check, and the accepted signature it yields, if any.
   [inference] is inference's error, if it reported one. *)
let answer ~check ?inference signature =
  match check signature with
  | Signature_check.Accepted a when trivial a ->
    "rejected: too imprecise: every parameter and the result are `any`. \
     Give a more precise signature.", true, None
  | Signature_check.Accepted a ->
    let notes =
      (if a.any_args = [] then []
       else [ "note: typed `any`: " ^ String.concat ", " a.any_args ])
      @ (if a.degenerate then [ "note: this type says the function never returns" ] else []) in
    String.concat "\n" (("accepted: " ^ a.printed) :: notes), false, Some (signature, a)
  | Signature_check.Invalid m -> "invalid signature: " ^ m, true, None
  | Signature_check.Rejected u ->
    let note =
      if same_error u inference then
        [ "note: this is the error inference reported. It does not depend on the \
           signature: the failure is in the body, or in a function it calls." ]
      else [] in
    String.concat "\n" ((("rejected: " ^ u.title) :: Option.to_list u.descr) @ note), true, None
  | Signature_check.Timeout s -> Printf.sprintf "checking took longer than %g s" s, true, None
  | Signature_check.Failed m -> "the checker failed: " ^ m, true, None

let shorten n s = if String.length s <= n then s else String.sub s 0 n ^ "..."

(* The line of [text] that starts with [prefix] (ignoring case, and the
   Markdown a model may wrap it in), and what follows the prefix. *)
let line_after prefix text =
  let clean l =
    String.trim l |> Str.global_replace (Str.regexp "^[*`_ ]+\\|[*`_ ]+$") "" in
  String.split_on_char '\n' text |> List.map clean
  |> List.find_map (fun l ->
    let p = String.length prefix in
    if String.length l >= p
    && String.uppercase_ascii (String.sub l 0 p) = String.uppercase_ascii prefix
    then Some (String.sub l p (String.length l - p)
               |> Str.global_replace (Str.regexp "^[*`: ]+\\|[*` ]+$") "")
    else None)

(* What the model's final message means, given the signatures accepted so far
   (latest first). *)
let conclude text accepted =
  match line_after "GIVE UP" text, line_after "DONE" text, accepted with
  | Some why, _, _ -> Gave_up (if why = "" then shorten 300 (String.trim text) else why)
  | None, done_sig, (s, a) :: _ ->
    (* The signature it names, if that one was accepted; the last one
       otherwise. *)
    let named = Option.bind done_sig (fun d ->
      List.find_opt (fun (s', _) -> String.trim s' = d) accepted) in
    let signature, accepted = Option.value named ~default:(s, a) in
    Typed { signature ; accepted }
  | None, Some d, [] when d <> "" -> Gave_up ("claimed a signature that was not accepted: " ^ d)
  | None, _, [] -> Gave_up ("no signature accepted: " ^ shorten 300 (String.trim text))

let run cfg ~complete ~check (i : input) =
  let t0 = Unix.gettimeofday () in
  let system = system_prompt cfg in
  if not !system_logged then begin
    log cfg "llm_system" [ ("text", `String system) ] ;
    system_logged := true
  end ;
  let user = user_prompt cfg i in
  log cfg "llm_prompt"
    [ ("def", `String i.name) ; ("file", `String i.file) ; ("model", `String cfg.client.model) ;
      ("context", `List (List.map (fun p -> `String (string_of_part p)) cfg.context)) ;
      ("text", `String user) ] ;
  let accepted = ref [] and checks = ref 0 and tin = ref 0 and tout = ref 0 in
  let finish outcome turns =
    let fields = match outcome with
      | Typed t -> [ ("outcome", `String "typed") ; ("signature", `String t.signature) ;
                     ("type", `String t.accepted.printed) ]
      | Gave_up m -> [ ("outcome", `String "gave_up") ; ("explanation", `String m) ]
      | Failed m -> [ ("outcome", `String "error") ; ("explanation", `String m) ] in
    log cfg "llm_end" (("def", `String i.name) :: fields) ;
    { outcome ; turns ; checks = !checks ; input_tokens = !tin ; output_tokens = !tout ;
      elapsed = Unix.gettimeofday () -. t0 } in
  (* Out of turns, or of API: the last accepted signature, if any. *)
  let last_accepted ~otherwise n =
    match !accepted with
    | (signature, accepted) :: _ -> finish (Typed { signature ; accepted }) n
    | [] -> finish otherwise n in
  let inference = match i.failure with Untypeable u -> Some u | Timed_out _ -> None in
  (* The model's last message, when it checks nothing more. *)
  let conclude_reply (r : response) =
    if String.trim r.assistant.text = "" && !accepted = [] then
      Gave_up (match r.stop with
        | Max_tokens ->
          Printf.sprintf "the model ran out of output tokens (%d) before answering" cfg.max_tokens
        | _ -> "the model gave no answer")
    else conclude r.assistant.text !accepted in
  (* Out of turns with nothing accepted, the model is asked once more, to
     conclude: give up with a reason, most likely. Checks it still asks for
     then are not run. *)
  let closing =
    "You have no turns left. Answer now, without checking anything more, with one \
     line: `DONE: <an accepted signature>` or `GIVE UP: <one sentence saying why no \
     signature works>`." in
  let rec turn n ~last messages =
    match complete (request ~system ~tools:[ check_type ] ~max_tokens:cfg.max_tokens
                      ~temperature:0. ~seed:1 messages) with
    | exception Client.Error m -> last_accepted ~otherwise:(Failed m) (n - 1)
    | r ->
      tin := !tin + r.usage.input_tokens ;
      tout := !tout + r.usage.output_tokens ;
      let results =
        if last then []
        else r.assistant.tool_calls |> List.map (fun (c : tool_call) ->
          let signature = Yojson.Safe.Util.(member "signature" c.arguments |> to_string_option) in
          let content, is_error, signature = match c.name, signature with
            | "check_type", Some s ->
              incr checks ;
              let content, is_error, ok = answer ~check ?inference s in
              Option.iter (fun a -> accepted := a :: !accepted) ok ;
              content, is_error, Some s
            | "check_type", None -> "missing argument: signature", true, None
            | name, _ -> "unknown tool: " ^ name, true, None in
          signature, { call_id = c.id ; content ; is_error }) in
      log cfg "llm_turn"
        [ ("def", `String i.name) ; ("turn", `Int n) ; ("closing", `Bool last) ;
          ("reply", r.assistant.raw) ;
          ("input_tokens", `Int r.usage.input_tokens) ; ("output_tokens", `Int r.usage.output_tokens) ;
          ("checks", `List (results |> List.map (fun (s, (res : tool_result)) ->
             `Assoc [ ("signature", match s with Some s -> `String s | None -> `Null) ;
                      ("answer", `String res.content) ]))) ] ;
      match results with
      | [] -> finish (conclude_reply r) n
      | _ when n >= cfg.max_turns ->
        (match !accepted with
         | _ :: _ -> last_accepted ~otherwise:(Gave_up "") n
         | [] ->
           turn (n + 1) ~last:true
             (messages @ [ Assistant r.assistant ; Tool_results (List.map snd results) ; User closing ]))
      | _ ->
        turn (n + 1) ~last:false
          (messages @ [ Assistant r.assistant ; Tool_results (List.map snd results) ])
  in
  turn 1 ~last:false [ User user ]
