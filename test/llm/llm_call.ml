(* One conversation with a real model, by hand, to check an API end to end:
     dune exec test/llm/llm_call.exe -- provider:model [--raw]
   The model has one tool, [check_type], played by a fake checker that
   rejects the first signature it gets and accepts the next ones. A working
   API shows a tool call, an error result, another call, then the end. With
   [--raw], the messages of the model are printed as the API returned them. *)

open Llm
open Conversation

let check_type = {
  name = "check_type" ;
  description = "Check a type signature against the R definition. Answers \"accepted\" or the type error." ;
  parameters = Yojson.Safe.from_string
      {|{"type": "object",
         "properties": {"signature": {"type": "string", "description": "The signature, e.g. (x: dbl) -> dbl"}},
         "required": ["signature"]}|} ;
}

let system =
  "You give types to R functions. Signatures are written (x: T, y: U) -> V, \
   with the types dbl (numbers), chr (strings), lgl (logicals) and any. \
   Check each candidate with the check_type tool. Once a signature is \
   accepted, answer DONE followed by it."

let prompt = "Type this definition:\n\nf <- function(x) x * 2\n"

let () =
  let usage () = prerr_endline "usage: llm_call provider:model [--raw] [--effort LEVEL]" ; exit 2 in
  let rec args (spec, raw, effort) = function
    | [] -> (spec, raw, effort)
    | "--raw" :: l -> args (spec, true, effort) l
    | "--effort" :: e :: l -> args (spec, raw, Some e) l
    | s :: l when spec = None && s.[0] <> '-' -> args (Some s, raw, effort) l
    | _ -> usage () in
  let spec, raw, effort = args (None, false, None) (List.tl (Array.to_list Sys.argv)) in
  let spec = match spec with Some s -> s | None -> usage () in
  let cfg = try Client.of_spec spec with Failure m -> prerr_endline m ; exit 2 in
  let cfg = { cfg with effort } in
  Printf.printf "%s, model %s, at %s\n%!" cfg.provider cfg.model cfg.base_url ;
  let checks = ref 0 in
  let answer (c : tool_call) =
    incr checks ;
    let signature = Yojson.Safe.Util.(member "signature" c.arguments |> to_string_option) in
    match signature with
    | None -> { call_id = c.id ; content = "missing argument: signature" ; is_error = true }
    | Some _ when !checks = 1 ->
      { call_id = c.id ; is_error = true ;
        content = "untypeable: the argument may be \"a\", not a number" }
    | Some _ -> { call_id = c.id ; content = "accepted" ; is_error = false } in
  let rec turn n messages =
    if n > 4 then print_endline "(stopped after 4 turns)"
    else begin
      let t0 = Unix.gettimeofday () in
      let r = Client.complete cfg
          (request ~system ~tools:[ check_type ] ~max_tokens:2048 ~temperature:0. ~seed:1 messages) in
      Printf.printf "\n-- turn %d (%.1f s, %d tokens in, %d out, stop %s)\n" n
        (Unix.gettimeofday () -. t0) r.usage.input_tokens r.usage.output_tokens
        (match r.stop with End_turn -> "end_turn" | Tool_use -> "tool_use"
                         | Max_tokens -> "max_tokens" | Other s -> s) ;
      if raw then print_endline (Yojson.Safe.pretty_to_string r.assistant.raw) ;
      if r.assistant.text <> "" then Printf.printf "text: %s\n" (String.trim r.assistant.text) ;
      match r.assistant.tool_calls with
      | [] -> ()
      | calls ->
        let results = List.map (fun c ->
          let res = answer c in
          Printf.printf "call %s %s -> %s\n" c.id (Yojson.Safe.to_string c.arguments) res.content ;
          res) calls in
        turn (n + 1) (messages @ [ Assistant r.assistant ; Tool_results results ])
    end in
  try turn 1 [ User prompt ]
  with Client.Error m -> Printf.printf "error: %s\n" m ; exit 1
