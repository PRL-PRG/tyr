(* The translation of conversations to and from the two API formats, on fixed
   replies: no request is made. Each reply is parsed, and its message sent
   back in the next request, which is printed. *)

open Llm
open Conversation

let pp_json j = print_endline (Yojson.Safe.pretty_to_string j)

let string_of_stop = function
  | End_turn -> "end_turn" | Tool_use -> "tool_use" | Max_tokens -> "max_tokens"
  | Other s -> "other: " ^ s

let print_response (r : response) =
  Printf.printf "stop: %s, tokens: %d in, %d out, cache: %d read, %d written\n"
    (string_of_stop r.stop) r.usage.input_tokens r.usage.output_tokens r.usage.cache_read
    r.usage.cache_write ;
  Printf.printf "text: %S\n" r.assistant.text ;
  r.assistant.tool_calls |> List.iter (fun c ->
    Printf.printf "tool call %s: %s %s\n" c.id c.name (Yojson.Safe.to_string c.arguments))

let check_type = {
  name = "check_type" ;
  description = "Check a signature against the definition." ;
  parameters = Yojson.Safe.from_string
      {|{"type": "object", "properties": {"signature": {"type": "string"}}, "required": ["signature"]}|} ;
}

(* The conversation after the model's first answer [a]: one result per tool
   call, the second of them an error. *)
let next a =
  let results = a.tool_calls |> List.mapi (fun i c ->
    { call_id = c.id ; content = if i = 0 then "accepted" else "parse error" ; is_error = i > 0 }) in
  request ~system:"You type R functions." ~tools:[ check_type ] ~max_tokens:1024
    ~temperature:0. ~seed:7
    [ User "Type f." ; Assistant a ; Tool_results results ]

let section s = Printf.printf "\n===== %s =====\n" s

let () =
  section "Anthropic: thinking, text and two tool calls" ;
  let r = Anthropic.response (Yojson.Safe.from_string {|{
    "id": "msg_1", "type": "message", "role": "assistant", "model": "claude",
    "content": [
      {"type": "thinking", "thinking": "Let me check.", "signature": "sig=="},
      {"type": "text", "text": "I will check two signatures."},
      {"type": "tool_use", "id": "toolu_A", "name": "check_type", "input": {"signature": "(x: dbl) -> dbl"}},
      {"type": "tool_use", "id": "toolu_B", "name": "check_type", "input": {"signature": "(x: "}}
    ],
    "stop_reason": "tool_use",
    "usage": {"input_tokens": 120, "output_tokens": 45,
              "cache_read_input_tokens": 3000, "cache_creation_input_tokens": 500}}|}) in
  print_response r ;
  print_endline "next request:" ;
  pp_json (Anthropic.body ~model:"claude-x" (next r.assistant)) ;
  print_endline "with an effort and a summary of the thinking (the fields before \"messages\"):" ;
  (match Anthropic.body ~effort:"high" ~display:"summarized" ~model:"claude-x" (next r.assistant) with
   | `Assoc fields ->
     fields |> List.filter (fun (k, _) -> List.mem k [ "thinking" ; "output_config" ])
     |> List.iter (fun (k, v) -> Printf.printf "%s: %s\n" k (Yojson.Safe.to_string v))
   | _ -> ()) ;

  section "OpenAI: reasoning_content, null fields, arguments as a string" ;
  let r = Openai.response (Yojson.Safe.from_string {|{
    "id": "c1", "object": "chat.completion",
    "choices": [{"index": 0, "finish_reason": "tool_calls", "message": {
      "role": "assistant", "content": null, "refusal": null,
      "reasoning_content": "The argument is numeric.",
      "tool_calls": [
        {"id": "call_1", "type": "function", "function": {"name": "check_type", "arguments": "{\"signature\": \"(x: dbl) -> dbl\"}"}},
        {"id": "abc123XYZ", "type": "function", "function": {"name": "check_type", "arguments": "{not json"}}
      ]}}],
    "usage": {"prompt_tokens": 200, "completion_tokens": 30,
              "prompt_tokens_details": {"cached_tokens": 150}}}|}) in
  print_response r ;
  print_endline "next request (max_tokens):" ;
  pp_json (Openai.body ~model:"glm" (next r.assistant)) ;
  print_endline "the token limit, OpenAI's own:" ;
  (match Openai.body ~dialect:Openai.openai ~model:"gpt" (next r.assistant) with
   | `Assoc fields -> List.iter (fun (k, _) -> if String.starts_with ~prefix:"max" k then print_endline k) fields
   | _ -> ()) ;
  print_endline "Mistral's fields (it rejects seed, and temperature 0 unless top_p is 1):" ;
  (match Openai.body ~dialect:Openai.mistral ~model:"mistral-large-4" (next r.assistant) with
   | `Assoc fields ->
     fields |> List.iter (fun (k, v) ->
       if List.mem k [ "random_seed" ; "seed" ; "temperature" ; "top_p" ; "prompt_cache_key" ] then
         Printf.printf "%s: %s\n" k (Yojson.Safe.to_string v))
   | _ -> ()) ;

  section "OpenAI: content as parts, finish_reason stop with a tool call" ;
  print_response (Openai.response (Yojson.Safe.from_string {|{
    "choices": [{"finish_reason": "stop", "message": {
      "role": "assistant", "content": [{"type": "text", "text": "Done"}, {"type": "text", "text": "."}],
      "tool_calls": [{"id": "x", "type": "function", "function": {"name": "check_type", "arguments": {"signature": "() -> null"}}}]}}]}|})) ;

  section "OpenAI: the end, no usage" ;
  print_response (Openai.response (Yojson.Safe.from_string {|{
    "choices": [{"finish_reason": "length", "message": {"role": "assistant", "content": "I give up: "}}]}|})) ;

  section "Replies of the wrong shape" ;
  let bad name f s =
    match f (Yojson.Safe.from_string s) with
    | _ -> Printf.printf "%s: parsed\n" name
    | exception Bad_response e -> Printf.printf "%s: Bad_response (%s)\n" name e in
  bad "openai, no choice" Openai.response {|{"choices": []}|} ;
  bad "openai, no choices field" Openai.response {|{"object": "error"}|} ;
  bad "anthropic, no content" Anthropic.response {|{"stop_reason": "end_turn"}|} ;

  section "Error messages" ;
  List.iter (fun b -> print_endline (Client.error_message b)) [
    {|{"type": "error", "error": {"type": "overloaded_error", "message": "Overloaded"}}|} ;
    {|{"error": {"message": "Invalid model: foo", "type": "invalid_request_error"}}|} ;
    {|{"error": "rate limited"}|} ;
    "<html><body>502 Bad Gateway</body></html>\n" ;
  ] ;

  section "Retry-After" ;
  List.iter (fun h -> Printf.printf "%S: %s\n" h
                (match Http.retry_after_of_header h with Some s -> string_of_float s | None -> "-"))
    [ "Retry-After: 12\r\n" ; "retry-after:3.5" ; "Retry-After: Wed, 21 Oct 2026 07:28:00 GMT" ;
      "content-type: application/json" ] ;

  section "Provider specs" ;
  Unix.putenv "EINFRA_API_KEY" "k1" ;
  Unix.putenv "MISTRAL_API_KEY" "" ;
  Unix.putenv "TYR_LLM_BASE_URL" "" ;
  let spec s =
    match Client.of_spec s with
    | c -> Printf.printf "%s: %s, %s, model %s, %s\n" s c.provider c.base_url c.model
             (match c.format with Anthropic -> "anthropic"
                                | Openai { token_limit = Max_tokens ; seed ; _ } -> "openai/max_tokens/" ^ seed
                                | Openai { token_limit = Max_completion_tokens ; seed ; _ } ->
                                  "openai/max_completion_tokens/" ^ seed
                                | Script -> "script")
    | exception Failure m -> Printf.printf "%s: %s\n" s m in
  List.iter spec [ "einfra:glm-5.3" ; "einfra:glm-5.3:high" ; "mistral:mistral-large-4" ;
                   "nowhere:x" ; "einfra" ; "einfra:" ; "local:qwen" ] ;
  Unix.putenv "TYR_LLM_BASE_URL" "http://localhost:11434/v1/" ;
  List.iter spec [ "local:qwen" ; "einfra:glm-5.3" ; "script:replies.json" ; "script:" ] ;

  section "Script" ;
  let file = Filename.temp_file "llm_test" ".json" in
  Out_channel.with_open_text file (fun oc -> output_string oc {|[
    {"choices": [{"finish_reason": "stop", "message": {"role": "assistant", "content": "one"}}]},
    {"error": "the server is down"}
  ]|}) ;
  let script = Client.of_spec ("script:" ^ file) in
  let ask () =
    match Client.complete script (request [ User "hi" ]) with
    | r -> Printf.printf "reply: %s\n" r.assistant.text
    | exception Client.Error m ->
      (* The file name is a temporary one. *)
      print_endline ("error: " ^ Str.global_replace (Str.regexp_string file) "FILE" m) in
  ask () ; ask () ; ask () ;
  Sys.remove file ;

  section "Cache" ;
  (* Nothing listens on port 9: a request that reaches the network fails at
     once, so a reply can only come from the cache. *)
  Unix.putenv "TYR_LLM_BASE_URL" "http://127.0.0.1:9/v1" ;
  let local = Client.of_spec ~retries:0 "local:m" in
  let dir = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "llm_cache_%d" (Unix.getpid ())) in
  let req = request ~seed:3 [ User "cached?" ] in
  let ask () =
    match Client.complete ~cache:dir local req with
    | r -> Printf.printf "reply: %s\n" r.assistant.text
    | exception Client.Error _ -> print_endline "error: no server, no cached reply" in
  ask () ;
  let cached = Option.get (Client.cache_file dir local req) in
  Printf.printf "cache file written: %b\n" (Sys.file_exists cached) ;
  Sys.mkdir dir 0o755 ;
  Out_channel.with_open_text cached (fun oc -> output_string oc {|
    {"choices": [{"finish_reason": "stop", "message": {"role": "assistant", "content": "from the cache"}}]}|}) ;
  ask () ;
  Printf.printf "another request, same file: %b\n"
    (Client.cache_file dir local (request ~seed:4 [ User "cached?" ]) = Some cached) ;
  Sys.remove cached ; Sys.rmdir dir
