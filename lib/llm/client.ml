type format = Anthropic | Openai of Openai.dialect | Script

type config = {
  provider : string ;
  format : format ;
  base_url : string ;
  api_key : string option ;
  model : string ;
  timeout : int ;
  connect_timeout : int ;
  retries : int ;
  effort : string option ;
  thinking_display : string option ;
}

(* Name, format, default base URL, and the variable holding the key ([None]:
   a key is optional). *)
let presets = [
  ("anthropic", Anthropic, Some "https://api.anthropic.com", Some "ANTHROPIC_API_KEY") ;
  ("openai", Openai Openai.openai, Some "https://api.openai.com/v1", Some "OPENAI_API_KEY") ;
  ("einfra", Openai Openai.standard, Some "https://llm.ai.e-infra.cz/v1", Some "EINFRA_API_KEY") ;
  ("mistral", Openai Openai.mistral, Some "https://api.mistral.ai/v1", Some "MISTRAL_API_KEY") ;
  ("meta", Openai Openai.standard, Some "https://api.meta.ai/v1", Some "META_API_KEY") ;
  ("openrouter", Openai Openai.standard, Some "https://openrouter.ai/api/v1", Some "OPENROUTER_API_KEY") ;
  ("local", Openai Openai.standard, None, None) ;
]

let providers = List.map (fun (n, _, _, _) -> n) presets @ [ "script" ]

let getenv v = match Sys.getenv_opt v with Some "" | None -> None | s -> s

let strip_slash s =
  if String.ends_with ~suffix:"/" s then String.sub s 0 (String.length s - 1) else s

let of_spec ?(timeout = 300) ?(retries = 4) spec =
  let provider, model = match String.index_opt spec ':' with
    | Some i -> String.sub spec 0 i, String.sub spec (i + 1) (String.length spec - i - 1)
    | None -> failwith (Printf.sprintf "%s: expected provider:model" spec) in
  match List.find_opt (fun (n, _, _, _) -> n = provider) presets with
  | None when provider = "script" ->
    if model = "" then failwith (spec ^ ": no file") ;
    { provider ; format = Script ; base_url = model ; api_key = None ; model ;
      timeout ; connect_timeout = 30 ; retries = 0 ; effort = None ;
      thinking_display = None }
  | None ->
    failwith (Printf.sprintf "unknown LLM provider %s (one of: %s)" provider
                (String.concat ", " providers))
  | Some (_, format, default_url, key_var) ->
    let base_url = match getenv "TYR_LLM_BASE_URL", default_url with
      | Some u, _ | None, Some u -> strip_slash u
      | None, None -> failwith (provider ^ ": TYR_LLM_BASE_URL is not set") in
    let api_key = match key_var with
      | Some v ->
        (match getenv v with
         | Some k -> Some k
         | None -> failwith (Printf.sprintf "%s: %s is not set" provider v))
      | None -> getenv "TYR_LLM_API_KEY" in
    if model = "" then failwith (spec ^ ": no model") ;
    { provider ; format ; base_url ; api_key ; model ; timeout ; connect_timeout = 30 ; retries ;
      effort = None ; thinking_display = None }

exception Error of string

let error_message body =
  let short s = if String.length s <= 300 then s else String.sub s 0 300 ^ "..." in
  match Yojson.Safe.from_string body with
  | j ->
    let open Yojson.Safe.Util in
    (match member "error" j with
     | `Assoc _ as e -> (match member "message" e with `String m -> m | _ -> short body)
     | `String m -> m
     | _ -> short body)
  | exception _ -> short (String.trim body)

let retryable status = status = 408 || status = 409 || status = 429 || status >= 500

(* 1, 2, 4, ... seconds, at most 60; a server's [Retry-After] wins, within
   the same bound. *)
let backoff attempt retry_after =
  let s = match retry_after with Some s -> s | None -> 2. ** float_of_int attempt in
  Float.min 60. (Float.max 0. s)

(* The replies of each script file, and how many were used. *)
let scripts : (string, Yojson.Safe.t array * int ref) Hashtbl.t = Hashtbl.create 4

let scripted cfg =
  let replies, next =
    match Hashtbl.find_opt scripts cfg.base_url with
    | Some s -> s
    | None ->
      let replies = match Yojson.Safe.from_file cfg.base_url with
        | `List l -> Array.of_list l
        | _ -> raise (Error (cfg.base_url ^ ": a list of replies was expected"))
        | exception (Sys_error m | Yojson.Json_error m) -> raise (Error m) in
      let s = (replies, ref 0) in
      Hashtbl.add scripts cfg.base_url s ; s in
  if !next >= Array.length replies then raise (Error (cfg.base_url ^ ": no reply left")) ;
  let reply = replies.(!next) in
  incr next ;
  match reply with
  | `Assoc [ ("error", `String m) ] -> raise (Error ("script: " ^ m))
  | reply ->
    try Openai.response reply
    with Conversation.Bad_response m -> raise (Error ("script: unexpected reply (" ^ m ^ ")"))

(* The body of the reply to [body], from the API: retried on transient
   failures. *)
let fetch cfg ~url ~headers body =
  let fail fmt = Printf.ksprintf (fun s -> raise (Error (cfg.provider ^ ": " ^ s))) fmt in
  let rec attempt n =
    match Http.post ~url ~headers ~body ~timeout:cfg.timeout ~connect_timeout:cfg.connect_timeout with
    | exception Http.Transport_error msg ->
      if n < cfg.retries then (Unix.sleepf (backoff n None) ; attempt (n + 1))
      else fail "%s" msg
    | { status = 200 ; body ; _ } -> body
    | { status ; retry_after ; _ } when retryable status && n < cfg.retries ->
      Unix.sleepf (backoff n retry_after) ; attempt (n + 1)
    | { status ; body ; _ } -> fail "HTTP %d: %s" status (error_message body)
  in
  attempt 0

(* The cache file of a request: a digest of the provider, the model and the
   request body. *)
let cache_file_of_body dir cfg body =
  Filename.concat dir
    (Digest.to_hex (Digest.string (String.concat "\000" [ cfg.provider ; cfg.model ; body ])) ^ ".json")

let rec mkdir_p d =
  if not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d) ;
    try Sys.mkdir d 0o755 with Sys_error _ when Sys.file_exists d -> ()
  end

let write_atomically path contents =
  let tmp = path ^ ".tmp" ^ string_of_int (Unix.getpid ()) in
  Out_channel.with_open_bin tmp (fun oc -> output_string oc contents) ;
  Sys.rename tmp path

(* The URL, headers and body of [r] for an API, and the parser of its
   reply. *)
let http_request cfg r =
  match cfg.format with
  | Anthropic ->
    let key = Option.value cfg.api_key ~default:"" in
    Some (cfg.base_url ^ Anthropic.path, Anthropic.headers ~api_key:key,
          Anthropic.body ?effort:cfg.effort ?display:cfg.thinking_display ~model:cfg.model r,
          Anthropic.response)
  | Openai dialect ->
    Some (cfg.base_url ^ Openai.path, Openai.headers ~api_key:cfg.api_key,
          Openai.body ~dialect ?effort:cfg.effort ~model:cfg.model r, Openai.response)
  | Script -> None

let complete ?cache cfg (r : Conversation.request) =
  match http_request cfg r with
  | None -> scripted cfg
  | Some (url, headers, body, parse) ->
    let body = Yojson.Safe.to_string body in
    let cached = Option.map (fun dir -> cache_file_of_body dir cfg body) cache in
    let reply =
      match cached with
      | Some file when Sys.file_exists file -> In_channel.with_open_bin file In_channel.input_all
      | _ ->
        let reply = fetch cfg ~url ~headers body in
        Option.iter (fun file ->
          mkdir_p (Filename.dirname file) ; write_atomically file reply) cached ;
        reply in
    let fail fmt = Printf.ksprintf (fun s -> raise (Error (cfg.provider ^ ": " ^ s))) fmt in
    match parse (Yojson.Safe.from_string reply) with
    | resp -> resp
    | exception Yojson.Json_error e -> fail "invalid JSON in the reply: %s" e
    | exception Conversation.Bad_response e -> fail "unexpected reply (%s): %s" e (error_message reply)

let cache_file dir cfg r =
  http_request cfg r |> Option.map (fun (_, _, body, _) ->
    cache_file_of_body dir cfg (Yojson.Safe.to_string body))
