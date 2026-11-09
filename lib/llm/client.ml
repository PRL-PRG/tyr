type format = Anthropic | Openai of Openai.token_limit

type config = {
  provider : string ;
  format : format ;
  base_url : string ;
  api_key : string option ;
  model : string ;
  timeout : int ;
  connect_timeout : int ;
  retries : int ;
}

(* Name, format, default base URL, and the variable holding the key ([None]:
   a key is optional). *)
let presets = [
  ("anthropic", Anthropic, Some "https://api.anthropic.com", Some "ANTHROPIC_API_KEY") ;
  ("openai", Openai Max_completion_tokens, Some "https://api.openai.com/v1", Some "OPENAI_API_KEY") ;
  ("einfra", Openai Max_tokens, Some "https://llm.ai.e-infra.cz/v1", Some "EINFRA_API_KEY") ;
  ("mistral", Openai Max_tokens, Some "https://api.mistral.ai/v1", Some "MISTRAL_API_KEY") ;
  ("meta", Openai Max_tokens, Some "https://api.meta.ai/v1", Some "META_API_KEY") ;
  ("openrouter", Openai Max_tokens, Some "https://openrouter.ai/api/v1", Some "OPENROUTER_API_KEY") ;
  ("local", Openai Max_tokens, None, None) ;
]

let providers = List.map (fun (n, _, _, _) -> n) presets

let getenv v = match Sys.getenv_opt v with Some "" | None -> None | s -> s

let strip_slash s =
  if String.ends_with ~suffix:"/" s then String.sub s 0 (String.length s - 1) else s

let of_spec ?(timeout = 300) ?(retries = 4) spec =
  let provider, model = match String.index_opt spec ':' with
    | Some i -> String.sub spec 0 i, String.sub spec (i + 1) (String.length spec - i - 1)
    | None -> failwith (Printf.sprintf "%s: expected provider:model" spec) in
  match List.find_opt (fun (n, _, _, _) -> n = provider) presets with
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
    { provider ; format ; base_url ; api_key ; model ; timeout ; connect_timeout = 30 ; retries }

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

let complete cfg (r : Conversation.request) =
  let url, headers, body, parse = match cfg.format with
    | Anthropic ->
      let key = Option.value cfg.api_key ~default:"" in
      cfg.base_url ^ Anthropic.path, Anthropic.headers ~api_key:key,
      Anthropic.body ~model:cfg.model r, Anthropic.response
    | Openai token_limit ->
      cfg.base_url ^ Openai.path, Openai.headers ~api_key:cfg.api_key,
      Openai.body ~token_limit ~model:cfg.model r, Openai.response in
  let body = Yojson.Safe.to_string body in
  let fail fmt = Printf.ksprintf (fun s -> raise (Error (cfg.provider ^ ": " ^ s))) fmt in
  let rec attempt n =
    match Http.post ~url ~headers ~body ~timeout:cfg.timeout ~connect_timeout:cfg.connect_timeout with
    | exception Http.Transport_error msg ->
      if n < cfg.retries then (Unix.sleepf (backoff n None) ; attempt (n + 1))
      else fail "%s" msg
    | { status = 200 ; body ; _ } ->
      (match parse (Yojson.Safe.from_string body) with
       | resp -> resp
       | exception Yojson.Json_error e -> fail "invalid JSON in the reply: %s" e
       | exception Conversation.Bad_response e -> fail "unexpected reply (%s): %s" e (error_message body))
    | { status ; retry_after ; _ } when retryable status && n < cfg.retries ->
      Unix.sleepf (backoff n retry_after) ; attempt (n + 1)
    | { status ; body ; _ } -> fail "HTTP %d: %s" status (error_message body)
  in
  attempt 0
