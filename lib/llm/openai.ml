(** OpenAI's Chat Completions API ([POST /chat/completions]), which most
    other providers serve too: e-INFRA, Mistral, Meta, OpenRouter, vLLM,
    Ollama.

    The system prompt is the first message. Each tool result is a message of
    its own, with role [tool]. The arguments of a tool call are a JSON
    string. A message of the model is the [message] object of the first
    choice, kept whole in [raw]. *)

open Conversation

let path = "/chat/completions"

(** The name of the token limit: [max_tokens] almost everywhere, but OpenAI's
    own API wants [max_completion_tokens] (its reasoning models reject
    [max_tokens]). *)
type token_limit = Max_tokens | Max_completion_tokens

(** What a server of this format wants of the request, beyond the common
    fields:
    - [token_limit], [seed]: the names of the token limit and of the seed
      ([seed] almost everywhere, [random_seed] at Mistral, which rejects
      unknown fields such as [seed]);
    - [greedy_top_p]: send [top_p: 1] with a temperature of 0. Mistral
      rejects a temperature of 0 ("greedy sampling") unless [top_p] is 1,
      and some of its models default to another [top_p];
    - [cache_key]: a [prompt_cache_key], so that requests sharing a prefix
      read it from the provider's cache (Mistral: billed at 10%). *)
type dialect = {
  token_limit : token_limit ;
  seed : string ;
  greedy_top_p : bool ;
  cache_key : string option ;
}

let standard = { token_limit = Max_tokens ; seed = "seed" ; greedy_top_p = false ; cache_key = None }
let openai = { standard with token_limit = Max_completion_tokens }
let mistral =
  { standard with seed = "random_seed" ; greedy_top_p = true ; cache_key = Some "tyr-llm-fallback" }

let headers ~api_key =
  "content-type: application/json"
  :: (match api_key with Some k -> [ "authorization: Bearer " ^ k ] | None -> [])

(* The fields of a message that are [null]: they carry nothing, and some
   servers reject them on input ([refusal], [audio], ...). *)
let strip_nulls = function
  | `Assoc fields -> `Assoc (List.filter (fun (_, v) -> v <> `Null) fields)
  | j -> j

let messages = function
  | User text -> [ `Assoc [ ("role", `String "user") ; ("content", `String text) ] ]
  | Tool_results results ->
    results |> List.map (fun r ->
      (* The format has no error flag. *)
      let content = if r.is_error then "Error: " ^ r.content else r.content in
      `Assoc [ ("role", `String "tool") ; ("tool_call_id", `String r.call_id) ;
               ("content", `String content) ])
  | Assistant a -> [ strip_nulls a.raw ]

let tool (t : tool) =
  `Assoc [ ("type", `String "function") ;
           ("function", `Assoc [ ("name", `String t.name) ; ("description", `String t.description) ;
                                 ("parameters", t.parameters) ]) ]

let body ?(dialect = standard) ~model (r : request) : json =
  let token_limit = dialect.token_limit in
  let system =
    if r.system = "" then []
    else [ `Assoc [ ("role", `String "system") ; ("content", `String r.system) ] ] in
  let limit = match token_limit with
    | Max_tokens -> "max_tokens" | Max_completion_tokens -> "max_completion_tokens" in
  `Assoc (
    [ ("model", `String model) ;
      ("messages", `List (system @ List.concat_map messages r.messages)) ;
      (limit, `Int r.max_tokens) ]
    @ (if r.tools = [] then [] else [ ("tools", `List (List.map tool r.tools)) ])
    @ (match r.temperature with Some t -> [ ("temperature", `Float t) ] | None -> [])
    @ (match r.temperature with
        | Some 0. when dialect.greedy_top_p -> [ ("top_p", `Float 1.) ]
        | _ -> [])
    @ (match dialect.cache_key with Some k -> [ ("prompt_cache_key", `String k) ] | None -> [])
    @ (match r.seed with Some s -> [ (dialect.seed, `Int s) ] | None -> []))

let response (j : json) : response =
  let open Yojson.Safe.Util in
  try
    let choice = match member "choices" j |> to_list with
      | c :: _ -> c
      | [] -> raise (Bad_response "no choice") in
    let m = member "message" choice in
    (* A string, or (some servers) a list of parts. *)
    let text = match member "content" m with
      | `String s -> s
      | `List parts ->
        parts |> List.filter_map (fun p -> member "text" p |> to_string_option)
        |> String.concat ""
      | _ -> "" in
    let tool_calls =
      member "tool_calls" m |> to_option to_list |> Option.value ~default:[]
      |> List.map (fun c ->
        let f = member "function" c in
        let arguments = match member "arguments" f with
          | `String s -> (try Yojson.Safe.from_string s with Yojson.Json_error _ -> `String s)
          | a -> a in
        { id = member "id" c |> to_string ; name = member "name" f |> to_string ; arguments }) in
    let stop = match member "finish_reason" choice with
      | `String "stop" -> End_turn
      | `String ("tool_calls" | "function_call") -> Tool_use
      | `String "length" -> Max_tokens
      | `String s -> Other s
      | _ -> Other "none" in
    (* A server may leave the usage out. *)
    let count k = match member "usage" j with
      | `Assoc _ as u -> member k u |> to_int_option |> Option.value ~default:0
      | _ -> 0 in
    (* [prompt_tokens] includes the cached ones, which some servers report
       apart. *)
    let cached = match member "usage" j with
      | `Assoc _ as u ->
        (match member "prompt_tokens_details" u with
         | `Assoc _ as d -> member "cached_tokens" d |> to_int_option |> Option.value ~default:0
         | _ -> 0)
      | _ -> 0 in
    { assistant = { raw = m ; text ; tool_calls } ; stop ;
      usage = { input_tokens = count "prompt_tokens" - cached ;
                output_tokens = count "completion_tokens" ; cache_read = cached ; cache_write = 0 ;
                raw = member "usage" j } }
  with Type_error (msg, _) -> raise (Bad_response msg)
