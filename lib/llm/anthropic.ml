(** Anthropic's Messages API ([POST /v1/messages]).

    The system prompt is a top-level field. A tool result is a [tool_result]
    block of a user message, and the results of one turn go in one message.
    A message of the model is its list of content blocks ([text], [tool_use],
    [thinking], ...), kept whole in [raw]. There is no seed, and the
    temperature is not sent: recent models reject it ("`temperature` is
    deprecated for this model"), and without a seed it would not make a run
    reproducible anyway. *)

open Conversation

let version = "2023-06-01"

let path = "/v1/messages"

let headers ~api_key =
  [ "x-api-key: " ^ api_key ; "anthropic-version: " ^ version ;
    "content-type: application/json" ]

let message = function
  | User text -> `Assoc [ ("role", `String "user") ; ("content", `String text) ]
  | Tool_results results ->
    let block r =
      `Assoc [ ("type", `String "tool_result") ; ("tool_use_id", `String r.call_id) ;
               ("content", `String r.content) ; ("is_error", `Bool r.is_error) ] in
    `Assoc [ ("role", `String "user") ; ("content", `List (List.map block results)) ]
  | Assistant a -> `Assoc [ ("role", `String "assistant") ; ("content", a.raw) ]

let tool (t : tool) =
  `Assoc [ ("name", `String t.name) ; ("description", `String t.description) ;
           ("input_schema", t.parameters) ]

(* [effort] ([output_config.effort]: low, medium, high, xhigh or max) sets
   how much the model thinks; recent models think by default, at their own
   default effort. [display] ([summarized] or [omitted]) is whether the
   [thinking] blocks of the reply hold a summary of the reasoning or nothing:
   it changes only what is returned, not what is thought or billed. *)
let ephemeral = `Assoc [ ("type", `String "ephemeral") ]

let body ?effort ?display ~model (r : request) : json =
  `Assoc (
    [ ("model", `String model) ; ("max_tokens", `Int r.max_tokens) ]
    @ (match display with
        | Some d -> [ ("thinking", `Assoc [ ("type", `String "adaptive") ; ("display", `String d) ]) ]
        | None -> [])
    @ (match effort with
        | Some e -> [ ("output_config", `Assoc [ ("effort", `String e) ]) ]
        | None -> [])
    (* Prompt caching: a cache point at the end of the system prompt, which
       the tools precede, the same for every conversation of a run; and the
       automatic one (top-level), which follows the end of the messages, so
       that each turn reads the previous ones from the cache. 5 minutes. *)
    @ [ ("cache_control", ephemeral) ]
    @ (if r.system = "" then []
       else [ ("system", `List [ `Assoc [ ("type", `String "text") ; ("text", `String r.system) ;
                                          ("cache_control", ephemeral) ] ]) ])
    @ [ ("messages", `List (List.map message r.messages)) ]
    @ (if r.tools = [] then [] else [ ("tools", `List (List.map tool r.tools)) ]))

let response (j : json) : response =
  let open Yojson.Safe.Util in
  try
    let content = member "content" j in
    let blocks = to_list content in
    let of_type ty b = member "type" b = `String ty in
    let text =
      blocks |> List.filter (of_type "text")
      |> List.map (fun b -> member "text" b |> to_string) |> String.concat "" in
    let tool_calls =
      blocks |> List.filter (of_type "tool_use")
      |> List.map (fun b ->
        { id = member "id" b |> to_string ; name = member "name" b |> to_string ;
          arguments = member "input" b }) in
    let stop = match member "stop_reason" j with
      | `String ("end_turn" | "stop_sequence") -> End_turn
      | `String "tool_use" -> Tool_use
      | `String "max_tokens" -> Max_tokens
      | `String s -> Other s
      | _ -> Other "none" in
    (* A server may leave the usage out. *)
    let count k = match member "usage" j with
      | `Assoc _ as u -> member k u |> to_int_option |> Option.value ~default:0
      | _ -> 0 in
    { assistant = { raw = content ; text ; tool_calls } ; stop ;
      usage = { input_tokens = count "input_tokens" ; output_tokens = count "output_tokens" ;
                cache_read = count "cache_read_input_tokens" ;
                cache_write = count "cache_creation_input_tokens" ; raw = member "usage" j } }
  with Type_error (msg, _) -> raise (Bad_response msg)
