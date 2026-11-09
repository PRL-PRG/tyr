(** Anthropic's Messages API ([POST /v1/messages]).

    The system prompt is a top-level field. A tool result is a [tool_result]
    block of a user message, and the results of one turn go in one message.
    A message of the model is its list of content blocks ([text], [tool_use],
    [thinking], ...), kept whole in [raw]. There is no seed. *)

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

let body ~model (r : request) : json =
  `Assoc (
    [ ("model", `String model) ; ("max_tokens", `Int r.max_tokens) ]
    @ (if r.system = "" then [] else [ ("system", `String r.system) ])
    @ [ ("messages", `List (List.map message r.messages)) ]
    @ (if r.tools = [] then [] else [ ("tools", `List (List.map tool r.tools)) ])
    @ (match r.temperature with Some t -> [ ("temperature", `Float t) ] | None -> []))

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
      usage = { input_tokens = count "input_tokens" ; output_tokens = count "output_tokens" } }
  with Type_error (msg, _) -> raise (Bad_response msg)
