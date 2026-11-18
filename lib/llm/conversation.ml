(** A conversation with a language model, independent of the API that carries
    it.

    What we write ourselves (the system prompt, the user's messages, the
    results of the tools) is described here, and each API module translates
    it. A message of the model is kept as the API returned it, in [raw], and
    sent back unchanged: the fields a server adds (Mistral's
    [reasoning_content], Anthropic's [thinking] blocks) survive, and the ids
    of its tool calls are the server's own. A conversation never moves from
    one API to another, so a raw message never needs translating. *)

type json = Yojson.Safe.t

(** A tool the model may call. [parameters] is the JSON Schema of its
    arguments, an object. *)
type tool = { name : string ; description : string ; parameters : json }

(** A call of a tool by the model. [arguments] is a JSON object, or the
    string the model wrote when it is not valid JSON. *)
type tool_call = { id : string ; name : string ; arguments : json }

(** The answer to a tool call [call_id]. *)
type tool_result = { call_id : string ; content : string ; is_error : bool }

(** A message of the model: as the API returned it ([raw]), and what we read
    in it. *)
type assistant = { raw : json ; text : string ; tool_calls : tool_call list }

type message =
  | User of string
  | Tool_results of tool_result list
  | Assistant of assistant

(** [seed] is ignored by the APIs that have none (Anthropic). *)
type request = {
  system : string ;
  messages : message list ;
  tools : tool list ;
  max_tokens : int ;
  temperature : float option ;
  seed : int option ;
}

(** Why the model stopped. Some servers say [End_turn] although the message
    has tool calls: what to do next is decided by [tool_calls], not by
    [stop]. *)
type stop = End_turn | Tool_use | Max_tokens | Other of string

(** The tokens of one turn. [input_tokens] are those billed at the full
    price; [cache_read] the input read from the provider's prompt cache (about
    a tenth of the price at Anthropic), [cache_write] the input written to it
    (a quarter more than the full price). The whole prompt is the sum of the
    three. [raw] is the provider's own [usage] object ([`Null] if there is
    none), whose fields differ between providers. *)
type usage = {
  input_tokens : int ; output_tokens : int ; cache_read : int ; cache_write : int ; raw : json }

type response = { assistant : assistant ; stop : stop ; usage : usage }

(** A reply that does not have the shape the API documents. *)
exception Bad_response of string

let request ?(system = "") ?(tools = []) ?(max_tokens = 4096) ?temperature ?seed messages =
  { system ; messages ; tools ; max_tokens ; temperature ; seed }
