(** A language model behind an API: which one, and one turn of a
    conversation with it. *)

(** [Script]: no API, the replies listed in a file (cf. {!of_spec}). *)
type format = Anthropic | Openai of Openai.dialect | Script

type config = {
  provider : string ;        (** its name, for messages *)
  format : format ;
  base_url : string ;        (** without a trailing slash *)
  api_key : string option ;  (** [None] for a local server that needs none *)
  model : string ;
  timeout : int ;            (** seconds, for a whole request *)
  connect_timeout : int ;    (** seconds *)
  retries : int ;            (** additional attempts after a transient failure *)
  effort : string option ;   (** [output_config.effort] for Anthropic, [reasoning_effort] otherwise *)
  thinking_display : string option ;
  (** Anthropic only: whether the thinking blocks hold a summary
      ([summarized]) or nothing ([omitted], the default of recent models) *)
}

(** The providers known by name: their format, default base URL and the
    environment variable holding their key:
    - [anthropic]: Anthropic, [ANTHROPIC_API_KEY];
    - [openai]: OpenAI, [OPENAI_API_KEY];
    - [einfra]: e-INFRA CZ, [EINFRA_API_KEY];
    - [mistral]: Mistral, [MISTRAL_API_KEY];
    - [meta]: Meta's Model API, [META_API_KEY];
    - [openrouter]: OpenRouter, [OPENROUTER_API_KEY];
    - [local]: any server of OpenAI's format, at [TYR_LLM_BASE_URL], with
      the key in [TYR_LLM_API_KEY] if it needs one;
    - [script]: no model: [script:FILE] answers each request with the next
      reply of [FILE], a JSON list of replies in OpenAI's format, or
      [{"error": "message"}] for a failure. For tests.
    [TYR_LLM_BASE_URL] also overrides the base URL of the others. *)
val providers : string list

(** [of_spec "provider:model"]: the model [model] of a known provider
    (the model is everything after the first [:]).
    @raise Failure if the provider is unknown or its key is not set. *)
val of_spec : ?timeout:int -> ?retries:int -> string -> config

(** The request or its reply failed for good (after the retries): an HTTP
    error, a transport error, or a reply of the wrong shape. *)
exception Error of string

(** One turn: the model's answer to [request]. Retries, with exponential
    backoff (or the server's [Retry-After]), on transport errors and on the
    HTTP statuses 408, 409, 429 and 5xx.

    With [cache], a directory: the reply to a request is read from there if
    the same request (provider, model, body) was made before, and saved there
    otherwise. A rerun with the same inputs then replays the same
    conversation, without any request. A script is never cached.
    @raise Error *)
val complete : ?cache:string -> config -> Conversation.request -> Conversation.response

(** The file of [cache] that holds the reply to [request], whether or not it
    exists ([None] for a script). *)
val cache_file : string -> config -> Conversation.request -> string option

(** The message of an API's error reply ([{"error": {"message": ...}}] in
    both formats), or the reply itself, shortened. *)
val error_message : string -> string
