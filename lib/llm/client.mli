(** A language model behind an API: which one, and one turn of a
    conversation with it. *)

type format = Anthropic | Openai of Openai.token_limit

type config = {
  provider : string ;        (** its name, for messages *)
  format : format ;
  base_url : string ;        (** without a trailing slash *)
  api_key : string option ;  (** [None] for a local server that needs none *)
  model : string ;
  timeout : int ;            (** seconds, for a whole request *)
  connect_timeout : int ;    (** seconds *)
  retries : int ;            (** additional attempts after a transient failure *)
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
      the key in [TYR_LLM_API_KEY] if it needs one.
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
    @raise Error *)
val complete : config -> Conversation.request -> Conversation.response

(** The message of an API's error reply ([{"error": {"message": ...}}] in
    both formats), or the reply itself, shortened. *)
val error_message : string -> string
