(** A synchronous HTTP POST, through libcurl.

    [CURLOPT_NOSIGNAL] is set: tyr's timeouts are [SIGALRM] handlers that
    raise, and libcurl would otherwise use [SIGALRM] itself, for the timeouts
    of name resolution. A request must not be made while such a timeout is
    armed: an exception raised during [Curl.perform] would leave the request
    in an unknown state. The request has its own timeouts, libcurl's. *)

type reply = {
  status : int ;
  body : string ;
  retry_after : float option ;  (** [Retry-After], in seconds, when given as such *)
}

(** The request failed before any HTTP status: name resolution, connection,
    TLS, timeout. *)
exception Transport_error of string

let init = lazy (Curl.global_init Curl.CURLINIT_GLOBALALL)

(* [Retry-After: <seconds>] (the form the APIs use; an HTTP date is
   ignored). *)
let retry_after_of_header line =
  match String.index_opt line ':' with
  | Some i when String.lowercase_ascii (String.trim (String.sub line 0 i)) = "retry-after" ->
    float_of_string_opt (String.trim (String.sub line (i + 1) (String.length line - i - 1)))
  | _ -> None

let post ~url ~headers ~body ~timeout ~connect_timeout =
  Lazy.force init ;
  let c = Curl.init () in
  Fun.protect ~finally:(fun () -> Curl.cleanup c) (fun () ->
    let buf = Buffer.create 4096 in
    let retry_after = ref None in
    Curl.set_url c url ;
    Curl.set_nosignal c true ;
    Curl.set_post c true ;
    Curl.set_httpheader c headers ;
    Curl.set_postfields c body ;
    Curl.set_postfieldsize c (String.length body) ;
    Curl.set_timeout c timeout ;
    Curl.set_connecttimeout c connect_timeout ;
    Curl.set_writefunction c (fun s -> Buffer.add_string buf s ; String.length s) ;
    Curl.set_headerfunction c (fun line ->
      Option.iter (fun s -> retry_after := Some s) (retry_after_of_header line) ;
      String.length line) ;
    (try Curl.perform c
     with Curl.CurlException (_, _, msg) -> raise (Transport_error msg)) ;
    { status = Curl.get_responsecode c ; body = Buffer.contents buf ; retry_after = !retry_after })
