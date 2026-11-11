(* The LLM fallback's conversation ([Tyr.Llm_fallback]), with a scripted
   model and the real checker on the definitions of [signatures.R]: no
   request is made. For each case, the answers the model gets to its checks,
   then the outcome. The prompts come first, so that a change to them shows. *)

open Lang
open Llm.Conversation
module F = Tyr.Llm_fallback
module SC = Tyr.Signature_check

let cfg ?(max_turns = 4) () = {
  F.client = Llm.Client.of_spec "script:none" ; context = F.default_context ; syntax = F.Short ;
  max_turns ; max_tokens = 4096 ; bind = true ; cache = None ; transcript = None }

(* A reply of the model: its text, and the signatures it checks. *)
let reply ?(text = "") ?stop checks =
  let tool_calls = checks |> List.mapi (fun i s ->
    { id = Printf.sprintf "call_%d" i ; name = "check_type" ;
      arguments = `Assoc [ ("signature", `String s) ] }) in
  { assistant = { raw = `Assoc [ ("role", `String "assistant") ] ; text ; tool_calls } ;
    stop = (match stop with Some s -> s | None -> if checks = [] then End_turn else Tool_use) ;
    usage = { input_tokens = 100 ; output_tokens = 10 ; cache_read = 0 ; cache_write = 0 ;
              raw = `Null } }

(* A model that gives the [replies] in order, and prints the answers to its
   checks as it receives them. [None] is an API failure. *)
let scripted replies =
  let rest = ref replies in
  fun (r : request) ->
    (match List.rev r.messages with
     | Tool_results results :: _ ->
       results |> List.iter (fun (t : tool_result) ->
         String.split_on_char '\n' t.content |> List.iteri (fun i l ->
           Printf.printf "%s%s\n" (if i = 0 then "  -> " else "     ") l))
     | _ -> ()) ;
    match !rest with
    | [] -> failwith "the script has no reply left"
    | Some x :: tl -> rest := tl ; x
    | None :: tl -> rest := tl ; raise (Llm.Client.Error "script: HTTP 503: overloaded")

let print_result (r : F.result) =
  (match r.outcome with
   | Typed { signature ; accepted } ->
     Printf.printf "typed: %s (checked as %s)\n" accepted.printed signature
   | Gave_up m -> Printf.printf "gave up: %s\n" m
   | Failed m -> Printf.printf "error: %s\n" m) ;
  Printf.printf "turns %d, checks %d, tokens %d in, %d out\n" r.turns r.checks r.input_tokens
    r.output_tokens

let cases = [
  "fact", "a rejection, then an accepted signature", false,
  4, [ Some (reply [ "(n: dbl1) -> dbl1" ]) ; Some (reply [ "(n: dbl) -> dbl" ]) ;
       Some (reply ~text:"The recursion works on numbers.\n**DONE: (n: dbl) -> dbl**" []) ] ;
  "wrong", "no signature fits: the model gives up", false,
  4, [ Some (reply [ "(x: any) -> chr" ]) ;
       Some (reply ~text:"GIVE UP: toupper is always given a list, which it rejects." []) ] ;
  "count_na", "a signature of any only is refused, a precise one is accepted", false,
  4, [ Some (reply [ "(v: any) -> any" ]) ; Some (reply [ "(v: any) -> INT1" ]) ;
       Some (reply ~text:"DONE: (v: any) -> INT1" []) ] ;
  "double_it", "an invalid signature and a valid one in one turn; the final text names none", false,
  4, [ Some (reply [ "(x: dbl" ; "(x: dbl) -> dbl" ]) ; Some (reply ~text:"That works." []) ] ;
  "double_it", "two accepted signatures: DONE names the first", false,
  4, [ Some (reply [ "(x: dbl) -> dbl" ; "(x: dbl) -> vec" ]) ;
       Some (reply ~text:"DONE: (x: dbl) -> dbl" []) ] ;
  "wrong", "the turn limit: the model is asked to conclude, and gives up", false,
  2, [ Some (reply [ "(x: any) -> chr" ]) ; Some (reply [ "(x: dbl) -> chr" ]) ;
       Some (reply ~text:"GIVE UP: toupper is given a list whatever x is." []) ] ;
  "wrong", "the turn limit: asked to conclude, the model checks again (not run)", false,
  1, [ Some (reply [ "(x: any) -> chr" ]) ; Some (reply [ "(x: dbl) -> chr" ]) ] ;
  "count_na", "a signature no call can match (an empty parameter) is refused", false,
  4, [ Some (reply [ "(v: empty) -> empty" ]) ; Some (reply [ "(v: any) -> INT1" ]) ;
       Some (reply ~text:"DONE: (v: any) -> INT1" []) ] ;
  "count_na", "the turn limit with a signature accepted: no question asked", false,
  1, [ Some (reply [ "(v: any) -> INT1" ]) ] ;
  "double_it", "the model runs out of output tokens", false,
  4, [ Some (reply ~stop:Max_tokens []) ] ;
  "fact", "the API fails after an accepted signature: it is kept", false,
  4, [ Some (reply [ "(n: dbl) -> dbl" ]) ; None ] ;
  "wrong", "the API fails with nothing accepted", false,
  4, [ Some (reply [ "(x: any) -> chr" ]) ; None ] ;
  "wrong", "a turn of rejections with inference's own error ends the conversation", true,
  4, [ Some (reply [ "(x: any) -> chr" ; "(x: any) -> CHR" ]) ] ;
  "scale_by", "parameters other than the definition's, then a result of any (noted)", false,
  4, [ Some (reply [ "(x: dbl, by: dbl) -> dbl" ; "(x: dbl, by: dbl?, ...: any) -> any" ]) ;
       Some (reply ~text:"DONE: (x: dbl, by: dbl?, ...: any) -> any" []) ] ;
  "name_or", "two accepted, neither including the other: noted; the better union is kept, \
              though DONE names another", false,
  4, [ Some (reply [ "(x: null) -> CHR" ; "(x: vec) -> CHR" ]) ;
       Some (reply [ "(x: vec | null) -> CHR" ]) ;
       Some (reply ~text:"DONE: (x: null) -> CHR" []) ] ;
  "name_or", "out of turns: the best accepted is kept, not the last", false,
  1, [ Some (reply [ "(x: vec | null) -> CHR" ; "(x: null) -> CHR" ]) ] ;
  "double_it", "a signature claimed but never checked", false,
  4, [ Some (reply ~text:"DONE: (x: dbl) -> dbl" []) ] ;
]

(* Inference's error: with [real], [wrong]'s own, so that its rejections are
   noted as the same; a stand-in otherwise. *)
let input ?(real = false) ~name ~source () = {
  F.name ; file = "signatures.R" ; source ;
  failure = F.Untypeable (
      if real then
        { title = "untypeable application of base::toupper" ; loc = None ;
          descr = Some "argument: @(list<>)\nfailing call: base::toupper(list())" }
      else { title = "unbound variable" ; loc = None ; descr = Some ("name: " ^ name) }) ;
  callees = [ ("*", "((x: dbl1, y: dbl1) -> dbl1) & ...") ] ;
  docs = Some ("man/fact.Rd", "\\name{fact}\n\\alias{fact}\n\\title{Factorial}") ;
  callsites = [ "use.R:3: fact(5)" ] ;
  examples = [ ("double_it <- function(x) x * 2", "(x: dbl) -> dbl") ] }

let () =
  Driver.setup () ;
  let parse f = match Driver.parse f with Some p -> p | None -> failwith ("cannot parse " ^ f) in
  let prelude = parse "prelude/base.R" and file = parse "signatures.R" in
  print_endline "===== The system prompt (short syntax)\n" ;
  print_endline (F.system_prompt (cfg ())) ;
  print_endline "\n===== The prompt of fact, with every part\n" ;
  let all = { (cfg ()) with context = List.map snd F.part_names } in
  print_string (F.user_prompt all (input ~name:"fact" ~source:"fact <- function(n) ..." ())) ;
  print_endline "\n===== The prompt of fact, after a timeout, source only\n" ;
  print_string (F.user_prompt { (cfg ()) with context = [ F.Source ; F.Error ] }
                  { (input ~name:"fact" ~source:"fact <- function(n) ..." ()) with failure = F.Timed_out 20. }) ;
  let run () =
    let ctx = List.fold_left Driver.declare_overloads Driver.initial_ctx [ prelude ; file ] in
    MetaEnv.declare_classes ctx.Driver.tenv ;
    let ctx = SC.quietly (fun () -> Driver.process ctx prelude) in
    let prog, _ = file in
    cases |> List.iter (fun (name, what, real, max_turns, replies) ->
      Printf.printf "\n===== %s: %s\n" name what ;
      let past = List.find (fun p -> Driver.toplevel_name p = Some name) prog in
      let check = SC.check ~timeout:20. ctx past ~name in
      print_result (F.run (cfg ~max_turns ()) ~complete:(scripted replies) ~check
                      (input ~real ~name ~source:"" ())))
  in
  Mlsem.Types.PEnv.sequential_handler Mlsem.Types.PEnv.empty run () |> ignore
