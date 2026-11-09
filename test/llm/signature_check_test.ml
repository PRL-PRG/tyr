(* The signature-checking tool of the LLM fallback ([Tyr.Signature_check]),
   on hand-written signatures: no model is involved. For each definition of
   [signatures.R], what inference alone gives, then the outcome of checking
   each candidate signature, every one from the same context (the prelude's).

   What it shows:
   - [fact]: inference fails (a recursive definition is not bound in its own
     body), checking against a signature succeeds;
   - lowercase types are permissive: [chr] is any vector, so
     [(x: dbl) -> chr] is accepted for [double_it];
   - [any] is not free: [(x: any) -> any] is rejected wherever the argument
     is used ([double_it]), accepted where it is not constrained ([count_na]);
   - [wrong] has no type at all; [mutual_a] has none on its own either, as
     [mutual_b] is not bound yet. *)

open Lang
module SC = Tyr.Signature_check

let cases = [
  "double_it", [ "(x: dbl) -> dbl" ; "(x: chr) -> chr" ; "(x: dbl) -> chr" ; "(x: any) -> any" ;
                 "(x: dbl" ; "(x: nosuchtype) -> dbl" ] ;
  "fact", [ "(n: dbl) -> dbl" ; "(n: dbl1) -> dbl1" ] ;
  "first", [ "(x: chr) -> chr" ; "(x: list) -> any" ] ;
  "shout", [ "(s: chr) -> chr" ; "(s: dbl) -> chr" ] ;
  "wrong", [ "(x: any) -> chr" ; "(x: any) -> any" ] ;
  "pick", [ "(flag: lgl1, a: dbl, b: dbl) -> dbl" ; "(flag: lgl1, a: 'a, b: 'a) -> 'a" ] ;
  "count_na", [ "(v: any) -> int1" ; "(v: vec) -> INT1" ] ;
  "mutual_a", [ "(x: any) -> any" ] ;
]

let timeout = Some 20.

let print_outcome = function
  | SC.Accepted a ->
    Printf.printf "accepted: %s%s%s\n" a.printed
      (if a.any_args = [] then "" else "  [any: " ^ String.concat ", " a.any_args ^ "]")
      (if a.degenerate then "  [degenerate]" else "")
  | SC.Invalid m -> Printf.printf "invalid: %s\n" m
  | SC.Rejected u ->
    Printf.printf "rejected: %s\n" u.title ;
    Option.iter (fun d ->
      String.split_on_char '\n' d |> List.iter (Printf.printf "    %s\n")) u.descr
  | SC.Timeout s -> Printf.printf "timeout (%g s)\n" s
  | SC.Failed m -> Printf.printf "failed: %s\n" m

(* What inference alone gives: the definition typed without a signature. *)
let infer ctx past name =
  let error = ref None in
  let saved = !Driver.on_outcome in
  Driver.on_outcome := (fun v o -> match o with
    | Driver.Untypeable (err, pos) when Mlsem.Common.Variable.get_name v = Some name ->
      error := Some (Tyr.Report.untypeable_of_error err pos)
    | _ -> ()) ;
  let r = Fun.protect ~finally:(fun () -> Driver.on_outcome := saved) (fun () ->
    SC.quietly (fun () ->
      Tyr.Timeout.guard' timeout ~name ~unchanged:ctx (fun () -> Driver.treat_def ctx past))) in
  match r, !error with
  | (_, true), _ -> print_endline "inferred: timeout"
  | _, Some u -> Printf.printf "inferred: untypeable (%s)\n" u.title
  | (ctx', false), None ->
    match Driver.find ctx' name with
    | Some ty -> Printf.printf "inferred: %s\n" (Tyr.Report.one_line Mlsem.Types.TyScheme.pp_short ty)
    | None -> print_endline "inferred: no type"

let () =
  Driver.setup () ;
  let parse f = match Driver.parse f with Some p -> p | None -> failwith ("cannot parse " ^ f) in
  let prelude = parse "prelude/base.R" and file = parse "signatures.R" in
  let run () =
    let ctx = List.fold_left Driver.declare_overloads Driver.initial_ctx [ prelude ; file ] in
    MetaEnv.declare_classes ctx.Driver.tenv ;
    let ctx = SC.quietly (fun () -> Driver.process ctx prelude) in
    let prog, _ = file in
    cases |> List.iter (fun (name, signatures) ->
      Printf.printf "\n===== %s\n" name ;
      let past = List.find (fun p -> Driver.toplevel_name p = Some name) prog in
      infer ctx past name ;
      signatures |> List.iter (fun s ->
        Printf.printf "%s\n  " s ;
        print_outcome (SC.check ?timeout ctx past ~name s))) ;
    (* A check cut short by its time limit. *)
    print_endline "\n===== double_it, with a limit of a microsecond" ;
    let past = List.find (fun p -> Driver.toplevel_name p = Some "double_it") prog in
    print_outcome (SC.check ~timeout:1e-6 ctx past ~name:"double_it" "(x: dbl) -> dbl")
  in
  Mlsem.Types.PEnv.sequential_handler Mlsem.Types.PEnv.empty run () |> ignore
