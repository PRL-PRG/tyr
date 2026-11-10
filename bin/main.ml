let usage =
  "tyr [--deps] [--gradual] [--debug] [--timeout SECONDS] [--prelude FILE]\n\
  \      [--report FILE] [--fallback-c-signature] [--fallback-mode MODE]\n\
  \      [--lambda-rec SECONDS] [--log-times]\n\
  \      [--call-graph FILE]\n\
  \      [--fuzz-runner CMD [--fuzz-harness DIR] [--fuzz-samples N] [--fuzz-seed N]\n\
  \       [--fuzz-dir DIR] [--fuzz-call-timeout SECONDS]]\n\
  \      [--llm PROVIDER:MODEL [--llm-context PARTS] [--llm-syntax short|full]\n\
  \       [--llm-max-turns N] [--llm-max-tokens N] [--llm-no-bind] [--llm-cache DIR]\n\
  \       [--llm-transcript FILE]]\n\
  \      [-I DIR] <package-directory>"

let deps_only = ref false
let gradual = ref false
let debug = ref false
let timeout = ref None
let include_dirs = ref []
let prelude = ref []
let report = ref None
let fallback = ref false
let fallback_mode = ref None
let log_times = ref false
let lambda_rec = ref None
let call_graph = ref None
let root = ref None
let fuzz_runner = ref None
(* Next to the binary, as dune lays out its build directory: the image keeps
   that layout. *)
let fuzz_harness =
  ref (Filename.concat (Filename.dirname (Filename.dirname Sys.executable_name)) "fuzz")
let fuzz_samples = ref 8
let fuzz_seed = ref 1
let fuzz_dir = ref "fuzz-results"
let fuzz_call_timeout = ref 5.
let llm = ref None
let llm_context = ref None
let llm_syntax = ref "short"
let llm_max_turns = ref 4
let llm_max_tokens = ref 32768
let llm_bind = ref true
let llm_cache = ref None
let llm_transcript = ref None

let speclist =
  [ ("--deps", Arg.Set deps_only,
     "Only report the dependencies between R and native functions") ;
    ("--debug", Arg.Set debug, "Print intermediate information") ;
    ("--gradual", Arg.Set gradual,
     "Give the dyn type to the R names nothing binds, instead of reporting them") ;
    ("--timeout", Arg.Float (fun f -> timeout := Some f),
     "SECONDS  Give up on a function whose type-checking takes longer") ;
    ("-I", Arg.String (fun d -> include_dirs := !include_dirs @ [d]),
     "DIR  Additional directory to search for C headers") ;
    ("--prelude", Arg.String (fun f -> prelude := !prelude @ [f]),
     "FILE  R file of signatures to load before the package (repeatable)") ;
    ("--report", Arg.String (fun f -> report := Some f),
     "FILE  Write a machine-readable account of the run (one JSON record per line)") ;
    ("--fallback-c-signature", Arg.Set fallback,
     "Bind a native function that fails to type at its declared C signature") ;
    ("--fallback-mode", Arg.Symbol ([ "all" ; "except-all-sexp" ; "none" ], fun m -> fallback_mode := Some m),
     " Which functions --fallback-c-signature applies to: all of them (the default), \
      all but those whose parameters and result are all SEXP, or none. Implies \
      --fallback-c-signature, but for none") ;
    ("--lambda-rec", Arg.Float (fun f -> lambda_rec := Some f),
     "SECONDS  Also type recursive native functions as recursive groups, within that time") ;
    ("--log-times", Arg.Set log_times,
     "Print NativeSem's per-function timing lines (the report has them regardless)") ;
    ("--call-graph", Arg.String (fun f -> call_graph := Some f),
     "FILE  Write the native call graph in Graphviz format") ;
    ("--fuzz-runner", Arg.String (fun c -> fuzz_runner := Some c),
     "CMD  Fuzz the inferred types: run the R harness with this command (e.g. Rscript, or a sandbox around it)") ;
    ("--fuzz-harness", Arg.Set_string fuzz_harness,
     "DIR  Directory of harness.R and describe.R (default: fuzz/ next to bin/)") ;
    ("--fuzz-samples", Arg.Set_int fuzz_samples,
     "N  Calls per function, inside its domain and outside each (default 8)") ;
    ("--fuzz-seed", Arg.Set_int fuzz_seed, "N  Seed of the fuzzing (default 1)") ;
    ("--fuzz-dir", Arg.Set_string fuzz_dir,
     "DIR  Where the samples and results files go (default fuzz-results)") ;
    ("--fuzz-call-timeout", Arg.Set_float fuzz_call_timeout,
     "SECONDS  Time limit of one call when fuzzing (default 5)") ;
    ("--llm", Arg.String (fun s -> llm := Some s),
     "PROVIDER:MODEL  Ask this model for the R definitions inference fails on \
      (providers: " ^ String.concat ", " Llm.Client.providers ^ ")") ;
    ("--llm-context", Arg.String (fun s -> llm_context := Some s),
     "PARTS  What the model gets, comma-separated, among source, error, callees, docs, \
      callsites, examples (default source,error,callees)") ;
    ("--llm-syntax", Arg.Symbol ([ "short" ; "full" ], fun s -> llm_syntax := s),
     " How the type language is explained: a short reference (the default) or the full documents") ;
    ("--llm-max-turns", Arg.Set_int llm_max_turns, "N  Turns per definition (default 4)") ;
    ("--llm-max-tokens", Arg.Set_int llm_max_tokens,
     "N  Output tokens per turn, reasoning included (default 32768)") ;
    ("--llm-no-bind", Arg.Clear llm_bind,
     "Do not bind the types the model finds for the definitions that follow") ;
    ("--llm-cache", Arg.String (fun d -> llm_cache := Some d),
     "DIR  Cache the model's replies there, and reuse them") ;
    ("--llm-transcript", Arg.String (fun f -> llm_transcript := Some f),
     "FILE  Write the conversations there (one JSON record per line)") ]

let () =
  Printexc.record_backtrace true ;
  (* NativeSem configures the printers when its library is loaded; this adds
     what the R side needs on top (in particular [void_ty]). *)
  Lang.Driver.setup () ;
  Arg.parse speclist (fun a -> root := Some a) usage ;
  match !root with
  | None -> prerr_endline usage ; exit 1
  | Some root when not (Sys.file_exists root && Sys.is_directory root) ->
    Printf.eprintf "tyr: not a package directory: %s\n" root ; exit 1
  | Some _ when !gradual && !fuzz_runner <> None ->
    prerr_endline "tyr: --fuzz-runner cannot be used with --gradual (dyn has no values to draw)" ;
    exit 2
  | Some root ->
    let fuzz = !fuzz_runner |> Option.map (fun runner ->
      { Tyr.Fuzz.runner ; harness = !fuzz_harness ; samples = !fuzz_samples ;
        seed = !fuzz_seed ; dir = !fuzz_dir ; call_timeout = !fuzz_call_timeout }) in
    (* [--fallback-mode] picks the functions the fallback applies to, and
       turns it on unless it is [none]. *)
    let fallback_on, fallback_mode =
      match !fallback_mode with
      | Some "none" -> false, R_c_typing.Runner.All
      | Some "except-all-sexp" -> true, R_c_typing.Runner.Except_all_sexp
      | Some _ -> true, R_c_typing.Runner.All
      | None -> !fallback, R_c_typing.Runner.All in
    let native =
      { Tyr.Pipeline.default_native_options with
        debug = !debug ; fallback_c_signature = fallback_on ; log_times = !log_times ;
        lambda_rec = !lambda_rec ;
        call_graph = !call_graph } in
    let llm =
      try
        !llm |> Option.map (fun spec ->
          { Tyr.Llm_fallback.client = Llm.Client.of_spec spec ;
            context = (match !llm_context with
              | Some s -> Tyr.Llm_fallback.parts_of_string s
              | None -> Tyr.Llm_fallback.default_context) ;
            syntax = (if !llm_syntax = "full" then Full else Short) ;
            max_turns = max 1 !llm_max_turns ; max_tokens = max 256 !llm_max_tokens ;
            bind = !llm_bind ; cache = !llm_cache ;
            transcript = Option.map open_out !llm_transcript })
      with Failure m | Sys_error m -> Printf.eprintf "tyr: --llm: %s\n" m ; exit 2 in
    Tyr.Pipeline.run
      { native ; prelude = !prelude ; include_dirs = !include_dirs ;
        timeout = !timeout ; gradual = !gradual ; deps_only = !deps_only ;
        report = !report ; fuzz ; fallback_mode ; llm } root
