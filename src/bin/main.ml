(*
   Entrypoint for command line normalizer.
*)

open Printf
open Cmdliner

type config = {
  ema_alpha: float;
  num_bins: int;
  flush_line: bool;
}

let map ~flush_line tracker ic oc =
  try
    while true do
      let x = input_line ic |> float_of_string in
      let y = Uniformize.map tracker x in
      printf "%g\n" y;
      if flush_line then
        flush oc
    done;
    assert false
  with End_of_file ->
    ()

let run config ic oc =
  let tracker =
    Uniformize.create
      ~ema_alpha:config.ema_alpha
      ~num_bins:config.num_bins
      ()
  in
  map ~flush_line:config.flush_line tracker ic oc

let ema_alpha_term =
  let info =
    Arg.info ["ema-alpha"; "a"]
      ~docv:"NUM"
      ~doc:"$(docv) specifies the alpha parameter used by the exponential
            moving average (EMA). It must be a value within (0, 1).
            A greater value gives a greater weight to the recent samples."
  in
  Arg.value (Arg.opt Arg.float Uniformize.default_ema_alpha info)

let num_bins_term =
  let info =
    Arg.info ["num-bins"; "n"]
      ~docv:"N"
      ~doc:"$(docv) specifies the number of points to use to model the
            input distribution.
            A greater value increases the accuracy with which the
            original distribution is modeled but increases memory usage
            accordingly."
  in
  Arg.value (Arg.opt Arg.int Uniformize.default_num_bins info)

let flush_line_term =
  let info =
    Arg.info ["flush-line"; "f"]
     ~doc:"Print each line of output as soon as it is available."
  in
  Arg.value (Arg.flag info)

let cmdline_term =
  let combine ema_alpha num_bins flush_line =
    { ema_alpha; num_bins; flush_line }
  in
  Term.(const combine
        $ ema_alpha_term
        $ num_bins_term
        $ flush_line_term
       )

let doc =
  "make an input signal uniformly distributed over [0, 1]"

let parse_command_line () =
  let info =
    Term.info
      ~doc
      "uniformize"
  in
  match Term.eval (cmdline_term, info) with
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
  | `Ok config -> config

let main () =
  let config = parse_command_line () in
  run config stdin stdout

let () = main ()
