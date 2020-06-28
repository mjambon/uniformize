(* Exponential moving average. See .mli *)

type param = {
  alpha: float;
  age_min: int; (* derived from alpha *)
}

type t = {
  param: param;
  mutable m: float;
  mutable age: int;
}

let default_alpha = 0.1

let create_param ?(alpha = default_alpha) () =
  if not (alpha >= 0. && alpha <= 1.) then
    invalid_arg "EMA.create_param: invalid alpha";
  let age_min = truncate (ceil (1. /. alpha)) in
  {
    alpha;
    age_min;
  }

let default_param = create_param ()

let init ?(param = default_param) () =
  {
    param;
    m = nan;
    age = 0
  }

let update_age state =
  let age = state.age in
  if age >= 0 then
    state.age <- age + 1

let update state x =
  if x <> x then
    invalid_arg "EMA.update: not a number";
  update_age state;
  let param = state.param in
  let alpha =
    if state.age > param.age_min then
      param.alpha
    else (
      if state.age = 1 then
        (* replace nan to avoid contamination when it's multiplied by 0 *)
        state.m <- 0.;
      (* arithmetic mean until we reach 1/r observations *)
      1. /. float state.age
    )
  in
  state.m <- (1. -. alpha) *. state.m +. alpha *. x

let get state = state.m

let get_obs_count state = state.age

let of_list ?(alpha = default_alpha) data =
  let param = create_param ~alpha () in
  let state = init ~param () in
  List.iter (update state) data;
  get state

(* Equality within some absolute error 'err'. *)
let float_eq ?(err = 1e-6) a b =
  if not (err >= 0.) then
    invalid_arg "Float.eq: invalid 'err' parameter";
  abs_float (a -. b) <= err

let test_initial_samples () =
  let ( =~ ) = float_eq ~err:1e-6 in
  let ema l = of_list ~alpha:0.5 l in
  assert (ema [10.] =~ 10.);
  assert (ema [10.; 20.] =~ 15.);
  assert (ema [10.; 20.; 25.] =~ 20.)

let test_convergence () =
  let ( =~ ) = float_eq ~err:1e-3 in
  let ema l = of_list ~alpha:0.5 l in
  assert (ema [0.; 1.] =~ 0.5);
  assert (ema [0.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1.; 1. ] =~ 1.)

let tests = [
  "initial samples", test_initial_samples;
  "convergence", test_convergence;
]
