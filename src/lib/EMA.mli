(*
   Exponential moving average.

   See https://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average
*)

type param

type t = private {
  param: param;

  mutable m: float;
    (* Current estimate of the average, initially nan. *)

  mutable age: int;
    (* Number of observations. *)
}

val default_alpha : float
val create_param : ?alpha:float -> unit -> param
val default_param : param

val init : ?param:param -> unit -> t

val update : t -> float -> unit

val get : t -> float
  (* Get the current average. *)

val get_obs_count : t -> int
  (* Return the number of observations. *)

val of_list : ?alpha:float -> float list -> float
  (* Shorthand for getting the EMA from a list of numbers.
     This is for evaluation purposes. *)

val tests : (string * (unit -> unit)) list
