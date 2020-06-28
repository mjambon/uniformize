(**
   Dynamic Signal Uniformization.

   An algorithm for tracking a distribution and estimating the rank of
   sample values.

   Algorithm outline

   Tracker update cycle:
   1. Create an array of n bins.
   2. Accumulate n sample points (floats) into a buffer.
   3. Sort the buffer of n sample points.
   4. Add the data of the sorted buffer to the corresponding bin, which
      tracks an exponential moving average of the samples it receives.
   5. Go to 2.

   Rank estimation:
   Given a query, we use binary search to find the two best bins whose value
   can be used to estimate the rank of the query using linear interpolation.
   The rank is normalized to range from 0 to 1.

   Queries that fall below the value of the lowest bin and above the value of
   the greatest bin use linear interpolation based on the first two or last
   two bins. If this results in a estimated rank outside of the allowed range
   [0, 1], it is rounded to 0 to 1. Therefore, two different extreme values
   may result in the same rank of 0 or 1. So the following doesn't hold
   for extreme values but holds elsewhere:

     a < b => rank(a) < rank(b)

   If this turns out to be a critical property, it can be implemented using
   a non-linear function (for example, an exponential) that converges smoothly
   toward 0 (resp. 1) without reaching it. (See scanned figure in personal
   notes).

   This implementation dates back from April 2019 and was referred to
   privately as rank-normalization. It was published in June 2020.
   Copyright (c) 2020 Martin Jambon
*)

(**
   The type of a distribution tracker.
*)
type t

val default_ema_alpha : float
val default_num_bins : int

(**
   Create a rank tracker.

   @param num_bins specifies the number of bins to use to model the
   distribution.

   @param ema_alpha specifies the alpha parameter used when updating the
   exponential moving averages used to represent each bin.
*)
val create : ?ema_alpha:float -> ?num_bins:int -> unit -> t

val get_num_bins : t -> int

(**
   Add a sample to the tracker.
*)
val add : t -> float -> unit

(**
   Estimate the rank of a sample. The result ranges from [0.] to [1.].
   Half of the samples matching the lowest bin will result in exactly [0.]
   and half of the samples matching the greatest bin will result in exactly
   [1.].
*)
val rank : t -> float -> float

(**
   Add an input sample to the tracker and return its estimated rank.
*)
val map : t -> float -> float

val ema_tests : (string * (unit -> unit)) list
val tests : (string * (unit -> unit)) list
