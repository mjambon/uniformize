(* see mli *)

open Printf

let debug = false

type buffer = {
  buf_array : float array;
  mutable buf_count : int;
}

type t = {
  num_bins : int;
  mutable num_rounds : int;
  averages : EMA.t array;
  buffer : buffer;
}

let default_ema_alpha = 0.01
let default_num_bins = 101

let get_num_bins t = t.num_bins

let create_buffer capacity =
  {
    buf_array = Array.make capacity nan;
    buf_count = 0;
  }

let create
    ?(ema_alpha = default_ema_alpha)
    ?(num_bins = default_num_bins)
    () =

  if num_bins <= 0 then
    invalid_arg (
      sprintf "Rank_normalize.create: num_bins=%i must be >= 1"
        num_bins
    );
  let ema_param = EMA.create_param ~alpha:ema_alpha () in
  let averages =
    Array.init num_bins (fun _ -> EMA.init ~param:ema_param ())
  in
  {
    num_bins;
    num_rounds = 0;
    averages;
    buffer = create_buffer num_bins;
  }

let add_array averages samples =
  Array.sort (compare : float -> float -> int) samples;
  Array.iteri (fun i x ->
    EMA.update averages.(i) x
  ) samples

let add t x =
  if not (Float.is_finite x) then
    invalid_arg (sprintf "Rank_normalize.add: %g" x);
  let n = t.num_bins in
  let b = t.buffer in
  let pos = b.buf_count in
  b.buf_array.(pos) <- x;
  let count = pos + 1 in
  if count = n then (
    add_array t.averages b.buf_array;
    b.buf_count <- 0;
    t.num_rounds <- t.num_rounds + 1;
  )
  else
    b.buf_count <- count

(*
   "Procedure for finding the leftmost element" from Wikipedia.

   Returns a whole rank, as you would obtain if you were inserting the
   performance of a contestant and calculate their rank. Therefore,
   this cannot return a rank less than 0 (first) but can return a rank of
   n (new dead last among n+1). In case of a tie, the most favorable (lowest)
   rank is returned.

   This assumes a sorted array and returns the leftmost position such that
   all elements to its left are strictly lower than the query.
   See the tests for examples.

  function binary_search_leftmost(A, n, T):
    L := 0
    R := n
    while L < R:
        m := floor((L + R) / 2)
        if A[m] < T:
            L := m + 1
        else:
            R := m
    return L
*)
let find_rank_from_left get a x =
  let n = Array.length a in
  let left = ref 0 in
  let right = ref n in
  while !left < !right do
    let m = (!left + !right) / 2 in
    if get a m < x then
      left := m + 1
    else
      right := m
  done;
  !left

let string_of_array a =
   a |> Array.map string_of_float |> Array.to_list |> String.concat " "

let test_find_rank_from_left () =
  let f a x =
    let rank = find_rank_from_left Array.get a x in
    if debug then
      printf "[%s] query: %g -> rank: %i\n" (string_of_array a) x rank;
    rank
  in
  assert (f [| 1. |] 2. = 1);
  assert (f [| 1. |] 1. = 0);
  assert (f [| 1. |] 0. = 0);
  assert (f [| 1.; 2. |] 1.5 = 1);
  assert (f [| 1.; 2. |] 2.5 = 2);
  assert (f [| 1.; 2.; 3. |] 2.5 = 2);
  assert (f [| 1.; 2.; 2.; 3. |] 2. = 1);
  assert (f [| 1.; 2.; 2.; 3. |] 2.5 = 3);
  assert (f [| 1.; 2.; 2.; 2.; 3. |] 2. = 1);
  assert (f [| 1.; 2.; 2.; 2.; 3. |] 2.5 = 4)

(*
   Assuming a sorted array and a query, find the position of the most suitable
   lower value to be used in linear interpolation.

   [ 3; 4 ], query = 3.5 -> 0
   [ 3; 4 ], query = 3   -> 0
   [ 3; 4 ], query = 1   -> -1   (out of bounds on purpose)
   [ 3; 4 ], query = 6   -> 1

   See the tests for examples.
*)
let find_inf get a x =
  if Array.length a = 0 then
    invalid_arg "Rank_normalize.find_inf: empty array";
  let rank = find_rank_from_left get a x in
  if rank = Array.length a || x < get a rank then
    rank - 1
  else
    rank

let test_find_inf () =
  let inf a x =
    let inf = find_inf Array.get a x in
    printf "[%s] query: %g -> inf: %i\n" (string_of_array a) x inf;
    inf
  in
  assert (inf [| 1. |] 2. = 0);
  assert (inf [| 1. |] 1. = 0);
  assert (inf [| 1. |] 0. = -1);
  assert (inf [| 1.; 2. |] 1.5 = 0);
  assert (inf [| 1.; 2. |] 2.5 = 1);
  assert (inf [| 1.; 2.; 3. |] 2.5 = 1);
  assert (inf [| 1.; 2.; 2.; 3. |] 2. = 1);
  assert (inf [| 1.; 2.; 2.; 3. |] 2.5 = 2);
  assert (inf [| 1.; 2.; 2.; 2.; 3. |] 2. = 1);
  assert (inf [| 1.; 2.; 2.; 2.; 3. |] 2.5 = 3)

(*
   "Procedure for finding the rightmost element" from Wikipedia.
   Same as find_rank_from_left, but determines the rank from the last position.

   See the tests for examples.

   Original code from Wikipedia:

   function binary_search_rightmost(A, n, T):
    L := 0
    R := n
    while L < R:
        m := floor((L + R) / 2)
        if A[m] <= T:
            L := m + 1
        else:
            R := m
    return L - 1
*)
let find_rank_from_right get a x =
  let n = Array.length a in
  let left = ref 0 in
  let right = ref n in
  while !left < !right do
    let m = (!left + !right) / 2 in
    if get a m <= x then
      left := m + 1
    else
      right := m
  done;
  !right - 1

let test_find_rank_from_right () =
  let f a x =
    let rank = find_rank_from_right Array.get a x in
    printf "[%s] query: %g -> rank: %i\n" (string_of_array a) x rank;
    rank
  in
  assert (f [| 1. |] 2. = 0);
  assert (f [| 1. |] 1. = 0);
  assert (f [| 1. |] 0. = -1);
  assert (f [| 1.; 2. |] 1.5 = 0);
  assert (f [| 1.; 2. |] 0.5 = -1);
  assert (f [| 0.; 1.; 2.; |] 0.5 = 0);
  assert (f [| 1.; 2.; 2.; 3. |] 2. = 2);
  assert (f [| 1.; 2.; 2.; 3. |] 1.5 = 0);
  assert (f [| 1.; 2.; 2.; 2.; 3. |] 2. = 3);
  assert (f [| 1.; 2.; 2.; 2.; 3. |] 1.5 = 0)

(*
   Assuming a sorted array and a query, find the position of the most suitable
   higher value to be used in linear interpolation.

   [ 3; 4 ], query = 3.5 -> 1
   [ 3; 4 ], query = 3   -> 0
   [ 3; 4 ], query = 1   -> 0
   [ 3; 4 ], query = 6   -> 2  (out of bounds on purpose)

   See the tests for examples.
*)
let find_sup get a x =
  if Array.length a = 0 then
    invalid_arg "Rank_normalize.find_sup: empty array";
  let rank = find_rank_from_right get a x in
  if rank = -1 || x > get a rank then
    rank + 1
  else
    rank

let test_find_sup () =
  let f a x =
    let sup = find_sup Array.get a x in
    printf "[%s] query: %g -> sup: %i\n" (string_of_array a) x sup;
    sup
  in
  assert (f [| 1. |] 2. = 1);
  assert (f [| 1. |] 1. = 0);
  assert (f [| 1. |] 0. = 0);
  assert (f [| 1.; 2. |] 1.5 = 1);
  assert (f [| 1.; 2. |] 0.5 = 0);
  assert (f [| 0.; 1.; 2.; |] 0.5 = 1);
  assert (f [| 1.; 2.; 2.; 3. |] 2. = 2);
  assert (f [| 1.; 2.; 2.; 3. |] 1.5 = 1);
  assert (f [| 1.; 2.; 2.; 2.; 3. |] 2. = 3);
  assert (f [| 1.; 2.; 2.; 2.; 3. |] 1.5 = 1)

(*
   Estimate the rank of a query in an unsorted array by scanning the whole
   array.
   This is used in the initialization phase, before num_bins have been added
   to the tracker.
*)
let find_rank_in_buffer b x =
  let n = b.buf_count in
  match n with
  | 0 | 1 -> 0.5
  | _ ->
      (*
         left_count and right_count are the number of elements lower than
         and greater than the query.
         We don't keep track of which elements are closest to the query,
         so don't do any interpolation to obtain a non-discrete rank.
      *)
      let left_count = ref 0 in
      let right_count = ref 0 in
      let a = b.buf_array in
      for i = 0 to n - 1 do
        let c = compare a.(i) x in
        if c < 0 then
          incr left_count
        else if c > 0 then
          incr right_count
      done;
      assert (!left_count + !right_count <= n);
      (* inf and sup are the indices, possibly out of bounds, of the positions
         of the elements surrounding the query in a sorted array:
         query = 2
         [| 3; 4.5 |] -> inf = -1, sup = 0
         [| 0; 2; 2; 3.1 |] -> inf = 1, sup = 2
         [| 0; 0 |] -> inf = 1, sup = 2
      *)
      let inf = !left_count - 1 in
      let sup = n - !right_count in
      assert (inf < sup);
      (*
         Index i represents the bin whose ranks span [i, i+1] and of average
         rank i + 0.5.
         Hence, we add 0.5 to inf and 0.5 to sup to obtain the average rank
         of the corresponding bins.
         The rank of the query is the average of the ranks of the bins
         inf and sup.
      *)
      let rank = 0.5 *. float (inf + sup + 1) in
      rank /. float n

let test_find_rank_in_buffer () =
  let f array query =
    let b = {
      buf_array = Array.copy array;
      buf_count = Array.length array;
    } in
    let rank = find_rank_in_buffer b query in
    printf "[%s] query: %g -> rank = %g\n" (string_of_array array) query rank;
    rank
  in
  assert (f [| |] 2. = 0.5);
  assert (f [| 3. |] 2. = 0.5);
  assert (f [| 3. |] 4. = 0.5);
  assert (f [| 3.; 4. |] 1. = 0.);
  assert (f [| 3.; 4. |] 3. = 0.25);
  assert (f [| 4.; 3. |] 3. = 0.25);
  assert (f [| 3.; 4. |] 3.5 = 0.5);
  assert (f [| 3.; 4. |] 3.1 = 0.5);
  assert (f [| 3.; 4. |] 4. = 0.75);
  assert (f [| 3.; 4. |] 5. = 1.)

(*
   Linear interpolation: find y such that (x, y) lies on the line that goes
   through (x1, y1) and (x2, y2).
*)
let interpolate x1 y1 x2 y2 x =
  if debug then
    printf "(x1=%g, y1=%i) (x2=%g, y2=%i) x=%g\n"
      x1 y1 x2 y2 x;
  if x1 = x2 then
    if x < x1 then
      float y1
    else if x > x2 then
      float y2
    else (* x = x1 = x2 *)
      0.5 *. (float y1 +. float y2)
  else
    let slope = float (y2 - y1) /. (x2 -. x1) in
    float y1 +. slope *. (x -. x1)

let test_interpolate () =
  let f x1 y1 x2 y2 x =
    let y = interpolate x1 y1 x2 y2 x in
    printf "(x1=%g, y1=%i) (x2=%g, y2=%i) x=%g -> y=%g\n"
      x1 y1 x2 y2 x y;
    y
  in
  let ( =~ ) a b = abs_float (a -. b) < 1e-6 in
  assert (f 0. 0 1. 1 0.5 =~ 0.5);
  assert (f 0. 0 1. 1 0. =~ 0.);
  assert (f 0. 0 1. 1 1. =~ 1.);
  assert (f 0. 0 1. 1 (-1.) =~ -1.);
  assert (f 10. 10 1. 1 0.5 =~ 0.5);
  assert (f 10. 10 1. 1 0. =~ 0.);
  assert (f 10. 10 1. 1 1. =~ 1.);
  assert (f 10. 10 1. 1 (-1.) =~ -1.);
  assert (f 2. 0 3. 1 3.5 =~ 1.5);

  (* x1 = x2 *)
  assert (f 0. 10 0. 20 0. =~ 15.);
  assert (f 0. 10 0. 20 (-1.) =~ 10.);
  assert (f 0. 10 0. 20 1. =~ 20.)

let normalize t x =
  match t.num_rounds with
  | 0 ->
      (* The bins haven't been initialized yet, fall back to scanning the
         buffer. *)
      let buffer = t.buffer in
      find_rank_in_buffer buffer x
  | _ ->
      (*
         The bins have been initialized. We use binary search and linear
         interpolation to estimate the rank such that

              a < b => rank(a) < rank(b) or rank(b) = 0 or rank(a) = 1
              a = b => rank(a) = rank(b)
              For all x, rank(x) >= 0 and rank(x) <= 1

              (assuming no rounding errors)

         - If the query is equal to the values in one or more bins, the average
           rank of these bins is returned.
         - If the query falls to the left of the minimum (value in bin 0),
           then the values in bins 0 and 1 are used for linear interpolation.
         - If the query falls to the right of the maximum (value in bin n-1),
           then the values in bins n-2 and n-1 are used for linear
           interpolation.
         - If the query falls between the values of two (consecutive) bins,
           the values in these two bins are used for linear interpolation.
      *)
      match t.num_bins with
      | 0 | 1 -> 0.5
      | n ->
          let a = t.averages in
          let get a i = EMA.get a.(i) in
          let inf = find_inf get a x in
          let sup = find_sup get a x in
          assert (inf >= -1);
          assert (inf <= sup);
          assert (sup <= n);
          let rank =
            if inf = -1 then
              let inf = inf + 1 in
              let sup = sup + 1 in
              max 0.
                (0.5 +. interpolate (get a inf) inf (get a sup) sup x)
            else if sup = n then
              let inf = inf - 1 in
              let sup = sup - 1 in
              min (float n)
                (0.5 +. interpolate (get a inf) inf (get a sup) sup x)
            else
              let a_inf = get a inf in
              let a_sup = get a sup in
              if x = a_inf then (
                assert (x = a_sup);
                0.5 *. float (inf + sup + 1)
              )
              else (
                0.5 +. interpolate a_inf inf a_sup sup x
              )
          in
          rank /. float n

let map t x =
  add t x;
  normalize t x

let string_of_list l =
   l |> List.map string_of_float |> String.concat " "

let test_normalize () =
  let f n samples query =
    let t = create ~num_bins:n () in
    List.iter (add t) samples;
    let r = normalize t query in
    printf "n=%i [%s] query=%g -> rank=%g\n"
      n (string_of_list samples) query r;
    r
  in
  let ( =~ ) a b = abs_float (a -. b) < 1e-6 in
  (* Test initialization phase *)
  assert (f 1 [] 0. =~ 0.5);
  assert (f 5 [] 0. =~ 0.5);
  assert (f 5 [3.] 0. =~ 0.5);
  assert (f 5 [3.; 2.] 0. =~ 0.);
  assert (f 5 [3.; 2.] 1.5 =~ 0.);
  assert (f 5 [3.; 2.] 1.75 =~ 0.);
  assert (f 5 [3.; 2.] 2. =~ 0.25);
  assert (f 5 [3.; 2.] 2.25 =~ 0.5);
  assert (f 5 [3.; 2.] 2.5 =~ 0.5);
  assert (f 5 [3.; 2.] 2.75 =~ 0.5);
  assert (f 5 [3.; 2.] 3. =~ 0.75);
  assert (f 5 [3.; 2.] 3.5 =~ 1.);
  assert (f 5 [3.; 2.] 4. =~ 1.);
  (* Test normal operation *)
  assert (f 2 [3.; 2.; 10.] 0. =~ 0.);
  assert (f 2 [3.; 2.; 10.] 1.5 =~ 0.);
  assert (f 2 [3.; 2.; 10.] 1.75 =~ 0.125);
  assert (f 2 [3.; 2.; 10.] 2. =~ 0.25);
  assert (f 2 [3.; 2.; 10.] 2.25 =~ 0.375);
  assert (f 2 [3.; 2.; 10.] 2.5 =~ 0.5);
  assert (f 2 [3.; 2.; 10.] 2.75 =~ 0.625);
  assert (f 2 [3.; 2.; 10.] 3. =~ 0.75);
  assert (f 2 [3.; 2.; 10.] 3.5 =~ 1.);
  assert (f 2 [3.; 2.; 10.] 4. =~ 1.);
  (* Test after two rounds of identical samples *)
  assert (f 2 [3.; 2.; 2.; 3.] 0. =~ 0.);
  assert (f 2 [3.; 2.; 2.; 3.] 1.5 =~ 0.);
  assert (f 2 [3.; 2.; 2.; 3.] 1.75 =~ 0.125);
  assert (f 2 [3.; 2.; 2.; 3.] 2. =~ 0.25);
  assert (f 2 [3.; 2.; 2.; 3.] 2.25 =~ 0.375);
  assert (f 2 [3.; 2.; 2.; 3.] 2.5 =~ 0.5);
  assert (f 2 [3.; 2.; 2.; 3.] 2.75 =~ 0.625);
  assert (f 2 [3.; 2.; 2.; 3.] 3. =~ 0.75);
  assert (f 2 [3.; 2.; 2.; 3.] 3.5 =~ 1.);
  assert (f 2 [3.; 2.; 2.; 3.] 4. =~ 1.);
  (* Test after two rounds of different samples, shifting avg. values by 1. *)
  assert (f 2 [3.; 2.; 4.; 5.] 1. =~ 0.);
  assert (f 2 [3.; 2.; 4.; 5.] 2.5 =~ 0.);
  assert (f 2 [3.; 2.; 4.; 5.] 2.75 =~ 0.125);
  assert (f 2 [3.; 2.; 4.; 5.] 3. =~ 0.25);
  assert (f 2 [3.; 2.; 4.; 5.] 3.25 =~ 0.375);
  assert (f 2 [3.; 2.; 4.; 5.] 3.5 =~ 0.5);
  assert (f 2 [3.; 2.; 4.; 5.] 3.75 =~ 0.625);
  assert (f 2 [3.; 2.; 4.; 5.] 4. =~ 0.75);
  assert (f 2 [3.; 2.; 4.; 5.] 4.5 =~ 1.);
  assert (f 2 [3.; 2.; 4.; 5.] 5. =~ 1.)

let ema_tests = EMA.tests

let tests = [
  "find_rank_from_left", test_find_rank_from_left;
  "find_inf", test_find_inf;
  "find_rank_from_right", test_find_rank_from_right;
  "find_sup", test_find_sup;
  "find_rank_in_buffer", test_find_rank_in_buffer;
  "interpolate", test_interpolate;
  "normalize", test_normalize;
]
