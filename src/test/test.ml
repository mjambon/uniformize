(*
   Entrypoint to run the unit tests from the command line.
*)

let make_alcotest_suite tests_list : unit Alcotest.test list =
  List.map (fun (name, tests) ->
    name, List.map (fun (name, f) -> (name, `Quick, f)) tests
  ) tests_list

let test_suites =
  make_alcotest_suite [
    "EMA", Uniformize.ema_tests;
    "Uniformize", Uniformize.tests;
  ]

let main () = Alcotest.run "uniformize" test_suites

let () = main ()
