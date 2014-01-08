open Types
open Pretty
open LocalCompiler

TEST "drop compiles to empty flowtable" =
  let sw = VInt.Int32 Int32.zero in
  [] = RunTime.(to_table (compile sw Types.drop))

TEST "inconsistent assignment and check compile to empty flowtable" =
  let sw = VInt.Int32 Int32.zero in
  let one = VInt.Int32 (Int32.of_int 1) in
  let two = VInt.Int32 (Int32.of_int 2) in
  let inp = Types.Header SDN_Types.InPort in
  let p0 = Types.(Seq(Mod(inp, one), Filter(Test(inp, two)))) in
  [] = RunTime.(to_table (compile sw p0))

TEST "seq_pat_act" =
  let open QuickCheck in
  let open NetKAT_Arbitrary in
  let open LocalCompiler.Pattern in
  (* let total = ref 0 in *)
  (* let nogo  = ref 0 in *)
  (* NB(seliopou): the call to seq_act_pat has about an 80% failure rate, so
   * pumping up the number of tests to run to compensate
   * *)
  let config = { quick with maxTest = 1000 } in
  let arbitrary =
    let open QuickCheck_gen in
    arbitrary_packet >>= fun pkt ->
    arbitrary_header_val_map >>= fun x ->
    arbitrary_header_val_map >>= fun a ->
    arbitrary_header_val_map >>= fun y ->
      ret_gen (pkt, x, a, y) in
  let test = testable_fun arbitrary (fun _ -> "<???>") testable_bool in
  let pred (pkt, x, a, y) =
    (* incr total; *)
    match seq_act_pat x a y with
      | Some(z) ->
        matches_packet pkt z =
        (matches_packet pkt x && matches_packet (LocalCompiler.Action.apply a pkt) y)
      | None ->
        (* incr nogo; *)
        true in
  match check test config pred with
    | Success -> Printf.printf "total: %d; nogo: %d\n%!" !total !nogo; true
    | Failure _ -> failwith "No failure expected"
    | Exhausted _ -> failwith "No exhaustion expected"
