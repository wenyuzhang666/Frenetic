open NetKAT_Types

let mk_and pr1 pr2 =
  match pr1, pr2 with
    | True, _ ->
      pr2
    | _, True ->
      pr1
    | False, _ ->
      False
    | _, False ->
      False
    | _ ->
      And(pr1, pr2)

let mk_or pr1 pr2 =
  match pr1, pr2 with
    | True, _ ->
      True
    | _, True ->
      True
    | False, _ ->
      pr2
    | _, False ->
      pr1
    | _ ->
      Or(pr1, pr2)

let mk_not pat =
  match pat with
    | False -> True
    | True -> False
    | _ -> Neg(pat)

let mk_filter pr =
  Filter (pr)

let mk_union pol1 pol2 =
  match pol1, pol2 with
    | Filter False, _ ->
      pol2
    | _, Filter False ->
      pol1
    | _ ->
      Union(pol1,pol2)

let mk_seq pol1 pol2 =
  match pol1, pol2 with
    | Filter True, _ ->
      pol2
    | _, Filter True ->
      pol1
    | Filter False, _ ->
      pol1
    | _, Filter False ->
      pol2
    | _ ->
      Seq(pol1,pol2)

let mk_star pol =
  match pol with
    | Filter True ->
      pol
    | Filter False ->
      Filter True
    | Star(pol1) -> pol
    | _ -> Star(pol)

let specialize_pred sw pr =
  let rec loop pr k =
    match pr with
      | True ->
        k pr
      | False ->
        k pr
      | Neg pr1 ->
        loop pr1 (fun pr -> k (mk_not pr))
      | Test (IP4Src _) ->
        k (And (Test (EthType 0x800), pr))
      | Test (IP4Dst _) ->
        k (And (Test (EthType 0x800), pr))
      | Test (TCPSrcPort _) ->
        k (And (Test (EthType 0x800), And (Test (IPProto 6), pr)))
      | Test (TCPDstPort _) ->
        k (And (Test (EthType 0x800), And (Test (IPProto 6), pr)))
      | Test (Switch v) ->
        if v = sw then
          k True
        else
          k False
      | Test _ ->
        k pr
      | And (pr1, pr2) ->
        loop pr1 (fun p1 -> loop pr2 (fun p2 -> k (mk_and p1 p2)))
      | Or (pr1, pr2) ->
        loop pr1 (fun p1 -> loop pr2 (fun p2 -> k (mk_or p1 p2))) in
  loop pr (fun x -> x)

let specialize_policy sw pol =
  let rec loop pol k =
    match pol with
      | Filter pr ->
        k (Filter (specialize_pred sw pr))
      | Mod hv ->
        k pol
      | Union (pol1, pol2) ->
        loop pol1 (fun p1 -> loop pol2 (fun p2 -> k (mk_union p1 p2)))
      | Seq (pol1, pol2) ->
        loop pol1 (fun p1 -> loop pol2 (fun p2 -> k (mk_seq p1 p2)))
      | Star pol ->
        loop pol (fun p -> k (mk_star p))
      | Link(sw,pt,sw',pt') ->
	failwith "Not a local policy" in
  loop pol (fun x -> x)
