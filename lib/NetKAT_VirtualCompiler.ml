open NetKAT_Types

(* physical location *)
type ploc = switchId * portId

(* virtual location *)
type vloc = vswitchId * vportId

(* we need to differentiate between leaving a virtual location and entering a virtual location *)
type typed_vloc
  | Leaving of vloc
  | Entering of vloc

(* FIXME: these are already defined in GlobalCompiler *)
let union (ps : policy list) : policy = List.fold_left mk_union drop ps
let seq (ps : policy list) : policy = List.fold_left mk_seq id ps
let match_ploc (sw,pt) = Filter (And (Test(Switch sw), Test(Location(Physical(pt)))))
let match_vloc (vsw,vpt) = Filter (And (Test(VSwitch vsw), Test(VPort vpt)))
let set_vloc (vsw,vpt) = seq [Mod (VSwitch vsw); Mod (VPort vpt)]

let mk_union (indices : 'a list) (f : 'a -> policy) =
  union (List.map f indices)

let compile (virtual_policy : policy) (virtual_topo : policy) (fabric : typed_vloc -> ploc -> policy) 
(vlocs : vloc list) (plocs : ploc list) =
  let fout = mk_union vlocs 
    (fun vl -> mk_union plocs (fun pl -> 
      seq [match_vloc vl; match_ploc pl; fabric (Leaving vl); set_vloc vl])) in
  let fin = mk_union vlocs 
    (fun vl -> mk_union plocs (fun pl -> 
      seq [match_vloc vl; match_ploc pl; fabric (Entering vl); set_vloc vl])) in
  seq [virtual_policy; fout; virtual_topo; fin]
  