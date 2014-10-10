open NetKAT_Types

(* physical location *)
type ploc = switchId * portId

(* virtual location *)
type vloc = vswitchId * vportId

(* FIXME: these are already defined in GlobalCompiler *)
let union (ps : policy list) : policy = List.fold_left Optimize.mk_union drop ps
let seq (ps : policy list) : policy = List.fold_left Optimize.mk_seq id ps
let match_ploc (sw,pt) = Filter (And (Test(Switch sw), Test(Location(Physical(pt)))))
let match_vloc (vsw,vpt) = Filter (And (Test(VSwitch vsw), Test(VPort vpt)))
let set_vloc (vsw,vpt) = seq [Mod (VSwitch vsw); Mod (VPort vpt)]

let mk_union (indices : 'a list) (f : 'a -> policy) =
  union (List.map f indices)

let star = Optimize.mk_star

let dedup xs =
  xs |> Core.Core_list.of_list |> Core.Core_list.dedup |> Core.Core_list.to_list

let get_vlocs (p : policy) =
  let rec get = function
    | Filter (And (Test (VSwitch vsw), Test (VPort vpt))) -> [(vsw, vpt)]
    | Filter _ | Mod _ | Link _ -> []
    | Union (q, r) | Seq (q, r) -> (get q) @ (get r)
    | Star q -> get q in
  dedup (get p)

(* 
  
  out_fabric/in_fabric have to be of the form
     UNION_{vl : Vloc}{
      match_vloc vl; UNION_{pl : Ploc}{
        match_ploc pl; path(vl, pl)
      }
    }
  where path(vl, pl) is a path in the physical topology ending with a link
    (i) going out of pl in the case of out_fabric, or
    (ii) going into pl in the case of in_fabric
  
  Note that the framework allows out_fabric/in_fabric to be specified by hand in NetKAT,
  but we could also generate them automatically, e.g. from a virtualization relation and
  the physical topology using Dijkstra.

 ---------------------------

  Vingress defines the virtual ingress. Examples:

   1) The physical ingress is {(1,1)} (i.e. packets can enter the network only
      through port 1 of switch 1), and we want packets to enter the virtual network
      at vport 3 of vswitch 3. This is encoded as
        vingress =
          vswitch := 3; vport := 3

   2) The physical ingress is {(1,1), (2,2)} (i.e. packets can enter the network only
      through port 1 of switch 1 and port 2 of switch 2),
      and we want packets to enter the virtual network
      at vport 3 of vswitch 3. This is encoded as
        vingress =
          vswitch := 3; vport := 3

   3) The physical ingress is {(1,1)} and we want packets to enter the virtual network
      at both vport 3 of vswitch 3 and vport 4 of switch 4. This is encoded as
        vingress =
          (vswitch := 3; vport := 3) + (vswitch := 4; vport := 4)

   4) The physical ingress is {(1,1), (2,2)} and we want packets from (1,1) to
      enter the virtual network at vport 3 of vswitch 3, and packet from (2,2)
      shall enter at vport 4 of vswitch 4. This is encoded as
        vingress =
          switch = 1; port = 1; vswitch := 3; vport := 3
        + switch = 2; port = 2; vswitch := 4; vport := 4

   
  To gurantee correctness we will have to do some sort of "type checking", i.e. we have to make sure
  certain pre conditions are met.

*)

let compile (vpolicy : policy) (vtopo : policy) (vingress : policy)
(out_fabric : policy) (in_fabric : policy) =
  let fout = mk_union (get_vlocs out_fabric)
    (fun vl -> seq [match_vloc vl; out_fabric; set_vloc vl]) in
  let fin = mk_union (get_vlocs in_fabric)
    (fun vl -> seq [match_vloc vl; in_fabric; set_vloc vl]) in
  let ing = seq [vingress; fin] in
  let p = seq [vpolicy; fout] in
  let t = seq [vtopo; fin] in
  (* ing; p; (t;p)^*  *)
  seq [ing; p; star (seq [t; p])]

  