module N = IPv6__Neighbors

let mac = Macaddr.of_string_exn "02:00:00:00:00:01"
let router = Ipaddr.V6.of_string_exn "fe80::1"
let router_mac = Macaddr.of_string_exn "02:00:00:00:00:fe"
let host = Ipaddr.V6.of_string_exn "fe80::2"
let me = Ipaddr.V6.of_string_exn "fe80::10"

let query t ~now addr =
  let t, _, _ = N.query t ~mac ~now addr in
  t

let na ~target ~tlla =
  `NA
    ( target
    , me
    , {
        N.NA.router= false
      ; solicited= true
      ; override= true
      ; target
      ; tlla= Some tlla
      } )

let tick t ~now event = N.tick t ~mac ~now event

let check name ok =
  if not ok then (
    Printf.printf "FAIL: %s\n" name;
    exit 1)

(* An NA only resolves the entry of its target. *)
let () =
  let t = query (N.make 16) ~now:0 router in
  let t = query t ~now:0 host in
  let _, t = tick t ~now:0 (na ~target:router ~tlla:router_mac) in
  check "router resolved" (N.lladdr t router = Some router_mac);
  check "host still unresolved" (N.lladdr t host = None)
