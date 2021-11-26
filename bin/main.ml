open Projet.Gfile
open Projet.Tools
open Projet.Algorithms

let () =
  let graph_capacite = from_file "capacite.gr" in
  let graph_capacite_int = gmap graph_capacite (fun v -> int_of_string v) in
  let graph_flot = from_file "flot.gr" in
  let graph_flot_int = gmap graph_flot (fun v -> int_of_string v) in

  let graph_flot_fulkerson = ford_fulkerson graph_capacite_int 0 8 in

  (* Rewrite the graph that has been read. *)
  let graph_capacite_str = gmap graph_capacite_int (fun v -> string_of_int v) in
  let graph_flot_str = gmap graph_flot_int (fun v -> string_of_int v) in
  let graph_flot_fulkerson_str = gmap graph_flot_fulkerson (fun v -> string_of_int v) in
  let () = write_file "graph_capacite_str.gr" graph_capacite_str in
  let () = write_file "graph_flot_str.gr" graph_flot_str in
  let () = write_file "graph_flot_fulkerson.gr" graph_flot_fulkerson_str in

  ()


