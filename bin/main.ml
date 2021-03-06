open Projet.Gfile
open Projet.Tools
open Projet.Algorithms

let () =
  let graph_capacite = from_file "graph1.gr" in
  let graph_capacite_int = gmap graph_capacite (fun v -> float_of_string v) in

  let graph_flot_fulkerson = ford_fulkerson graph_capacite_int 0 5 in


  (* Rewrite the graph that has been read. *)
  let graph_capacite_str = gmap graph_capacite_int (fun v -> string_of_float v) in
  let graph_flot_fulkerson_str = gmap graph_flot_fulkerson (fun v -> string_of_float v) in
  let () = write_file "graph_capacite_str.txt" graph_capacite_str in
  let () = write_file "graph_flot_fulkerson_str.txt" graph_flot_fulkerson_str in

  let () = export "graph_flot_fulkerson.dot" graph_flot_fulkerson_str in
  let () = export "graph_capacite.dot" graph_capacite in

  ()


