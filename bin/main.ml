open Projet.Gfile
open Projet.Tools
open Projet.Algorithms
open Printf
open List

let () =
 
  (* Open file *)
  let graph = from_file "graph_out_str.gr" in
  let int_graph = gmap graph (fun v -> int_of_string v) in

  let path = find_path_source_to_sink int_graph 0 8 in
  let () = iter (
    fun (source,sink) -> printf "%d -> %d;\n" source sink
  ) path in

  ()

