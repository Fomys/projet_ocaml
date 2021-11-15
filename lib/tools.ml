(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let rec clone_nodes (gr:'a graph) = match gr with
| empty_graph -> []
| head :: rest -> head :: clone_nodes rest


let gmap (gr:'a graph) fn = assert false

let add_arc (gr:'a graph) source destination weight = assert false
