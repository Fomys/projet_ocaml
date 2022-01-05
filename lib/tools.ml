(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let  clone_nodes (gr:'a graph) = n_fold gr new_node empty_graph

let gmap (gr:'a graph) fn = e_fold gr (fun gr id1 id2 lbl -> new_arc gr id1 id2 (fn lbl)) (clone_nodes gr) 

(*
VERSION BOURRINE QUI MARCHE
let add_arc (gr:'a graph) source destination weight =  
  if find_arc gr source destination = None then new_arc gr source destination weight
  else 
    e_fold gr (fun gr id1 id2 lbl -> if id1 = source && id2 = destination then new_arc gr id1 id2 (lbl+weight) else new_arc gr id1 id2 lbl ) (clone_nodes gr)
*)

let add_arc (gr:'a graph) source destination weight =
  match find_arc gr source destination with
  | None -> new_arc gr source destination weight
  | Some lbl -> new_arc gr source destination (lbl +. weight) (*il écrase donc l'ancier arc*)
