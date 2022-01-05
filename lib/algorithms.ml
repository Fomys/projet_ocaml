open Tools
open Graph
open List
open Printf


let rec explore gr node node_mark sink =

  let rec loop1 origin arcs_voisins = match arcs_voisins with
      | [] -> []
      | (voisin, a) :: rest ->
            if (mem voisin node_mark) || a == 0
            then
                  loop1 origin rest (*si le voisin est déjà marqué, ou que l'arc est "vide", je regarde le prochain voisin*)
            else
                  if voisin == sink
                  then
                        [(origin, sink)] (*si j'atteins ma cible, j'arrête la recherche et j'enregistre le dernier arc*)
                  else
                        match explore gr voisin (voisin :: node_mark) sink with (*Si je n'atteind pas ma cible, je marque le voisin et continue l'exploration*)
                              | [] -> loop1 origin rest (*si je n'atteins pas ma cible à partir de ce voisin, je regarde le prochain voisin*)
                              | path -> (origin, voisin) :: path (*si j'atteins ma cible, j'arrête la recherche et j'enregistre l'arc au chemin alors trouvé*)

  in

  loop1 node (out_arcs gr node) (*pour tout les voisins de node*)

let find_path_source_to_sink gr source sink = explore gr source [source] sink


let graph_gap gr_cap gr_flot =
        e_fold gr_cap (
            fun gr_gap origine destination cap ->
                match find_arc gr_flot origine destination with
                    | None -> add_arc gr_gap origine destination cap;
                    | Some v -> add_arc (add_arc gr_gap destination origine (v)) origine destination (cap - v)
        )
        (clone_nodes gr_cap)

let rec get_min_on_path_rec graph path poids_min =
    match path with
        | [] -> poids_min
        | (source, destination) :: rest ->
            match find_arc graph source destination with
                | None -> None
                | Some w -> get_min_on_path_rec graph rest (match poids_min with
                    | None -> Some(w)
                    | Some(w2) -> Some(if w2 < w then w2 else w))


let get_min_on_path graph path =
    match get_min_on_path_rec graph path None with
        | None -> 0
        | Some(w) -> w


let rec update_flot gr_flot path min = 
    match path with 
        | [] -> gr_flot 
        | (node1,node2) :: rest -> match find_arc gr_flot node1 node2 with
                                     | Some w -> update_flot (new_arc gr_flot node1 node2 (w + min)) rest min
                                     | None -> match find_arc gr_flot node2 node1 with
                                        | None -> update_flot (new_arc gr_flot node1 node2 min) rest min
                                        | Some w -> update_flot (new_arc (new_arc gr_flot node2 node1 (min - w)) node1 node2 (w - min)) rest min


let rec ford_fulkerson_rec gr_cap gr_flot source sink =
    match find_path_source_to_sink (graph_gap gr_cap gr_flot) source sink with
        | [] -> gr_flot
        | path ->
            let min = get_min_on_path (graph_gap gr_cap gr_flot) path in
                List.iter (fun (a,b)->printf "%d->%d\t" a b) path;
                printf "Min: %d\n" min;
                ford_fulkerson_rec gr_cap (update_flot gr_flot path (min)) source sink


let ford_fulkerson gr_cap source sink =
    ford_fulkerson_rec gr_cap (clone_nodes gr_cap) source sink
