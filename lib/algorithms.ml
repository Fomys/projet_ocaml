open Tools
open Graph

let graph_gap gr_cap gr_flot =
        e_fold gr_cap (
            fun gr_gap source destination cap ->
                match find_arc gr_flot source destination with
                    | None -> add_arc gr_gap source destination cap;
                    | Some v -> add_arc (add_arc gr_gap destination source (v)) source destination (cap - v)
        )
        (clone_nodes gr_cap)


let rec get_min_on_path_rec graph path poids_min =
    match path with
        | [] -> poids_min
        | [(source, destination)::rest] ->
            match find_arc graph source destination with
                | None -> 0
                | Some w -> get_min_on_path_rec graph path (if w < poids_min then w else poids_min)

let get_min_on_path graph path =
    get_min_on_path_rec graph path 0


let ford_fulkerson_rec gr_cap gr_flot source sink =
    match find_path_source_to_sink source sink with
        | [] -> gr_flot
        | path -> update_flot path (get_min_on_path gr_flot path )

let ford_fulkerson gr source sink =
    ford_fulkerson_rec gr (clone_nodes gr) source sink
