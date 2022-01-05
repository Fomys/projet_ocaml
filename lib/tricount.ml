open Graph
open Tools

let try_add_node gr feeder_id =
    if node_exists gr feeder_id then gr
    else let graph = new_node gr feeder_id in
        n_fold graph ( fun gr node -> new_arc (new_arc gr feeder_id node Float.infinity) node feeder_id Float.infinity ) graph

let rec get_id_for_name names name = (*recherche le tupple nom et id associé*)
    match names with
        | [] -> ([name], 2) (*si name n'apparaît pas dans names, je lui donne l'id 2 (0 pour source et 1 pour sink) et arrête la recherche*)
        | head :: rest ->
            if head = name then
                (head :: rest, 2) (*si name apparait en haut de names, je lui donne l'id 2 et arrête la recherche*)
            else
                let (new_names, id) = get_id_for_name rest name in
                (head :: new_names, id + 1) (*sinon, je continue la recherche sans oublier d'incrémenter l'id trouver*)

let read_file path =
    let infile = open_in path in
    let add_indebted shared_amount (graph, names) name =
        let (names, indebted_id) = get_id_for_name names name in
        let graph = try_add_node graph indebted_id in
        let graph = add_arc graph 0 indebted_id shared_amount in
        (graph, names)
    in
    let rec loop graph names =
        try
            let line = input_line infile in
            let line = String.trim line in
            if line = "" then (graph, names)
            else
                let line = String.split_on_char ' ' line in
                    if List.length line < 3 then (graph, names)
                    else
                        let feeder = List.nth line 0 in
                        let indebted = List.tl (List.tl line) in
                        let amount = float_of_string (List.nth line 1) in
                        let shared_amount = amount /. Float.of_int (List.length indebted) in
                        let (names, feeder_id) = get_id_for_name names feeder in
                        let graph = try_add_node graph feeder_id in
                        let graph = add_arc graph feeder_id 1 amount in
                        let (graph, names) = List.fold_left (add_indebted shared_amount) (graph, names) indebted in
                        loop graph names
        with End_of_file -> (graph, names)
    in
    let (final_graph, names) = loop empty_graph [] in
    close_in infile;
    (final_graph, names)

let solve_tricount in_file _ =
    let _ = read_file in_file in
    ()
