open Graph
open Tools

let try_add_node gr feeder_id =
    if node_exists gr feeder_id then gr
    else let graph = new_node gr feeder_id in
        n_fold graph ( fun gr node -> if node = 0 or node = 1 then gr else new_arc (new_arc gr feeder_id node Float.infinity) node feeder_id Float.infinity ) graph

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
    let rec loop graph names = (*création du graphe et de ces correspondances nom id*)
        try
            let line = input_line infile in (*lecture de la ligne*)
            let line = String.trim line in (*enlève espace et retour à la ligne en trop*)
            if line = "" then (graph, names) (*si la ligne est vide, retrourne le graphe et les correspondances nom id*)
            else 
                let line = String.split_on_char ' ' line in (*sépart la ligne en liste de mots (=> les noms ne peuvent pas contenir d'espace)*)
                    if List.length line < 3 then (graph, names) (*si la ligne n'a pas d'information valident, retourn le graphe et les correspondances nom id*)
                    else
                        let feeder = List.nth line 0 in (*feeder = premier mots*)
                        let amount = float_of_string (List.nth line 1) in (*amount = deuxième mot convertit en int*)
                        let indebted = List.tl (List.tl line) in (*indebted = ensemble des mots excétés le premier et le deuxième*)
                        let shared_amount = amount /. Float.of_int (List.length indebted) in (*shared_amount = prix identique pour chaque endetté*)
                        let (names, feeder_id) = get_id_for_name names feeder in (*ajoute l'id du feeder à names*)
                        let graph = try_add_node graph feeder_id in (*ajoute les arcs aller retour entre le feeder et toutes les personnes déjà presentes*)
                        let graph = add_arc graph feeder_id 1 amount in (*ajoute un arc du feeder vers sink d'un poids d'ammount*)
                        let (graph, names) = List.fold_left (add_indebted shared_amount) (graph, names) indebted in (*ajoute un arc du feeder vers sink d'un poids d'ammount*)
                        loop graph names
        with End_of_file -> (graph, names)
    in
    let initial_graph = new_node (new_node empty_graph 0) 1 in
    let (final_graph, names) = loop initial_graph [] in
    close_in infile;
    (final_graph, names)

let export_tricount out_file graph =
    ()

let solve_tricount in_file _ =
    let (graph, names) = read_file in_file in
    let solved = ford_fulkerson 
    export_tricount (ford_fulkerson (read_file in_file))
