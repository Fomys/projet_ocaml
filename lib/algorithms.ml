open Tools
open Graph
open List


let rec explore gr node node_mark sink = (*exploration de gr à partir de node, en connaissant la liste des noeuds marqués node_mark, jusqu'à atteindre ma cible sink*)

  let rec loop1 origin arcs_voisins = match arcs_voisins with
      | [] -> [] (*si il n'y a plus de voisins, j'arrête l'exploration*)
      | (voisin, a) :: rest ->
            if (mem voisin node_mark) || a == 0
            then
                  loop1 origin rest (*si ce voisin est déjà marqué, ou que l'arc est "vide", je regarde le prochain voisin*)
            else
                  if voisin == sink
                  then
                        [(origin, sink)] (*si ce voisin est ma cible, j'arrête l'exploration et j'enregistre le dernier arc*)
                  else
                        match explore gr voisin (voisin :: node_mark) sink with (*si ce voisin n'est pas ma cible, je le marque et commence une exploration à partir de ce voisin*)
                              | [] -> loop1 origin rest (*si cette exploration ne mène pas à ma cible, je regarde le prochain voisin*)
                              | path -> (origin, voisin) :: path (*si cette exploration mène à ma cible, j'arrête l'exploration initiale et j'enregistre l'arc au chemin alors trouvé*)

  in

  loop1 node (out_arcs gr node) (*pour tout les voisins de node*)

let find_path_source_to_sink gr source sink = explore gr source [source] sink (*lance une exploration pour trouver un chemin entre source et sink*)


let graph_gap gr_cap gr_flot = (*creation du graphe d'écart associé à gr_flot à partir du graphe des capacités*)
        e_fold gr_cap ( (*pour tout les arcs du graphe des capacités, je créée un arc dans mon futur graphe d'écart*)
            fun gr_gap origine destination cap -> 
                match find_arc gr_flot origine destination with (*je regarde l'arc associé dans le graphe de flot, appelons le arc_capacite*)
                    | None -> add_arc gr_gap origine destination cap; (*s'il n'y en a pas, je créée l'arc dans le graphe d'écart d'un poids égale au poids de l'arc_capacite*)
                    | Some v -> add_arc (add_arc gr_gap destination origine (v)) origine destination (cap - v) (*s'il en existe un, je créée dans le graphe d'écart un arc aller et un arc retour en fonction du poids de l'arc_capacite*)
        )
        (clone_nodes gr_cap) (*je retourne le graphe d'écart ainsi créé*)

let rec get_min_on_path_rec graph path poids_min = (*parcourir le path dans le graph afin de trouver le poids_min*)
    match path with
        | [] -> poids_min (*si on a fini de parcourir le path, je renvoie poids_min*)
        | (source, destination) :: rest ->
            match find_arc graph source destination with
                | None -> None (*si path contient un arc qui n'existe pas dans le graphe, j'arrête le parcours et je renvoie rien*)
                | Some w -> get_min_on_path_rec graph rest (match poids_min with (*sinon, je parcours la suite du path en modifiant poids_min si nécessaire*)
                    | None -> Some(w)
                    | Some(w2) -> Some(if w2 < w then w2 else w))


let get_min_on_path graph path = (*adapte get_min_on_path_rec pour une utilisation concrète*)
    match get_min_on_path_rec graph path None with
        | None -> 0
        | Some(w) -> w


let rec update_flot gr_flot path min = (*modifie le graphe de flot en lui ajoutant min au poids de chaque arc de path*)
    match path with (*pour tous les arcs du path*)
        | [] -> gr_flot (*si on a finit de modifier le path, je renvoie le graphe de flot mise à jour*)
        | (node1,node2) :: rest -> match find_arc gr_flot node1 node2 with 
                                     | Some w -> update_flot (new_arc gr_flot node1 node2 (w + min)) rest min (*si l'arc du path existe dans le graphe de flot, je le met à jour puis je continue la modification*)
                                     | None -> match find_arc gr_flot node2 node1 with
                                        | None -> update_flot (new_arc gr_flot node1 node2 min) rest min (*si son opposé n'existe pas non plus dans le graphe de flot, je créée l'arc puis continue la modification*)
                                        | Some w -> update_flot (new_arc (new_arc gr_flot node2 node1 (min - w)) node1 node2 (w - min)) rest min (*si son opposé existe dans le graphe de flot, je le créée l'arc, met à jour son opposé puis continue la modification*)


let rec ford_fulkerson_rec gr_cap gr_flot source sink =
    match find_path_source_to_sink (graph_gap gr_cap gr_flot) source sink with (*cherche un chemin entre source et sink dans le graphe d'écart*)
        | [] -> gr_flot (*si il a plus de chemin, je renvoie gr_flot alors optimisé*)
        | path ->
            let min = get_min_on_path (graph_gap gr_cap gr_flot) path in
                ford_fulkerson_rec gr_cap (update_flot gr_flot path (min)) source sink (*si je trouve un chemin, je met à jour le graph de flot et continue l'optimisation*)


let ford_fulkerson gr_cap source sink = (*adapte ford_fulkerson_rec pour une utilisation concrète*)
    ford_fulkerson_rec gr_cap (clone_nodes gr_cap) source sink
