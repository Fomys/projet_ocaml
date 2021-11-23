open Graph
open List
open Printf

(* let ford_fulkerson_rec gr_cap gr_flot source sink = ()

let ford_fulkerson gr source sink = ford_fulkerson_rec gr (clone_nodes gr) source sink *)


 (* creation graphe résiduel
 cherche chemin source -> sink : pas de chemin on renvoi gr_flot 
                               : sinon cherche valeur minimal du chemin
                                       gr_flot devient gr-flot avec + valeur minimal sur le chemin
                                       ford_fulkerson_rec gr_cap gr_flot source sink *)

let rec explore gr node node_mark sink = 
  
  let rec loop1 origin arcs_voisins = match arcs_voisins with
      | [] -> []
      | (voisin, a) :: rest -> 
            if (mem voisin node_mark) || a == 0
            then 
                  loop1 origin rest
            else 
                  if voisin == sink
                  then 
                        [(origin, sink)]
                  else 
                        match explore gr voisin (voisin :: node_mark) sink with
                              | [] -> loop1 origin rest
                              | path -> (origin, voisin) :: path
                                    
  in

  loop1 node (out_arcs gr node)

let find_path_source_to_sink gr source sink = explore gr source [source] sink