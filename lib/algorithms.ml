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


