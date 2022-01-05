open Graph

val ford_fulkerson: float graph -> id -> id -> float graph
val graph_gap: float graph -> float graph -> float graph
val get_min_on_path: float graph -> (id*id) list -> float
val find_path_source_to_sink: float graph -> id -> id -> (id*id) list
val update_flot: float graph -> (id*id) list -> float -> float graph
