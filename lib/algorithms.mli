open Graph

val ford_fulkerson: int graph -> id -> id -> int graph
val graph_gap: int graph -> int graph -> int graph
val get_min_on_path: int graph -> (id*id) list -> int
val find_path_source_to_sink: int graph -> id -> id -> (id*id) list
val update_flot: int graph -> (id*id) list -> int -> int graph