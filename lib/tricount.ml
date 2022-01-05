let get_id_for_name names name =
    match names with
        | [] -> ([name], 0)
        | head :: rest ->
            if head = name then
                (head :: rest, 0)
            else
                let (new_names, id) = get_id_for_name rest name in
                (head :: new_names, id + 1)

let read_file path =
    let infile = open_in path in
    let rec loop graph names =
        try
            let line = input_line infile in
            let line = String.trim line in
            if line = "" then (graph, names)
            else
                let line = String.split_on_char " " line in
                    if List.length line < 3 then (graph, names)
                    else
                        let feeder = List.nth line 0 in
                        let indebted = List.tl (List.tl line) in
                        let amount = int_of_string (List.nth line 1) in
                        let (names, feeder_id) = get_id_for_name names feeder in
                        let graph = try_add_node graph (feeder_id + 2) in
                                                (* + 2 car il y a deja les noeuds 0 et 1 dans le graphe "fausse source" "fausse destination" *)







      let infile = open_in path in

      (* Read all lines until end of file. *)
      let rec loop graph =
        try
          let line = input_line infile in

          (* Remove leading and trailing spaces. *)
          let line = String.trim line in

          let graph2 =
            (* Ignore empty lines *)
            if line = "" then graph

            (* The first character of a line determines its content : n or e. *)
            else match line.[0] with
              | 'n' -> read_node graph line
              | 'e' -> read_arc graph line

              (* It should be a comment, otherwise we complain. *)
              | _ -> read_comment graph line
          in
          loop graph2

        with End_of_file -> graph (* Done *)
      in

      let final_graph = loop empty_graph in

      close_in infile ;
      final_graph

let solve_tricount in_file out_file =
    ()


