open Projet.Gfile
open Projet.Tools

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n âœ»  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    ðŸŸ„  infile  : input file containing a graph\n" ^
         "    ðŸŸ„  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    ðŸŸ„  sink    : identifier of the sink vertex (ditto)\n" ^
         "    ðŸŸ„  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in
  let int_graph = gmap graph (fun v -> int_of_string v) in


  let out_graph = add_arc int_graph 0 2 2341 in


  (* Rewrite the graph that has been read. *)
  let str_graph = gmap out_graph (fun v -> string_of_int v) in
  let () = write_file outfile str_graph in
  let () = export "test.dot" str_graph in

  ()

