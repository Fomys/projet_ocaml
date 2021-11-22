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

  (*  ******  TEST  ******  *)

  (* Test clone_nodes *)

  let graph_sans_arc = clone_nodes graph in

  (*  ******  ****  ******  *)


  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graph_sans_arc in




  ()

