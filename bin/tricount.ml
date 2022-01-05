open Projet.Tricount

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf
        "\nUtilisation: %s fichier_transaction fichier_bilan\n  fichier_transaction : Fichier contenant les transactions\n  fichier_bilan       : Fichier de sortie du bilan%!" Sys.argv.(0);
      exit 0
    end ;
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2) in
  solve_tricount infile outfile