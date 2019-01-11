open Graph
open FordFulkersonAlgo

let () =

  if Array.length Sys.argv < 5 then
	begin
	  Printf.printf "\nUsage: %s infile outfile -s sources -d sinks\n\n%!" Sys.argv.(0) ;
	  exit 0
	end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2) in

  let rec arg args sources sinks t = match args with
    | [] -> (sources, sinks)
    | x::tl when x="-s" -> arg tl sources sinks "s"
    | x::tl when x="-d" -> arg tl sources sinks "d"
    | x::tl -> (match t with
      | "s" -> arg tl (x::sources) sinks "s"
      | "d" -> arg tl sources (x::sinks) "d"
      | _ -> raise (Graph_error "Error in arguments"))
    in

  let (sources, sinks) = arg (Array.to_list (Array.sub Sys.argv 3 ((Array.length Sys.argv)-3))) [] [] ""
  in

  (* Open file *)
  let graph = Gfile.from_file infile in
	(* C'est entre ces deux lignes qu'on met nos trucs. *)
  let () = Gfile.export outfile (FordFulkersonAlgo.ford_fulkerson graph sources sinks) in

  (* Rewrite the graph that has been read. *)
  (*let () = Gfile.write_file outfile graph in*)

  ()
