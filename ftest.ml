open Graph
open FordFulkersonAlgo

let () =

  if Array.length Sys.argv < 7 then
	begin
	  Printf.printf "\nUsage: %s infile outfile -s sources -p sinks\n\n%!" Sys.argv.(0) ;
	  exit 0
	end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2) in

  let rec arg args sources sinks t = match args with
    |[] -> (sources, sinks)
    |x::tl when x="-s" -> loop sources sinks s
    |x::tl when x="-d" -> loop sources sinks d
    |x::tl -> match t with
      |s -> loop (x::sources) sinks s
      |d -> loop sources (x::sinks) d
    in

  let (sources, sinks) = arg (Array.sub Sys.argv 3 (Array.length Sys.argv)) [] [] s
  in

  (* Open file *)
  let graph = Gfile.from_file infile in
		(* C'est entre ces deux lignes qu'on met nos trucs. *)
  let () = Gfile.export outfile (FordFulkersonAlgo.ford_fulkerson graph sources sinks) in

  (* Rewrite the graph that has been read. *)
  (*let () = Gfile.write_file outfile graph in*)

  ()
