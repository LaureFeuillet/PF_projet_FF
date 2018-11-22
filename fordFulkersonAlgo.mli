open Graph

type fc = {
	flow : int ; 
	capacity : int } 

(**************  INITIALISATION  **************)
(* Construct a flow graph from a given capacity graph *)
val init_graph: int graph -> fc graph

(**************  PARCOURS  **************)
(* Construct a residual graph from a capacity graph *)
val residual_graph: fc graph -> int graph

(* Tour a residual graph to find a path from source to sink, and its minimal cost *)
val tour_graph: 'a graph -> 'a graph * int

(**************  MISE A JOUR  **************)
(*  *)
val update_path: 

val add_path_to_graph:

(*  *)
val update_graph: 'a graph -> 'b graph -> int -> 'b graph

(**************  FORD-FULKERSON  **************)

