open Graph

type fc = {
	flow : int ; 
	capacity : int } 

type path = unit

(**************  INITIALISATION  **************)
(* Construct a flow graph from a given capacity graph *)
val init_graph: int graph -> fc graph

(**************  PARCOURS  **************)
(* Return the same graph but without arcs*)
val graph_without_arcs: fc graph -> 'a graph

(* Construct a residual graph from a capacity graph *)
val residual_graph: fc graph -> int graph

(* Tour a residual graph to find a path from source to sink, and its minimal cost *)
val tour_graph: int  graph -> path * int

(**************  MISE A JOUR  **************)
(* Update a flow graph according to an incrementation path and its minimal cost *)
val update_graph: fc graph -> path -> int -> fc graph

(**************  FORD-FULKERSON  **************)

