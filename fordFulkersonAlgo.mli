(* Type of a directed graph in which arcs have labels of type 'a. *)
type 'a graph

(* Each node has a unique identifier (a name). *)
type id = string

(* The empty graph. *)
val empty_graph: 'a graph

(**************  INITIALISATION  **************)
val init_graph: 'a graph -> 'b graph

(**************  PARCOURS  **************)
val residual_graph: 'a graph -> 'b graph
val tour_graph: 'a graph -> 'a graph * int

(**************  MISE A JOUR  **************)
(*  *)
val update_path: 

val add_path_to_graph:

val update_graph: 'a graph -> 'b graph -> int -> 'b graph

(**************  FORD-FULKERSON  **************)

