(* Type of a directed graph in which arcs have labels of type 'a. *)
type 'a graph

(* Each node has a unique identifier (a name). *)
type id = string

(* The empty graph. *)
val empty_graph: 'a graph

(**************  INITIALISATION  **************)
val init_graph: 'a graph -> 'b graph

(**************  PARCOURS  **************)


(**************  MISE A JOUR  **************)


(**************  FORD-FULKERSON  **************)

