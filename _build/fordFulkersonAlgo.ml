open Graph

type fc = {
	flow : int ; 
	capacity : int } 

type path = unit

(**************  INITIALISATION  **************)
(* Fonction permettant le passage d'un "capacité graph" à un "(flot * capacité) graph". *)
let init_graph gr = Graph.map gr (fun capacity -> {flow = 0; capacity})

(**************  PARCOURS  **************)
(* Return the same graph but without arcs *)
let graph_without_arcs gr = 
	let result = empty_graph in
	let f acu id out_arcs = add_node acu id in
	v_fold gr f result

(* Construct a residual graph from a capacity graph *)
let residual_graph gr = assert false
	
(* Tour a residual graph to find a path from source to sink, and its minimal cost *)
let tour_graph gr = assert false

(**************  MISE A JOUR  **************)
(* Update a flow graph according to an incrementation path and its minimal cost *)
let update_graph gr path cost = assert false

(**************  FORD-FULKERSON  **************)

