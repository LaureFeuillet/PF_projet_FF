open Graph

type fc = {
	flow : int ; 
	capacity : int } 

(**************  INITIALISATION  **************)

(* Fonction permettant le passage d'un "capacité graph" à un "(flot * capacité) graph". *)
let init_graph gr = Graph.map gr (capacity -> (0, capacity))

(**************  PARCOURS  **************)
(* Construct a residual graph from a capacity graph *)
let residual_graph gr = 
	let result = empty_graph in 
	Graph.map gr 
	


(**************  MISE A JOUR  **************)



(**************  FORD-FULKERSON  **************)


