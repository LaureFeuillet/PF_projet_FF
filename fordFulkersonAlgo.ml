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


let rec f g sommet = 
  match g with
  [] -> failwith "Accès à un sommet non existant"
 |(s, voisins)::reste -> if s = sommet then 
                               voisins
                        else  
                               getVoisins reste sommet;;


(* Construct a residual graph from a capacity graph *)
let residual_graph gr = 
	let result = graph_without_arcs gr in
	(* Je veux que cette fonction regarde chaque arc et crée le ou les arcs qui correspondent dans le graphe d'écart. *)
	let rec f acu id out_arcs = match out_arcs with
		| (idD,{flow = 0; capacity})::tail -> f (add_arc acu id idD capacity) id tail
		| (idD,{flow = flot; capacity = capa})::tail -> if flot == capa then f (add_arc acu idD id flot) id tail 
	else f (add_arc (add_arc acu idD id flot) id idD (capa-flot)) id tail  	
	in
	v_fold gr f result
	
(* Tour a residual graph to find a path from source to sink, and its minimal cost *)
let tour_graph gr = assert false

(**************  MISE A JOUR  **************)
(* Update a flow graph according to an incrementation path and its minimal cost *)
let update_graph gr path cost = assert false

(**************  FORD-FULKERSON  **************)

 