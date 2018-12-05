open Graph

type fc = {
	flow : int ; 
	capacity : int } 

type path = (id*id) list

type queue = (id*id*bool) list


(************************************)
(* ----------- INITIALIZE --------- *)
(************************************)

(* Construct a flow graph from a given capacity graph *)
let init_graph gr = Graph.map gr (fun capacity -> {flow = 0; capacity})

(************************************)
(* ------------- TOUR ------------- *)
(************************************)

(* Return the same graph but without arcs *)
let graph_without_arcs gr = 
	let result = empty_graph in
	let f acu id out_arcs = add_node acu id in
	v_fold gr f result

(* ----- QUEUE ----- *)

(* Add an element to the queue *)
let q_add q e = e::q

(* Return the id of the first element not marked in the queue *)
let rec q_first_not_marked q = match q with
	|[] -> None
	|(id, _,false)::tl -> Some id
	|hd::tl -> q_first_not_marked tl

(* Check if the given node already exists in the queue *)
let q_exists q id = List.mem (id, _, _) q 

(* Mark the element with the given id *)
let q_mark_element q id = 
	let rec loop q id acu = match q with
		|[] -> acu 
		|(id, _, false)::tl -> List.append (List.append acu (id, true)) tl
		|(id, _, true)::tl -> failwith "element already marked"
		|hd::tl -> loop tl id (List.append acu [hd])
	in loop q id []

(* Build a path from a queue *)
let q_build_path q = assert false

(* Construct a residual graph from a capacity graph *)
let residual_graph gr = 
	let result = graph_without_arcs gr in
	let rec f acu id out_arcs = match out_arcs with
		| [] -> acu
		| (idD,{flow = 0; capacity})::tail -> f (add_arc acu id idD capacity) id tail
		| (idD,{flow = flot; capacity = capa})::tail -> 
			if flot == capa 
			then f (add_arc acu idD id flot) id tail 
			else f (add_arc (add_arc acu idD id flot) id idD (capa-flot)) id tail  	
	in
	v_fold gr f result
	
(* Tour a residual graph to find a path from source to sink, and its minimal cost *)
let tour_graph gr = 

(************************************)
(* ------------ UPDATE ------------ *)
(************************************)

(* Update a flow graph according to an incrementation path and its minimal cost *)
let update_graph gr path cost = assert false

(************************************)
(* -------- FORD-FULKERSON -------- *)
(************************************)
