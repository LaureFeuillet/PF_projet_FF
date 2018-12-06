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
let init_graph gr = map gr (fun capacity -> {flow = 0; capacity})

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
let q_first_not_marked q = 
	let rec loop q = match q with 
		|[] -> None
		|(id, _,false)::tl -> id
		|hd::tl -> loop tl
	in loop (List.rev q) 

(* Check if the given node already exists in the queue *)
let q_exists q id = List.exists (fun (x, _, _) -> x = id) q 

(* Mark the element with the given id *)
let q_mark_element q id = 
	let rec loop q acu = match q with
		|[] -> failwith "element not found in the queue"
		|(id, father, false)::tl -> List.append (List.rev tl) ((id, father, true)::acu)
		|(id, _, true)::tl -> failwith "element already marked"
		|hd::tl -> loop tl (hd::acu)
	in loop (List.rev q) []

(* Build a path from a queue *)
let q_build_path q = 
	let q_id_first q = match q with
		| [] -> []
		| (id, _, _)::tail -> id 
	in
	let rec loop q id path = match q with
		|[] -> path
		|(id, father, _)::tl -> loop tl father ((id, father)::path)
		|(_, _, _)::tl -> loop tl id path
	in loop q (q_id_first q) []

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

(* Find the minimal cost of a path *)
let find_min_from_path gr path = 
	let path_label_first = match path with
		|[] -> None
		|(idS,idD)::tail -> find_arc gr idS idD
	in 
	let rec loop remaining_path min = match remaining_path with
		| [] -> min
		| (idS, idD)::tl -> 
			if (find_arc gr idS idD) < min 
			then loop tl (find_arc gr idS idD) 
			else loop tl min
	in loop path path_label_first
	
(* Tour a residual graph to find a path from source to sink, and its minimal cost *)
let tour_residual_graph gr source sink = 
    (* Here we build the queue of the course in width. *)
    let rec loop_queue current_node current_outarcs q = 
		match (current_node, current_outarcs) with
		    (* We just found the sink ! We add it to the q and finish loop_queue. *)
		    | (_,((sink, _)::tail)) -> ((sink, current_node, false)::q)
		    (* There are no more arcs, we iterate on the next unmarked node of the queue. *)
		    | (_,[]) -> (match (q_first_not_marked q) with
				|None -> q 
				|Some w -> loop_queue w (out_arcs gr w) q
			)
		    (* There still are some arcs so : if the destination is in the queue, we dont add it and check the next destination, else we put it in the queue unmarked and we iterate. *)
		    | (_,((idD, _)::tail)) -> 
				if (q_exists q idD) 
				then (loop_queue current_node tail q) 
				else (loop_queue current_node tail ((idD, current_node, false)::q)) 
	in 
	let qq = loop_queue source (out_arcs gr source) [] 
	in
	let path = q_build_path qq
	in
	let min = find_min_from_path gr path 
	(* Return (path, min) *)
	in (path, min)

(************************************)
(* ------------ UPDATE ------------ *)
(************************************)

(* Update a flow graph according to an incrementation path and its minimal cost *)
let update_graph gr path cost = assert false

(************************************)
(* -------- FORD-FULKERSON -------- *)
(************************************)

