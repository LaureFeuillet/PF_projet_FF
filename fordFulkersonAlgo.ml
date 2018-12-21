open Graph

(* Record to represent flow/capacity to have some "fc graph" types. *)
type fc = {
	flow : int ;
	capacity : int }

(* A path is a list of arcs between nodes. *)
type path = (id*id*id) list

(* A queue is a list of (node, father, marked_or_not, ).
 * A marked element means that all neighbors of the node are already in the queue.
 *)
type queue = (id*id*bool*id) list

(************************************)
(* ----------- INITIALIZE --------- *)
(************************************)

(* Construct a flow graph from a given capacity graph. *)
(* string graph -> (int * int) graph *)
let init_graph gr = map gr (fun c -> {flow = 0; capacity = (int_of_string c)})

(************************************)
(* ------------- TOUR ------------- *)
(************************************)

(* Return the same graph but without arcs. *)
(* 'a graph -> 'a graph *)
let graph_without_arcs gr =
	let result = empty_graph in
	let f acu id out_arcs = add_node acu id in
	v_fold gr f result

(* ----- QUEUE ----- *)

(* Return the id of the first element not marked in the queue. *)
(* queue -> id option *)
let q_first_not_marked q =
	let rec loop q = match q with
		|[] -> None
		|(id, _,false, _)::tl -> Some id
		|hd::tl -> loop tl
	in loop (List.rev q)

(* Check if the given node already exists in the queue. *)
(* queue -> id -> bool *)
let q_exists q id = List.exists (fun (x, _, _, _) -> x = id) q

(* Mark the element with the given id. *)
(* queue -> id -> queue *)
let q_mark_element q id =
	let rec loop current_q acu = match current_q with
		|[] -> failwith "element not found in the queue"
		|(node, _, true, _)::tl when node = id -> failwith "element already marked"
		(* We just found the element to mark, let's do this ! *)
		|(node, father, false, origin)::tl when node = id -> Printf.printf "a-%!"; List.append (List.rev tl) ((node, father, true, origin)::acu)
		(* We have to iterate to find the element to mark. *)
		|hd::tl -> Printf.printf "b-%!"; loop tl (hd::acu)
	in loop (List.rev q) []

(* Build a path from a queue *)
(* q -> path *)
let q_build_path q source = Printf.printf "q_build_path-%!";
	(* Last element entered in the queue, supposed to be the sink, needed to call the loop. *)
	let q_id_first = match q with
		| [] -> None
		| (id, _, _, _)::_ -> Some id
	in
	let rec loop q id path = match id with
		(* We are going to start by the end of the queue, the sink, and move to its father, and so on, until we reach the source. *)
		(* The queue is empty, there is no path to build. *)
		|None -> []
		|Some id_without_option -> (match q with
			(* We reached the end of the queue, we can return the builded path. *)
			|[] -> List.rev path
			(* We reached the source from the sink, we can return the builded path. *)
			|(node1, node2, _, _)::tl when ((node1 = source) && (node2 = source)) -> List.rev path
			(* We found an arc that approaches us to the source, we add it to the path and continue. *)
			|(node, father, _, origin)::tl when node = id_without_option -> loop tl (Some father) ((father, id_without_option, origin)::path)
			(* A useless entry of the queue, we have to iterate.  *)
			|_::tl -> loop tl id path
		)
	in loop q q_id_first []

(* Construct a residual graph from a capacity graph *)
(* fc graph -> int graph *)
let residual_graph gr = Printf.printf "residual_graph-%!";
	let result = graph_without_arcs gr in
	let rec f acu id out_arcs = match out_arcs with
		(* We are going to start from an empty graph (only nodes) and add the corresponding arcs to build the residual graph. *)
		(* We will use add_arc_without_erase instead of add_arc in case we have 2 arcs between the 2 same nodes. *)
		| [] -> acu
		| (idD,{flow = 0; capacity})::tail -> Printf.printf "r1";f (add_arc_without_erase acu id idD (capacity, id)) id tail
		| (idD,{flow = fl; capacity = capa})::tail -> Printf.printf "r2";
			if fl == capa
			then f (add_arc_without_erase acu idD id (fl,id)) id tail
			else f (add_arc_without_erase (add_arc_without_erase acu idD id (fl,id)) id idD ((capa-fl),id)) id tail
	in
	v_fold gr f result

(* Find the minimal cost of a path *)
(* 'a graph -> path -> int option *)
let find_min_from_path gr path = Printf.printf "find_min_from_path-%!";
	let path_label_first = match path with
		(* There is no path from source to sink. *)
		|[] -> None
		(* Label of the arc from sink to its father, needed to start the recursion. *)
		|(idS,idD)::tail -> find_arc gr idS idD
	in
	let rec loop remaining_path min = match remaining_path with
		(* We toured all the path, we can return the min cost. *)
		| [] -> min
		(* Compare the label of the next arc to the actual min, and update this value if needed. *)
		| (idS, idD)::tl ->
			if find_arc gr idS idD < min
			then loop tl (find_arc gr idS idD)
			else loop tl min
	in loop path path_label_first

(* Tour a residual graph to find a path from source to sink, and its minimal cost *)
(* 'a graph -> id -> id -> path * int option *)
let tour_residual_graph gr source sink =
    (* Here we build the queue of the course in width. *)
    let rec loop_queue current_node current_outarcs q = Printf.printf "1-%!";
		match (current_node, current_outarcs) with
		    (* We just found the sink ! We add it to the queue and finish loop_queue. *)
		    | (_,((node, (_, origin))::tail)) when node = sink -> Printf.printf "2-%!"; ((sink, current_node, false, origin)::q)
		    (* There are no more arcs from current_node, we iterate on the next unmarked node of the queue. *)
		    | (_,[]) -> (match (q_first_not_marked (q_mark_element q current_node)) with
				(* All elements of the queue are marked, there is no path from source to sink. *)
				|None -> Printf.printf "4-%!"; []
				(* Just an iteration on the next available (not marked) element of the queue. *)
				|Some w -> Printf.printf "5-%!"; loop_queue w (out_arcs gr w) (q_mark_element q current_node)
			)
		    (* There still are some arcs from the current_node. *)
		    | (_,((idD, (_,origin))::tail)) -> Printf.printf "6-%!";
				(* The destination is already in the queue, we dont add it and check the next destination. *)
				if (q_exists q idD)
				then (loop_queue current_node tail q)
				(* This destination isn't in the queue, wae add it unmarked and iterate. *)
				else (loop_queue current_node tail ((idD, current_node, false, origin)::q))
	in
	(* Calling the loop to build the queue from source to sink. *)
	let q = Printf.printf "7-%!"; loop_queue source (out_arcs gr source) [(source, source, false, source)]
	in
	(* Building the associated path of the queue.  *)
	let path = Printf.printf "8-%!"; q_build_path q source
	in
	(* Finding the incrementation value on the path. *)
	let min = Printf.printf "9-%!"; find_min_from_path gr path
	(* Return (path, min) *)
	in (path, min)


(************************************)
(* ------------ UPDATE ------------ *)
(************************************)

(* Update a flow graph according to an incrementation path and its minimal cost. *)
(* fc graph -> path -> int -> fc graph *)
let update_graph gr path cost = Printf.printf "update_graph-%!";
	(* The base is the given graph. *)
	let result_path = gr
	in
	(* For each arc of the path, we find the corresponding arc in the initial graph and update it. *)
	let update_arc result_arc idS idD origin = match (find_arc gr idS idD) with
		(* The considering arc doesn't exist, there are 2 possibilities, *)
		|None -> (match (find_arc gr idD idS) with
			(* - this arc really doesn't exist. *)
			|None -> failwith "error in update_graphe, path invalid"
			(* - the corresponding arc is inversed in the initial graph, because of the residual graph. *)
			|Some {flow = f; capacity = c} -> add_arc result_arc idD idS {flow = f - cost; capacity = c}
		)
		(* We increment the flow of the arc according to the cost. *)
		|Some {flow = f; capacity = c} -> add_arc result_arc idS idD {flow = f + cost; capacity = c}
	in
	let rec loop path result_path = match path with
		(* An empty path means that we have the final updated graph. *)
		|[] -> result_path
		(* Otherwise we iterate on the rest of the path after updating the graph considering the current arc of the path. *)
		|(idS, idD, origin)::tl -> loop tl (update_arc result_path idS idD origin)
	(* Calling the loop on the initial graph. *)
	in loop path result_path


(************************************)
(* -------- FORD-FULKERSON -------- *)
(************************************)

(* Applying the Ford Fulkerson Algorithm to a given graph with specified source and sink. *)
(* string graph -> id -> id -> string graph *)
let ford_fulkerson gr source sink =
	(* Construct a flow graph from the given capacity graph, needed to launch the loop. *)
	let init_fc_gr = init_graph gr
	in
	(* Loop on (path, min). *)
	let rec loop_ff fc_gr = match (tour_residual_graph (residual_graph fc_gr) source sink) with
		(* The min doesn't exist, which means we can't improve the flow repartition, we return the current_graph. *)
		|([], None) -> fc_gr
		|(smthg, None) -> fc_gr
		(* We can improve the flow repartition, we iterate on the updated graph. *)
		|(path, Some cost) -> loop_ff (update_graph fc_gr path cost)
	in
	let result = loop_ff init_fc_gr
	in
		(* We transform the (int*int) graph to a string graph to be able to print it. *)
		Graph.map result (fun {flow = f; capacity = c} -> (string_of_int f)^"/"^(string_of_int c))


(*****************************)
(* -------- THE END -------- *)
(*****************************)
