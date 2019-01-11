type id = string

type 'a out_arcs = (id * 'a) list

(* A graph is just a list of pairs: a node & its outgoing arcs. *)
type 'a graph = (id * 'a out_arcs) list

exception Graph_error of string

let empty_graph = []

let node_exists gr id = List.mem_assoc id gr

let out_arcs gr id =
  try List.assoc id gr
  with Not_found -> raise (Graph_error ("Node " ^ id ^ " does not exist in this graph."))

let find_arc gr id1 id2 =
  let out = out_arcs gr id1 in
  try Some (List.assoc id2 out)
  with Not_found -> None

let add_node gr id =
  if node_exists gr id then raise (Graph_error ("Node " ^ id ^ " already exists in the graph."))
  else (id, []) :: gr

let add_arc gr id1 id2 lbl =
  (* Existing out-arcs *)
  let outa = out_arcs gr id1 in
  (* Update out-arcs.
   * remove_assoc does not fail if id2 is not bound.  *)
  let outb = (id2, lbl) :: List.remove_assoc id2 outa in
  (* Replace out-arcs in the graph. *)
  let gr2 = List.remove_assoc id1 gr in
  (id1, outb) :: gr2


let v_iter gr f = List.iter (fun (id, out) -> f id out) gr

let v_fold gr f acu = List.fold_left (fun acu (id, out) -> f acu id out) acu gr

let map gr f =
List.map (fun (idO, outarc) -> (idO, (List.map (fun (idD,label) -> (idD,(f label))) outarc))) gr

(* Rebuild a multi-sources/multi-sinks graph from a virtual classic graph. *)
let rebuild_multi_graph gr =
  (* Add all nodes except the virtual nodes "theChosenSource" and "theChosenSource". *)
	let rec loop gr new_gr = match gr with
		| [] -> new_gr
		| (id, _)::tl when id = "theChosenSource"-> loop tl new_gr
		| (id, _)::tl when id = "theChosenSink"-> loop tl new_gr
		| (id, outArcs)::tl -> loop tl (add_node new_gr id)
	in
  (* Function to be use in v_flod to add all arcs except the ones coming from "theChosenSource" and going to "theChosenSink". *)
	let rec f acu current_id outArcs = match (current_id, outArcs) with
		| (_, []) -> acu
    | ("theChosenSource", _) -> acu
    | ("theChosenSink", _) -> acu
		| (_ , (id, _)::tl) when id = "theChosenSink"-> f acu current_id tl
		| (_ , (id, lbl)::tl) -> f (add_arc acu current_id id lbl) current_id tl
	in
  v_fold gr f (loop gr empty_graph)
