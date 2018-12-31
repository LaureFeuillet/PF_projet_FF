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

let find_arc_with_origin gr id1 id2 origin =
  let out_id1 = out_arcs gr id1 in 
  let rec loop out = match out with
    | [] -> None
    | (id,(lbl, ori))::_ when (id = id2) && (ori = origin) -> Some lbl
    | _::tail -> loop tail
  in loop out_id1

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

let add_arc_without_erase gr id1 id2 lbl =
  let current_outarcs = out_arcs gr id1 in
  let new_outarcs = List.append [(id2, lbl)] current_outarcs in    (* in le graph dans lequel on enlève les arcs de id1 ... *)
  let new_gr = List.remove_assoc id1 gr in
  (id1, new_outarcs) :: new_gr


let v_iter gr f = List.iter (fun (id, out) -> f id out) gr

let v_fold gr f acu = List.fold_left (fun acu (id, out) -> f acu id out) acu gr

let map gr f =
List.map (fun (idO, outarc) -> (idO, (List.map (fun (idD,label) -> (idD,(f label))) outarc))) gr
