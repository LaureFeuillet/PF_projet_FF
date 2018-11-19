type id = string

type 'a out_arcs = (id * 'a) list

(* A graph is just a list of pairs: a node & its outgoing arcs. *)
type 'a graph = (id * 'a out_arcs) list

let empty_graph = []

(**************  INITIALISATION  **************)

(* Fonction permettant le passage d'un "capacité graph" à un "(flot * capacité) graph". *)
let init_graph gr = Graph.map gr (label -> (0, label))

(**************  PARCOURS  **************)



(**************  MISE A JOUR  **************)



(**************  FORD-FULKERSON  **************)


