open Graph

(**************  FORD-FULKERSON  **************)
(* Apply the ford-fulkerson algorithm to a graph with specified source and sink. *)
(* Return a flow/capacity graph with the maximum flow. *)
val ford_fulkerson: string graph -> id list -> id list -> string graph
