type var = int;;
type symb = string;;

type term = Var of var | Term of symb * term list;;
type rule = term * term;;
type system = rule list;;
type dp = term * term;;

type can_be_removed = Strict | Large | No;;

type def_symbs = int;; (* FIXME *)

module VarMap = Map.Make (struct type t = var let compare = (-) end);;
module VarSet = Set.Make (struct type t = var let compare = (-) end);;
module IntMap = Map.Make (struct type t = int let compare = (-) end);;

type substitution = term VarMap.t ;;

type 'a pairs =
    Pair of ('a * 'a)
  | LPairs of ('a list * ('a list));;

let rec gmatching sigma pb =
  match pb with
    | [] -> sigma
    | Pair (p,s) :: pb' ->
	begin
	  match p, s  with
	    | Var x, _ ->
		begin
		  try
		    let s' = VarMap.find x sigma in
		      if s = s'
		      then gmatching sigma pb'
		      else raise Exit
		  with Not_found -> gmatching (VarMap.add x s sigma) pb'
		end
	    | Term (f, lp), Term (g, ls) ->
		if f = g
		then gmatching sigma ((LPairs (lp,ls)) :: pb')
		else raise Exit
	    | Term _, Var _ -> raise Exit
	end
    | LPairs ([],[]) :: pb' -> gmatching sigma pb'
    | LPairs ([],_::_) :: _ -> raise Exit
    | LPairs (_::_,[]) :: pb' -> raise Exit
    | LPairs (p::lp,s :: ls) :: pb' ->
	gmatching sigma (Pair (p,s) :: LPairs (lp,ls) :: pb');;



let matching t1 t2 =
    try
      let sigma = gmatching VarMap.empty [Pair (t1,t2)] in
	Some sigma
    with Exit -> None;;

let rec size t =
  match t with
    | Var _ | Term(_,[]) -> 1
    | Term(f,l) -> List.fold_left (fun acc s -> acc + (size s)) 1 l;;


let rec set_of_variables t =
  match t with
    | Var x -> VarSet.singleton x
    | Term (_, l) ->
	List.fold_left (fun set t -> VarSet.union (set_of_variables t) set)
	  VarSet.empty l;;


let rec apply_raw_subst vsigma t =
  match t with
    | Var x ->
	begin
	  try Var (VarMap.find x vsigma)
	  with Not_found -> t
	end
    | Term (_,[]) -> t
    | Term (f,l) -> Term (f, List.map (apply_raw_subst vsigma) l);;


let rec substitute sigma t =
  match t with
    | Var x -> begin try VarMap.find x sigma with Not_found -> t end
    | Term(f,l) -> Term (f, List.map (substitute sigma) l);;


let rec unif_dec vsigma sigma pb =
  match pb with
    | [] -> (vsigma, sigma)
    | Pair (p,s) :: pb' ->
	begin
	  match p, s with
	    | Term (f, lp), Term (g, ls) ->
		if f = g
		then unif_dec vsigma sigma ((LPairs (lp,ls)) :: pb')
		else raise Exit
	    | Var x, (Term _ as s)
	    | (Term _ as s),  Var x ->
		begin
		  let x' = try VarMap.find x vsigma with Not_found -> x in
		  let s' = apply_raw_subst vsigma s in
		    try
		      let t = VarMap.find x' sigma in
			if s' = t
			then unif_dec vsigma sigma pb'
			else
			  if size s' < size t
			  then unif_dec vsigma (VarMap.add x' s' sigma) (Pair (t,s') :: pb')
			  else unif_dec vsigma sigma (Pair (t,s') :: pb')
		    with Not_found -> unif_dec vsigma (VarMap.add x' s' sigma) pb'
		end
	    | Var x, Var y ->
		let x' = try VarMap.find x vsigma with Not_found -> x
		and y' = try VarMap.find y vsigma with Not_found -> y in
		  if x' = y'
		  then 	unif_dec vsigma sigma pb'
		  else
		    let vsigma' =
		      VarMap.add x' y'
			(VarMap.map (fun v -> if v = x' then y' else v)
			   vsigma) in
		    let sigma' =
		      VarMap.fold
			(fun v t acc ->
			   let v' = if v = x' then y' else v
			   and t' = apply_raw_subst vsigma' t in
			     VarMap.add v' t' acc)
			sigma VarMap.empty in
		      try
			let x_val = VarMap.find x' sigma
			and y_val = VarMap.find y' sigma in
			  if size x_val < size y_val
			  then
			    unif_dec vsigma'
			      (VarMap.add y' (apply_raw_subst vsigma' x_val) sigma')
			      (Pair(x_val,y_val) :: pb')
			  else unif_dec vsigma' sigma' (Pair(x_val,y_val) :: pb')
		      with Not_found -> unif_dec vsigma' sigma' pb'

	end
    | LPairs ([],[]) :: pb' -> unif_dec vsigma sigma pb'
    | LPairs ([],_::_) :: _ -> raise Exit
    | LPairs (_::_,[]) :: pb' -> raise Exit
    | LPairs (p::lp,s :: ls) :: pb' ->
	unif_dec vsigma sigma (Pair (p,s) :: LPairs (lp,ls) :: pb');;

(*

  A graph is described by its incidence matrix with the redundant
  informations on the number of sucessors and predecessors of each
  (integer) node. It also contains an information over the number of
  nodes.

*)

type graph =
    {
      nb_nodes : int;
      mat : int array array;
      nb_succ : int array;
      nb_pred : int array;
    };;

type 'a cycle =
  |  Cycle of 'a list
  |  No_cycle of 'a list;;


let print_graph fmt g =
  for i = 0 to pred g.nb_nodes do
    for j = 0 to pred g.nb_nodes do
      if g.mat.(i).(j) > 0
      then Format.fprintf fmt "@[%d -> %d@]@." i j
    done
  done;;

let remove_a_node g ix =
  for j=0 to pred g.nb_nodes do
    if g.mat.(j).(ix) > 0
    then g.nb_succ.(j) <- pred g.nb_succ.(j);
    g.mat.(j).(ix) <- -1;
    if g.mat.(ix).(j) > 0
    then g.nb_pred.(j) <- pred g.nb_pred.(j);
    g.mat.(ix).(j) <- -1;
  done;
  g.nb_succ.(ix) <- -1;
  g.nb_pred.(ix) <- -1;;



let filter_indices test v =
  let res = ref [] in
    for i = 0 to pred (Array.length v) do
      if test v.(i) then res:= i :: !res
    done;
    !res;;

let without_pred g = filter_indices (fun n -> n = 0) g.nb_pred;;
let without_succ g = filter_indices (fun n -> n = 0) g.nb_succ;;

exception Found of int;;

(*
  [(next_node_in_cycle g node)] returns a successor of
  the node [node] in the graph described by the graph [g], and both
  [node] and its successor are in a cycle.

  This function has has to be called only once all nodes without
  successor have been removed in the graph [g].
*)
  let next_node_in_cycle g node =
    try
      for j = 0 to g.nb_nodes do
	if g.mat.(node).(j) = 1 && g.nb_succ.(j) > 0
	then raise (Found j)
      done;
      failwith "Topological.next_node_in_cycle"
    with Found next -> next;;


(*
   [(complete_partial_cycle nb_nodes g partial_cycle)] adds
   a node to [partial_cycle], a part of a cycle which is already built.

   This function has has to be called only once all nodes without
   successor have been removed in the graph [g].

*)
  let rec complete_partial_cycle g partial_cycle =
    match partial_cycle with
      | last_node :: rest_of_partial_cycle ->
	  if List.exists
	    (function u -> (g.mat.(last_node).(u) = 1))
	    rest_of_partial_cycle
	  then (* the partial cycle is actually a cycle *)
	    Cycle partial_cycle
	  else
	    let next = next_node_in_cycle g last_node in
	      complete_partial_cycle g (next :: partial_cycle)
      | [] -> assert false;;


let find_index test v =
  try
    for i = 0 to pred (Array.length v) do
      if test v.(i) then raise (Found i)
    done;
    raise Not_found
  with Found i -> i ;;

(*
   [(find_a_cycle g maximal_nodes)] finds a cycle
   in the graph [g].

   This function has has to be called only once all nodes without
   predecessor have been recursively removed in [g].

*)

let rec find_a_cycle g maximal_nodes =
  List.iter (remove_a_node g) maximal_nodes;

  match without_succ g with
    | [] ->
	let v = find_index (function n -> n > 0) g.nb_succ in
	  complete_partial_cycle g [v]
    | maximal_nodes -> find_a_cycle g maximal_nodes;;



(*
  [(top_rec g n sorted_nodes nodes_without_pred)]
  \begin{itemize}
  \item when there is no cycle in the graph [g],
  returns [No_cycle list_of_nodes] where
  [list_of_nodes] provides a total ordering over the nodes, compatible
  with the partial ordering induced by the graph.
  \item when there is a cycle in the graph [g], returns
  [Cycle partial_list_of_nodes] where [partial_list_of_nodes] is a cycle
  in the graph.
  \end{itemize}
*)

let rec top_rec g n sorted_nodes minimal_nodes =
  if n = 0 (* success, all variables have been popped *)
  then No_cycle sorted_nodes
  else
    begin
      match minimal_nodes with
	| [] ->
	    begin
	      match without_pred g with
		| [] ->
		    (* there is a cycle *)
		    find_a_cycle g (without_succ g)
		| minimal_nodes ->
		    top_rec g n sorted_nodes minimal_nodes
	    end

	| _ ->
	    let n' = List.length minimal_nodes in
	    if n = n'
            then (* success *)
	      No_cycle (minimal_nodes @ sorted_nodes)
            else
	      let _ = List.iter (remove_a_node g) minimal_nodes in
		top_rec g (n - n') (minimal_nodes @ sorted_nodes) []
    end;;


let topological_sort g = top_rec g g.nb_nodes [] [] ;;

let rename vsigma x = try VarMap.find x vsigma with Not_found -> x;;

let gen_oc return_fun with_trace substitute vsigma sigma =
  let vset,oc_list =
    VarMap.fold
      (fun v t (vset,oc_list) ->
	 let v' = rename vsigma v in
	 let vset', oc_list' =
	   VarSet.fold (fun u (vset'', oc_list'') ->
			  let u' = rename vsigma u in
		           (VarSet.add u' vset''),
			   (let vu' = (v',u') in
			      if List.mem vu' oc_list''
			      then oc_list''
			      else vu' :: oc_list''))
	     (set_of_variables t) (vset,oc_list) in
	   (VarSet.add v' vset'), oc_list')
      sigma (VarMap.fold (fun x y acc -> VarSet.add x (VarSet.add y acc)) vsigma VarSet.empty,[]) in

  let nb_var = VarSet.cardinal vset in

  let _, var_int, int_var =
    VarSet.fold
      (fun x (i,vi,iv) -> (succ i, VarMap.add x i vi, IntMap.add i x iv))
      vset (0,VarMap.empty, IntMap.empty) in

  let g =
    {
      nb_nodes = nb_var;
      mat = Array.create_matrix nb_var nb_var 0;
      nb_succ = Array.create nb_var 0;
      nb_pred = Array.create nb_var 0
    } in

  let _ =
    List.iter (fun (x,y) ->
		 let ix = VarMap.find x var_int
		 and iy = VarMap.find y var_int in
	      g.mat.(ix).(iy) <- 1;
	      g.nb_pred.(iy) <- succ g.nb_pred.(iy);
	      g.nb_succ.(ix) <- succ g.nb_succ.(ix))
      oc_list in

  match topological_sort g with
      | Cycle il -> raise Exit
      | No_cycle il ->
	  let vl = List.map (fun i -> IntMap.find i int_var) il in
	  (* ignore (List.iter (print_variable Format.std_formatter) vl); *)
	    let sigma' =
	      List.fold_left
		(fun subst x ->
		   let x_val1 = (try VarMap.find x sigma with Not_found -> Var x) in
		   let x_val = substitute subst x_val1 in
		   VarMap.add x x_val subst)
		VarMap.empty vl in
	    let sigma =
	      VarMap.fold
		(fun x y sigma'' ->
		   VarMap.add x (VarMap.find y sigma') sigma'')
		vsigma sigma'  in
	    let vl = List.map (fun i -> IntMap.find i int_var) il in
	    return_fun(sigma,vl);;

let oc = gen_oc (fun (x,_) -> x) false;;

let unification t1 t2 =
    try
      let vsigma, sigma =
	unif_dec VarMap.empty VarMap.empty [Pair (t1,t2)] in
	Some (oc substitute vsigma sigma)
    with Exit | Not_found -> None;;

(* ***************************** *)

(* Compute D set for DP *)
let rec compute_dp_d sys =
  match sys with
  | [] -> []
  | (left, right)::l -> match left with
    | Var _ -> compute_dp_d l
    | Term (s, tl) -> s::(compute_dp_d l)
;;

(* Search if right's subterms allows to create DP *)
let rec dp_search_subterms dp_d sys left right =
  match right with
  | Var _ -> []
  | Term (s, tl) ->
      let res = List.flatten
          (List.map (fun e -> dp_search_subterms dp_d sys left e) tl)
      in
      if List.exists (fun e -> (String.compare e s) == 0) dp_d then
        (left, right)::res
      else
        res
;;

(* Compute DPs *)
let compute_dps sys =
  let rec compute dp_d sys =
    match sys with
    | [] -> []
    | (left, right)::l ->
        List.append (dp_search_subterms dp_d sys left right) (compute dp_d l)
  in compute (compute_dp_d sys) sys
;;


let compute_graph symbl dpl = {
  nb_nodes = 0;
  mat = [|[||]|];
  nb_succ = [||];
  nb_pred = [||];
};;

let extract_components g = [];;

let compute_n_step_reds sys n t = [];;

let project f t = t;;

let removable sys f d = No;;

let find_projection sys g dsymbs n = ([], (Var 0, Var 0));;
let main sys =
  [];;

