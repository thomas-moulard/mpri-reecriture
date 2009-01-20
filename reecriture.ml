open Format;;

(****************************************************************************
 * Types.                                                                   *
 ****************************************************************************)
type var = int;;
type symb = string;;

type term = Var of var | Term of symb * term list;;
type rule = term * term;;
type system = rule list;;
type dp = term * term;;

type can_be_removed = Strict | Large | No;;

module VarMap = Map.Make (struct type t = var let compare = (-) end);;
module VarSet = Set.Make (struct type t = var let compare = (-) end);;
module IntMap = Map.Make (struct type t = int let compare = (-) end);;

type substitution = term VarMap.t ;;

type 'a pairs =
    Pair of ('a * 'a)
  | LPairs of ('a list * ('a list));;

type projection = string -> int;;


(****************************************************************************
 * Unification, matching, substitution (providen functions).                *
 ****************************************************************************)
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

(****************************************************************************
 * Debug, test, printing functions.                                         *
 ****************************************************************************)

let print_with str f e = f e; Format.print_string str;;
let print_int_ws = printf "%d@ ";;
let print_str_ws = printf "%s@ ";;

let print_array printfct array =
  if Array.length array == 0 then
      printf "empty"
  else
    Array.iter printfct array
;;

let rec print_list printfct list =
  let rec print = function
    | [] -> ()
    | e::t -> printfct e; print t in
  if list == [] then
    printf "empty"
  else
    print list
;;

let print_option print opt =
  match opt with
  | None -> printf "empty"
  | Some x -> print x
;;

let print_symblist printfct symbls =
  let rec print = function
    | [] -> ()
    | e::l -> printfct e; print l in
  if symbls == [] then
    printf "empty"
  else
    print symbls
;;

let rec print_term =
  let print_var x =
    printf "X%d" x
  and print_binop symbl = function
    | a::b::[] ->
        print_term a;
        printf "@ %s@ " symbl;
        print_term b;
    | _ -> assert false
  and print_args = function
    | [] -> ()
    | e::[] -> print_term e
    | u::v::l ->
        print_term u;
        List.iter (print_with ", " print_term) (v::l)
  in let print_symb symbl args =
    match List.length args with
    | 0 -> print_string symbl
    | 2 -> print_binop symbl args
    | _ -> printf "%s@ (" symbl; print_args args; printf ")"
  in function
    | Var x -> print_var x
    | Term (symbl, args) -> print_symb symbl args
;;

let print_map print map =
  let printmap key e =
    printf "X%d @ |->" key;
    print_term e; print_newline () in
  VarMap.iter printmap map
;;

let print_dp (left, right) = print_term left; printf ",@ "; print_term right;;

let print_dps dps =
  let rec print = function
    | [] -> ()
    | dp::l -> printf "@\n"; print_dp dp; print l in
 if dps == [] then
   printf "empty"
 else
   begin
     printf "@[<hov>";
     print dps;
     printf "@]@\n"
   end
;;

let print_rule (left, right) = print_term left; printf " ->@ "; print_term right;;

let print_system rules =
  let rec print = function
    | [] -> ()
    | rule::l -> printf "@\n"; print_rule rule; print l
  in
  if rules == [] then
    printf "empty"
  else
    begin
      printf "@[<hov>";
      print rules;
      printf "@]@\n"
    end
;;

let rec print_proj proj = function
  | [] -> ()
  | e::l -> Format.printf "%s:@ %d\n" e (proj e)
;;

let print_graph g =
  printf "@[<v>";
  printf "@[Nodes: %d@]@\n" g.nb_nodes;
  for i = 0 to pred g.nb_nodes do
    printf "@[<h>";
    for j = 0 to pred g.nb_nodes do
      printf "%d@ " g.mat.(i).(j);
    done;
    printf "@]@\n";
  done;
  printf "@]";
;;

(****************************************************************************
 * Generic functions.                                                       *
 ****************************************************************************)

let eq_string a b = String.compare a b == 0;;

let uniq eq list =
  let rec drop_double l elt =
    if not (List.exists (eq elt) l) then elt::l else l in
  List.fold_left drop_double [] list
;;

let uniq_int = uniq (==);;
let uniq_string = uniq eq_string;;

let rec swap_nth_child list n elt =
  let rec compute n = function
    | [] ->  []
    | e::l -> let res = if n == 0 then elt else e in
      res::compute (n - 1) l in
  if n >= List.length list then
    let error_str = sprintf "Can not swap %d-th child (length = %d)\n"
        n (List.length list) in
    print_string error_str;
    assert false
  else
    compute n list
;;

let option_to_bool = function
  | None -> false
  | Some _ -> true
;;

(****************************************************************************
 * Term related functions.                                                  *
 ****************************************************************************)

let rec eq_term a b = match (a, b) with
| (Var _, Term _) | (Term _, Var _) -> false
| (Var x, Var y) -> x == y
| (Term (s1, []), Term (s2, [])) -> eq_string s1 s2
| (Term (s1, args1), Term (s2, args2)) ->
    if eq_string s1 s2 then
      List.exists2 eq_term args1 args2
    else
      false
;;

let eq_dp (u1, v1) (u2, v2) = eq_term u1 u2 && eq_term v1 v2;;

let eq_graph g1 g2 =
  if g1.nb_nodes != g2.nb_nodes then
    false
  else
    begin
      let n = g1.nb_nodes
      and res = ref true in
      for i = 0 to pred n do
        for j = 0 to pred n do
          if g1.mat.(i).(j) != g2.mat.(i).(j) then
            res := false
        done;
      done;
      !res
    end
;;

let uniq_term = uniq eq_term;;

let build_symblist term =
  let rec build = function
    | Var _ -> []
    | Term (symbl, args) ->
        let res =  List.fold_left (fun e l -> List.append e (build l)) [] args in
        symbl::res
  in uniq_string (build term)
;;

let rec build_system_symblist = function
  | [] -> []
  | (left, right)::l -> uniq_string
        (List.append
           (List.append (build_symblist left) (build_symblist right))
           (build_system_symblist l))
;;


let compute_symbols rules =
  let rec build = function
    | [] -> []
    | (Var _, _)::l -> build l
    | (Term (symbl, _), _)::l -> symbl::(build l)
  in uniq_string (build rules)
;;

let get_var_max term =
  let rec compute n = function
    | Var x ->max n x
    | Term (_, args) ->
        max n (List.fold_left (fun n e -> max n (compute n e)) n args)
  in compute 0 term
;;

let mk_fresh_var n = n := !n + 1; Var !n;;

let cap symbls term =
  let max_vid = ref (get_var_max term)
  and must_refresh symbl = List.exists (eq_string symbl) symbls in
  let rec build = function
    | Var x -> Var x
    | Term (symbl, args) ->
        if must_refresh symbl then
          mk_fresh_var max_vid
        else
          Term (symbl, List.map build args) in
  build term
;;


let ren term =
  let max_vid = ref (get_var_max term) in
  let rec build = function
    | Var _ -> mk_fresh_var max_vid
    | Term (symbl, args) ->
        Term (symbl, List.map build args) in
  build term
;;


let rec subterms term =
  let subs = strict_subterms term in
  uniq_term (term::subs)
and strict_subterms term =
  match term with
  | Var _ -> []
  | Term (s, args) -> uniq_term (List.flatten (List.map subterms args))
;;


let symbol_arity symbl term =
    let rec build = function
      | Var _ -> -1
      | Term (s, args) ->
          if eq_string s symbl then
            List.length args
          else
            List.fold_left (fun n elt -> max n (build elt)) (-1) args
    in
    build term
;;


let rec system_symbol_arity rules symbl =
  let mymax a b = if a >= 0 then a else b in
  let rec build = function
    | [] -> -1
    | (left, right)::l ->
        mymax (mymax (symbol_arity symbl left) (symbol_arity symbl right)) (build l) in
  build rules
;;


(****************************************************************************
 * Graph related functions.                                                 *
 ****************************************************************************)
let graph_nb_nodes g = g.nb_nodes;;

let make_empty_graph nb_nodes = {
   nb_nodes = nb_nodes;
   mat = Array.create_matrix nb_nodes nb_nodes 0;
   nb_succ = Array.create nb_nodes 0;
   nb_pred = Array.create nb_nodes 0
 };;

let add_edge x y g =
  g.mat.(x).(y) <- succ g.mat.(x).(y);
  g.nb_pred.(y) <- succ g.nb_pred.(y);
  g.nb_succ.(x) <- succ g.nb_succ.(x)
;;

let graph_acc g node =
  let rec graph_acc g node vnodes =
    if g.nb_succ.(node) == 0 then
      [node]
    else
      let res = ref [node] in
      begin
        for i = 0 to pred g.nb_nodes do
          if g.mat.(node).(i) > 0
              && not (List.exists ((==) i) vnodes) then
            res := List.append !res (graph_acc g i (node::vnodes));
        done;
        uniq_int !res;
      end
  in
  graph_acc g node []
;;

let graph_coacc g node =
  let rec graph_coacc g node vnodes =
    if g.nb_pred.(node) == 0 then
      [node]
    else
      let res = ref [node] in
      begin
        for i = 0 to pred g.nb_nodes do
          if g.mat.(i).(node) > 0 && not (List.exists ((==) i) vnodes) then
            res := List.append !res (graph_coacc g i (node::vnodes));
        done;
        uniq_int !res;
      end
  in
  graph_coacc g node []
;;

let graph_strong_connectivity g =
  let res = Array.create g.nb_nodes 0
  and maxind = ref 1 in
  for i = 0 to pred g.nb_nodes do
    for j = 0 to pred g.nb_nodes do
      let acc2 = graph_acc g j
      and coacc2 = graph_coacc g j in
      if List.exists ((==) i) acc2
          && List.exists ((==) i) coacc2 then
        let ind = max res.(i) res.(j) in
        if ind == 0 then
          (res.(i) <- !maxind; res.(j) <- !maxind; maxind := !maxind + 1)
        else
          (res.(i) <- ind; res.(j) <- ind)
    done;
  done;
  res
;;

let compute_graph const_symbls dp_list =
  let nb_var = List.length dp_list in
  let g = make_empty_graph nb_var in
  try
    for i = 0 to pred nb_var do
      for j = 0 to pred nb_var do
        let (s1, t1) = List.nth dp_list i
        and (s2, t2) = List.nth dp_list j in
        match unification (ren (cap const_symbls t1)) s2 with
        | None -> ()
        | Some _ -> add_edge i j g
      done;
    done;
    g
  with Failure _ -> assert false (* should never happen *)
;;

let extract_components g =
  let conn = graph_strong_connectivity g in
  let maxind = Array.fold_left max 0 conn
  and res = ref [] in
  for i = 1 to maxind do
    let comp = make_empty_graph g.nb_nodes in
    for x = 0 to pred g.nb_nodes do
      for y = 0 to pred g.nb_nodes do
        if g.mat.(x).(y) > 0
            && conn.(x) == i
            && conn.(x) == conn.(y) then
          add_edge x y comp
      done;
    done;
    res := comp::(!res)
  done;
  !res
;;

let write_graph_dot filename g =
  let oc = open_out filename in
  Printf.fprintf oc "digraph graph_dep {\n";
  for i = 0 to pred g.nb_nodes do
    for j = 0 to pred g.nb_nodes do
      if g.mat.(i).(j) > 0
      then Printf.fprintf oc "   %d -> %d;\n" (i+1) (j+1)
    done;
  done;
  Printf.fprintf oc "}\n";
  close_out oc;
;;

(****************************************************************************
 * Dependencies pairs.                                                      *
 ****************************************************************************)

let compute_dps sys =
  let symbls = compute_symbols sys in
  let is_symbl str = List.exists (eq_string str) symbls in
  let rec search_subterms left = function
    | Var _ -> []
    | Term (symbl, args) ->
        let res = List.flatten (List.map (search_subterms left) args) in
        if is_symbl symbl then (left, Term (symbl, args))::res else res in
  let rec build = function
    | [] -> []
    | (left, right)::l ->
        List.append (search_subterms left right) (build l)
  in build sys
;;


(****************************************************************************
 * Rewriting functions.                                                     *
 ****************************************************************************)

let apply_rule term (left, right) =
  match unification left term with
  | None -> None
  | Some sigma -> Some (substitute sigma right)
;;

let rec apply_rules term = function
  | [] -> [term]
  | rule::l ->
      match apply_rule term rule with
      | None -> apply_rules term l
      | Some res -> uniq_term (res::apply_rules term l)
;;

let compute_step sys t = match t with
| Var x -> apply_rules t sys
| Term (s, args) ->
    begin
      let subterms = List.map (fun e -> apply_rules e sys) args in
      let rebuild_term n e = Term (s, swap_nth_child args n e) in
      let rebuild subterms =
        let rec rebuild n = function
          | [] -> []
          | subterms::l -> List.append
                (List.map (fun e -> rebuild_term n e) subterms)
                (rebuild (n+1) l) in
        rebuild 0 subterms in
      uniq_term (List.append (rebuild subterms) (apply_rules t sys))
    end
;;

let rec compute_n_step_reds sys n t =
  if n == 0 then
    [t]
  else
    let one_step = compute_step sys t in
    let res = List.flatten
        (List.map (compute_n_step_reds sys (n - 1)) one_step) in
    uniq_term (List.append one_step res)
;;


(****************************************************************************
 * Projection functions.                                                    *
 ****************************************************************************)

let project proj term =
  try
    match term with
    | Var x -> Var x
    | Term (symbl, args) ->
        if (proj symbl) < 0 then
          term (* Arity is zero, no projection *)
        else
          List.nth args (proj symbl)
  with Failure _ -> failwith "Invalid projection used."
  | Invalid_argument _ -> failwith "Invalid projection used."
;;

let removable r p (u, v) n =
  let pu = project p u
  and pv = project p v in
  let st = subterms pu
  and st_strict = strict_subterms pu
  and one_step = compute_n_step_reds r 1 pu
  and n_step = compute_n_step_reds r n pu in

  if List.exists (eq_term pv) (List.append st n_step) then
    Strict
  else if List.exists (eq_term pv) (List.append st_strict one_step) then
    Large
  else
    No
;;


let gen_seq arity =
  let rec gen n =
    if n >= arity then
      []
    else
      n::(gen (n+1)) in
  if arity < 0 then
    assert false
  else if arity == 0 then
    [-1]
  else
    gen 0
;;

let rec complete_proj arity = function
  | [] -> List.map (fun e -> [e]) (gen_seq arity)
  | proj::l ->
      let res = List.map
          (fun elt -> List.append proj [elt])
          (gen_seq arity) in
      if l == [] then res else List.append res (complete_proj arity l)
;;

let rec gen_projs rules symbls =
  let rec gen proj = function
  | [] -> proj
  | symbl::l ->
      let arity = system_symbol_arity rules symbl in
      gen (complete_proj arity proj) l in
  gen [] symbls
;;



let rec check_proj_comp rules graph p dp nstep =
  let i = ref 0
  and res = ref true in
  while !i < graph.nb_nodes
      && !res == true do
    if graph.nb_succ.(!i) > 0
        && graph.nb_pred.(!i) > 0
        && removable rules p dp nstep == No then
      res := false;
    i := !i + 1
  done;
  !res
;;

let rec find_component n comps =
  match comps with
  | [] -> raise Not_found
  | graph::l ->
      if graph.nb_pred.(n) > 0
          && graph.nb_succ.(n) > 0 then
        graph
      else
        find_component n l
;;

let rec check_proj rules comps p dps n nstep =
  match dps with
  | [] -> raise Not_found
  | dp::l ->
      if removable rules p dp nstep == Strict
        && check_proj_comp rules (find_component n comps) p dp nstep then
        dp
      else
        check_proj rules comps p l (n+1) nstep
;;

let proj_list_to_fun proj_list =
  fun symbol ->
    let rec find_symbl = function
      | [] -> raise Not_found
      | (symbl, n)::l -> if eq_string symbl symbol then n else find_symbl l in
    find_symbl proj_list
;;

let rec proj_fun_to_list proj_fn = function
  | [] -> []
  | symbl::l -> (symbl, proj_fn symbl)::proj_fun_to_list proj_fn l
;;

let rec find_projection rules g symbls n =
  let get_symb_pos symb symbls =
    let (res, _) = List.fold_right
        (fun e (n, pos) ->
          if String.compare symb e == 0 then
            (pos, pos+1)
          else
            (n, pos+1))
        symbls (0, 0)
    in res
  in
  let to_fun projs =
    try
      List.map (fun proj -> (fun symb -> List.nth proj (get_symb_pos symb symbls))) projs
    with Failure _ -> assert false in

  let all_symbs = build_system_symblist rules in
  let dps = compute_dps rules in
  let g = compute_graph symbls dps in
  let comps = extract_components g in
  let projs = gen_projs rules all_symbs in
  let chk_proj elt =
    try
      Some (elt, check_proj rules comps elt dps 0 n)
    with Not_found -> None in
  let valid_projs = List.map chk_proj (to_fun projs) in
  match List.find option_to_bool valid_projs with
  | None -> assert false
  | Some (f, dp) -> (proj_fun_to_list f symbls, dp)
;;


(****************************************************************************
 * Main.                                                                    *
 ****************************************************************************)

(* Remove a node put -1 in a state. *)
let is_null_graph g =
  let res = ref true in
  for i = 0 to pred g.nb_nodes do
    if g.nb_succ.(i) >= 0 then
      res := false
  done;
  !res
;;

let main rules nstep =
  let dps = compute_dps rules in
  let symbs = compute_symbols dps in
  let graph = compute_graph symbs dps in
  let rec remove graph =
    try
      let (proj, dp) = find_projection rules graph symbs nstep
      and state = ref 0
      and found = ref false in

      while !found == false && !state < graph.nb_nodes do
        try
          if eq_dp (List.nth dps !state) dp then
            found := true
          else
            state := !state + 1
        with Failure _ -> assert false
      done;
      if is_null_graph graph || graph.mat.(!state).(0) < 0 then
        []
      else
        begin
          remove_a_node graph !state;
          graph::(remove graph);
        end
    with Not_found -> [graph] in
  remove graph
;;



