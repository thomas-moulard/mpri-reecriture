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

let print_list f l =
  if l == [] then
      print_string "empty"
  else
    let rec print_list l =
      match l with
      | [] -> ()
      | e::t -> f e; print_list t
    in
    print_list l
;;

let print_array f arr =
  if Array.length arr == 0 then
      print_string "Empty\n."
  else
    Array.iter (fun e -> print_string " "; f e) arr
;;


let print_graph fmt g =
  print_string "** Number of nodes: ";
  print_int g.nb_nodes;
  print_newline ();
  print_string "** Succ:";
  print_array print_int g.nb_succ;
  print_newline ();
  print_string "** Pred:";
  print_array print_int g.nb_pred;
  print_newline ();

  for i = 0 to pred g.nb_nodes do
    for j = 0 to pred g.nb_nodes do
      if g.mat.(i).(j) > 0
      then Format.fprintf fmt "@[%d -> %d@]@." i j
    done
  done
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

let print_symblist sl =
  let rec p sl =
    match sl with
    | [] -> ()
    | e::l ->
        print_string e;
        print_string " ";
        p l
  in
  if sl == [] then
    print_string "Empty\n"
  else
    p sl
;;

let rec print_term t =
  match t with
  | Var x ->
      print_string "X";
      print_int x
  | Term (s, tl) ->
      if List.length tl == 2 then
        match tl with
        | a::b::[] ->
            begin
              print_term a;
              print_string " ";
              print_string s;
              print_string " ";
              print_term b;
            end;
        | _ -> assert false
      else if List.length tl > 0 then
        begin
          print_string s;
          print_string (" (");
          begin
            match tl with
            | [] -> ()
            | e::[] -> print_term e
            | u::v::l ->
                print_term u;
                List.iter (fun e -> print_string ", "; print_term e) (v::l)
          end;
          print_string (")");
        end
      else
        print_string s
;;

let print_dp dp =
  let (left, right) = dp
  in
  print_term left;
  print_string ", ";
  print_term right
;;

let print_dps dpl =
  let rec p dpl =
    match dpl with
    | [] -> ()
    | e::l ->
        print_dp e;
        print_newline ();
        p l
  in
  if dpl == [] then
    print_string "empty\n"
  else
    p dpl
;;

let rec print_system sys =
  match sys with
  | [] -> print_string ""
  | (t1, t2)::l ->
      print_term t1; print_string " -> "; print_term t2;
      print_newline ();
      print_system l
;;

let print_option print opt =
  match opt with
  | None -> print_string "empty"
  | Some x -> print x
;;

let print_map map =
  VarMap.iter
    (fun key e ->
      print_string "X"; print_int key;
      print_string " |-> ";
      print_term e;
      print_newline ()) map
;;

(* ***************************** *)

let rec eq_term a b =
  match (a, b) with
  | (Var _, Term _) | (Term _, Var _) -> false
  | (Var x, Var y) -> x == y
  | (Term (s1, args1), Term (s2, args2)) ->
      if String.compare s1 s2 != 0 then
        false
      else
        List.exists2 eq_term args1 args2
;;

let eq_dp (u1, v1) (u2, v2) =
  eq_term u1 u2 && eq_term v1 v2
;;


let uniq eq list =
  List.fold_left (fun l elt -> if not (List.exists (eq elt) l) then elt::l else l) [] list
;;
let uniq_int = uniq (==);;
let uniq_string = uniq (fun a b -> String.compare a b == 0);;
let uniq_term = uniq eq_term;;


let make_empty_graph nb_nodes =
  {
   nb_nodes = nb_nodes;
   mat = Array.create_matrix nb_nodes nb_nodes 0;
   nb_succ = Array.create nb_nodes 0;
   nb_pred = Array.create nb_nodes 0
 }
;;

let add_edge x y g =
  g.mat.(x).(y) <- succ g.mat.(x).(y);
  g.nb_pred.(y) <- succ g.nb_pred.(y);
  g.nb_succ.(x) <- succ g.nb_succ.(x)
;;

let remove_state graph n =
  graph
;;

let graph_nb_nodes g = g.nb_nodes;;

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

(*
  Colorize the graph depending on each connectivity.
  Return an array with each nodes' color (0 = no component,
  >0 = component's number).
*)
let graph_strong_connexity g =
  let res = Array.create g.nb_nodes 0
  and maxind = ref 1 in
  begin
    for i = 0 to pred g.nb_nodes do
      for j = 0 to pred g.nb_nodes do
        let acc2 = graph_acc g j
        and coacc2 = graph_coacc g j in
        if List.exists ((==) i) acc2
        && List.exists ((==) i) coacc2 then
          let ind = max res.(i) res.(j) in
          begin
            if ind == 0 then
              begin
                res.(i) <- !maxind;
                res.(j) <- !maxind;
                maxind := !maxind + 1;
              end
            else
              begin
                res.(i) <- ind;
                res.(j) <- ind;
              end
          end
      done;
    done;
    res
  end
;;

(* ***************************** *)

(* Compute D set for DP *)
let rec compute_dp_d sys =
  match sys with
  | [] -> []
  | (left, right)::l -> match left with
    | Var _ -> compute_dp_d l
    | Term (s, tl) -> uniq_string (s::(compute_dp_d l))
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

(* CAP function *)
let cap symbl term =
  let rec cap n symbl term =
    match term with
    | Var _ -> term
    | Term (s, tl) ->
        if List.exists (fun e -> String.compare s e == 0) symbl then
          Var (n * 10)
        else
          let rec comp_map n symbl tl =
            match tl with
            | [] -> []
            | e::l -> (cap n symbl e)::(comp_map (n + 1) symbl l)
          in
          Term (s, comp_map n symbl tl)
  in
  cap 100 symbl term (* FIXME: *)
;;

(* REN function *)
let ren term =
  let rec ren n term =
    match term with
    | Var _ -> Var (10 * n)
    | Term (s, tl) ->
        let rec comp_map n tl =
          match tl with
          | [] -> []
          | e::l -> (ren n e)::(comp_map (n + 1) l)
        in
        Term (s, comp_map n tl)
  in
  ren 200 term (* FIXME: *)
;;

(* Compute symb
=> compute_d!!!
let rec compute_symb r =
  let rec compute_term t =
    match t with
    | Var _ -> []
    | Term (s, tl) ->
        s::(List.flatten
              (List.map compute_term tl))
  in match r with
  | [] -> []
  | (t1, t2)::l ->
gauche uniquement ou gauche/droite?
      List.append (List.append (compute_term t1) (compute_term t2)) (compute_symb l)

      List.append (compute_term t1) (compute_symb l)
;;
*)

(* Compute G_init *)
let compute_graph symbl dpl =
  let nb_var = List.length dpl
  in let g = {
    nb_nodes = nb_var;
    mat = Array.create_matrix nb_var nb_var 0;
    nb_succ = Array.create nb_var 0;
    nb_pred = Array.create nb_var 0
  }
  in let _ =
    for i = 0 to List.length dpl - 1 do
      for j = 0 to List.length dpl - 1 do
        let (s1, t1) = List.nth dpl i
        and (s2, t2) = List.nth dpl j
        in
        (* FIXME:
        print_newline ();
        print_string "---";
        print_newline ();
        print_string "symnl: "; print_list print_string symbl;
        print_newline ();
        print_string "t1: "; print_term t1;
        print_newline ();
        print_string "ren t1: "; print_term (ren t1);
        print_newline ();
        print_string "cap t1: "; print_term (cap symbl t1);
        print_newline ();
        print_string "cap ren t1: "; print_term (ren (cap symbl t1));
        print_newline ();
        print_string "s2: "; print_term s2;
        print_newline ();
         *)
        match unification (ren (cap symbl t1)) s2 with
        | None -> ()
        | Some _ -> add_edge i j g
      done;
    done
  in g
;;

let extract_components g =
  let conn = graph_strong_connexity g in
  let maxind = Array.fold_left (fun e m -> max e m) 0 conn
  and res = ref []
  in
  begin
    for i = 1 to maxind do
      let g2 = make_empty_graph g.nb_nodes
      in
      begin
        for x = 0 to pred g.nb_nodes do
          for y = 0 to pred g.nb_nodes do
            if g.mat.(x).(y) > 0
                && conn.(x) == i
                && conn.(x) == conn.(y) then
              add_edge x y g2
          done;
        done
      end;
      res := g2::(!res)
    done;
    !res
  end
;;

let rec term_map var_f term_f term =
  match term with
  | Var x -> var_f x
  | Term (s, tl) ->
      term_f s
        (List.map (term_map var_f term_f) tl)
;;


let rec swap_nth_child list n elt =
  match list with
  | [] ->  []
  | e::l -> let res =
      if n == 0 then elt else e
  in res::swap_nth_child l (n-1) elt
;;




let apply_rule term (left, right) =
  let sigmaopt = unification left term in
  match sigmaopt with
  | None -> None
  | Some sigma ->
(*
  print_newline ();
  print_map sigma;
  print_term (substitute sigma right);
  print_newline ();
  print_string "\t\t";
*)
  Some (substitute sigma right)
;;

let rec apply_rules term rules =
  match rules with
  | [] -> [term]
  | rule::l ->
      match apply_rule term rule with
      | None -> apply_rules term l
      | Some res -> uniq_term (res::(apply_rules term l))
;;

let compute_step sys t =
  match t with
  | Var _ -> apply_rules t sys
  | Term (s, args) ->
      begin
        let subterms = List.flatten
            (List.map (fun e -> apply_rules e sys) args) in
        let rebuild_term n e =
          Term (s, swap_nth_child args n e) in
        let (res, _) = List.fold_left
            (fun (l, n) e -> ((rebuild_term n e)::l, n+1))
            ([], 0)
            subterms
        in
        uniq_term (apply_rules t sys)
      end
;;

let rec compute_n_step_reds sys n t =
  if n == 0 then
    [t]
  else
    let one_step = compute_step sys t in
    let res = List.flatten
        (List.map (compute_n_step_reds sys (n-1)) one_step) in
    uniq_term (List.append one_step res)
;;


let project p t =
  match t with
  | Var _ -> t
  | Term (s, tl) -> List.nth tl (p s)
;;

(*
  Return Strict if t2 is strict subterm of t1,
  large if t2 is a subterm of t2,
  no otherwise.
 *)
let rec is_subterm t1 t2 =
  match (t1, t2) with
  | (Var x, Var y) -> if x == y then Large else No
  | (Var _, Term _) -> No
  | (Term (s, tl), Var _) ->
      let doit prev e =
        if prev != No
            && is_subterm e t2 != No then
          Strict
        else
          No
      in
      List.fold_left doit Strict tl

  | (Term (s1, tl1), Term (s2, tl2)) ->
      if String.compare s1 s2 != 0 then
        No
      else
        let rec doit prev e1 e2 =
          if prev != No then
            let res = is_subterm e1 e2 in
            if res == No then
              match e1 with
              | Var _ -> No
              | Term (s, tl_) ->
                  List.fold_left2 doit Large tl_ tl2
            else
              res
          else
            No
        in
        List.fold_left2 doit Strict tl1 tl2
;;

let rec subterms term =
  let subs = strict_subterms term in
  uniq_term (term::subs)
and strict_subterms term =
  match term with
  | Var _ -> []
  | Term (s, args) ->
      uniq_term (List.flatten (List.map subterms args))
;;

let removable r p (u, v) =
  let pu = project p u
  and pv = project p v in
  let st = subterms pu
  and st_strict = strict_subterms pu
  and one_step = compute_n_step_reds r 1 pu in
  let res = List.append st one_step
  and res_strict = List.append st_strict one_step in

  if List.exists (eq_term pv) res then
    Strict
  else if List.exists (eq_term pv) res_strict then
    Large
  else
    No
;;

let rec find_projection sys g symbl n =
  let dps = compute_dps sys in
  let g = compute_graph symbl dps in
  let gcomps = extract_components g in

  let rec find_p symbl =
    match symbl with
    | [] -> []
    | e::l -> [] (* FIXME: *)
  in let rec find_dp p dps =
    match dps with
    | [] -> raise Not_found
    | e::l ->
        if removable sys p e == Strict then
          e
        else
          find_dp p l

  in let proj = find_p symbl
  and to_proj_fn proj symb =
    let (s, n) = List.find (fun (s, n) -> String.compare symb s == 0) proj in n
  in (proj, find_dp (to_proj_fn proj) dps)
;;

let main sys =
  let dps = compute_dps sys in
  let symbs = compute_dp_d dps in
  let graph = compute_graph symbs dps in

  let rec remove graph =
    try
      let (proj, dp) = find_projection sys graph symbs 1
      and state = ref 0 and found = ref false in

      while !found == false && !state < graph.nb_nodes do
        if eq_dp (List.nth dps !state) dp then
          found := true
        else
          state := !state + 1
      done;
      let new_graph = remove_state graph state in
      new_graph::(remove new_graph);
    with Not_found -> [graph]
  in
  remove graph
;;
