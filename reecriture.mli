(****************************************************************************
 * Types.                                                                   *
 ****************************************************************************)

type var = int;;
type symb = string;;

type graph =
    {
      nb_nodes : int;
      mat : int array array;
      nb_succ : int array;
      nb_pred : int array;
    };;

type term =
  | Var of var
  | Term of symb * term list;;

module VarMap : Map.S with type key = var;;


type substitution = term VarMap.t;;
(* where type substitution = (var * term) list *)

type rule = term * term;; (* l -> r is represented by (l,r) *)
type system = rule list;; (* regular rules: left member is constant *)
type dp = term * term;;
type projection = string -> int;;


(****************************************************************************
 * Unification, matching, substitution (providen functions).                *
 ****************************************************************************)
val substitute : substitution -> term -> term;;
val matching : term -> term -> substitution option;;
val unification : term -> term -> substitution option;;


(****************************************************************************
 * Debug/printing functions.                                                *
 ****************************************************************************)
val print_with : string -> ('a -> unit) -> 'a -> unit;;
val print_int_ws : int -> unit;;
val print_str_ws : string -> unit;;

val print_array : ('a -> unit) -> 'a array -> unit;;
val print_list : ('a -> 'b) -> 'a list -> unit;;
val print_option : ('a -> unit) -> 'a option -> unit;;

val print_symblist : (symb -> unit) -> symb list -> unit;;
val print_term  : term -> unit;;
val print_map : (VarMap.key -> unit) -> term VarMap.t -> unit;;
val print_dp : dp -> unit;;
val print_dps : dp list -> unit;;

val print_rule : rule -> unit;;
val print_system : system -> unit;;

val print_proj : (string -> int) -> symb list -> unit;;

val print_graph : graph -> unit;;


(****************************************************************************
 * Generic functions.                                                       *
 ****************************************************************************)

val uniq : ('a -> 'a -> bool) -> 'a list -> 'a list;;
val uniq_int : int list -> int list;;
val uniq_string : string list -> string list;;

val eq_string : string -> string -> bool;;

val swap_nth_child : 'a list -> int -> 'a -> 'a list;;

val option_to_bool : 'a option -> bool;;


(****************************************************************************
 * Term related functions.                                                  *
 ****************************************************************************)
val eq_term : term -> term -> bool;;
val eq_dp : dp -> dp -> bool;;
val uniq_term : term list -> term list;;

(* Compute of a term's symbols. *)
val build_symblist : term -> symb list;;

(* Compute the D set from a given system. *)
val compute_symbols : system -> symb list;;

(* Compute the max variable id of a term. *)
val get_var_max : term -> int;;

(* Build a new fresh variable from the current max variable id
   of a term. *)
val mk_fresh_var : int ref -> term;;

(* Cap function replaces by variables all subterms in D (see 7.14) *)
val cap : symb list -> term -> term;;

(* Ren function replaces variables by fresh ones. *)
val ren : term -> term;;

val subterms : term -> term list;;
val strict_subterms : term -> term list;;

val symbol_arity : symb -> term -> int;;
val system_symbol_arity : system -> symb -> int;;


(****************************************************************************
 * Graph related functions.                                                 *
 ****************************************************************************)
val graph_nb_nodes : graph -> int;;
val make_empty_graph : int -> graph;;
val add_edge : int -> int -> graph -> unit;;
val graph_acc : graph -> int -> int list;;
val graph_coacc : graph -> int -> int list;;

(* Colorize the graph depending on each connectivity.
  Return an array with each nodes' color (0 = no component,
  >0 = component's number). *)
val graph_strong_connectivity : graph -> int array;;

(* (extract_components G) returns the list of strongly connex components. *)
val extract_components : graph -> graph list;;

(* Compute over-approximation of dependency graph, see 7.14 p. 81 *)
val compute_graph : symb list -> dp list -> graph;;

val write_graph_dot : string -> graph -> unit;;

(****************************************************************************
 * Dependencies pairs.                                                      *
 ****************************************************************************)

(* Compute a system's dependencies pair. *)
val compute_dps : system -> dp list;;


(****************************************************************************
 * Rewriting functions.                                                     *
 ****************************************************************************)

(* Apply one rule to the current term (not subterms). *)
val apply_rule : term -> rule -> term option;;

(* Apply system rules to the current term (not subterms). *)
val apply_rules : term -> system -> term list;;

(* Return rewrited termrs after /one/ step. *)
val compute_step : system -> term -> term list;;

(* Return rewrited termrs after n steps. *)
val compute_n_step_reds : system -> int -> term -> term list;;


(****************************************************************************
 * Projection functions.                                                    *
 ****************************************************************************)

type can_be_removed = Strict | Large | No;;

(* Project returns a variable if the term is a variable of the n-th
   argument of the function. *)
val project : (string -> int) -> term -> term;;

(* removable R P (u,v) returns
 - Strict if (P u) (|> U ->R)+ (P v)
 - Large if (P u) (|> U ->R)* (P v)
 - No otherwise *)
val removable : system -> (string -> int) -> dp -> int -> can_be_removed;;

(* Generate all possible projections *)
val gen_projs : system -> int list list -> symb list -> int list list;;

(* Check that every node in this component is removable *)
val check_proj_comp : system -> graph -> projection -> dp -> int -> bool;;

(* Return  the component which belong to then n-th nodes. *)
val find_component : int -> graph list -> graph;;

(* Search if the projection is valid with some dp *)
val check_proj : system -> graph list -> projection -> dp list -> int -> int -> dp;;

(* Convert projection represented as a list to a function. *)
val proj_list_to_fun : (string * int) list -> (string -> int);;

(* Convert projection represented as a function to a list. *)
val proj_fun_to_list : (string -> int) -> symb list -> (string * int) list;;

(* Return a projection represnted as a list and a dependency pair such that
   removable R P (u0, v0) = Strict and for all (u, v) in the same strong
   connectivity component, removable R P (u0, v0) = Large.
   Raise a Not_found exception if no projection can be found. *)
val find_projection : system -> graph -> symb list -> int -> ((symb * int) list * dp)



(****************************************************************************
 * Main.                                                                    *
 ****************************************************************************)

(* Main returns minimal subgraphs of G_init
   which termination is equivalent to G. *)
val main : system -> int -> graph list;;
