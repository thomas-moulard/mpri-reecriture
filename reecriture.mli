type var = int;;
type symb = string;;

module VarMap : Map.S with type key = var;;

(* Used types *)
type term = Var of var | Term of symb * term list;;
type substitution = term VarMap.t;;  (* where type substitution = (var * term) list;;*)
type rule = term * term;; (* l -> r is represented by (l,r) *)
type system = rule list;; (* regular rules: left member is constant *)
type dp = term * term;;

type graph;;

(* Provided functions *)
val substitute : substitution -> term -> term
val matching : term -> term -> substitution option
val unification : term -> term -> substitution option


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

val print_graph : Format.formatter -> graph -> unit;;


(****************************************************************************
 * Generic functions.                                                       *
 ****************************************************************************)

val uniq : ('a -> 'a -> bool) -> 'a list -> 'a list;;
val uniq_int : int list -> int list;;
val uniq_string : string list -> string list;;

val eq_string : string -> string -> bool;;


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

val write_graph_dot : string -> graph -> unit;;

(****************************************************************************
 * Dependencies pairs.                                                      *
 ****************************************************************************)

(* Compute a system's dependencies pair. *)
val compute_dps : system -> dp list;;





(* (computes_graph const_symbs dp_list) calcule une sur-approximation G_init du graphe de dépendance
pour la liste de paires de dépendance dp_list, en supposant que const_symbs est la liste
des symboles contructeurs,
suivant la méthode du cours (cfr définition 7.14 p. 81 *)
val compute_graph : symb list -> dp list -> graph;;

(* (extract_components G) retrourne la liste des composantes fortement connexes de son entrée G *)
val extract_components : graph -> graph list;;

(* (compute_n_step_reds R n t) retourne la liste des réduits en n pas du terme t par le système R *)
val compute_n_step_reds : system -> int -> term -> term list;;
val apply_rule : term -> rule -> term option;;
val apply_rules : term -> system -> term list;;
val compute_step : system -> term -> term list;;

(* (project P t) retourne t si t est une variable,
                          ti tel que ti est le (P f)-ieme element de l si t = (Term f l) *)
val project : (string -> int) -> term -> term

type can_be_removed = Strict | Large | No

(* removable R P (u,v) retourne
 - Strict si (P u) (|> U ->R)+ (P v)
 - Large si (P u) (|> U ->R)* (P v)
 - No dans les autres case *)

val strict_subterms : term -> term list;;
val subterms : term -> term list;;

val removable : system -> (string -> int) -> dp -> int -> can_be_removed


(* find_projection R G Def n retourne une projection P sous forme d'une liste d'association entre
les symboles définis donnés par def_symbs, et une paire de dépendance (u0,v0) telle que
 removable R P (u0,v0) = Strict,
 et pour toute paire (u,v) dans la composante fortement connexe de (u0,v0) dans G,
 removable R P (u,v) = Strict ou removable R P (u,v) = Large,

 si aucune telle projection n'existe, on peut lever une exception.
 *)
val find_projection : system -> graph -> symb list -> int -> ((symb * int) list * dp)

(* main R retourne une liste de sous-graphes Gi du graphe Ginit dont
la terminaison est équivalente à celle de Ginit, et qui sont minimaux au sens
où le théorème de l'enoncé ne permet plus de les réduire. *)
val main : system -> graph list



(* Debug *)
