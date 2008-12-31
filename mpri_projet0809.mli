type var = int;;
type symb = string;;

module VarMap = Map.Make (struct type t = var let compare = (-) end);;

(* les types principaux *)
type term = Var of var | Term of symb * term list;;
type substitution = term VarMap.t;;  (* ou  type substitution =  (var * term) list;;*)
type rule = term * term;; (* l -> r est repr�sent�e par (l,r) *)
type system = rule list;; (* les regles sont suppos�es reguli�res, avec membre � gauche non variable *)
type dp = term * term;; 
type graph

val substitute : substitution -> term -> term (* fourni *)
val matching : term -> term -> substitution option (* fourni *)
val unification : term -> term -> substitution option (* fourni *)

(* computes_dps calcule toutes les paires de d�pendance du syst�me donn� en entr�e *)
val compute_dps : system -> dp list;;

(* (computes_graph const_symbs dp_list) calcule une sur-approximation G_init du graphe de d�pendance 
pour la liste de paires de d�pendance dp_list, en supposant que const_symbs est la liste
des symboles contructeurs,
suivant la m�thode du cours (cfr d�finition 7.14 p. 81 *)
val compute_graph : symb list -> dp list -> graph;;

(* (extract_components G) retrourne la liste des composantes fortement connexes de son entr�e G *)
val extract_components : graph -> graph list;;

(* (compute_n_step_reds R n t) retourne la liste des r�duits en n pas du terme t par le syst�me R *)
val compute_n_step_reds : system -> int -> term -> term list;;

(* (project P t) retourne t si t est une variable,
                          ti tel que ti est le (P f)-ieme element de l si t = (Term f l) *)
val project : (string -> int) -> term -> term 

type can_be_removed = Strict | Large | No

(* removable R P (u,v) retourne 
 - Strict si (P u) (|> U ->R)+ (P v)
 - Large si (P u) (|> U ->R)* (P v)
 - No dans les autres case *)
val removable : system -> (string -> int) -> dp -> can_be_removed


(* find_projection R G Def n retourne une projection P sous forme d'une liste d'association entre
les symboles d�finis donn�s par def_symbs, et une paire de d�pendance (u0,v0) telle que
 removable R P (u0,v0) = Strict, 
 et pour toute paire (u,v) dans la composante fortement connexe de (u0,v0) dans G,
 removable R P (u,v) = Strict ou removable R P (u,v) = Large,

 si aucune telle projection n'existe, on peut lever une exception.
 *)
val find_projection : system -> graph -> def_symbs -> int -> ((symb * int) list * dp)

(* main R retourne une liste de sous-graphes Gi du graphe Ginit dont
la terminaison est �quivalente � celle de Ginit, et qui sont minimaux au sens
o� le th�or�me de l'enonc� ne permet plus de les r�duire. *) 
val main : system -> graph list
