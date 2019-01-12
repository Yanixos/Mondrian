#!/usr/bin/ocaml
#load "graphics.cma";;
open Graphics;;
open Printf;;

(*========================================== BSP FUNCTIONS PART ===============================================================*)
 
type point = { x : int ; y : int } ;;
type rectangle = { mutable rc : color option ; a : point ; w : int ; h : int } ;;
type line = { mutable lc : color option ; start : point ; end_ : point } ;;
type label = { coord : int; colored : bool; }
type bsp = R of color option | L of label * bsp * bsp;;

(* Fonctions qui affiche le BSP, on l'a utilisé pour debugger  *)

let print_node s depth pere =
	printf "%d:%s\n" depth ((String.make depth ' ') ^ (string_of_int s) ^ " parent: "^ (string_of_int pere))
;;

let rec print_tree t depth pere = match t with 
      R None -> ()
    | R Some c -> ()
    | L(label, g, d) -> (print_node label.coord depth pere; print_tree g (depth+1) label.coord; print_tree d (depth+1) label.coord)
    ;;

Random.init;;

(* Fonction qui sera utilisée dans random_bsp pour générer le plus de lignes coloriées possible, parce que si on venait à utiliser 
Random.bool on aurait la moitié des lignes non coloriées donc beaucoup moins de contraintes*)

let colored_line_bool_bsp x = x > 5;;

(* Fonction qui sera utilisée dans random_bsp pour affecter une couleur aléatoire aux rectangles ( rouge | bleu ) *)

let random_color () : color option =
	if Random.bool() then Some red else Some blue
;;

(* random_bsp sera la fonction qui nous générera notre tree  bsp
   il prend en paramètres la hauteur et la largeur de la fenêtre, et une profondeur de l'tree 
   la fonction appellera une fonction recursive auxilliaire qui prendra plusieurs paramètres:
   le début et la fin des largeurs et hauteurs de la zone de la fenêtre sur laquelle elle agira
   la profondeur maximale, et la profondeur courrante. Elle fonctionnera comme suit: Si la profondeur 
   maximale est atteinte, on retournera une feuille contenant ou non une couleur, sinon si on est à
   une hauteur paire, alors on séparera la largeur de la fenetre courante en deux de façon équilibrée
   en définissant une marge de taille 1/6 de la taille totale de chaque coté du rectangle, pour
   ne pas avoir de zones trop petites et trop grandes dans notre plan de jeu, et ce en créant un
   noeud contenant l'abscisse et on appellera la fonction auxilliaire recursivement pour les deux
   zones séparés. Et dans le cas d'une profondeur impaire, on fera le même traitement pour les 
   hauteurs
 *)

let random_bsp ( depth : int ) : bsp =
  assert (depth > 0);
  let finished = ref false
  in

  let rec aux x_y min_x max_x min_y max_y current_depth =
      if current_depth = depth then ( finished := true; R ( random_color() ) )
      else if  ! finished = false || colored_line_bool_bsp (Random.int 30)
            then
            if x_y = 0 
              then 
              let space =(max_x-min_x)/6 
              in
              let tmp = min_x+ space + Random.int ( max_x - min_x - space - space )(* espace gauche et droit *)
              in
                  L ( { coord = tmp ; colored = colored_line_bool_bsp(Random.int 30) }, 
                       aux 1  min_x tmp min_y max_y (current_depth+1),
                       aux 1  tmp max_x min_y max_y (current_depth+1)
                    )
              else
                  let space =(max_y-min_y)/6 in
                  let tmp = min_y+ space + Random.int ( max_y - min_y - space - space ) (*espace haut et bas*)
                      in
                      
                      L ( { coord = tmp ; colored = colored_line_bool_bsp(Random.int 30) }, 
                           aux 0  min_x max_x min_y tmp (current_depth+1),
                           aux 0  min_x max_x tmp max_y (current_depth+1)
                        )     
    else
          R ( random_color() )
                               
  in aux 0 0 (size_x()) 0 (size_y()) 0 
;;  


(* Notre fonction rectangles from bsp, elle fonctionne comme suit: On utilisera une fonction auxilliaire
  qui prendra en paramètre deux valeurs supplémentaires : pos, qui indiquera si on travaille sur des lignes
  verticales ou horizontales, et rect, qui sera notre réctangle qui sera construit progressivement à chaque 
  appel récursif, lequel aura un coté défini à chaque niveau de l'appel récursif, et qu'on renverra une fois
  qu'on atteindra une feuille de notre tree  BSP, qui contiendra ou non la couleur de notre réctangle
*)

         
let rectangles_from_bsp ( tree : bsp ) : rectangle list = (*marche*)
	
	let rec rectangles_from_bsp_aux tree x_y l r d u = 
	
	  match tree with
	| R color -> (
		      { 
		          rc = color;
		          a = { x=l ; y=d } ; 
		          w =  r - l  ;
          		  h =  u - d  ;
		      } :: []
		         )
	| L ( head , left , right ) ->  
	                            ( 
			                   if  x_y = 0  then
			                       ( rectangles_from_bsp_aux left  1 l head.coord d u )  
			                       @ 
			                       ( rectangles_from_bsp_aux right 1 head.coord r d u )
			                   else
			                       ( rectangles_from_bsp_aux left  0 l r d head.coord )
			                       @ 
			                       ( rectangles_from_bsp_aux right 0 l r head.coord u )
								)
	in rectangles_from_bsp_aux tree 0 0 (size_x()) 0 (size_y())
;;

        
 
(* Fonction qui renvoie la liste des rectangles vides*)

let rectangles_from_bsp_none (tree : bsp) : rectangle list=   (*marche*)
 
    let rec rectangles_from_bsp_aux_none tree x_y l r d u =
	      match tree with
	    | R color -> (
		          { 
		              rc = None;
		              a = { x=l ; y=d } ; 
		              w =  r - l  ;
              		  h =  u - d  ;
		          } :: []
		             )
	    | L ( head , left , right ) ->  
	                                ( 
			                       if  x_y = 0  then
			                           ( rectangles_from_bsp_aux_none left  1 l head.coord d u )  
			                           @ 
			                           ( rectangles_from_bsp_aux_none right 1 head.coord r d u )
			                       else
			                           ( rectangles_from_bsp_aux_none left  0 l r d head.coord )
			                           @ 
			                           ( rectangles_from_bsp_aux_none right 0 l r head.coord u )
								    )
	in rectangles_from_bsp_aux_none tree 0 0 (size_x()) 0 (size_y())
;;


(* Fonction qu'on utilisera après, elle permettera d'intialiser les lignes afin qu'on puisse les colorier selon les contraintes*)

let init_color label = if label.colored then None else (Some black)
;;

(* Fonction pour déterminer si une ligne et verticale ou horizontale*)

let line_direction (l : line) : int = if l.start.x=l.end_.x then 0 else 1
;;


(* On défini une fonction qui, étant donnée une liste de lignes et une liste de rectangles, elle renverrai
   une liste de couples contenant une ligne et la liste de tous les rectangles qui lui sont adjascents.
*)

let lines_from_bsp_none ( tree  : bsp ) : line list=       (*marche*)
  
    if tree = R None then []
    else   
    
        let rec lignes_aux tree_temp pos min_x max_x max_y min_y =   
          if tree_temp = R None then []
          else
                  match tree_temp with
                | R couleur -> []
                | L ( head , left , right ) ->    if pos = 'b' then
                                                        [{lc= init_color head;
                                                        start = {x = head.coord ; y= max_y};
                                                        end_=   {x= head.coord  ; y= min_y};}]
                                                        @ 
                                                        lignes_aux left 'g' min_x head.coord max_y min_y
                                                        @ 
                                                        lignes_aux right 'd' head.coord max_x max_y min_y
                                                          
                                                   else if pos = 'h' then
                                                        [{lc= init_color head;
                                                        start = {x = head.coord  ; y=min_y};
                                                        end_=   {x=head.coord    ; y= max_y};}]
                                                        @ 
                                                        lignes_aux left 'g' min_x head.coord max_y min_y
                                                        @
                                                        lignes_aux right 'd' head.coord max_x max_y min_y
                                                          
                                                   else if pos = 'g' then
                                                        [{lc= init_color head;
                                                        start = {x = max_x  ; y= head.coord  };
                                                        end_=   {x= min_x   ; y= head.coord  };}]
                                                        @
                                                        lignes_aux left 'h' min_x max_x max_y head.coord
                                                        @ 
                                                        lignes_aux right 'b' min_x max_x head.coord min_y
                                                        
                                                    else
                                                        [{lc= init_color head;
                                                        start = {x = min_x  ; y= head.coord};
                                                        end_  = {x=max_x    ; y=head.coord };}]
                                                        @ 
                                                        lignes_aux left 'h' min_x max_x max_y head.coord
                                                        @ 
                                                        lignes_aux right 'b' min_x max_x head.coord min_y

     in
	    lignes_aux tree  'b' 0 (size_x ()) 0 (size_y ())
;;
  
  

(* Fonction qui coloriera une liste de lignes selon les rectangles qui lui sont adjascents, Elle fonctionnera
   comme suit : pour chaque ligne, tout d'abord on vérifie qu'elle peut être coloriée, ensuite avec une fonction
   auxilliaire recursive, on parcourera ses rectangles tout en incrémentant les compteurs correspondants. Une
   fois le parcours achevé, on coloriera la ligne selon les valeurs obtenues des compteurs.
*)

let rec get_rectangles (ll : line list) (rectlist : rectangle list) : (line* (rectangle list)) list=
	
	let rec get_rectangles_aux (l : line) (rectlist : rectangle list) (ret : rectangle list) : line * (rectangle list) = 
		match rectlist with
	  | [] -> (l, ret)
	  | t::q -> if ((line_direction l)=0 && (t.a.x=l.start.x || t.a.x + t.w=l.start.x)) ||
	                (line_direction l)=1 && ( t.a.y + t.h =l.start.y|| t.a.y=l.start.y ) 
	            then
				  get_rectangles_aux l q (t::ret)
				else
				  get_rectangles_aux l q ret
	in
	  match ll with
	| [] -> []
	| t::q -> (get_rectangles_aux t rectlist [])::get_rectangles q rectlist
  ;;
  
  (* Fonction qui coloriera une liste de lignes selon les rectangles qui lui sont adjascents, Elle fonctionnera
	 comme suit : pour chaque ligne, tout d'abord on vérifie qu'elle peut être coloriée, ensuite avec une fonction
	 auxilliaire recursive, on parcourera ses rectangles tout en incrémentant les compteurs correspondants. Une
	 fois le parcours achevé, on coloriera la ligne selon les valeurs obtenues des compteurs.
  *)
  
let rec color_lines (linesrectlist : (line * (rectangle list)) list) =

	let rec color_aux (l :line) (rectlist: rectangle list) cpt =
	    match rectlist with
	  | [] -> if cpt > 0 then l.lc <- (Some red)
			  else if cpt < 0 then l.lc <- (Some blue)
			  else l.lc <- (Some magenta)
	  | t::q ->
		 (
		    match t.rc with
		  | Some c -> if c = red then color_aux l q (cpt+1)
			          else if c = blue then color_aux l q (cpt-1)
					  else failwith "color_lines : color indifined"
					  
		  | None -> failwith "color_lines : None value"
		 )
	in
	match linesrectlist with
	| [] -> ()
	| (ll,listrectlist)::q -> if ll.lc = None then
							     ( (color_aux ll listrectlist 0 ); color_lines q)
						      else
							     color_lines q
;;      




(* Pour finir, notre fonction lines_from_bsp qui fera tout d'abord appel à la fonction lines_from_bsp_uncolored
    puis coloriera les lignes en appelant color_lines et retournera lines à la fin
*)


let lines_from_bsp (tree : bsp) : line list =
	let lines = lines_from_bsp_none tree 
	in
	(color_lines (get_rectangles lines (rectangles_from_bsp tree));	lines)
;;

(* Notre fonction draw_current_bsp qui étant donné une liste de lignes, les dessinera sur notre plan de travail,
  tout d'abord on videra au préalable le précédent plan de travail, ensuite on défini la largeur de nos lignes à 3
  pour plus de lisibilité, ensuite pour chaque ligne, on défini la couleur courante comme étant la couleur de cette
  ligne et on la dessine utilisant ses coordonnées de départ et de fin
*)
let rec draw_rectlist (rectlist : rectangle list) : unit = (*marche*)
    match rectlist with 
  | [] -> ()
  | t::q -> (
                 match t.rc with
               | None -> 
                 (
                   set_color white;
			       fill_rect t.a.x t.a.y t.w t.h;               
			       draw_rectlist q
                 )
               | Some color -> 
               (
                 if color=red then 
                 (
                    set_color (rgb 127 1 1);
                    fill_rect t.a.x t.a.y t.w t.h;
                    draw_rectlist q
                 )
                 else if color=blue then 
                 (
                    set_color (rgb 1 1 127);
				    fill_rect t.a.x t.a.y t.w t.h;               
				     draw_rectlist q
                 )
                 else
                   failwith "draw_rectlist : configuration error"
               )
           )
;;

let rec draw_lines ll =
      match ll with
    | [] -> () ;
    | t::q -> (
                  match t.lc with 
                | Some color -> set_color color;
                | None -> set_color black
              );
              moveto t.start.x t.start.y; 
			  lineto t.end_.x t.end_.y; draw_lines q; 
;;

let draw_current_bsp (tree : bsp) : unit =
      set_line_width 2;
      draw_lines (lines_from_bsp tree);
;; 

(*Fonction utilisée uniquement dans le cadre de tests. Est une fonction à laquelle est donnée une liste de rectangles, et qui les dessine 
  sur le graphe
*)


(* Fonction auxilliaire check_current aux qui s'occuper de verifier si pour une ligne donnée et ses rectangles adjascents
   est-ce qu'elle satisfait les règles de victoire du Mondarian, elle fonctionnera comme suit: elle dispose de deux
   attributs supplémentaires, nbb et nbr, qui serviront de compteurs. Pour chaque rectangle adjascent, si il est
   rouge, on incrémentera le compteur nbr, si il est bleu, on incrémentera le compteur bleu, sinon notre rectangle
   est erroné. Une fois tous les rectangles parcourus, on vérifiera quatre situations de victoire: Si la couleur de notre
   ligne est noire, si la couleur est bleue (resp. rouge) et que le compteur des nbb (resp.nbr) est supérieur à celui 
   des nbr (resp. nbb), ou bien que la couleur soit magenta, et que les deux compteurs soient égaux. Si aucune des
   conditions n'est respectée, alors la ligne ne respecte pas la condition de victoire.
*)

let rec check_current_aux (l:line) (rectlist : rectangle list) (cpt: int): bool =
  match rectlist with
    |  [] ->
       (
            match l.lc with
          | None -> false
		  | Some color -> if ((color=red && cpt>0) || (color=blue && cpt < 0) || (color=magenta && cpt=0) || (color=black)) then true
		  				  else false
        )
    | t::q ->
       (
           match t.rc with
         | None -> false
      	 | Some c -> if c=red then check_current_aux l q (cpt+1)
              			else if c=blue then check_current_aux l q (cpt-1)
                   				else failwith "check_current_aux : indifined color"
       )

(* Notre fonction check current qui effectuera le traitement de current_check_aux sur chaque
   ligne de notre jeu.
*)

let rec check_current (linesrectlist : (line * rectangle list) list) : bool =
    match linesrectlist with
  | [] -> true
  | ( lline,lrectlist ) :: q -> if (check_current_aux lline lrectlist 0 ) then check_current q
                                else false
;;

(*Fonction check_possible_extension, son fonctionnement est très similaire à check_current et check_current_aux, à la difference qu'elle prend
  en compte les rectangles vides. Elle sera appellée comme la première étape pour tester la validité du coloriage actuel et la possibilité
  de l'étendre en un coloriage complet. Elle tester pour chaque ligne si les rectangles qui sont déjà coloriés et les vides restants ne rentrent 
  pas en conflit avec la couleur de la ligne
*)

let rec check_possible_extension (linesrectlist : (line * rectangle list) list) : bool = 
  let rec check_possible_extension_aux (l: line) (rectlist : rectangle list) (nbb : int) (nbr : int) (nbnone : int) : bool =
      match rectlist with 
    | [] ->
          (
              match l.lc with  
            | None -> failwith "check_possible_extension : None value."
            | Some c -> if 
            (
              ( nbr = 0 && nbb = 0) ||
              ( c = red && ( nbr + nbnone ) > nbb ) || 
              ( c = blue && ( nbb + nbnone ) > nbr ) || 
              ( c = magenta && ( nbr = nbb || ( ( ( nbr+nbnone + nbb ) / 2 ) >= nbb && ( ( nbr + nbnone + nbb ) / 2 ) >= nbr ) ) ) ||
              ( c = black )
            ) 
            then true else false
          )
    | t::q -> 
          (
             match t.rc with
           | None -> check_possible_extension_aux l q nbb nbr ( nbnone + 1 )
           | Some c -> if c = red then check_possible_extension_aux l q nbb ( nbr + 1 ) nbnone
                       else if c = blue then check_possible_extension_aux l q ( nbb + 1 ) nbr nbnone
                       else failwith "check_possible_extension : color indifined"
          )
  in
  match linesrectlist with
    [] -> true
  | ( lline, lrectlist ) :: q -> if (check_possible_extension_aux lline lrectlist 0 0 0) then check_possible_extension q
                                 else false
;;

(*==========================================END OF BSP FUNCTIONS PART ===============================================================*)

(*=================================================================================================================================*)

(*========================================== START SAT SOLVER ============================================================================*)


(* reférence : 
   SAT-MICRO: petit mais costaud !
   par Sylvain Conchon, Johannes Kanig, Stéphane Lescuyer
*)

module type VARIABLES = sig
  type t
  val compare : t -> t -> int
end

module Make (V : VARIABLES) = struct

  type literal = bool * V.t

  module L = struct
   type t = literal
    let compare (b1,v1) (b2,v2) =
      let r = compare b1 b2 in
      if r = 0 then compare v1 v2
      else r

    let mk_not (b,v) = (not b, v)
  end

  module S = Set.Make(L)

  exception Unsat
  exception Sat of S.t

  type t = { gamma : S.t ; delta : L.t list list }

  let rec assume env f =
    if S.mem f env.gamma then env
    else bcp { gamma = S.add f env.gamma ; delta = env.delta }

  and bcp env =
    List.fold_left
      (fun env l -> try
          let l = List.filter
              (fun f ->
                 if S.mem f env.gamma then raise Exit;
                 not (S.mem (L.mk_not f) env.gamma)
              ) l
          in
          match l with
          | [] -> raise Unsat (* conflict *)
          | [f] -> assume env f
          | _ -> { env with delta = l :: env.delta }
        with Exit -> env)
      { env with delta = [] }
      env.delta

  let rec unsat env = try
      match env.delta with
      | [] -> raise (Sat env.gamma)
      | ([_] | []) :: _ -> assert false
      | (a :: _ ) :: _ ->
        begin try unsat (assume env a) with Unsat -> () end ;
        unsat (assume env (L.mk_not a))
    with Unsat -> ()

  let solve delta = try
      unsat (bcp { gamma = S.empty ; delta }) ; None
    with
    | Sat g -> Some (S.elements g)
    | Unsat -> None

end

(*========================================== END SAT SOLVER ============================================================================*)

(*=================================================================================================================================*)

(*========================================== START VERIF COLORIAGE ============================================================================*)


module Variables = struct
  type t = rectangle
  let compare x y = if (x.a.x = y.a.x && x.a.y=y.a.y) then 0 
                        else if ((x.a.x < y.a.x || x.a.x = y.a.x) && (x.a.y < y.a.y)) then 1 
                              else (-1)
  end;;

  module Sat = Make(Variables);;


(* Fonction auxilliaire qui retourne une liste de couples de lignes avec leurs rectangles non colorés *)
(*_________________________________________________________________________________________________________________________________*)

let rec lines_rectangle_empty (linerectlistlist : (line * (rectangle list)) list): (line * (rectangle list)) list = 
  let rec lines_rectangle_empty_aux (linerectlist : line * (rectangle list)) (ret : line * (rectangle list)): line * (rectangle list) =
       match (linerectlist,ret) with
     | ( _ , [] ) , _ -> ret
     | (line, t::q),(l, rectangles_empty) -> if t.rc = None then lines_rectangle_empty_aux (line,q) (l,t::rectangles_empty)
                                       else lines_rectangle_empty_aux (line,q) (l,rectangles_empty)
  in
  match linerectlistlist with
    [] -> []
  | t::q -> lines_rectangle_empty_aux t ((fst t), []) :: lines_rectangle_empty q
;;

(*Création d'un nouveau type personnale bspbool qui sera un arbre de rectangles ayant des feuilles à sa base.
  Se type servivra à construire tous les coloriages possibles pour n rectangles vides données pour créer une liste de possibilités
  de taille 2^n.
*)

type bspbool = None | Node of rectangle * bspbool * bspbool;;


(* bool_tree_from_rectangles : La fonction auxilliaire qui créera notre arobrescence à partir d'une liste donnée de rectangles, à chaque niveau i
  tous i-noeuds contiendront le même rectangle à la position i dans la liste
 *)

(*_________________________________________________________________________________________________________________________________*)

let rec bspbool_from_rectangles (rectlist: rectangle list) : bspbool=
  match rectlist with
    | [] -> None
    | t::q -> Node (t, bspbool_from_rectangles q, bspbool_from_rectangles q)
;;
(*_________________________________________________________________________________________________________________________________*)


(* coloring_bool_from_tree: est une fonction qui, étant donné un arbre de type bool tree, crée tous les coloriage possibles sous
  forme de liste de liste de couple booleen (true, rectangle) pour un rectangle de couleur bleue, et false pour un rectangle de couleur
  rouge. le parcours utilise un accumulateur qui attribue au noeud courant true (resp. false) quand il l'ajoute à l'accumulateur en allant 
  au fils gauche (resp. droit)
*)
(*_________________________________________________________________________________________________________________________________*)

let rec coloring_from_bspbool (bool_tree : bspbool) : (bool*rectangle) list list =
  let rec coloring_aux (tree_bool : bspbool) (temp : (bool*rectangle) list) =
    match tree_bool with
      None -> [temp]
    | Node (rectangle, left, right) -> (coloring_aux left ((true, rectangle)::temp)) @ (coloring_aux left ((false, rectangle)::temp))
  in
  coloring_aux bool_tree []
;;

(*_________________________________________________________________________________________________________________________________*)

(* line_possible_coloring  utilisera les fonctions et structures précédentes pour générer une liste de couple lignes accompagnés
  des coloriages possibles pour leurs réctangles non coloriés.
*)
(*_________________________________________________________________________________________________________________________________*)

let line_possible_coloring (linesrectlist : (line * (rectangle list)) list) : (line * (bool * rectangle) list list) list = 
  let emptylinesrectlist=lines_rectangle_empty linesrectlist in
  let rec line_possible_coloring linesrectlist ret =
    match linesrectlist with
      [] -> ret
     |(line, rectlist)::q -> line_possible_coloring q ((line, coloring_from_bspbool (bspbool_from_rectangles rectlist))::ret)
  in
  List.rev (line_possible_coloring emptylinesrectlist [])
;;
(*_________________________________________________________________________________________________________________________________*)


(* lines_colored_rectangles : fonction auxilliaire qui retourne le nombre de rectangles déjà colorés en bleu ou en rouge d'une ligne donnée
*)
(*_________________________________________________________________________________________________________________________________*)

let lines_colored_rectangles (linerectlist : line * (rectangle list)) : (int*int) = 
  let rec lines_colored_rectangles_aux line (rectlist : rectangle list) nbb nbr = 
    match rectlist with
      [] -> (nbb,nbr)
     |t::q -> (match t.rc with 
                None -> lines_colored_rectangles_aux line q nbb nbr
               |Some c -> if c=red then lines_colored_rectangles_aux line q nbb (nbr+1)
                          else if c=blue then lines_colored_rectangles_aux line q (nbb+1) nbr
                          else lines_colored_rectangles_aux line q nbb nbr)
  in
  lines_colored_rectangles_aux (fst linerectlist) (snd linerectlist) 0 0
;;
(*_________________________________________________________________________________________________________________________________*)


(* test_line : fonction auxilliaire qui teste si un coloriage sous forme booléenne donné couplé aux rectangles déjà coloriés est valide *)

let rec test_line (l : line) (p : (bool * rectangle) list) (count : (int * int)) : bool =
  match p,count with 
    [],(b,r) -> (match l.lc with 
                 None -> failwith "test_line : None value"
                |Some c -> if ((c=red && r>b) || (c=blue && b > r) || (c=magenta && r=b) 
                                              || (c=black)) then true else false)
   |(boolval,rect)::q,(b,r) -> if boolval then test_line l q (b+1, r) else test_line l q (b, r+1)
;;
(*_________________________________________________________________________________________________________________________________*)


(* possible_colorings fonction auxilliaire qui étant donné une ligne avec tous les coloriages de ses rectangles vides possible, et la même ligne
avec la liste de tous ses rectangles, renvoie les coloriages possibles valides des rectangles vides pour cette ligne *)

let rec possible_colorings (line_all : (line * (bool * rectangle) list list)) (linerectlist : (line * (rectangle list))) : (bool * rectangle) list list =
  let color_count = lines_colored_rectangles linerectlist in
  match line_all with
   (_, []) -> []
  |(line, t::q) -> if (test_line line t color_count) then t::possible_colorings (line, q) linerectlist else possible_colorings (line, q) linerectlist
;;
(*_________________________________________________________________________________________________________________________________*)


(* remove_doublon : fonction auxilliaire qui comme son nom l'indique, enlève les doublons d'une liste*)

let rec remove_doublon (l : 'a list) = 
  match l with
    [] -> []
   |t::q -> if (List.mem t q) then remove_doublon q else t::(remove_doublon q)
;;
(*_________________________________________________________________________________________________________________________________*)


(* get_all_possible_colorings : fonction utilisant les fonctions précédentes pour donner générer une liste de listes de couples bool * rectangles,
qui servira de forme normale, que l'on donnera à la fonction Sat.Solve pour nous extraire un coloriage possible satisfaisant les conditions données
Les lignes noires n'ayant pas d'insidence sur le coloriage, elles seront ignorées par cette fonction pour économiser sur l'espace utilisé *)

let get_all_possible_colorings (lrlist : (line * (rectangle list)) list) : (bool * rectangle) list list = 
  let all=List.filter (fun x -> match x with (line, _) -> 
                                                    (match line.lc with 
                                                    None -> failwith "get_all_possible_colorings : None value" 
                                                    | Some c -> c <> black)) (line_possible_coloring lrlist) 
  
in

  let rec all_aux (lines_all : (line * (bool * rectangle) list list) list) (lrlist : (line * (rectangle list)) list) (returnlist : (bool * rectangle) list list) =
    
    match lines_all,lrlist with
  	  [],[] -> returnlist 
         |lineall::q, linerect::ql -> all_aux q ql (returnlist @ possible_colorings lineall linerect)
         |_,_ -> failwith "all_aux : error 1"
                                     
  in
  remove_doublon (List.filter (fun x -> x <> []) (all_aux all (List.filter (fun x -> match x with (line, _) -> (match line.lc with None -> failwith "all_aux : error 2" | Some c -> c <> black)) lrlist) []))
;;
(*_________________________________________________________________________________________________________________________________*)



(*========================================== END COLORIAGE ============================================================================*)



(*========================================== START MONDRIAN LAUNCH ============================================================================*)


let key_handler event = 
  match event.key with
                          |  'q' -> (close_graph (); 'q')
                          | '!' -> (close_graph (); '!')
                          | _ -> 'n'
;;

(*_________________________________________________________________________________________________________________________________*)

let mouse_handler event : char = 
  if (event.mouse_x >= 150 && event.mouse_x <=350) then (if (event.mouse_y>=80 && event.mouse_y<=380) then (close_graph (); 'f')
                                                           else 'n')
    else ( if(event.mouse_x >= 355 && event.mouse_x <=555) then (if (event.mouse_y>=80 && event.mouse_y<=230) 
                                                                        then (close_graph (); 'd')
                                                                    else if (event.mouse_y>=235 && event.mouse_y<380) 
                                                                          then (close_graph (); 'm') else 'n')

              else 'n'
  )   
;;

(*_________________________________________________________________________________________________________________________________*)

let fenetre_principale ()=
  
  open_graph (" 700x600");
  set_window_title "Mondrian";
  
  set_color (rgb 181 107 85);  (* rectangle orange recouvrant toute la fenêtre *)
  fill_rect 0 0 700 600; 
  set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";

  set_color (rgb 114 143 186);
  fill_rect 150 80 200 300;                 (*pour les rectangles facile puis moyen puis difficile*)
  fill_rect 355 80 200 150;
  fill_rect 355 235 200 145;
  set_color white;
  
  moveto 220 550;
  draw_string "WELCOME TO MONDRIAN !";
  moveto 100 450;
 
  draw_string "LEVEL  :  ";
  moveto 220 220;
  draw_string "EASY";
  moveto 420 290;
  draw_string "MEDIUM";
  moveto 426 140;
  draw_string "HARD";
 
  set_color black;
  moveto 570 20;
  draw_string " ! : HELP";
  
  let rec wait_for_command ()=
    
    let event = wait_next_event [Button_down; Key_pressed] in
    
    if event.keypressed then
    let key = key_handler event
     in if key!= 'n' then key else wait_for_command ();

    else
      let click = mouse_handler event 
      in if click != 'n' then click else wait_for_command ();
  
      in wait_for_command ();
;;

(*_________________________________________________________________________________________________________________________________*)


(*permet de colorier un rectangle*)

let rec play_rect (click : Graphics.status) (tree : bsp) (ll: line list) (rectlist: rectangle list) =
  match rectlist with
    [] -> failwith "play_rect : configuration error"
  | t::q ->
     if (click.mouse_x >= t.a.x && click.mouse_x<t.a.x+t.w && click.mouse_y >= t.a.y && click.mouse_y<t.a.y+t.h) then
          (match t.rc with
             None -> (
               set_color (rgb 127 1 1);
               fill_rect t.a.x t.a.y t.w t.h;
               draw_current_bsp tree;
               t.rc <- Some red;
           )
           | Some c -> (
             if c=red then (
                set_color (rgb 1 1 127);
                fill_rect t.a.x t.a.y t.w t.h;               
                draw_current_bsp tree;
                t.rc <- Some blue;
             )
             else if c=blue then (
                set_color white;
                fill_rect t.a.x t.a.y t.w t.h;
                draw_current_bsp tree;
                t.rc <- None;
             )
             else
               failwith "play_rect : color indifined"
           )
          )
     else
       play_rect click tree ll q
;;

(*_________________________________________________________________________________________________________________________________*)


let affichage_fin_jeu () =
  clear_graph ();
  set_color black;
  moveto ((size_x ()/2)-((size_x ()/8))) (size_y ()/2);
  draw_string " You win !";
  moveto  ((size_x ()/2)-((size_x ()/3))) 50;
  draw_string " Q  : Menu";
  
  let rec wait_for_command_exit ()=
    let eve = wait_next_event [Button_down; Key_pressed] in
    if eve.keypressed then
      match eve.key with
        'q' -> close_graph ()
      | _ -> wait_for_command_exit();
    else
      wait_for_command_exit ()
  in
  wait_for_command_exit ()
;;

(*_________________________________________________________________________________________________________________________________*)


let check_ext size lrlist rects lines= 
  
  open_graph size;
  moveto ((size_x ()/2)-((size_x ()/3))) (size_y ()/2);
  set_color black;
  (
      if check_possible_extension lrlist then 
      (
          let p = (get_all_possible_colorings lrlist) in
          let s = Sat.solve p in
          (
              match s with 
            | None -> draw_string "No possible extensions"
            | Some l ->
            ( if l=[] then draw_string "No possible extensions"
                     else draw_rectlist rects;
                          set_line_width 2;
                          draw_lines lines;
             )
           )
      )
      else
        draw_string "No possible extensions"
  );
  (*moveto ((size_x ()/2)-((size_x ()/4))) 50;*)
  moveto 0 0;
  draw_string "Click on 'R' to resume";
  let rec loop () =
    let eve = wait_next_event [Button_down; Key_pressed] in
    if eve.keypressed then
      (match eve.key with
        'r' -> ()
      | _ -> loop ())
    else
      (loop ())
  in
  loop ()
;;


(*Fonction game, qui s'occupe d'afficher le jeu selon la difficulté choisie et controle les interactions
avec le joueur*)

let launch_game (size : string) (depth : int) =
  
  open_graph size;
  let tree=random_bsp depth in
  let rectangles=rectangles_from_bsp_none tree in
  let lines = lines_from_bsp tree in
  let lrlist = get_rectangles lines rectangles in 
  
  draw_current_bsp tree;
  
  let rec wait_for_play ()=
    
  let eve = wait_next_event [Button_down; Key_pressed] in
    
  if eve.keypressed then
    
        match eve.key with
      | 'q' -> (close_graph ())
      | 'c' -> (close_graph ();
                open_graph size;
                check_ext size lrlist (rectangles_from_bsp tree) (lines_from_bsp tree);
                open_graph size;
                draw_rectlist rectangles;
                draw_current_bsp tree;
                wait_for_play ()
               )
      | _ -> wait_for_play ()

    else
      ( 
      play_rect eve tree lines rectangles;
        
        if check_current lrlist then
          (affichage_fin_jeu ())
        else
       wait_for_play ()
     )
  in
  wait_for_play ()
;;



(* Fonction help; qui s'occupe d'afficher au joueur toutes les instructions nécessaires à l'utilisation du jeu
*)

let affichage_aide () =
 
  open_graph " 460x350";
  set_window_title "Help";
  set_color (rgb 237 235 235);
  fill_rect 0 0 660 350;
  set_color black;
  moveto 30 280;
  draw_string "In the principal menu :";
  moveto 30 265;
  draw_string " 1 - Choose a level.";
  moveto 30 250;
  draw_string " 2 - Q : Quit the game.";
  moveto 30 200;
  draw_string "In game :";
  moveto 30 185;
  draw_string " - CLICK on the rectangles to change their colors.";
  moveto 30 170;
  draw_string " - C : Check if the current configurations has a valid extenion";
  moveto 30 155;
  draw_string " - Q : Quit the current game and go back the menu.";
  moveto 30 140;
  draw_string " - R : Replay";
  moveto 30 125;
  draw_string " - Q : Quit the game completely.";
  
  let rec loop ()=
    let eve = wait_next_event [Button_down; Key_pressed] in
    if eve.keypressed then
        match eve.key with
      | 'q' -> close_graph ()
      | _ -> loop ()
    else
      loop ()
  in
  loop ()
;;




let rec main () =
  let choice = fenetre_principale () in
  (
        match choice with
      | 'q' -> ()
      | '!' -> (affichage_aide (); main ())
      | 'f' -> (launch_game " 400x400" 3; main ())
      | 'm' -> (launch_game " 600x600" 6; main ())
      | 'd' -> (launch_game " 710x710" 7; main ())
      | _ -> ()
  )
;;


(*Enfin pour finir, le main est lancé pour débuter le jeu*)

main()
;;
		 
  
