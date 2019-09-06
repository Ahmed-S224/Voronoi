#load "graphics.cma";;
#use "sat_solver.ml";;

open Graphics;;

(*declaration des types seed et voronoi*)
type seed = {c : color option; x : int; y : int};;
type voronoi = {dim : int * int; seeds : seed array};;


(* module de voronoi qui a un type t qui prend une region et sa couleur et une methode compare qui compare les couleurs de deux regions differn*)
module Voronoi = struct
  type t = int * int (* la position de la region* la couleur*)
  let compare (p1,c1) (p2,c2) =
    if (p1<>p2)then -1
    else if (c1 <> c2) then -1
    else 0
      
end;;


module Solver = Make(Voronoi);;


(* cest une methode pour calculer l' existence et l'unicite*)
let existence_voronoi v form =
  for i = 0 to (Array.length (v.seeds)-1) do
    let liste = ref [] in
    for j = 1 to 4 do
      liste := (true,(j,i)) :: !liste;
    done;
    form := !liste :: !form;
  done;
  for k = 0 to (Array.length (v.seeds)-1) do
    for l = 1 to 4 do
      for m = 1 to 4 do
	if(l != m) then
	  form := [(true,(l,k));(false,(l,k))] :: !form;
      done;
    done;
  done
;;





(**)
let couleur v =
  if(v.c = Some red) then
    1
  else if(v.c = Some blue) then
    2
  else if(v.c = Some yellow) then
    3
  else if(v.c = Some green) then
    4
  else
    0
;;



(*let contraintes tab =
  let liste = ref [] in
  existence_voronoi v3 liste;
  matrice_voisin v3 tab;
  ;;


  let res = Solver.solve !liste;;*)




