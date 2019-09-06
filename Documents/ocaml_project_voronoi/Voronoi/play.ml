#load "graphics.cma";;
#use "programme.ml";;
#use "examples.ml";;

open Graphics;;
 
(*Envoi une couleur si elle existe ou blanc sinon*)
let get_color some_option = match some_option with
  |None-> white
  |Some c -> c
;;

(*Envoi les coordonnees x et y d'un point*)
let get_element c i = match c with
  |(x,y) -> if i = 0 then x else y
;;

(*Envoi la valeur x de la dimension du voronoi*)
let dim1 v = get_element(v.dim) 0;;

(*Envoi la valeur y de la dimension du voronoi*)
let dim2 v = get_element(v.dim) 1;;

(*Envoi la distance euclidienne entre deux points c1 et c2 *)
let distance c1 c2 =
  let x1 = float_of_int(get_element c1 0) in
  let y1 = float_of_int(get_element c1 1) in
  let x2 = float_of_int(get_element c2 0) in
  let y2 = float_of_int(get_element c2 1) in
  int_of_float(sqrt((x1 -. x2) ** 2.0 +. (y1 -. y2) ** 2.0))
;;

(*Valeur absolu d'un nombre *)
let val_abs n =
  if n < 0 then (-n)
  else n
;;

(*Envoi la distance taxicab entre deux points c1 et c2*)
let distance_taxicab c1 c2 =
  let x1 = get_element c1 0 in
  let y1 = get_element c1 1 in
  let x2 = get_element c2 0 in
  let y2 = get_element c2 1 in
  let res = val_abs(x1 - x2) + val_abs(y1 - y2) in
  val_abs res
;;

(*Initialise les couleurs des regions du voronoi. Elle prend en argument un voronoi v*)
let voronoi_init_colorier v =
  let l = ref [] in
  (*parcourts les differentes regions du voronoi et y ajoute les regions colorié dans une liste de voronoi *)
  for i = 0 to (Array.length(v.seeds)-1) do
    if(v.seeds.(i).c<>None )then
      l := !l@[i];
  done;
  !l
;;

(*Calcul la matrice des regions adjacentes. Elle prend en argument un voronoi v et une matrice m.*)
let matrice_voisin v m = 
  let n = Array.length(v.seeds) in  
  let b = Array.make_matrix n n false in
  (* On parcourt toutes les regions et on compare chaque regions voisine selon la region actuelle. On renvoi sa valeur de verite*)
  for i = 0 to ((dim1 v)-1) do
    for j = 0 to ((dim2 v)-1) do 
      if(m.(i).(j) <> m.(i-1).(j))then
        b.(m.(i).(j)).(m.(i-1).(j)) <- true;
      if(m.(i).(j) <>m.(i+1).(j))then
        b.(m.(i).(j)).(m.(i+1).(j)) <- true;
      if(m.(i).(j) <>m.(i).(j-1))then
        b.(m.(i).(j)).(m.(i).(j-1)) <- true;
      if(m.(i).(j) <>m.(i).(j+1))then
        b.(m.(i).(j)).(m.(i).(j+1)) <- true;
    done;
  done;
  b
;;

(*Creation des regions du voronoi. Elle prend un argument une distance et un voronoi v*)
let regions_voronoi distance v =
  let m = Array.make_matrix (dim1 v) (dim2 v) 0 in
  for i = 0 to (dim1 v) - 1 do
    let res1 = ref 0 in
    for j = 0 to (dim2 v) - 1 do
      let res = ref (distance (v.seeds.(0).x, v.seeds.(0).y) (i,j))  in
      for k = 1 to (Array.length v.seeds) - 1 do
	res1 := distance (v.seeds.(k).x, v.seeds.(k).y) (i,j);
	if(!res1 < !res) then
	  begin
            res := !res1;
            m.(i).(j) <- k;
          end
      done;
    done;
  done;
  m
;;

(*Dessine le voronoi. Elle prend en argument une matrice m et un voronoi v*)
let draw_voronoi m v =
  (*parcourt la matrice et dessine les frontieres des voronoi tout en initialisant les voronoi precolorié*)
  for i = 0 to (Array.length(m)-1) do
    for j = 0 to (Array.length(m)-1) do
      if(1 <=i && m.(i).(j) <> m.(i-1).(j) || (i+1) <= (Array.length(m)-1) && m.(i).(j) <> m.(i+1).(j) ||
	    1 <= j && m.(i).(j) <> m.(i).(j-1) || (j+1) <= (Array.length(m)-1) && m.(i).(j) <> m.(i).(j+1)) then
	set_color black
      else
	set_color (get_color v.seeds.(m.(i).(j)).c);
      plot i j;
    done;
  done;
  set_color red;
  (* 4 carrés de 4 couleurs differentes*)
  fill_rect ((dim1 v)+100) 450 30 30;
  set_color blue;
  fill_rect ((dim1 v)+150) 450 30 30;
  set_color green;
  fill_rect ((dim1 v)+100) 500 30 30;
  set_color yellow;
  fill_rect ((dim1 v)+150) 500 30 30;
  set_color black;
  fill_rect (dim1 v) 0 10 (dim1 v)
;;

(*Dessine les regions du voronoi. Elle prend en argument une matrice m et un entier correspondant au numero de la region du voronoi*)
let dessine_region_i m x =
  (*Parcourt la matrice, test si la region choisi peut etre colorié initialement et on rempli la region seulement si elle n'est pas tracé*)
  for i = 0 to (Array.length(m)-1) do
    for j = 0 to (Array.length(m)-1) do
      if(m.(i).(j) = x && not( 1 <=i && m.(i).(j)<> m.(i-1).(j)|| (i+1) <=(Array.length(m)-1)&& m.(i).(j)<>m.(i+1).(j)||
				 1 <= j && m.(i).(j)<> m.(i).(j-1)|| (j+1) <= (Array.length(m)-1) && m.(i).(j)<>m.(i).(j+1))) then
        plot i j;
      
    done;
  done
;;



(*Selection des couleurs selon leur coordonnees. Elle prend en argument une couleur c, un voronoi v et des coordonnées x,y.*)
let choix_couleur c v x y =
  (*test les coordonnees x,y données en arguments qui sont dans la case couleur *)
  if ((x >= ((dim1 v)+100) && x <= ((dim1 v)+130)) && (y >= 450 && y <= 480)) then
    c := Some red; 
  if ((x >= ((dim1 v)+100) && x <= ((dim1 v)+130)) && (y >= 500 && y <= 530)) then
    c := Some green;
  if ((x >= ((dim1 v)+150) && x <= ((dim1 v)+180)) && (y >= 450 && y <= 480)) then
    c := Some blue;
  if ((x >= ((dim1 v)+150) && x <= ((dim1 v)+180)) && (y >= 500 && y <= 530)) then
    c := Some yellow;
;;

(*Colorie la region du voronoi ou l'on a cliqué. Elle prend en argument une matrice m, un voronoi v, une couleur c, des coordonnées x,y et une liste de voronoi initialement colorié*)
let coloriage m v c x y l=
  (*test si on est dans la region des voronoi afin de leur attribuer une couleur *)
  if ((x >= 0) && (x < dim1 v) && (y >= 0) &&(y < dim2 v) && !c = Some red) then
    v.seeds.(m.(x).(y)) <- {c = Some red; x=v.seeds.(m.(x).(y)).x; y=v.seeds.(m.(x).(y)).y};
  if ((x >= 0) && (x < dim1 v) && (y >= 0) &&(y < dim2 v) && !c = Some green ) then
    v.seeds.(m.(x).(y)) <- {c = Some green; x=v.seeds.(m.(x).(y)).x; y=v.seeds.(m.(x).(y)).y};
  if ((x >= 0) && (x < dim1 v) && (y >= 0) &&(y < dim2 v) && !c = Some blue) then
    v.seeds.(m.(x).(y)) <- {c = Some blue; x=v.seeds.(m.(x).(y)).x; y=v.seeds.(m.(x).(y)).y};
  if ((x >= 0) && (x < dim1 v) && (y >= 0) &&(y < dim2 v) && !c = Some yellow) then
    v.seeds.(m.(x).(y)) <- {c = Some yellow; x=v.seeds.(m.(x).(y)).x; y=v.seeds.(m.(x).(y)).y}; 
  set_color (get_color (!c));
  if((x >= 0) && (x < dim1 v) && (y >= 0) &&(y < dim2 v) && not(List.mem m.(x).(y) l))then
    dessine_region_i m (m.(x).(y));
;;

(*boucle permettant de faire tourner le jeu. Elle prend en argument une matrice m, un voronoi v, une couleur c et une liste de voronoi initialement colorié *)
let rec boucle m v c l =
  let e = wait_next_event[Button_down] in
  let x = e.mouse_x in
  let y = e.mouse_y in
  let _ = choix_couleur c v x y in
  let _ = coloriage m v c x y l in
  boucle m v c l 
;;


(*Methode de jeu*)
let jeu v =
  let _ =  open_graph " 1100x900" in
  let m = regions_voronoi distance_taxicab v in
  let _ = draw_voronoi m v in
  let l = voronoi_init_colorier v in
  let c = ref None in
  boucle m v c l
;;


(*some examples of voronoi*)


(*pour generer aléatoirement le chois des voronois*)

let listVoronoi = [v1;v2;v3;v4;v5];;
let randomVoroine listVoronoi = 
  Random.init 1234;
  List.nth listVoronoi (Random.int (List.length listVoronoi));;
let voronoi_list = [v1;v2;v3;v4;v5];;

let choix () = 
  Random.self_init ();
  let l = List.length voronoi_list in
 List.nth voronoi_list (Random.int l);;

let j = choix();;

jeu j;;

