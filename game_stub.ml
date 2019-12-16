
(* 
 **************************************************
 * Game data types
 * ************************************************ 
*)

type coordinate = int*int
(* type cell = coordinate*int *)

type level = { points: int;
               cells: int list (* cells 4x4, 16 cells in list *)
               }

type direction = North | East | South | West

(* 
 **************************************************
 * 
 * ************************************************ 
*)

exception Not_implemented

(* PART 1 *)

(* ************************ *)
(* Exercise: move *)
(* ************************ *)
(* given a list that looks something like [4;4;2;2] *)
(*                                 return [8;4;0;0] *)
let move lst =
  raise Not_implemented

(* ************************ *)
(* Exercise: c_s *)
(* ************************ *)
let c_s lst =
  raise Not_implemented


(* ************************ *)
(* Exercise: step *)
(* ************************ *)
(*
|0 |1 |2 |3 |
|4 |5 |6 |7 |
|8 |9 |10|11|
|12|13|14|15|
*)
let step {points = p; cells = lst} dir = 
  raise Not_implemented


(* PART 2 *)

(* ************************ *)
(* Exercise: insert_rand *)
(* ************************ *)
(* replace one of the 0's in the list with a 2 or a 4 *)
(* n is result of rand_pick0 *)
let rec insert_rand lst n =
  raise Not_implemented


(* PART 3 *)

(* ************************ *)
(* Exercise: end_state *)
(* ************************ *)
(* technically no winning/losing state *)
(* try to go in each direction. if all attempts are the same as original, then there is no move left *)
let end_state {points=p; cells = cs} =
  raise Not_implemented

(* ************************ *)
(* Exercise: match_grid *)
(* ************************ *)
(* match the 4x4 coordinates to the list of nums *)
let match_grid lst coor =
  raise Not_implemented

exception End

let process_key lev  = function
  | 'w' -> step lev  North
  | 'a' -> step lev  West
  | 's' -> step lev  South
  | 'd' -> step lev  East
  | _ -> lev

let rec add_row w h =
  if w = 0
  then []
  else (w, h):: add_row (w-1) h

let rec mk_col w h = 
  if h = (-1)
  then []
  else (w, h):: mk_col w (h-1)

let rec add_col w h =
  if h = 0
  then []
  else (add_row w h)@ (add_col w (h-1))

let rec gen_wall w h =
  let r1 = add_row w 0 in
  let r2 = add_row w (h+1) in
  let c1 = mk_col 0 (h+1) in
  let c2 = mk_col (w+1) (h+1) in
  r1@r2@c1@c2

let erase_state {points=p; cells = cs}  =
  Grid.erase_coordinates (add_col 4 4) 

let draw_state {points=p; cells = cs}  =
  Graphics.set_color 0xFF8F0E;
  Graphics.fill_rect 245 520 100 12;
  Grid.draw_string_at (250,520) (String.concat "" ["SCORE: "; string_of_int p]);
  List.iter (fun c -> Grid.draw_string_in_box_at c (myits (match_grid cs c))) (add_col 4 4)

let rec main_loop (prelvl:level) (level:level):level =
  if end_state level
  then begin
        draw_state level;
        (Grid.draw_string_at (150,520) "GAME OVER!");
        let _ = Graphics.wait_next_event [Graphics.Key_pressed]
        in     level
      end
  else 
      let nlvl = (if prelvl = level
            then level
            else {points = level.points; cells = insert_rand level.cells (rand_pick0 (count0 level.cells))}) in
        begin
          draw_state nlvl;
          let s = Graphics.wait_next_event [Graphics.Key_pressed]
          in let key = s.Graphics.key
          in if key = 'q'
              then nlvl
              else let _= erase_state nlvl in
                   main_loop nlvl (process_key nlvl key)
        end


let draw_level ()  = 
  List.iter (fun c -> Grid.color_box c Graphics.black) (gen_wall 4 4)

let lst1 = 
  let l = [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0] in 
  insert_rand l (rand_pick0 (count0 l))

let dummy_lvl =
  { points= 0;
    cells = [420]}

let level_1 =
  { points=0;
    cells = lst1}


let main () = 
  Graphics.open_graph " 600x600";
  Grid.draw_string_at (100,550) "wsad for movement; m to mark/unmark; n to reveal block; q to quit";
  Grid.draw_grid ();

  draw_level ();   
  ignore @@ (main_loop dummy_lvl level_1);
  Graphics.close_graph ()