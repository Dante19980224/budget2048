
type coordinate = int*int
(* type cell = coordinate*int *)

type level = { points: int;
               cells: int list (* cells 4x4, 16 cells in list *)
               }

type direction = North | East | South | West

(* remove all the 0's in a list *)
let rec rm0 lst = 
  match lst with
  | [] -> []
  | x::xs when x = 0 -> rm0 xs
  | x::xs -> x::rm0 xs

(* [2;2] -> [4], [2;4;8] -> [2;4;8], [4;4;4;4] -> [8;8] *)
let rec collapse lst =
  match lst with
  | [] -> []
  | [x] -> [x]
  | x::y::xs when x = y -> (x+y):: collapse xs
  | x::y::xs -> x:: collapse (y::xs)

let rec count_score lst = 
  match lst with
  | [] -> 0
  | [x] -> 0
  | x::y::xs when x = y -> x+y+ count_score xs
  | x::y::xs -> count_score (y::xs)

(* add 0's to the end of a list until the list is of length 4 *)
let rec pack lst =
  if List.length lst = 4
  then lst
  else pack (lst@[0])

(* given a list that looks something like [4;4;2;2] *)
(*                                 return [8;4;0;0] *)
let move lst =
  pack (collapse (rm0 lst))

let c_s lst =
  count_score (rm0 lst)

(* don't feel like typing "List.nth" *)
let nth = List.nth

(* [nth l1 0; nth l2 0; nth l3 0; nth l4 0; 
    nth l1 1; nth l2 1; nth l3 1; nth l4 1; 
    nth l1 2; nth l2 2; nth l3 2; nth l4 2; 
    nth l1 3; nth l2 3; nth l3 3; nth l4 3] *)
let rec spe_combine l1 l2 l3 l4 = 
  if (List.length l1 = List.length l2) && (List.length l1 = List.length l3) && (List.length l1 = List.length l4)
  then 
    match l1, l2, l3, l4 with
    | [],[],[],[] -> []
    | x::xs, y::ys, z::zs, b::bs -> x::y::z::b:: spe_combine xs ys zs bs
    | _ -> []
  else failwith "size of lists must match"

(*
|0 |1 |2 |3 |
|4 |5 |6 |7 |
|8 |9 |10|11|
|12|13|14|15|
*)
(* lst is level.cells *)
let step {points = p; cells = lst} = function
  | North -> let l1 = [nth lst 0; nth lst 4; nth lst 8; nth lst 12] 
            in let l2 = [nth lst 1; nth lst 5; nth lst 9; nth lst 13] 
            in let l3 = [nth lst 2; nth lst 6; nth lst 10; nth lst 14] 
            in let l4 = [nth lst 3; nth lst 7; nth lst 11; nth lst 15] 
          in {points = p+c_s l1+c_s l2+c_s l3+c_s l4 ; cells = spe_combine (move l1) (move l2) (move l3) (move l4)}
  | South -> let l1 = [nth lst 12; nth lst 8; nth lst 4; nth lst 0] 
            in let l2 = [nth lst 13; nth lst 9; nth lst 5; nth lst 1] 
            in let l3 = [nth lst 14; nth lst 10; nth lst 6; nth lst 2] 
            in let l4 = [nth lst 15; nth lst 11; nth lst 7; nth lst 3] 
          in {points = p+c_s l1+c_s l2+c_s l3+c_s l4 ; cells = List.rev (spe_combine (move l4) (move l3) (move l2) (move l1))}
  | West -> let l1 = [nth lst 0; nth lst 1; nth lst 2; nth lst 3] 
            in let l2 = [nth lst 4; nth lst 5; nth lst 6; nth lst 7] 
            in let l3 = [nth lst 8; nth lst 9; nth lst 10; nth lst 11] 
            in let l4 = [nth lst 12; nth lst 13; nth lst 14; nth lst 15] 
          in {points = p+c_s l1+c_s l2+c_s l3+c_s l4 ; cells = (move l1)@(move l2)@(move l3)@(move l4)}
  | East -> let l1 = [nth lst 3; nth lst 2; nth lst 1; nth lst 0] 
            in let l2 = [nth lst 7; nth lst 6; nth lst 5; nth lst 4] 
            in let l3 = [nth lst 11; nth lst 10; nth lst 9; nth lst 8] 
            in let l4 = [nth lst 15; nth lst 14; nth lst 13; nth lst 12] 
          in {points = p+c_s l1+c_s l2+c_s l3+c_s l4 ; cells = List.rev ((move l4)@(move l3)@(move l2)@(move l1))}

(* 1/20 chance for a 4, 19/20 chance for a 2*)
let rand_new_num () =
  begin
    let n = Random.int 20 in
    if n = 0
      then 4
      else 2
  end

(* help determine where to place the new number *)
let rec count0 lst = 
  match lst with
  | [] -> 0
  | x::xs when x = 0 -> 1+count0 xs
  | x::xs -> count0 xs

(* selected the nth 0 to replace *)
let rand_pick0 n =
  if n = 0
  then 0
  else
    begin
      let num = Random.int n in 
      num
    end

(* replace one of the 0's in the list with a 2 or a 4 *)
(* n is result of rand_pick0 *)
let rec insert_rand lst n =
  match lst with 
  | [] -> []
  | x::xs when x = 0 && n = 0 -> (rand_new_num ())::xs
  | x::xs when x = 0 && n != 0 -> x:: insert_rand xs (n-1)
  | x::xs -> x:: insert_rand xs n

exception End

let process_key lev  = function
  | 'w' -> step lev  North
  | 'a' -> step lev  West
  | 's' -> step lev  South
  | 'd' -> step lev  East
  | _ -> lev

(* if moving in a direction does not change the outcome, return false *)
let valid_keys lev k =
  if lev = (process_key lev k)
  then false
  else true

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

(* match the 4x4 coordinates to the list of nums *)
let match_grid lst = function
  | (1,1) -> nth lst 12
  | (2,1) -> nth lst 13
  | (3,1) -> nth lst 14
  | (4,1) -> nth lst 15
  | (1,2) -> nth lst 8
  | (2,2) -> nth lst 9
  | (3,2) -> nth lst 10
  | (4,2) -> nth lst 11
  | (1,3) -> nth lst 4
  | (2,3) -> nth lst 5
  | (3,3) -> nth lst 6
  | (4,3) -> nth lst 7
  | (1,4) -> nth lst 0
  | (2,4) -> nth lst 1
  | (3,4) -> nth lst 2
  | (4,4) -> nth lst 3
  | _ -> 0

let myits = function
  | 0 -> " "
  | n -> string_of_int n

let erase_state {points=p; cells = cs}  =
  Grid.erase_coordinates (add_col 4 4) 

let draw_state {points=p; cells = cs}  =
  Graphics.set_color 0xFF8F0E;
  Graphics.fill_rect 245 520 100 12;
  Grid.draw_string_at (250,520) (String.concat "" ["SCORE: "; string_of_int p]);
  List.iter (fun c -> Grid.draw_string_in_box_at c (myits (match_grid cs c))) (add_col 4 4)

(* technically no losing state *)
(* try to go in each direction. if all attempts are the same as original, then there is no move left *)
let winning_state {points=p; cells = cs} =
  if List.mem 0 cs
  then false
  else 
    begin
      let n = step {points=p; cells=cs} North in
      let e = step {points=p; cells=cs} East in
      let w = step {points=p; cells=cs} West in
      let s = step {points=p; cells=cs} South in
      if cs = n.cells && cs = e.cells && cs = w.cells && cs = s.cells
      then true
      else false
    end

let rec main_loop (prelvl:level) (level:level):level =
  if winning_state level
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