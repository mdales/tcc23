open Graphics

type screen = {
  width   : int ;
  height  : int ;
  palette : color list;
}

type point = {
  x : float ;
  y : float ;
  z : float ;
  c : int ;
}

let _tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"
let _vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb"
(* let vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb7400b86930c35e60ce5390d94ea8de48bfe3" *)

exception String_not_multiple_of_chunk_size

let string_to_chunks (x : string) (size : int) : string list =
  let rec loop sofar remainder =
    let length_left = String.length remainder in
    if length_left >= size then
      loop ((String.sub remainder 0 size) :: sofar) (String.sub remainder size (length_left - size))
    else if length_left == 0 then
      sofar
    else 
      raise String_not_multiple_of_chunk_size
  in
  List.rev (loop [] x)

let chunks_to_colors (raw : string list) : int list =
  List.map (fun (colorstr : string): int -> int_of_string ("0x" ^ colorstr)) raw

let load_tic80_palette (raw : string) =
  let parts = String.split_on_char ':' raw in
  let strchunks = string_to_chunks (List.nth parts 1) 6 in
  chunks_to_colors strchunks

let set_palette_color (palette : color list) (index : int) =
  let colour = List.nth palette index in
  set_color colour

(* ----- *)

(* let rotate_x (p : point) (a : float) : point =
  {
    x = p.x ;
    y = (p.y *. (cos (a))) -. (p.z *. sin(a)) ;
    z = (p.y *. sin(a)) +. (p.z *. cos(a)) ;
  }*)

let rotate_y (p : point) (a : float) : point =
  {
    x = (p.x *. cos(a)) -. (p.z *. sin(a)) ;
    y = p.y ;
    z = (p.x *. sin(a)) +. (p.z *. cos(a)) ;
    c = p.c ;
  }
    
(*let rotate_z (p : point) (a : float) : point =
  {
    x = (p.x *. cos(a)) -. (p.y *. sin(a)) ;
    y = (p.x *. sin(a)) +. (p.y *. cos(a)) ;
    z = p.z ;
  } *)
  
let point_cmp (a : point) (b : point) : int =
  if a.z == b.z then 0
  else if a.z < b.z then 1 else -1

(* ----- *)

let boot (screen : screen) =
  set_palette_color screen.palette ((List.length screen.palette) - 1);
  fill_rect 0 0 (screen.width - 1) (screen.height - 1)

let rec sum x = 
  if (x == 0) then 0 else x + sum (x - 1)

let tick (t : int) (screen : screen) =
  (* set_palette_color screen.palette (8 + ((t  / 20) mod 8)); *)
  set_color 0;
  fill_rect 0 0 (screen.width - 1) (screen.height - 1);

  let ft = float_of_int t in
  let tiers = 24 in
  let points : point array = Array.make (sum tiers) {x=10000. ; y=110. ; z=1000. ; c=15 } in
  for i = 0 to  (tiers - 1) do
    for j = 0 to i do 
      let fi = float_of_int i and fj = float_of_int j in
      let p : point = {
        x = fi *. sin(((fj +. 1.) /. (fi +. 1.)) *. 2. *. Float.pi) *. 10. ;
        y = 350. -. ((fi +. 1.) *. 20.) ; 
        z = fi *. cos(((fj +. 1.0) /. (fi +. 1.)) *. 2. *. Float.pi) *. 10. ;
        c = (tiers - i + (t / 20)) mod (List.length screen.palette) ;
      } in
      points.(j + (sum i)) <- rotate_y p (0.01 *. ft)  
    done
  done;
  Array.sort point_cmp points;
  let m = 200. in
  Array.iter (fun e ->
    set_palette_color screen.palette e.c;
    fill_circle 
      ((screen.width / 2) + int_of_float (m *. e.x /. (e.z +. 400.)))
		  ((screen.height / 2) + int_of_float (m *. e.y /. (e.z +. 400.)))
      ((int_of_float (200. /. ((e.z +. 500.) /. 10.)))) 
  ) points


let inner_tick (t : int) (screen : screen) =
  tick t screen;
  synchronize ()

let () =
  let screen : screen = {
    width = 640 ;
    height = 480 ;
    palette = load_tic80_palette havrekaka_palette ;
  } in

  open_graph (Printf.sprintf " %dx%d" screen.width screen.height);
  set_window_title "TCC Day 12";
  auto_synchronize false;
  set_font "-*-*-bold-r-*-*-32-*-*-*-m-*-iso8859-1";

  boot screen;

  let t = ref 0 in
  while true do
    (* Unix.sleepf 0.01; *)
    let status = wait_next_event[ Poll; Key_pressed ] in
    if status.keypressed && status.key == ' ' then
      raise Exit
    else (
      inner_tick !t screen;
      t := !t + 1
    )
  done
