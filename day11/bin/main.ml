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
}

let _tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let _havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"
let vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb"
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

let rotate_x (p : point) (a : float) : point =
  {
    x = p.x ;
    y = (p.y *. (cos (a))) -. (p.z *. sin(a)) ;
    z = (p.y *. sin(a)) +. (p.z *. cos(a)) ;
  }

let rotate_y (p : point) (a : float) : point =
  {
    x = (p.x *. cos(a)) -. (p.z *. sin(a)) ;
    y = p.y ;
    z = (p.x *. sin(a)) +. (p.z *. cos(a)) ;
  }
    
let rotate_z (p : point) (a : float) : point =
  {
    x = (p.x *. cos(a)) -. (p.y *. sin(a)) ;
    y = (p.x *. sin(a)) +. (p.y *. cos(a)) ;
    z = p.z ;
  }
  
let point_cmp (a : point) (b : point) : int =
  if a.z == b.z then 0
  else if a.z < b.z then 1 else -1

(* ----- *)

let boot (screen : screen) =
  set_palette_color screen.palette ((List.length screen.palette) - 1);
  fill_rect 0 0 (screen.width - 1) (screen.height - 1)

let tick (t : int) (screen : screen) =
  (* set_palette_color screen.palette (8 + ((t  / 20) mod 8)); *)
  set_color 0;
  fill_rect 0 0 (screen.width - 1) (screen.height - 1);

  set_palette_color screen.palette 1;
  let ft = float_of_int t in
  let o = sin (ft /. 10.) in
  let offset = if o < 0. then ((0. -. o) *. 4.0) else 0. in
  let points : point array = Array.make (17 * 17 * 17) {x=0. ; y=0. ; z=0. } in
  for z = 0 to 16 do
    for y = 0 to 16 do 
      for x = 0 to 16 do 
        let p : point = {
          x = float_of_int ((x - 8)) *. (4. +. offset) ; 
          y = float_of_int ((y - 8)) *. (4. +. offset) ; 
          z = float_of_int ((z - 8)) *. (4. +. offset) ;
        } in
        points.(x + (y * 17) + (z * 17 * 17)) <- rotate_z(
          rotate_x(
            rotate_y p (0.02 *. ft)
          ) (0.01 *. ft)
        ) (0.005 *. ft)
      done
    done
  done;
  Array.sort point_cmp points;
  let m = 2000. +. cos(ft /. 30.) *. 600. 
  and z = 10. +. sin(ft /. 1000.) *. 5. 
  and d = 10. +. cos(ft /. 1000.) *. 5. in
  Array.iter (fun e ->
    let fcol = (sin(sin((e.x +. ft) /. z)) +. sin(sin((e.y +. ft) /. d)) +. sin(sin((e.z +. ft) /. z))) *. 8. in
    let col = ((int_of_float fcol) mod (List.length screen.palette)) in
    set_palette_color screen.palette (if col < 0 then (col + (List.length screen.palette)) else col);
    fill_circle 
      ((screen.width / 2) + int_of_float(m *. e.x /. (e.z +. 400.))) 
      ((screen.height / 2) + int_of_float(m *. e.y /. (e.z +. 400.))) 
      ((int_of_float m) / 200)
  ) points

let inner_tick (t : int) (screen : screen) =
  tick t screen;
  synchronize ()

let () =
  let screen : screen = {
    width = 640 ;
    height = 480 ;
    palette = load_tic80_palette vapour_palette ;
  } in

  open_graph (Printf.sprintf " %dx%d" screen.width screen.height);
  set_window_title "TCC Day 11";
  auto_synchronize false;
  set_font "-*-*-bold-r-*-*-32-*-*-*-m-*-iso8859-1";

  boot screen;

  let t = ref 0 in
  while true do
    Unix.sleepf 0.01;
    let status = wait_next_event[ Poll; Key_pressed ] in
    if status.keypressed && status.key == ' ' then
      raise Exit
    else
      inner_tick !t screen;
      t := !t + 1;
  done
