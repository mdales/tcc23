open Graphics

type screen = {
  width           : int ;
  height          : int ;
  scale           : int ;
  palette         : color list ;
}

let tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let _havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"
let _vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb"
(* let _vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb7400b86930c35e60ce5390d94ea8de48bfe3" *)

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

let expanded_row (screen : screen) (row : int array) : color array =
  Array.concat (List.map (fun (vl : int) : color array ->
    let col = List.nth screen.palette vl in
      Array.make screen.scale col
    ) (Array.to_list row))

let buffer_to_image (screen : screen) (buffer : int array array) : image =
  let raw = Array.concat (List.map (fun (row : int array) : color array array ->
    let colrow = expanded_row screen row in
      Array.make screen.scale colrow
  ) (Array.to_list buffer)) in
   make_image raw


(* ----- *)

let distance (x1 : float) (y1 : float) (x2 : float) (y2 : float) : float =
    Float.sqrt((Float.pow (x2 -. x1) 2.) +. (Float.pow (y2 -. y1) 2.))

(* ----- *)

let boot (screen : screen) =
  set_palette_color screen.palette 0;
  fill_rect 0 0 screen.width screen.height

let tick (t : int) (screen : screen) : int array array =
  let ft = (float_of_int t) /. 50. in
  let x1 = 120. +. ((sin ft) *. 30.) 
  and y1 = 60.
  and x2 = 120. +. ((cos ft) *. 30.)
  and y2 = 76. in
  Array.init screen.height (fun (y : int) : int array ->
    Array.init screen.width (fun (x : int) : int ->
      let fx = float_of_int x and fy = float_of_int y in
      let d1 = 70. /. (distance x1 y1 fx fy)
      and d2 = 70. /. (distance x2 y2 fx fy) in
      let dist = int_of_float(d1 +. d2) in
      let r = Float.rem (d1 +. d2) 1. in
      let m = if r > (Random.float 1.) then 1  else 0 in
      let d = min (dist + m) ((List.length screen.palette) - 1) in
      if d < 0 then d + List.length screen.palette else d  
    )
  ) 

(* ----- *)

let inner_tick (t : int) (screen : screen) =
  let buffer = tick t screen in
  draw_image (buffer_to_image screen buffer) 0 0;
  synchronize ()

let () =
  let palette = load_tic80_palette tic80_palette
  and width = 240
  and height = 136 in
  let screen : screen = {
    width = width ;
    height = height ;
    scale = 3 ;
    palette = palette ;
  } in

  open_graph (Printf.sprintf " %dx%d" (screen.width * screen.scale) (screen.height * screen.scale));
  set_window_title "TCC Day 2 Extra";
  auto_synchronize false;
  (* set_font "-*-*-bold-r-*-*-32-*-*-*-m-*-iso8859-1"; *)

  boot screen;

  let t = ref 0 in
  while true do
    (* Unix.sleepf 0.05; *)
    let status = wait_next_event[ Poll; Key_pressed ] in
    if status.keypressed && status.key == ' ' then
      raise Exit
    else (
      inner_tick !t screen;
      t := !t + 1
    )
  done
