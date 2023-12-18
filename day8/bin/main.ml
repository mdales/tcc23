open Graphics

type screen = {
  width   : int ;
  height  : int ;
  palette : color list;
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

(* ----- *)

let random_line (width : int) (palette : color list) : (color array) =
  Array.init width (fun _x -> List.nth palette ((Random.int ((List.length palette) / 2)) + 8))

let random_screen (width: int) (height: int) (palette : color list) : (color array array) = 
  Array.init height (fun _x -> random_line width palette)

(* ----- *)

let boot (screen : screen) = 
  let raw_noise = random_screen screen.width screen.height screen.palette in 
  let img_noise = make_image raw_noise in 
  draw_image img_noise 0 0

let draw_splots (t : int) (screen : screen) = 
  if (t mod 3) == 0 then 
    let ft = float_of_int t in
    let dot_count = 5 in
    let col = (t / 3) mod (dot_count - 1) in
    set_palette_color screen.palette col;
    for i = 0 to dot_count do
      let d = 90. +. ((sin (ft /. 20.)) *. 30.)
      and	a = (float_of_int i) *. ((Float.pi *. 2.) /. (float_of_int dot_count)) in
      let fx = cos(a +. (ft /. 30.)) *. d
      and fy = sin(a +. (ft /. 20.)) *. d in 
      let x = int_of_float fx
      and y = int_of_float fy
      and rad = 14. +. (sin (ft /. 20.) *. 6.) in
      fill_circle (((2 * screen.width / 3)) + x) ((screen.height / 2) + y) (int_of_float rad)
    done

let tick (t : int) (screen : screen) =
  draw_splots t screen;

  let buffer = create_image (screen.width - 1) screen.height in
  blit_image buffer 1 0;
  draw_image buffer 0 0;

  let new_noise = make_image (random_screen 1 screen.height screen.palette) in
  draw_image new_noise (screen.width - 1) 0


(* ----- *)

let inner_tick (t : int) (screen : screen) =
  tick t screen;
  synchronize ()

let () =
  let screen : screen = { 
    width = 640 ; 
    height = 480 ;
    palette = load_tic80_palette tic80_palette ;
  } in

  open_graph (Printf.sprintf " %dx%d" screen.width screen.height);
  set_window_title "TCC Day 8";
  auto_synchronize false;
  set_font "-*-*-bold-r-*-*-32-*-*-*-m-*-iso8859-1";

  boot screen;

  let t = ref 0 in
  while true do
    (* Unix.sleepf 0.05; *)
    let status = wait_next_event[ Poll; Key_pressed ] in
    if status.keypressed && status.key == ' ' then 
      raise Exit 
    else
      inner_tick !t screen;
      t := !t + 1;
  done
