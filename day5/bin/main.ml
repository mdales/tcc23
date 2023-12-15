open Graphics

let _tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let _havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"
let _vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb" (* f4f4f494b0c2566c86333c57*)
let vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb7400b86930c35e60ce5390d94ea8de48bfe3" (* f4f4f494b0c2566c86333c57*)

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

let a_palette = load_tic80_palette vapour_palette

let tick (t : int) =
  let height = size_y () and width = size_x () and ft = (float_of_int t) and colors = (List.length a_palette) in
  let fcolors = float_of_int colors in
  for j = 0 to height do
    for i = 0 to width do
      let x = float_of_int (i - (width / 2))
      and y = float_of_int (j - (height / 2)) in
      let d1 = (float_of_int width) /. sqrt ((x *. x) +. (y *. y) +. 1.0)
      and c1 = ((atan2 y x) +. Float.pi) *. (fcolors /. (2.0 *. Float.pi)) in
      let c2 = c1 +. (sin (ft /. 70.0) *. Float.pi *. 2.0) 
      and d2 = d1 +. (Float.rem (ft /. 10.0) fcolors) in
      let p = (int_of_float (Float.floor c2)) lxor (int_of_float (Float.floor d2)) in
      let pindex = (p mod colors) in 
      let color = List.nth a_palette (if pindex < 0 then (colors + pindex) else pindex) in
      set_color color;
      plot i j
    done
  done;
  set_color (List.nth a_palette 1);
  fill_circle (width / 2) (height / 2) 15

let inner_tick (t : int) =
  tick t;
  synchronize ()

let () =
  open_graph " 640x480";
  set_window_title "TCC Day 4";
  auto_synchronize false;

  let t = ref 0 in
  while true do
    (* Unix.sleepf 0.05; *)
    let status = wait_next_event[ Poll; Key_pressed ] in
    if status.keypressed && status.key == ' ' then 
      raise Exit 
    else
      inner_tick !t;
      t := !t + 1;
  done
