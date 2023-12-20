open Graphics

type screen = {
  width           : int ;
  height          : int ;
  scale           : int ;
  palette         : color list ;
  buffer          : int array array;
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

let buffer_to_image (screen : screen) : image =
   let raw = (Array.map (fun (row : int array) -> 
    Array.map (fun (palette_val: int) -> 
      List.nth screen.palette palette_val
    ) row
  ) screen.buffer) in
      make_image raw

(* ----- *)

let boot (screen : screen) =
  set_palette_color screen.palette 0;
  fill_rect 0 0 screen.width screen.height

let tick (t : int) (screen : screen) =
  Random.init t;
  screen.buffer.(screen.height - 1) <- Array.init screen.width (fun _ -> Random.int (List.length screen.palette));
  for y = 1 to ((Array.length screen.buffer) - 2) do
    for x = 1 to ((Array.length screen.buffer.(y)) - 2) do
      let c = (screen.buffer.(y - 1).(x - 1) +screen.buffer.(y - 1).(x) + screen.buffer.(y - 1).(x + 1) +
      screen.buffer.(y).(x - 1) +screen.buffer.(y).(x) + screen.buffer.(y).(x + 1) +
      screen.buffer.(y + 1).(x - 1) +screen.buffer.(y + 1).(x) + screen.buffer.(y + 1).(x + 1)) / 9 in
      screen.buffer.(y - 1).(x) <- c
    done
  done

(* ----- *)

let inner_tick (t : int) (screen : screen) =
  tick t screen;
  draw_image (buffer_to_image screen) 0 0;
  synchronize ()

let () =
  let palette = load_tic80_palette tic80_palette
  and width = 240
  and height = 136 in
  let max_col = (List.length palette) - 1 in
  let screen : screen = {
    width = width ;
    height = height ;
    scale = 3 ;
    palette = palette ;
    buffer = Array.init height (fun _ -> Array.init width (fun _ -> max_col)) ;
  } in

  open_graph (Printf.sprintf " %dx%d" (screen.width * screen.scale) (screen.height * screen.scale));
  set_window_title "TCC Day 8";
  auto_synchronize false;
  set_font "-*-*-bold-r-*-*-32-*-*-*-m-*-iso8859-1";

  boot screen;

  let t = ref 0 in
  while true do
    (* Unix.sleepf 0.01; *)
    let status = wait_next_event[ Poll; Key_pressed ] in
    if status.keypressed && status.key == ' ' then
      raise Exit
    else
      inner_tick !t screen;
      t := !t + 1;
  done
