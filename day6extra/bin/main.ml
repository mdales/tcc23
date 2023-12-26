open Graphics

type screen = {
  width           : int ;
  height          : int ;
  scale           : int ;
  palette         : color list ;
}

let _tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"
let _vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb"

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

let _set_palette_color (palette : color list) (index : int) =
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

let boot (_screen : screen) =
  ()

let tick (t : int) (screen : screen) : int array array =
  let ft = (Float.of_int t) /. 20. in
  let buffer = Array.init screen.height (fun _ -> 
    Array.make screen.width 15
  ) in 

  for ly = 1 to 8 do
    let fly = Float.of_int ly in
    let sv = sin ((ft /. 2.) +. (fly /. 4.)) in
    let bary = (Int.of_float (sv *. 50.)) + 68 in
    for lx = 1 to 8 do
      let row = buffer.(bary + lx) in
      Array.fill row 0 screen.width (lx + 7)
    done
  done;

  for i = 1 to 64 do
    let fi = Float.of_int i in
    let sv = (sin ((fi /. 13.) +. (ft /. 8.))) *. (sin ((fi /. 7.) +. (ft /. 2.)) *. 60.) in
    let barx = (Int.of_float sv) + 120 in
    for y = (0 + i * 2) to 135 do
      let row = buffer.(y) in
      for j = 0 to 8 do
        row.(barx + j) <- (j + 7)
      done
    done
  done;

  buffer


(* ----- *)

let inner_tick (t : int) (screen : screen) =
  tick t screen |> buffer_to_image screen |> (fun b -> draw_image b 0 0);
  synchronize ()

let () =
  let palette = load_tic80_palette havrekaka_palette 
  and width = 240
  and height = 136 in
  let screen : screen = {
    width = width ;
    height = height ;
    scale = 3 ;
    palette = palette ;
  } in

  open_graph (Printf.sprintf " %dx%d" (screen.width * screen.scale) (screen.height * screen.scale));
  set_window_title "TCC Day 6 Extra";
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
