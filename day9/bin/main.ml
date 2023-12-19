open Graphics

module PaletteMap = Map.Make(struct type t = int let compare = compare end)

type screen = {
  width   : int ;
  height  : int ;
  palette : color list ;
  inverse_palette : color PaletteMap.t ;
}

let _tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let _havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"
let vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb"
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

let make_inverse_palette (palette : color list) : color PaletteMap.t =
  let indexed_palette = List.mapi (fun index col -> (index, col)) palette in
  let palette_map : color PaletteMap.t = PaletteMap.empty in
    List.fold_left (fun map icol ->
      let index, col = icol in
        PaletteMap.add col index map
      ) palette_map indexed_palette


(* ----- *)

let boot (screen : screen)  =
  set_palette_color screen.palette 0;
  fill_rect 0 0 screen.width screen.height


let tick (t : int) (screen : screen) =
  let buffer = create_image screen.width screen.height in
  blit_image buffer 0 0;

  let dot_size = 24 in
  let ft = float_of_int t
  and fw = float_of_int (screen.width - dot_size)
  and fh = float_of_int (screen.height - dot_size) in
  let fx = (fw /. 2.) +. (sin ((2.28 +. (0.2 *. cos (ft /. 100.))) *. ft /. 80.) *. (fw /. 2.))
  and fy = (fh /. 2.) +. (cos ((3.7 +. (0.4 *. sin (ft /. 10.))) *. ft /. 80.) *. (fh /. 2.)) in
  let x = int_of_float fx
  and y = int_of_float fy in

  let raw = dump_image buffer in


  (* if (t mod 50) == 0 then
    for j = 0 to (screen.height - 1) do
      let row = raw.(j) in
      for i = 0 to (screen.width - 1) do
        let old_val = row.(x + i) in
        let old_pal = PaletteMap.find old_val screen.inverse_palette in
        if old_pal <
        let new_pal = (old_pal + 1) mod (List.length screen.palette) in
        row.(x + i) <- List.nth screen.palette new_pal


       let a = i + j * screen.width in
       v=peek(a,4)
       if v<8 then
       		v=(v+1)%8
         poke(a,v,4)
       end
     end
   end
   poke(0x3FF8*2,v,4)
 end *)

  for j = 0 to (dot_size - 1) do
    let row = raw.(y + j) in
	  for i = 0 to (dot_size - 1) do
        let old_val = row.(x + i) in
        let old_pal = PaletteMap.find old_val screen.inverse_palette in
        let new_pal = (old_pal + 1) mod (List.length screen.palette) in
        row.(x + i) <- List.nth screen.palette new_pal
    done
  done;


  let new_buffer = make_image raw in
  draw_image new_buffer 0 0

(* ----- *)

let inner_tick (t : int) (screen : screen) =
  tick t screen;
  synchronize ()

let () =
  let palette = load_tic80_palette vapour_palette in
  let screen : screen = {
    width = 640 ;
    height = 480 ;
    palette = palette ;
    inverse_palette = make_inverse_palette palette ;
  } in

  open_graph (Printf.sprintf " %dx%d" screen.width screen.height);
  set_window_title "TCC Day 8";
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
