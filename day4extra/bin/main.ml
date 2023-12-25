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

let boot (screen : screen) =
  set_palette_color screen.palette 0;
  fill_rect 0 0 screen.width screen.height

let _tick (t : int) (screen : screen) : int array array =
  let ft = (Float.of_int t) /. 100.
  and fxoffset = (Float.of_int screen.width) /. 2.
  and fyoffset = (Float.of_int screen.height) /. 2. in
  Array.init screen.height (fun (y : int) : int array ->
    Array.init screen.width (fun (x : int) : int ->
      let x1 = ((Float.of_int x) -. fxoffset) +. (fxoffset *. sin ft)
      and y1 = ((Float.of_int y) -. fyoffset) +. (fyoffset *. sin ft) in
      let u = Int.of_float ((x1 *. cos ft) -. (y1 *. sin ft))
      and v = Int.of_float (((x1 *. sin ft) +. (y1 *. cos ft))) in
      let col = ((u land v) / 5) mod (List.length screen.palette) in
      if (col < 0) then (col + (List.length screen.palette)) else col
    )
  )

let tick (_t : int) (screen : screen) = 
  let fxoffset = (Float.of_int screen.width) /. 2.
  and fyoffset = (Float.of_int screen.height) /. 2. in
  let fxstep = 3. /. (Float.of_int screen.width) 
  and fystep = 3. /. (Float.of_int screen.height) in
  let max_iterations = 64 in
  Array.init screen.height (fun (y : int) : int array ->
    let fy = Float.of_int y in
    let py = (fy -. fyoffset) *. fystep in
    Array.init screen.width (fun (x : int) : int ->
      let fx = Float.of_int x in
      let px = (fx -. fxoffset) *. fxstep in

      let rec loop (a : float) (b : float) (i : int) : int = 
        match (i == max_iterations) with
        | true -> i
        | false -> (
          let current = ((a *. a) +. (b *. b)) <= 4. in
          match current with
          | false -> i
          | true -> (loop 
            (((a *. a) -. (b *. b)) +. px)
            ((2. *. a *. b) +. py)
            (i + 1))) in
      let col = loop 0.0 0.0 0 in
      col mod (List.length screen.palette)
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
  set_window_title "TCC Day 4 Extra";
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
