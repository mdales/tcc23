open Graphics

type screen = {
  width   : int ;
  height  : int ;
  palette : color list;
}

type point = {
  x : int ;
  y : int ;
}

type line = {
  a : point ;
  b : point ;
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

let generat_points (count : int) (t : int) (screen : screen) : point list =
  Random.init 42;
  List.init count (fun index -> 
    {
      x = ((Random.int screen.width) + (((index + 1) * t) / 10)) mod screen.width ;
      y = ((Random.int screen.height) + (((index + 1) * t) / 10)) mod screen.height ;
    }
  )

let distance (p1 : point) (p2 : point) : int =
  int_of_float (Float.sqrt((Float.pow (float_of_int (p2.x - p1.x)) 2.) +. (Float.pow (float_of_int (p2.y - p1.y)) 2.)))

let lines_from_points (points : point list) (threshold : int) : line list = 
  List.concat (List.map ( fun (outer : point) -> 
    List.filter_map ( fun (inner : point) : line option ->
      let d = distance inner outer in
      if (d > 0) && (d <= threshold) then Option.some {
        a = inner ;
        b = outer ;
      } else Option.none
    ) points
  ) points)

(* ----- *)

let boot (_screen : screen) = 
  ()

let tick (t : int) (screen : screen) =
  set_palette_color screen.palette 15;
  fill_rect 0 0 (screen.width - 1) (screen.height - 1);

  set_palette_color screen.palette 0;
  set_line_width 2;
  let ft = float_of_int t in
  let threshold = 40 + (int_of_float ((sin (ft /. 10.)) *. 5.)) in

  let points = generat_points 150 t screen in
  let lines = lines_from_points points threshold in 
  List.iter (fun (v : line) ->
    moveto v.a.x v.a.y;
    lineto v.b.x v.b.y
  ) lines

(* ----- *)

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
  set_window_title "TCC Day 3 Extra";
  auto_synchronize false;
  set_font "-*-*-bold-r-*-*-32-*-*-*-m-*-iso8859-1";

  boot screen;

  let t = ref 0 in
  while true do
    Unix.sleepf 0.05;
    let status = wait_next_event[ Poll; Key_pressed ] in
    if status.keypressed && status.key == ' ' then
      raise Exit
    else (
      inner_tick !t screen;
      t := !t + 1
    )
  done
