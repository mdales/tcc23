open Graphics

type screen = {
  width   : int ;
  height  : int ;
  palette : color list;
}

let _tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let _havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"
let vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb"
(* let vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb7400b86930c35e60ce5390d94ea8de48bfe3" *)

let _generate_mono_palette (size : int) : int list = 
  List.init size (fun (index : int): int ->
    let fi = float_of_int index and fsize = float_of_int size in
    let ch = (cos (fi *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in
    ((int_of_float ch) * 65536) + ((int_of_float ch) * 256) + (int_of_float ch)
  )

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



(* ----- *)

let boot (_screen : screen) = 
  ()

let tick (t : int) (screen : screen) =
  set_color 0;
  fill_rect 0 0 screen.width screen.height;
  set_palette_color screen.palette 0;
  fill_circle (screen.width / 2) (screen.height / 2) (screen.height / 2);

  let n = 40
  and m = 200
  and r = Float.pi *. 2. /. 235.
  and x = ref 0.
  and v = ref 0.
  and ft = (Float.of_int t) *. 0.025 in

  for i = 0 to n do
    for j = 0 to m do
      let fi = Float.of_int i in
      let a = fi +. !v
      and b = (r *. fi) +. !x in
      let u = (sin a) +. (sin b) in
      v := (cos a) +. (cos b);
      x := u +. ft;

      let col = 1 + ((i + (j / 36)) mod ((List.length screen.palette) - 1)) in
      set_palette_color screen.palette col;
      let qs = (Float.of_int screen.height) /. 4. in
      fill_circle ((screen.width / 2) + Int.of_float (u *. qs)) ((screen.height / 2) + Int.of_float (!v *. qs)) 1
    done
  done

(* ----- *)

let inner_tick (t : int) (screen : screen) =
  tick t screen;
  synchronize ()

let () =
  let screen : screen = {
    width = 640 ;
    height = 480 ;
    (* palette = generate_mono_palette 16; *)
    palette = load_tic80_palette vapour_palette;
  } in

  open_graph (Printf.sprintf " %dx%d" screen.width screen.height);
  set_window_title "TCC Day 7 Extra";
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
