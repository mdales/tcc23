open Graphics

type screen = {
  width           : int ;
  height          : int ;
  scale           : int ;
  palette         : color list ;
}

let _tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let _havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"
let vapour_palette = "000:2420387400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb"
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

let filled_circle (x : int) (y : int) (r : int) (col : int) (buffer : int array array) =
  let miny = y - (r - 1)
  and maxy = y + (r - 1) 
  and fr = Float.of_int (r - 1) in
  for yi = miny to maxy do
    let row = buffer.(yi) in
    let a = acos ((Float.of_int (yi - y)) /. fr) in
    let xw = Int.of_float ((sin a) *. fr) in
    let minx = x - xw
    and maxx = x + xw in
    for xi = minx to maxx do
      row.(xi) <- col
    done
  done

(* ----- *)

let boot (screen : screen) : int array array = 
  let buffer = Array.init screen.height (fun _ ->
    Array.make screen.width 0
  ) in
  filled_circle (screen.width / 2) (screen.height / 2) (screen.height / 2) 1 buffer;
  buffer

let tick (t : int) (screen : screen) (prev : int array array) : int array array =
  let buffer = Array.map (fun row ->
    Array.map (fun pixel -> 
      match pixel with
      | 0 -> 0
      | 1 -> 1
      | _ -> (pixel - 1)
    ) row
  ) prev in
  (* set_palette_color screen.palette 0;
  fill_circle (screen.width / 2) (screen.height / 2) (screen.height / 2); *)

  let n = 40
  and m = 200
  and r = Float.pi *. 2. /. 235.
  and x = ref 0.
  and v = ref 0.
  and ft = (Float.of_int t) *. (0.025 /. 2.) in

  let qs = ((Float.of_int screen.height) /. 4.) -. 1. in

  for i = 0 to n do
    for j = 0 to m do
      let fi = Float.of_int i in
      let a = fi +. !v
      and b = (r *. fi) +. !x in
      let u = (sin a) +. (sin b) in
      v := (cos a) +. (cos b);
      x := u +. ft;

      let col = 2 + ((i + (j / 36)) mod ((List.length screen.palette) - 2)) in
      let xpos = ((screen.width / 2) + Int.of_float (u *. qs))
      and ypos = ((screen.height / 2) + Int.of_float (!v *. qs)) in
      buffer.(ypos).(xpos) <- col
    done
  done;
  buffer

(* ----- *)

(* let _inner_tick (t : int) (screen : screen) =
  tick t screen |> buffer_to_image screen |> (fun b -> draw_image b 0 0);
  synchronize () *)

let () =
  let palette = load_tic80_palette vapour_palette 
  and width = 640
  and height = 480 in
  let screen : screen = {
    width = width ;
    height = height ;
    scale = 1 ;
    palette = palette ;
  } in

  open_graph (Printf.sprintf " %dx%d" (screen.width * screen.scale) (screen.height * screen.scale));
  set_window_title "TCC Day 8 Extra";
  auto_synchronize false;
  (* set_font "-*-*-bold-r-*-*-32-*-*-*-m-*-iso8859-1"; *)

  let initial_buffer = boot screen
  and initial_t = 0 in

  let rec loop (t : int) (prev_buffer : int array array) = (
    let status = wait_next_event[ Poll; Key_pressed ] in
    match status.keypressed with
    | true -> raise Exit
    | false -> (
      let updated_buffer = tick t screen prev_buffer in
      buffer_to_image screen updated_buffer |> (fun b -> draw_image b 0 0);
      synchronize ();
      (* Unix.sleepf 0.05; *)
      loop (t + 1) updated_buffer
    )
  ) in loop initial_t initial_buffer
