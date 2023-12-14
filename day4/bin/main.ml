open Graphics

let tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"

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

let generate_plasma_palette (size : int) : int list = 
  List.init size (fun (index : int): int ->
    let fi = float_of_int index and fsize = float_of_int size in
    let fred = (cos (fi *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in
    let fgreen = (cos ((fi +. (fsize /. 3.0)) *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in
    let fblue = (cos ((fi +. ((fsize *. 2.0) /. 3.0)) *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in

    ((int_of_float fred) * 65536) + ((int_of_float fgreen) * 256) + (int_of_float fblue)
  )

let a_palette = load_tic80_palette tic80_palette
let b_palette = load_tic80_palette havrekaka_palette
let c_palette = generate_plasma_palette 16

let tick (t : int) =
  let height = size_y () and width = size_x () in
  for y = 0 to height do
    for x = 0 to width do
      let palette = match ((t / 50) mod 3) with
        | 0 -> a_palette 
        | 1 -> b_palette
        | _ -> c_palette 
      in
      let ft = float_of_int t and fx = float_of_int (x / 2) and fy = float_of_int (y / 2) in
      let z = 10.0 +. (sin (ft /. 1000.0) *. 5.0)
      and d = 10.0 +. (cos (ft /. 1000.0) *. 5.0) in
      let fc = (sin (sin ((fx +. ft) /. z)) +. sin (sin ((fy +. ft) /. d))) *. 8.0 in
      let rc = (int_of_float fc) mod (List.length palette) in
      let color = List.nth palette (if rc >= 0 then rc else rc + List.length palette) in
      set_color color;
      plot x y
    done
  done

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
