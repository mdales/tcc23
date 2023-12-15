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
let _b_palette = load_tic80_palette havrekaka_palette
let _c_palette = generate_plasma_palette 16

let tick (t : int) =
  let height = size_y () and width = size_x () and ft = (float_of_int t) in
  for j = 0 to height do
    for i = 0 to width do
      let x = float_of_int (i - (width / 2))
      and y = float_of_int (j - (height / 2)) in
      let d1 = 400.0 /. sqrt ((x *. x) +. (y *. y) +. 1.0)
      and c1 = ((atan2 y x) +. Float.pi) *. (16.0 /. (2.0 *. Float.pi)) in
      let c2 = c1 +. (sin (ft /. 70.0) *. Float.pi *. 2.0) 
      and d2 = d1 +. (Float.rem (ft /. 10.0) 16.0) in
      let p = (int_of_float d2) lxor (int_of_float c2) in
      let color = List.nth a_palette ((p land 0x7) + 8) in
      set_color color;
      plot i j
    done
  done


let _tick (t : int) =
  let height = size_y () and width = size_x () and ft = (float_of_int t) in
  for j = 0 to height do
    for i = 0 to width do
      let x = float_of_int (i - (width / 2))
      and y = float_of_int (j - (height / 2)) in
      let d1 = 400.0 /. sqrt ((x *. x) +. (y *. y) +. 1.0)
      and c1 = ((atan2 y x) +. Float.pi) *. (16.0 /. (2.0 *. Float.pi)) in
      let c2 = c1 +. (sin (ft /. 70.0) *. Float.pi *. 2.0) 
      and d2 = (int_of_float d1) + ((t / 10) mod 16) in
      let p = (d2) lxor (int_of_float c2) in
      let color = List.nth a_palette ((p land 0x7) + 8) in
      set_color color;
      plot i j
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
