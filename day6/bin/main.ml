open Graphics

let tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let _havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"
let _vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb" 
(* let _vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb7400b86930c35e60ce5390d94ea8de48bfe3" *)
let prose = "hello, tiny code xmas!"

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

let a_palette = load_tic80_palette tic80_palette

let draw_stars (t: int) (seed: int) (density: int) = 
  set_color white;
  Random.init seed;
  for _i = 0 to density do
    let updated_i = (Random.int 640) - (t mod 640) in
      let adjusted_updated_i = 
        (if updated_i >= 0 then updated_i else updated_i + 640) in
          fill_circle adjusted_updated_i (Random.int 480) 1
  done

let tick (t : int) =
  let width = size_x () and height = size_y () in

  (* background *)
  set_color (List.nth a_palette 15);
  fill_rect 0 0 width height;
  draw_stars t 42 100;
  draw_stars (t * 2) 22 50;
  draw_stars (t * 3) 32 25;

  (* text scroller *)
  let char_width = 30 in 
  for index = 0 to ((String.length prose) - 1) do
    let x = 640 + ((index * char_width) - ((t * 2) mod (width + (char_width * String.length prose)))) in 
    let h1 = (sin ((float_of_int x) /. 30.)) *. 20. in
    let h2 = (h1 *. 2.) in
    let h3 = (height / 2) + int_of_float h2 in

    moveto x h3;
    set_color (List.nth a_palette 8);
    draw_char prose.[index];
  
    moveto (x - 2) (h3 + 2);
    set_color (List.nth a_palette 12);
    draw_char prose.[index]
  done


let inner_tick (t : int) =
  tick t;
  synchronize ()

let () =
  open_graph " 640x480";
  set_window_title "TCC Day 6";
  auto_synchronize false;
  set_font "-*-*-bold-r-*-*-32-*-*-*-m-*-iso8859-1";

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
