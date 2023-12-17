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

let set_palette_color (index : int) = 
  let colour = List.nth a_palette index in
  set_color colour

let draw_stars (t : int) (seed : int) (density : int) (diameter : int) = 
  let width = size_x () and height = size_y () in
  Random.init seed;
  for _i = 0 to density do
    let offset = (Random.int 50 + 30) in
    let updated_i = (Random.int (height)) - (t mod (height)) in
      let adjusted_updated_i = 
        (if updated_i >= 0 then updated_i else updated_i + height) in
          set_palette_color (11 + Random.int 3);
          fill_circle (Random.int width) (adjusted_updated_i + offset) diameter
  done

let draw_tree t x y =
  (* pot *)
  set_palette_color 2;
  let pot_height = 70 in
  let pot_upper_radius = 50 in 
  let pot_lower_radius = 30 in
  fill_poly [| 
    (x + pot_lower_radius, y); 
    (x + pot_upper_radius, y + pot_height); 
    (x - pot_upper_radius, y + pot_height); 
    (x - pot_lower_radius, y) 
  |];
  
  (* trunk *)
  set_palette_color 3;
  let trunk_height = 40 in
  let trunk_radius = 20 in
  fill_rect (x - trunk_radius) (y + pot_height) (trunk_radius * 2) (trunk_height); 

  (* leaves *)
  set_palette_color 7;
  let tree_height = 250 and tiers = 4 and tree_radius = 80 in
  for i = 0 to tiers do
    fill_poly [| 
      ((x + tree_radius) + ((tiers - i) * 15), y + pot_height + trunk_height + (i * 30)); 
      (x, y + pot_height + trunk_height + tree_height - ((tiers - i) * 30));
      ((x - tree_radius) - ((tiers - i) * 15), y + pot_height + trunk_height + (i * 30)) 
    |]
  done;
  
  (* star *)
  set_palette_color (3 + ((t / 20) mod 2));
  let star_offset = (y + pot_height + trunk_height + tree_height) in
  fill_poly [| (x, star_offset - 20); (x + 15, star_offset + 10); (x - 15, star_offset + 10) |];
  fill_poly [| (x, star_offset + 20); (x + 15, star_offset - 10); (x - 15, star_offset - 10) |]

let tick (t : int) =
  let width = size_x () and height = size_y () in

  (* background *)
  set_palette_color 15;
  fill_rect 0 0 width height;

  draw_stars t 42 100 1;

  set_palette_color 12;
  fill_rect 0 0 width (height / 3);

  (* trees *)
  let tree_count = 3 in
  let tree_gap = width / (tree_count + 1) in
  for index = 0 to (tree_count - 1) do
    if (index mod 2) == 1 then 
      draw_tree t ((index + 1) * tree_gap) (40 + (30 * (index mod 2)));
  done;

  draw_stars (t * 2) 22 50 1;

  (* text scroller *)
  let char_width = 30 in 
  for index = 0 to ((String.length prose) - 1) do
    let x = 640 + ((index * char_width) - ((t * 2) mod (width + (char_width * String.length prose)))) in 
    let h1 = (sin ((float_of_int x) /. 30.)) *. 20. in
    let h2 = (h1 *. 2.) in
    let h3 = (height / 2) + (int_of_float h2) - (16) in (* 16 is character heigh proxy *)

    moveto x h3;
    set_palette_color 8;
    draw_char prose.[index];
  
    moveto (x - 2) (h3 + 2);
    set_palette_color 12;
    draw_char prose.[index]
  done;

  for index = 0 to (tree_count - 1) do
    if (index mod 2) == 0 then 
      draw_tree t ((index + 1) * tree_gap) (40 + (3000 * (index mod 2)));
  done;

  draw_stars (t * 3) 32 25 2

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
