open Graphics
;;
auto_synchronize false

;;
display_mode false

;;
remember_mode true

;;
(* The space here before the res is important :/ *)
open_graph " 640x480"
;;

let draw_background = 
  let horizon_height = (480 / 3) in
    set_color white;
    fill_rect 0 0 640 horizon_height;
    set_color blue;
    fill_rect 0 horizon_height 640 (480  - horizon_height)

let draw_stars = 
  set_color white;
  for i = 0 to 480 do
    plot (Random.int 640) i
  done

let draw_tree x y =
  (* pot *)
  set_color red;
  let pot_height = 70 in
    let pot_upper_radius = 50 in 
      let pot_lower_radius = 30 in
        fill_poly [| 
          (x + pot_lower_radius, y); 
          (x + pot_upper_radius, y + pot_height); 
          (x - pot_upper_radius, y + pot_height); 
          (x - pot_lower_radius, y) 
        |]; ; ;
    (* trunk *)
    set_color (rgb 0xC0 0x80 0x20);
    let trunk_height = 50 in
      let trunk_radius = 20 in
          fill_rect (x - trunk_radius) (y + pot_height) (trunk_radius * 2) (trunk_height); ;
      (* leaves *)
      set_color (rgb 0x20 0xA0 0x20);
      let tree_height = 250 in
        let tiers = 4 in
          let tree_radius = 100 in
              for i = 0 to tiers do
              (* let i = 0 in *)
                fill_poly [| 
                  ((x + tree_radius) + ((tiers - i) * 15), y + pot_height + trunk_height + (i * 30)); 
                  (x, y + pot_height + trunk_height + tree_height - ((tiers - i) * 30));
                  ((x - tree_radius) - ((tiers - i) * 15), y + pot_height + trunk_height + (i * 30)) 
                |]
              done; ; ;
        (* star *)
        set_color yellow;
        let star_offset = (y + pot_height + trunk_height + tree_height) in
          fill_poly [| (x, star_offset - 20); (x + 15, star_offset + 10); (x - 15, star_offset + 10) |];
          fill_poly [| (x, star_offset + 20); (x + 15, star_offset - 10); (x - 15, star_offset - 10) |]

let draw_scene = 
  draw_background;
  draw_stars;
  draw_tree 320 60;
;;

draw_scene

;;

synchronize ()
;;
ignore (wait_next_event [ Key_pressed ])
