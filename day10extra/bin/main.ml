
let boot (screen : Tcc.screen) : Framebuffer.t = 
  Framebuffer.init screen.width screen.height (fun _x y -> if y < 40 then 0 else 8)

let tick (t : int) (screen : Tcc.screen) (_prev : Framebuffer.t) : Framebuffer.t =
  let ft = (Float.of_int t) *. 0.02 in
  let buffer = Framebuffer.init screen.width screen.height (fun _x y -> if y < 40 then 0 else 8) in

  Framebuffer.filled_circle 40 10 20. 1 buffer;

  for j = 0 to 2 do 
    for i = 0 to 4 do
      Framebuffer.filled_circle (((30 + (j * 80) + (i * 5)) +(t / 4)) mod screen.width) (20 + ((j mod 2) * 3) - ((i mod 2) * 5)) 6. 2 buffer
    done
  done;

  for z = 0 to 60 do
    let fz = (Float.of_int z) *. 8. in
    for x = -40 to 40 do 
      let fx = ((Float.of_int x) *. 3.) in
    
      let prey = ((sin ((sin (ft /. 2.)) +. fx /. 50.) *. 3.) +. (sin((fz /. 50.) -. ft)) *. 3.) in
      let y = if prey > 0. then prey else prey in
      let px = (screen.width / 2) + Int.of_float (fx /. (fz *. 1.8 -. 1200.) *. 1200.) 
      and py = (Int.of_float ((0. -. 40.) /. (fz *. 1.8 -. 1200.) *. 1200.)) 
      and col = (Int.of_float y) + 6 in
      let pby = (Int.of_float ((-10. -. 40.) /. (fz *. 1.8 -. 1200.) *. 1200.))  in
      Framebuffer.draw_line px py (px - col) pby (((Palette.size screen.palette) - 1) - col) buffer
    done
  done;
  buffer

(* ----- *)

let () =
  (* Mostly green but with sky/sun/cloud colours *)
  let mono_rgb = Palette.to_list (Palette.generate_mono_palette 16) in
  let short_green_rgb = List.map (fun x -> x land 0x0fff0f) (List.tl ( List.tl (List.tl mono_rgb))) in
  let this_rgb = 0x6666ff :: 0xffff00 :: 0xeeeeee :: short_green_rgb in
  let screen : Tcc.screen = {
    width = 240 ;
    height = 136 ;
    scale = 4 ;
    palette = Palette.from_list this_rgb ;
  } in
 Tcc.tcc_init screen "TCC Day 10 extra" boot tick
 