
let boot (screen : Tcc.screen) : Framebuffer.framebuffer = 
  Framebuffer.init screen.width screen.height (fun _x _y -> 0)

let tick (t : int) (screen : Tcc.screen) (_prev : Framebuffer.framebuffer) : Framebuffer.framebuffer =
  let ft = (Float.of_int t) *. 0.02 in
  let buffer = Framebuffer.init screen.width screen.height (fun _x _y -> 0) in

  for z = 0 to 75 do
    let fz = (Float.of_int z) *. 8. in
    for x = -100 to 100 do 
      let fx = ((Float.of_int x) *. 3.) in
    
      let y = ((sin ((sin (ft /. 2.)) +. fx /. 50.) *. 3.) +. (sin((fz /. 50.) -. ft)) *. 3.) in
      let px = (screen.width / 2) + Int.of_float (fx /. (fz *. 1.8 -. 1200.) *. 1200.) 
      and py = (Int.of_float ((y -. 40.) /. (fz *. 1.8 -. 1200.) *. 1200.)) + 100
      and col = (Int.of_float y) + 6 in

      (* if (py >= 0) && (py < screen.height) && (px >= 0) && (px < screen.width) then *)
        (* buffer.(py).(px) <- (if col < 0 then col + ((List.length screen.palette) - 1) else col) *)
      Framebuffer.filled_circle px py (20 / (20 - (z / 4))) (if col < 0 then col + ((List.length screen.palette) - 1) else col) buffer
    done
  done;
  buffer

(* ----- *)

let () =
  let screen : Tcc.screen = {
    width = 640 ;
    height = 480 ;
    scale = 1 ;
    palette = 0x666666 :: Palette.generate_plasma_palette 15 ;
    (* palette = (Palette.generate_mono_palette 16) ; *)
  } in

 Tcc.tcc_init screen "TCC Day 9 extra" boot tick
 