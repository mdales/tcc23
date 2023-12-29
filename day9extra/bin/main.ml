
let boot (screen : Tcc.screen) : Framebuffer.framebuffer = 
  Framebuffer.init screen.width screen.height (fun _x _y -> 0)

let tick (t : int) (screen : Tcc.screen) (_prev : Framebuffer.framebuffer) : Framebuffer.framebuffer =
  Random.init 42;
  let ft = (Float.of_int t) *. 0.02 in
  let buffer = Framebuffer.init screen.width screen.height (fun _x _y -> 0) in

  for z = 0 to 76 do
    let fz = (Float.of_int z) *. 8. in
    for x = -100 to 100 do 
      let fx = ((Float.of_int x) *. 3.) in
    
      let prey = ((sin ((sin (ft /. 2.)) +. fx /. 50.) *. 3.) +. (sin((fz /. 50.) -. ft)) *. 3.) in
      let y = if prey > 0. then prey *. 1.5 else prey in
      let px = (screen.width / 2) + Int.of_float (fx /. (fz *. 1.8 -. 1200.) *. 1200.) 
      and py = (Int.of_float ((y -. 40.) /. (fz *. 1.8 -. 1200.) *. 1200.)) + 100
      and col = (Int.of_float y) + 6 in
      let dither = if (Float.rem y 1.) > (Random.float 1.) then 1 else 0 in
      (* let dot = (20. /. (22. -. ((fz /. 8.) /. 4.))) in *)
      let dot = 20. /. (77. -. (fz /. 8.)) in
      (* let dotdither = if (Float.rem fdot 1.) > (Random.float 1.) then 1 else 0 in
      let dot = (Int.of_float fdot) + dotdither in *)

     Framebuffer.filled_circle px py dot (col + dither) buffer
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
  } in

 Tcc.tcc_init screen "TCC Day 9 extra" boot tick
 