


let vapour_palette = "000:0000007400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb"

(* ----- *)

type point = {
  x : float ;
  y : float ;
  z : float ;
}

let rotate_x (a : float) (p : point) : point =
  { p with
    y = (p.y *. (cos (a))) -. (p.z *. sin(a)) ;
    z = (p.y *. sin(a)) +. (p.z *. cos(a)) ;
  }

let rotate_y (a : float) (p : point) : point =
  { p with
    x = (p.x *. cos(a)) -. (p.z *. sin(a)) ;
    z = (p.x *. sin(a)) +. (p.z *. cos(a)) ;
  }
    
let rotate_z (a : float) (p : point) : point =
  { p with
    x = (p.x *. cos(a)) -. (p.y *. sin(a)) ;
    y = (p.x *. sin(a)) +. (p.y *. cos(a)) ;
  }

let translate_x (d : float) (p : point) : point = 
  { p with x = p.x +. d }
  
let point_cmp (a : point) (b : point) : int =
  if a.z == b.z then 0
  else if a.z < b.z then 1 else -1

(* ----- *)

let _generate_cube (ft : float) : point array =
  let o = sin (ft /. 10.) in
  let offset = if o < 0. then ((0. -. o) *. 4.0) else 0. in
  let points : point array = Array.make (17 * 17 * 17) {x=0. ; y=0. ; z=0. } in
  for z = 0 to 16 do
    for y = 0 to 16 do 
      for x = 0 to 16 do 
        let p : point = {
          x = Float.of_int ((x - 8)) *. (4. +. offset) ; 
          y = Float.of_int ((y - 8)) *. (4. +. offset) ; 
          z = Float.of_int ((z - 8)) *. (4. +. offset) ;
        } in
        points.(x + (y * 17) + (z * 17 * 17)) <- p
      done
    done
  done;
  points

  let generate_torus (ft : float) : point array =
    let o = sin (ft /. 10.) in
    let offset = if o < 0. then ((0. -. o) *. 10.0) else 0. in
    let thickness_radius = 10. 
    and dots_per_slice = 12
    and torus_radius = 20.
    and slices_per_torus = 24 in 
    let nested = Array.init slices_per_torus (fun s -> 
      let slice_angle = (2. *. (Float.of_int s) *. Float.pi /. (Float.of_int slices_per_torus)) in
      Array.init dots_per_slice (fun i -> 
        let fi = Float.of_int i in
        let a = (2. *. fi *. Float.pi /. (Float.of_int dots_per_slice)) in
        {
          x = (thickness_radius +. offset) *. cos a ;
          y = (thickness_radius +. offset) *. sin a ;
          z = 0. ;
        } |> translate_x (torus_radius +. offset) |> rotate_y (slice_angle +. sin (ft *. 0.05))
      ) 
    ) in
    let lested = Array.to_list nested in
    Array.concat lested

(* ----- *)

let boot (screen : Tcc.screen) : Framebuffer.t = 
  Framebuffer.init screen.width screen.height (fun _x _y -> 0)

let tick (t : int) (screen : Tcc.screen) (_prev : Framebuffer.t) : Framebuffer.t =
  let buffer = Framebuffer.init screen.width screen.height (fun _x _y -> 0) in

  let ft = Float.of_int t in
  let points = generate_torus ft |> Array.map (fun p ->
    rotate_y (0.02 *. ft) p |> rotate_x (0.01 *. ft) |> rotate_z (0.005 *. ft)
  ) in

  Array.sort point_cmp points;

  let m = 2000. +. cos(ft /. 30.) *. 600. 
  and z = 10. +. sin(ft /. 1000.) *. 5. 
  and d = 10. +. cos(ft /. 1000.) *. 5. in
  Array.iter (fun e ->
    let fcol = (sin(sin((e.x +. ft) /. z)) +. sin(sin((e.y +. ft) /. d)) +. sin(sin((e.z +. ft) /. z))) *. 8. in
    let col = ((int_of_float fcol) mod ((Palette.size screen.palette) - 1)) in
    let acol = if col < 0 then (col + ((Palette.size screen.palette) - 1)) else col in
    Framebuffer.filled_circle 
      ((screen.width / 2) + int_of_float(m *. e.x /. (e.z +. 400.))) 
      ((screen.height / 2) + int_of_float(m *. e.y /. (e.z +. 400.))) 
      (m /. 200.)
      (acol + 1)
      buffer
  ) points;
  
  buffer

(* ----- *)

let () =
  let screen : Tcc.screen = {
    width = 640 ;
    height = 480 ;
    scale = 1 ;
    palette = Palette.load_tic80_palette vapour_palette ;
  } in
  Tcc.tcc_init screen "TCC Day 11 extra" boot tick
