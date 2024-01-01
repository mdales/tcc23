


let havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"

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
  
let point_z_cmp (a : point) (b : point) : int =
  if a.z == b.z then 0
  else if a.z < b.z then 1 else -1

(* ----- *)

let generate_torus (ft : float) : point list =
  let o = sin (ft /. 20.) in
  let offset = if o < 0. then ((0. -. o) *. 20.0) else 0. in
  let thickness_radius = 10. 
  and dots_per_slice = 25
  and torus_radius = 20.
  and slices_per_torus = 70 in 
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
  Array.to_list (Array.concat lested)

let render_to_primatives (ft : float) (screen : Tcc.screen) (points : point list) : Primatives.t list =

  let m = 2000. +. cos(ft /. 30.) *. 600. 
  and z = 10. +. sin(ft /. 1000.) *. 5. 
  and d = 10. +. cos(ft /. 1000.) *. 5. in
  List.map (fun e ->
    let fcol = (sin(sin((e.x +. ft) /. z)) +. sin(sin((e.y +. ft) /. d)) +. sin(sin((e.z +. ft) /. z))) *. 8. in
    let col = ((int_of_float fcol) mod ((Palette.size screen.palette) - 1)) in
    let acol = if col < 0 then (col + ((Palette.size screen.palette) - 1)) else col in
    Primatives.Circle ({
      x = ((screen.width / 2) + int_of_float(m *. e.x /. (e.z +. 400.))) ; 
      y = ((screen.height / 2) + int_of_float(m *. e.y /. (e.z +. 400.))) ;
    }, (m /. 200.), (acol))
  ) points

(* ----- *)

let tick (t : int) (screen : Tcc.screen) (_prev : Framebuffer.t) : Framebuffer.t =
  let buffer = Framebuffer.init screen.width screen.height (fun _x _y -> 15) in

  let ft = Float.of_int t in

  generate_torus ft 
  |> List.map (fun p ->
    rotate_y (0.02 *. ft) p |> rotate_x (0.01 *. ft) |> rotate_z (0.005 *. ft)
  ) 
  |> List.sort point_z_cmp 
  |> render_to_primatives ft screen 
  |> Framebuffer.render buffer;

  buffer

(* ----- *)

let () =
  let screen : Tcc.screen = {
    width = 640 ;
    height = 480 ;
    scale = 1 ;
    palette = Palette.load_tic80_palette havrekaka_palette ;
  } in
  Tcc.tcc_init screen "TCC Day 12 extra" None tick
