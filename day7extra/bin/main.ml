open Graphics

type screen = {
  width   : int ;
  height  : int ;
  palette : color list;
}

type turtle = {
  angle : float;
  path : (float * float * bool) list;
  mark : bool ;
}

let _tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let _havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"
let _vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb"
(* let vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb7400b86930c35e60ce5390d94ea8de48bfe3" *)

let generate_mono_palette (size : int) : int list = 
  List.init size (fun (index : int): int ->
    let fi = float_of_int index and fsize = float_of_int size in
    let ch = (cos (fi *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in
    ((int_of_float ch) * 65536) + ((int_of_float ch) * 256) + (int_of_float ch)
  )

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

let _load_tic80_palette (raw : string) =
  let parts = String.split_on_char ':' raw in
  let strchunks = string_to_chunks (List.nth parts 1) 6 in
  chunks_to_colors strchunks

let set_palette_color (palette : color list) (index : int) =
  let colour = List.nth palette index in
  set_color colour

(* ----- *)

let left (a : float) (t : turtle) : turtle = 
  { t with angle = t.angle -. ((a /. 360.) *. 2. *. Float.pi)}

let right (a : float) (t : turtle) : turtle = 
  { t with angle = t.angle +. ((a /. 360.) *. 2. *. Float.pi)}

let forward (dist : float) (t : turtle) : turtle = 
  let x, y, _ = match t.path with
  | [] -> 0., 0., false
  | hd :: _ -> hd
  in
  let newpos = (
    x +. ((sin t.angle) *. dist),
    y +. ((cos t.angle) *. dist),
    t.mark
  ) in
  let f = (match t.mark with
  | true -> lineto
  | false -> moveto)
  in
  let fx, fy, _ = newpos in
  f (Int.of_float fx) (Int.of_float fy);
  { t with path = newpos :: t.path }

let penup (t : turtle) : turtle = 
  {t with mark = false}

let pendown (t : turtle) : turtle = 
  {t with mark = true}

(* ----- *)

let rec kock (length : float) (level : int) (t : turtle) : turtle  =
  match level with
  | 0 -> forward length t
  | _ -> (
    let l = length /. 3. in
      kock l (level - 1) t |>
      left 60. |>
      kock l (level - 1) |>
      right 120. |>
      kock l (level - 1) |>
      left 60. |>
      kock l (level - 1)
  )

let star (length : float) (level : int) (t : turtle) : turtle = 
  let moved_t = penup t |>
  left (360. /. 3.) |>
  forward length |>
  right (360. /. 3.) |>
  pendown in

  let rec loop (index : int) (t : turtle) : turtle =
    match index with
    | 0 -> t
    | _ -> 
      kock length level t |>
      right 60. |>
      loop (index - 1) 
    in
      loop 6 moved_t

(* ----- *)

let boot (_screen : screen) = 
  ()

let tick (t : int) (screen : screen) =
  set_palette_color screen.palette 0;
  fill_rect 0 0 screen.width screen.height;
  set_palette_color screen.palette 10;

  for i = 0 to 2 do
    moveto (screen.width / 2) (screen.height / 2);
    let turtle = {
      angle = 0. ;
      path = [ ((Float.of_int (screen.width / 2)), (Float.of_int (screen.height / 2)), false) ] ;
      mark = false ;
    } in
  
    let ft = Float.of_int t and fi = Float.of_int i in
    ignore (
      left (fi +. (3. *. ft)) turtle |>
      star (Float.rem (2. *. (ft +. (fi *. 140.))) 450.) (abs (Int.of_float (5. *. sin (ft /. 10.))))
    )
  done

(* ----- *)

let inner_tick (t : int) (screen : screen) =
  tick t screen;
  synchronize ()

let () =
  let screen : screen = {
    width = 640 ;
    height = 480 ;
    palette = generate_mono_palette 16;
  } in

  open_graph (Printf.sprintf " %dx%d" screen.width screen.height);
  set_window_title "TCC Day 7 Extra";
  auto_synchronize false;
  set_font "-*-*-bold-r-*-*-32-*-*-*-m-*-iso8859-1";

  boot screen;

  let t = ref 0 in
  while true do
    Unix.sleepf 0.05;
    let status = wait_next_event[ Poll; Key_pressed ] in
    if status.keypressed && status.key == ' ' then
      raise Exit
    else (
      inner_tick !t screen;
      t := !t + 1
    )
  done
