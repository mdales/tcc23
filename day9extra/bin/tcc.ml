open Graphics

type screen = {
  width   : int ;
  height  : int ;
  scale   : int ;
  palette : Palette.palette ;
}

let expanded_row (screen : screen) (row : int array) : color array =
  Array.concat (List.map (fun (vl : int) : color array ->
    let col = List.nth screen.palette vl in
      Array.make screen.scale col
    ) (Array.to_list row))

let buffer_to_image (screen : screen) (buffer : Framebuffer.framebuffer) : image =
  let raw = Array.concat (List.map (fun (row : int array) : color array array ->
    let colrow = expanded_row screen row in
      Array.make screen.scale colrow
  ) (Array.to_list buffer)) in
    make_image raw


(* ----- *)

let tcc_init (screen : screen) (title : string) (boot : screen -> Framebuffer.framebuffer) (tick : int -> screen -> Framebuffer.framebuffer -> Framebuffer.framebuffer)  =

  open_graph (Printf.sprintf " %dx%d" (screen.width * screen.scale) (screen.height * screen.scale));
  set_window_title title;
  auto_synchronize false;
  set_font "-*-*-bold-r-*-*-32-*-*-*-m-*-iso8859-1";

  let initial_buffer = boot screen in

  let rec loop (t : int) (prev_buffer : int array array) = (
    let status = wait_next_event[ Poll; Key_pressed ] in
    match status.keypressed with
    | true -> raise Exit
    | false -> (
      let updated_buffer = tick t screen prev_buffer in
      buffer_to_image screen updated_buffer |> (fun b -> draw_image b 0 0);
      synchronize ();
      (* Unix.sleepf 0.05; *)
      loop (t + 1) updated_buffer
    )
  ) in loop 0 initial_buffer
