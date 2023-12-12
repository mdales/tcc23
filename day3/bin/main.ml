open Graphics

let cls (c: color) = 
  set_color c;
  fill_rect 0 0 (size_x ()) (size_y ())

let tick (t: int) =
  cls black;
  set_color green;
  fill_rect (t mod 630) (t mod 470) 10 10;
  set_color red;
  fill_rect 110 110 100 100

let inner_tick (t: int) =
    tick t;
    synchronize ()

let () = 
  auto_synchronize false;
  display_mode false;
  remember_mode true;
  open_graph " 640x480";

    let t = ref 0 in
      while true do
        (* Unix.sleepf 0.05; *)
        let status = wait_next_event[ Poll; Key_pressed ] in
          if status.keypressed && status.key == ' ' then 
            raise Exit 
          else
            inner_tick !t;
            t := !t + 1;
      done;
