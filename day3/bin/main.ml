open Graphics

let tick (t: int) =
  set_color green;
  fill_rect (t mod 540) (t mod 380) 10 10;
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
        Unix.sleepf 0.05;
        let status = wait_next_event[ Poll; Key_pressed ] in
          if status.keypressed && status.key == ' ' then 
            raise Exit 
          else
            inner_tick !t;
            t := !t + 1;
      done;
