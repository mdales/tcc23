open Graphics

let tick (t: int) =
  let height = size_y () in 
    let width = size_x () in
      for y = 0 to height do
        for x = 0 to width do
          set_color ((x * y * t) mod 0xFFFFFF);
          plot x y
        done
      done

let inner_tick (t: int) =
    tick t;
    synchronize ()

let () = 
  open_graph " 640x480";
  set_window_title "day3";
  auto_synchronize false;
  display_mode false;
  remember_mode true;

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
