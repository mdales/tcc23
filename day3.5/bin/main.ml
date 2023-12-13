open Graphics

let palette = [
  0x1a1c2c;
  0x5d275d;
  0xb13e53;
  0xef7d57;
  0xffcd75;
  0xa7f070;
  0x38b764;
  0x257179;
  0x29366f;
  0x3b5dc9;
  0x41a6f6;
  0x73eff7;
  0xf4f4f4;
  0x94b0c2;
  0x566c86;
  0x333c57
]

let tick (t: int) =
  let height = size_y () in 
    let width = size_x () in
      for y = 0 to height do
        for x = 0 to width do
          let color = List.nth palette (((x / 3) + (y / 3) + t) mod 0xF) in
            set_color color;
            plot x y
        done
      done

let inner_tick (t: int) =
    tick t;
    synchronize ()

let () = 
  open_graph " 640x480";
  set_window_title "day3.5";
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
