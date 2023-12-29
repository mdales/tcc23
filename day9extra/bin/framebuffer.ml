
type framebuffer = int array array

let init (width : int) (height : int) (f : int -> int -> int) : framebuffer =
  Array.init height (fun y ->
    Array.init width (fun x -> 
        f x y
      )
  ) 

let filled_circle (x : int) (y : int) (r : int) (col : int) (buffer : framebuffer) =
  let pminy = y - (r - 1)
  and pmaxy = y + (r - 1) 
  and fr = Float.of_int (r - 1) in
  let miny = if (pminy < 0) then 0 else pminy
  and maxy = if (pmaxy >= Array.length buffer) then ((Array.length buffer) - 1) else pmaxy in
  for yi = miny to maxy do
    let row = buffer.(yi) in
    let a = acos ((Float.of_int (yi - y)) /. fr) in
    let xw = Int.of_float ((sin a) *. fr) in
    let pminx = x - xw
    and pmaxx = x + xw in
    let minx = if (pminx < 0) then 0 else pminx
    and maxx = if (pmaxx >= Array.length row) then ((Array.length row) - 1) else pmaxx in
    for xi = minx to maxx do
      row.(xi) <- col
    done
  done

let draw_line (x0 : int) (y0 : int) (x1 : int) (y1 : int) (col : int) (buffer : framebuffer) =
  let dx = abs (x1 - x0)
  and sx = if x0 < x1 then 1 else -1
  and dy = (abs (y1 - y0)) * -1
  and sy = if y0 < y1 then 1 else -1 in
  let initial_error = dx + dy in
    
  let rec loop (x : int) (y : int) (error : int) = 
    buffer.(y).(x) <- col;
    match (x == x1) && (y == y1) with
    | true -> ()
    | false -> (
      let e2 = 2 * error in
      let nx = match e2 >= dy with
      | false -> x
      | true -> x + sx in
      let ny = match e2 <= dx with
      | false -> y
      | true -> y + sy in
      let nex = match (e2 >= dy) with
      | false -> 0
      | true -> dy in
      let ney = match (e2 <= dx) with
      | false -> 0
      | true -> dx in
      loop nx ny (error + nex + ney)
    )
  in loop x0 y0 initial_error
