
type framebuffer = int array array

val init: int -> int -> (int -> int -> int) -> framebuffer

val filled_circle: int -> int -> int -> int -> framebuffer -> unit

val draw_line: int -> int -> int -> int -> int -> framebuffer -> unit
