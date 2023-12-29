
type screen = {
  width           : int ;
  height          : int ;
  scale           : int ;
  palette         : int list ;
}

val tcc_init: screen -> string -> (screen -> Framebuffer.t) -> (int -> screen -> Framebuffer.t -> Framebuffer.t) -> unit
