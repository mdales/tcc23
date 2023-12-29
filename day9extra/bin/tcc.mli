
type screen = {
  width           : int ;
  height          : int ;
  scale           : int ;
  palette         : int list ;
}

val tcc_init: screen -> string -> (screen -> Framebuffer.framebuffer) -> (int -> screen -> Framebuffer.framebuffer -> Framebuffer.framebuffer) -> unit
