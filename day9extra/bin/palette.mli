
type t = int list

exception String_not_multiple_of_chunk_size

val generate_mono_palette: int -> t

val generate_plasma_palette: int -> t

val load_tic80_palette: string -> t
