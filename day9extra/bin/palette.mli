
type palette = int list

exception String_not_multiple_of_chunk_size

val generate_mono_palette: int -> palette

val generate_plasma_palette: int -> palette

val load_tic80_palette: string -> palette
