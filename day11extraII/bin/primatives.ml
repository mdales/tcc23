type point = {
    x : int ;
    y : int ;
}

type t = 
| Circle of point * float * int 
| Line of point * point * int

