let curry f = fun x y -> f((x : int), (y : int)) : int

let uncurry f ((x : int), (y : int)) : int = f x y