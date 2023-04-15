// 41.4.1
let list_filter f xs =
    let list_folder x y =
        match (x, y) with
        | (x, y) when f x -> x :: y
        | _ -> y
    List.foldBack list_folder xs []

let sum (p : int -> bool, xs : int list) : int = 
    let list_folder = 
        fun x y -> if p y then x + y else x  
    List.fold list_folder 0 xs

// 41.4.3
let revrev xs =
    List.fold (fun head tail -> List.rev tail :: head) [] xs