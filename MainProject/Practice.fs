// 40.1
let rec sum (p : int -> bool, xs : int list) = 
    match (p, xs) with
    | p, [] -> 0
    | p, head :: tail when p head = true -> head + sum (p, tail)
    | p, _ :: tail -> sum (p, tail)

// 40.2.1
let rec count (xs : int list, n : int) : int = 
    match (xs, n) with
    | [], _ -> 0
    | head :: _, n when n < head -> 0
    | head :: tail, n when n = head -> 1 + count (tail, n)
    | _ :: tail, n -> count (tail, n)

// 40.2.2
let rec insert (xs : int list, n : int) : int list = 
    match (xs, n) with
    | [], n  -> [n]
    | head :: tail, n when n <= head -> n :: head :: tail
    | head :: tail, n  -> head :: insert(tail, n)

// 40.2.3
let rec intersect (xs1 : int list, xs2 : int list) : int list = 
    match (xs1, xs2) with
    | [], _ | _, [] -> []
    | head1 :: tail1, head2 :: tail2 when head1 > head2 -> intersect (xs1, tail2)
    | head1 :: tail1, head2 :: tail2 when head1 < head2 -> intersect (tail1, xs2)
    | head1 :: tail1, head2 :: tail2 -> head1 :: intersect (tail1, tail2)

// 40.2.4
let rec plus (xs1 : int list, xs2 : int list) : int list =
    match (xs1, xs2) with
    | [], xs2 -> xs2
    | head1 :: tail1, xs2 -> plus (tail1, insert (xs2, head1))

// 40.2.5
let rec minus (xs1 : int list, xs2 : int list) : int list = 
    match (xs1, xs2) with
    | [], _ -> []
    | _, [] -> xs1
    | head1 :: tail1, head2 :: _ when head1 < head2 -> head1 :: minus (tail1, xs2)
    | head1 :: _, head2 :: tail2 when head1 > head2 -> minus (xs1, tail2)
    | _ :: tail1, _ :: tail2 -> minus (tail1, tail2)

// 40.3.1
let min (x : int, y : int) : int =
    match (x, y) with
    | (x, y) when x > y -> y
    | (x, y) -> x

let rec smallest (x : int list) : int option = 
    match x with
    | [] -> None
    | [y] -> Some y
    | head :: tail -> Some (min (head, Option.get(smallest tail)))

// 40.3.2
let rec delete (n : int, xs : int list) : int list = 
    match (xs, n) with
    | [], n -> []
    | head :: tail, n when head <> n -> head :: delete (n, tail)
    | head :: tail, n -> tail

// 40.3.3
let rec sort (xs : int list) : int list = 
    let small = smallest xs
    match small with
    | None -> xs
    | Some(_) -> small.Value :: (delete (small.Value, xs) |> sort)

// 40.4
let rec revlist (xs : int list) : int list =
    match xs with
    | [] -> []
    | head :: tail -> revlist (tail) @ [head]

let rec revrev (xs : int list list) =
    match xs with
    | [] -> []
    | head :: tail -> revrev (tail) @ [revlist head]
