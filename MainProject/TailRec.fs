// 48.4.1
let rec fibo1 (n : int) (n1 : int) (n2 : int) : int = 
    if   n = 0 then n2
    elif n = 1 then n1
    else fibo1 (n - 1) (n1 + n2) (n1)

// 48.4.2
let rec fibo2 (n : int) (c : int -> int) : int =
    match (n, c) with
    | (0, c) | (1, c) -> c n
    | _ -> fibo2 (n - 1) (fun n1 -> fibo2 (n - 2) (fun n2 -> c(n1 + n2)))

// 48.4.3
let rec bigList n k = 
    let rec bigListInner n k lst =
        match n with
        | 0 -> k(lst)
        | _ -> bigListInner (n - 1) k (1 :: lst)
    bigListInner n k []