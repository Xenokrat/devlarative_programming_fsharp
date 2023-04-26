// 49.5.1
let even_seq = 
    Seq.filter (fun x -> x % 2 = 0) (Seq.initInfinite (fun x -> x + 1))

// 49.5.2
let fac_seq = 
    let rec fact n k =
        match n with
        | 0 | 1 -> k 1
        | _ -> fact (n - 1) (fun f -> k(f * n))
    Seq.initInfinite (fun x -> fact x id)

// 49.5.3
let seq_seq = 
    Seq.initInfinite (fun x -> 
        if x % 2 = 0 
        then (x + 1) / 2 
        else -((x + 1) / 2)
    )