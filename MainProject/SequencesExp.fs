// 50.2.1
let fac_seq = 
    let rec fact n acc =
        match n with
        | 0 | 1 -> acc
        | _ -> fact (n - 1) (n * acc)
    seq {
        for i in Seq.initInfinite (fun x -> x) do
        yield fact i 1
    }

// 50.2.2
let seq_seq =
    seq {
        for i in Seq.initInfinite (fun x -> x) do
        if i % 2 = 0 
        then (i + 1) / 2 
        else -((i + 1) / 2)
    }