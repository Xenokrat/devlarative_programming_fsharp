// 16.1
let notDivisible (n, m) =
    m % n = 0

// 16.2
let prime n =
    let rec recPrime (n, m) =
        let div = n % m
        match (m, div) with
        | (m, 0) -> false
        | (2, div) -> true
        | _ -> recPrime (n, m - 1)

    match n with
    | 0 | 1 -> false
    | 2 | 3 -> true
    | _ -> recPrime (n, n / 2)