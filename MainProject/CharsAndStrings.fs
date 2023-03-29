// 17.1
let rec pow (s, n) =
    match n with
    | 0 -> ""
    | n -> s + pow (s, n - 1)

// 17.2
let rec isIthChar (s, n, c) = 
    match n with
    | n when n >= String.length (s) || n < 0 || s.[n] <> c -> false
    | _ -> true

// 17.3
let rec occFromIth (s, n, c) = 
    match n with
    | n when n >= String.length (s) || n < 0 -> 0
    | n when s.[n] = c -> 1 + occFromIth (s, n + 1, c)
    | n -> occFromIth (s, n + 1, c)
    