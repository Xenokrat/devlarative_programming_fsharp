// 42.3
let rec helperSubsets (n : int list) (k : int) =
    match (n, k) with
    | (_, 0) -> set [ Set.empty ]
    | ([], _) -> Set.empty
    | (x :: xs, k) -> Set.union (Set.map (fun y -> Set.add x y) (allSubsets xs (k-1))) (allSubsets xs k)

let rec allSubsets (n : int) (k : int) =
    helperSubsets [ 1..n ] k