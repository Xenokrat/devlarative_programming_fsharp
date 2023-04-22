// 47.4.1
let f n = 
    if n = 0 then 1
    else 
        let mutable fact_res = 1
        List.iter (fun x -> (fact_res <- fact_res * x)) [ 1..n ]
        fact_res

// 47.4.2
// 0 1 1 2 3 5 8 13 21
// 0 1 2 3 4 5 6 7  8 
let fibo (n : int) : int = 
    if n = 0 then 0
    elif n = 1 then 1
    else
        let n1 = ref 0
        let n2 = ref 1
        let cnt = ref 0
        while ! cnt < n do
            let tmp = ! n1 + ! n2
            n1 := ! n2
            n2 := tmp
            cnt := ! cnt + 1
        ! n1
