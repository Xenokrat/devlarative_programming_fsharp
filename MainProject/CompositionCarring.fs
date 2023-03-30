// 20.3.1
let vat n x = (float n / 100.0 + 1.0) * x

// 20.3.2
let unvat n x = x / (float n / 100.0 + 1.0)

// 20.3.3
let min f = 
    let rec min_rec (f, n) = 
        if f n = 0 then n
        else min_rec (f, n + 1)
    min_rec (f, 1)
    
