// 23.4.1
let (.+.) (x : int*int*int) (y : int*int*int) =
    let g1, s1, c1 = x 
    let g2, s2, c2 = y 
    let c3, rc3 = (c1 + c2) % 12, (c1 + c2) / 12
    let s3, rs3 = (s1 + s2 + rc3) % 20, (s1 + s2 + rc3) / 20
    let g3 = g1 + g2 + rs3
    g3, s3, c3

let (.-.) (x : int*int*int) (y : int*int*int) =
    
    let g1, s1, c1 = x
    let g2, s2, c2 = y 
    
    let c3  = abs ((c1 - c2) % 12)
    let rc3 = (c1 - c2) / 12

    let s3  = abs ((s1 - s2 + rc3) % 20)
    let rs3 = (s1 - s2 + rc3) / 20
    
    let g3  = abs (g1 - g2 + rc3)

    if x < y then
        -g3, -s3, -c3
    else g3, s3, c3

// 23.4.2
let (.+) (x : float*float) (y : float*float) =
    let (x1, x2) = x
    let (y1, y2) = y
    x1 + y1, x2 + y2

let (.-) (x : float*float) (y : float*float) =
    let (x1, x2) = x
    let (y1, y2) = y
    x1 - y1, x2 - y2

let (.*) (x : float*float) (y : float*float) =
    let (a, b) = x
    let (c, d) = y
    a*c - b*d, b*c + a*d

let (./) (x : float*float) (y : float*float) =
    let (a, b) = x
    let (c, d) = y
    let c1, d1 = c / (c*c + d*d), -d / (c*c + d*d)
    a*c1 - b*d1, b*c1 + a*d1