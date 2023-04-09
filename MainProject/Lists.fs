// 34.1
let upto (n : int) = 
  let rec uptoRec (n1 : int, l : int list) = 
    match (n1, l) with
    | n1, l when n1 < 0 -> n1, []
    | 0, l -> n, l
    | _ -> uptoRec (n1 - 1, n1 :: l)
  let _, l = uptoRec (n, [])
  l
  
// 34.2
let dnto (n : int) = 
  let rec dntoRec (n1 : int, l : int list) = 
    match (n1, l) with
    | n1, l when n1 = n -> n1, n1 :: l
    | _ -> dntoRec (n1 + 1, n1 :: l)
  if n < 1 then []
  else 
    let _, l = dntoRec (1, [])
    l


// 34.3
// [0, 2, 4, 6, 8] (5)
let evenn (n : int) =
  let rec evennRec (n1 : int, l : int list) = 
    match (n1, l) with
    | n1, l when n1 < 0 -> n1, []
    | 0, l -> n, l
    | _ -> evennRec (n1 - 1, ((n1 - 1) * 2) :: l)
  let _, l = evennRec (n, [])
  l