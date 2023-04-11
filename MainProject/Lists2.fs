// 39.1
// [ 0; 1; 2; 3; 4; 5; ] -> [ 1; 3; 5; ]
let rec rmodd = function
  | [] -> []
  | [x] -> []
  | head :: (head2 :: tail2 as tail) -> head2 :: rmodd tail2

// 39.2
// [ 0; 1; 2; 3; 4; 5; ] -> [ 1; 3; 5; ]
let rec del_even = function
  | [] -> []
  | head :: tail when head % 2 <> 0 -> head :: del_even tail
  | head :: tail -> del_even tail

// 39.3

let rec multiplicity x xs : int = 
  match (x, xs) with
  | x, [] -> 0
  | x, head :: tail when x = head -> 1 + multiplicity x tail
  | x, head :: tail -> multiplicity x tail

// 39.4
let rec split x =
  let rec split1 = function
    | [] -> []
    | [x] -> [x]
    | head1 :: (_ :: tail) -> head1 :: split1 tail
  let rec split2 = function
    | [] -> []
    | [x] -> []
    | _ :: (head2 :: tail) -> head2 :: split2 tail
  (split1 x, split2 x)

// 39.5
exception DiffLengthException
let rec zip (xs1, xs2) =
  if List.length xs1 <> List.length xs2 then raise DiffLengthException
  else
    match (xs1, xs2) with
    | [], [] -> []
    | (head1 :: tail1, head2 :: tail2) -> (head1, head2) :: zip (tail1, tail2)