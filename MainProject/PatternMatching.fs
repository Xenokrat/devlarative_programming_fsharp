let days_in_month = function
    | 4 | 6 | 9 | 11 -> 30
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | 2 -> 28
    | _ -> 0