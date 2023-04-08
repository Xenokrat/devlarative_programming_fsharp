type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f : F }

let TimeOfDayTag = function
    | AM -> 0
    | PM -> 12

let (.>.) (x : TimeOfDay) (y : TimeOfDay) = 
    (x.hours + (TimeOfDayTag x.f)) * 60 + x.minutes > (y.hours + (TimeOfDayTag y.f)) * 60 + y.minutes