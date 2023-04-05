type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) (x : TimeOfDay) (y : TimeOfDay) = 
    let xHours =
    if x.f = "PM" then x.hours + 12
    else x.hours

    let yHours =
    if y.f = "PM" then y.hours + 12
    else y.hours
    { hours = xHours, minutes = x.minutes, f = x.f }
    >
    { hours = yHours, minutes = y.minutes, f = y.f }