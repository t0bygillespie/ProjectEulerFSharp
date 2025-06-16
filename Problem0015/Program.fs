// How many right/down routes are there through a 20x20 grid?

open System

let factorial n =
    let rec loop i acc = 
        match i with
        | 0 | 1 -> acc
        | _ -> loop (i - 1) (acc * bigint i)
    loop n 1I

let numberOfPathsInNByMGrid n m =
    let totalSteps = n+m
    (factorial totalSteps) / ((factorial n) * (factorial (totalSteps - n)))
    
numberOfPathsInNByMGrid 20 20
|> printf "Num: %A"
