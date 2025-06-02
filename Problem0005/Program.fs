//  2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

let rec gcd x y =
    match x,y with
    | x,0L -> x
    | _ -> gcd y (x % y)
    
let lcm x y =
    abs x * abs y / gcd x y

let seq_lcm nums =
    Seq.reduce lcm nums
    
{1L .. 20L}
|> seq_lcm
|> printf "Num: %d"

    