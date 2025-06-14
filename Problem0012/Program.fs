// What is the value of the first triangle number to have over five hundred divisors?

open System

let countDivisors n =
    let ceilingSquareRoot =
        int (Math.Ceiling (Math.Sqrt (float n)))
    {1..ceilingSquareRoot}
    |> Seq.filter (fun d -> n % d = 0)
    |> Seq.length
    |> fun n -> n*2

let triangleNumber i = (i*(i+1))/2
    
let firstTriangleNumberWith500Divisors =
    Seq.initInfinite (fun i -> triangleNumber (i+1))
    |> Seq.skipWhile (fun n -> (countDivisors n) < 500)
    |> Seq.head
    
firstTriangleNumberWith500Divisors
|> printf "Num: %d"
    