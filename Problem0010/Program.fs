open System.Collections

let sieve n =
    let array = BitArray(n+1, true)
    array.Set(0, false)
    array.Set(1, false)
    
    let mark_multiples p =
        {p*p .. p .. n}
        |> Seq.iter (fun i -> array.Set(i, false))
        
    {2  .. int (sqrt (float n))}
    |> Seq.iter mark_multiples
    array
    
let sum_of_primes_less_than_n n =
    let complete_sieve = sieve n
    {2 .. n-1}
    |> Seq.filter complete_sieve.Get
    |> Seq.map bigint
    |> Seq.sum
    
sum_of_primes_less_than_n 2_000_000
|> printf "Num: %A"