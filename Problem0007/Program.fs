// By listing the first six prime numbers: 2,3,5,7,11 and 13, we can see that the 6th prime is 13.
// What is the 10_001st prime number?

open System.Collections

// Tight bound on the max value the nth prime can have 0_0
let upper_bound n = int (float n * (log (float n) + log (log (float n))))

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
    
let get_nth_prime n =
    let sieve_size = upper_bound n
    let complete_sieve = sieve sieve_size
    {1 .. sieve_size}
    |> Seq.filter complete_sieve.Get
    |> Seq.skip (n-1)
    |> Seq.head

get_nth_prime 10_001
|> printf "Num: %d"