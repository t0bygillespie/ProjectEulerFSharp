// The prime factors of 13195 are 5,7,13 and 29
// What is the largest prime factor of the number 600851475143

let max_prime_factor n =
    let factors m =
        { int64 (sqrt(double m)) .. -1L .. 2L }
        |> Seq.filter(fun i -> m % i = 0L)
    let is_prime m =
        Seq.isEmpty (factors m)
    factors n
    |> Seq.filter is_prime
    |> Seq.head

max_prime_factor 600851475143L
|> printf "Num: %d"