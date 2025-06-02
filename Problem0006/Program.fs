// The sum of the squares of the first ten natural numbers is, 1^2 + 2^2 ... 10^2 = 385
// The square of the sum of the first ten natural numbers is, (1+2+...+10)^2 = 3025
// Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 2640.
// Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.


// The square of sums can be expanded to the sum of squares plus some extra terms.
// (1+2...+100)(1+2...+100) = 1^2 + 2^2 + ... + 100^2 + (1*2 + 1*3 + ... + 100*99)
// The sum of these extra terms is our desired answer, as it provides the difference between the sum of squares and the
// square of sums.
// By factoring out each of the numbers from their relevant terms in the difference terms, we get
// 1(2+3...+100) + 2(1+3+...+100) + ... + 100(1+2...+99)
// This is simply the sum from 1 to 100, without the coefficient, for each number 1 to 100. 


let difference_between_square_of_sums_and_sum_of_squares n =
    let sum_from_1_to_n =
        int64 n * (int64 n + 1L) / 2L
    {1 .. n}
    |> Seq.map (fun x -> ((sum_from_1_to_n) - int64 x) * int64 x )
    |> Seq.sum

difference_between_square_of_sums_and_sum_of_squares 100
|> printf "Num: %d"