// A palindromic number reads the same both ways. The largest palindrome made from the product of two -digit numbers is 9009 = 91 * 99.
// Find the largest palindrome made from the product of two 3-digit numbers.

let is_palindrome n =
    let rec reverseNumber x rev =
        if x = 0 then rev
        else reverseNumber (x / 10) (rev * 10 + x % 10)
    n = reverseNumber n 0

let max = 999
let min = 100

let is_a_product_of_two_three_digit_numbers x =
    {max .. -1 .. min}
    |> Seq.exists (fun y -> x % y = 0 && x / y >= min && x /y <= max)

{ max*max .. -1 .. min*min }
|> Seq.filter is_palindrome
|> Seq.filter is_a_product_of_two_three_digit_numbers
|> Seq.head
|> printf "Num: %d"