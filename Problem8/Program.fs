// The four adjacent digits in the 1000-digit number (below) that have the greatest product are 9*9*8*9 = 5832.
// Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?

let digit_string = "73167176531330624919225119674426574742355349194934
                            96983520312774506326239578318016984801869478851843
                            85861560789112949495459501737958331952853208805511
                            12540698747158523863050715693290963295227443043557
                            66896648950445244523161731856403098711121722383113
                            62229893423380308135336276614282806444486645238749
                            30358907296290491560440772390713810515859307960866
                            70172427121883998797908792274921901699720888093776
                            65727333001053367881220235421809751254540594752243
                            52584907711670556013604839586446706324415722155397
                            53697817977846174064955149290862569321978468622482
                            83972241375657056057490261407972968652414535100474
                            82166370484403199890008895243450658541227588666881
                            16427171479924442928230863465674813919123162824586
                            17866458359124566529476545682848912883142607690042
                            24219022671055626321111109370544217506941658960408
                            07198403850962455444362981230987879927244284909188
                            84580156166097919133875499200524063689912560717606
                            05886116467109405077541002256983155200055935729725
                            71636269561882670428252483600823257530420752963450"
let digits =
    digit_string
    |> Seq.filter System.Char.IsDigit
    |> Seq.map (fun c -> int (c - '0'))

// We run a sliding window, our state is the non-zero product, and the number of zeros in the window.
// We cant just keep track of the product, as the zeros will require recomputing this if a zero exits the window.

let greatest_n_digit_product digits n =
    let update_state state lost_digit gained_digit =
        match lost_digit, gained_digit with
        | 0,0 -> state
        | 0,gained -> (fst state * int64 gained, snd state - 1)
        | lost,0 -> (fst state / int64 lost, snd state + 1)
        | lost, gained -> (fst state / int64 lost * int64 gained, snd state)

    let initial_state =
        ((1L,0), Seq.take n digits)
        ||> Seq.fold (fun state -> update_state state 1)
    
    let start_and_end_of_windows =
        Seq.zip digits (Seq.skip n digits)
        
    (initial_state, start_and_end_of_windows)
    ||> Seq.scan (fun state (lost,gained) -> update_state state lost gained)
    |> Seq.map (fun (product,zero_count) -> if zero_count > 0 then 0L else product)
    |> Seq.max

greatest_n_digit_product digits 13
|> printf "Num: %d"