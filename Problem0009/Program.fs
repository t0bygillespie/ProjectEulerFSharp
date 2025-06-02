// A Pythagorean triplet is a set of three natural numbers, a<b<c, for which, a^2 + b^2 = c^2
// For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
// There exists exactly one Pythagorean triplet for which a + b + c = 1000.
// Find the product abc.

let find_triplet_product =
    let condition_holds (a, b) =
        1000*a + 1000*b - 500_000 = a*b
        
    let a,b =
        Seq.allPairs {1 .. 1000} {1 .. 1000}
        |> Seq.filter condition_holds 
        |> Seq.head
            
    let c = (1000 - a - b)
    a * b * c
    
find_triplet_product
|> printf "%d"

