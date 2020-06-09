
fizz_or_buzz :: Int -> String
fizz_or_buzz x = if x `mod` 3 == 0 then "fizz" else (if x `mod` 5 == 0 then "buzz" else show x)

--Either of the 2 implementations work

fizzbuzz :: [Int] -> [String]
fizzbuzz arr = [if x `mod` 15 == 0 then "fizzbuzz" else fizz_or_buzz x | x <- arr]

fizzbuzz x
    | mod x 15 == 0 = "fizzbuzz"
    | mod x 3 == 0 = "fizz"
    | mod x 5 == 0 = "buzz"
    | otherwise show x


    
main = do
    print(fizzbuzz [1..30])
