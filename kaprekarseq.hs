import Data.List
import Data.Char

numtodig x = if x < 10 then [x] else (rem x 10) : (numtodig (quot x 10))

descending x = reverse(sort(numtodig x))
ascending x = sort(numtodig x)

digtonum a b =  10 * a + b

largest x = foldl1 digtonum (descending x)
smallest x = foldl1 digtonum (ascending x)

    
diff x  = largest x - smallest x 

kaprekar x = if diff x == x then [x] else [x] ++ kaprekar (diff x)


main = do
    putStrLn "Enter a number "
    n <- getLine
    let num = (read n :: Int)
    print(kaprekar num)





