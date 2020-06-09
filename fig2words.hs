#!/usr/bin/env runhaskell

import qualified Data.Text
import qualified Data.Map.Strict as Map
import Data.Maybe

upto20 = ["", "one ", "two ", "three ", "four ", "five ", "six ", "seven ", "eight ", "nine ", "ten ", "eleven ", "twelve ", "thirteen ", "fourteen ", "fiften ", "sixteen ", "seventeen ", "eighteen ", "nineteen "]
tens = ["", "", "twenty ", "thirty ", "fourty ", "fifty ", "sixty ", "seventy ", "eighty ", "ninety "]

hun = "hundred "
han = "hundred and "

w_dValues = [10^9 , 10^6 , 10^3 , 1]
w_dNames = ["billion ", "million " , "thousand " ,""]
w_denomnames = Map.fromList $ (zip w_dValues w_dNames)



i_dValues = [10^7 , 10^5 , 10^3 , 1]
i_dNames = ["crore " , "lakh " , "thousand " , ""]
i_denomnames = Map.fromList $ (zip i_dValues i_dNames)

convert2digits :: Int -> String
convert2digits n | n < 20    = upto20 !! n
convert2digits n | otherwise = (tens !! (div n 10)) ++ (upto20 !! (mod n 10))

convert3digits :: Int -> String
convert3digits n | n < 100        = convert2digits n
convert3digits n | mod n 100 == 0 = convert2digits (div n 100) ++ hun
convert3digits n | otherwise      = convert2digits h ++ han ++ convert2digits tu where
    h = div n 100
    tu = mod n 100

split :: Int -> [Int] -> [(Int, Int)]
split amount denoms 
        | length denoms == 1 = (amount, head denoms) : []
        | otherwise          = (div amount $ head denoms , head denoms) : split (mod amount $ head denoms ) (tail denoms)

convert :: Ord k => (Int ,k) -> Map.Map k [Char] -> [Char]
convert element denomnames
    | fst element == 0 = ""
    | otherwise = (convert3digits $ fst element) ++ (fromJust(Map.lookup (snd element) denomnames))

fig2wordsIndian :: Int -> [Char]
fig2wordsIndian num = concat [convert x i_denomnames | x <- (split num i_dValues)]

fig2wordsWestern :: Int -> [Char]
fig2wordsWestern num = concat [convert x w_denomnames | x <- (split num w_dValues)]

main = do
    putStrLn "Enter the number :"
    n <- getLine
    let num = (read n :: Int)   
    print(fig2wordsIndian num)
    print(fig2wordsWestern num)

