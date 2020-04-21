module Main where

import Text.Printf

f a b c x | x < 1 && c /= 0 = -a * x^2 - 2 * b
          | x > 3 && c == 0 = (x - a) / x
          | otherwise = -3 * x / b

main = do
    putStrLn "Enter the xFrom, xTo, xDelta, a, b, c:"
    [xFrom, xTo, xDelta, a, b, c] <- readWords 6
    putStrLn "result:"
    putStr $ unlines $ map (showXY $ f a b c) [xFrom,xFrom+xDelta..xTo]

readWords n = do
     values <- fmap (map read.words) getLine
     if length values == n then return values else do
         putStrLn ("sorry, could you give me exactly " ++ show n ++
                   " values, " ++ "separated by spaces?")
         readWords n

showXY :: (Double -> Double) -> Double -> String
showXY f x = printf "%.2f => %.2f" x (f x)
