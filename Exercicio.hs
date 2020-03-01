module Exercicio where

soma :: Int -> Int -> Int
soma a b = a + b

mult :: Int -> Int -> Int
mult a b = a * b

hello :: [Char] -> [Char]
hello palavra = "bem vindo ao haskell " ++ palavra

nome :: [Char]
nome = "Marcos Sibilio"

dobroLista :: [Int] -> [Int]
dobroLista xs = [2 * x | x <- xs]

lista1 :: [Int]
lista1 = [2 * x + 1 | x <- [0..10], x /= 5, x > 3]