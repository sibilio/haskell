--2.1) Gere as listas:
module ExerciciosCap2 where

--a) [1, 11, 121, 1331, 14641, 161051, 1771561]
exA::[Int]
exA = [11 ^ x | x <- [0..6]]


--b) [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,
-- 25,26,27,29,30,31,33,34,35,37,38,39]
exB :: [Int]
exB = [x | x <- [1,2..39], (mod x 4) /= 0]


--c) ["AaBB", "AbBB", "AcBB", "AdBB", "AeBB", "AfBB",
--"AgBB"]
exC :: [String]
exC = ['A' : x : "BB" | x <- ['a', 'b'..'g']]


--d) [5,8,11,17,20,26,29,32,38,41]
exD :: [Int]
exD = [x+2 | x <- [3,6..39], (mod x 3) /= 0]


--e) [1.0,0.5,0.25,0.125,0.0625,0.03125]


--f) [1,10,19,28,37,46,55,64]
exF :: [Int]
exF = [x + 1 | x <- [0,9..63]] --não mostra resultado correto


--g) [2,4,8,10,12,16,18,22,24,28,30]


--h) ['@','A','C','D','E','G','J','L']
exH :: [[Char]]
exH = [[x] | x <- ['@', 'A'..'L']] --não encontrei padrão para remover elementos


--2.2) Crie uma função que verifique se o tamanho de uma
--String é par ou não. Use Bool como retorno.
stringPar :: String -> Bool
stringPar s = mod (length s) 2 == 0


--2.3) Escreva uma função que receba um vetor de Strings e
--retorne uma lista com todos os elementos em ordem reversa.
vetorInverso :: [String] -> [String]
vetorInverso s = [reverse x | x <- s]


--2.4) Escreva uma função que receba um vetor de Strings e
--retorne uma lista com o tamanho de cada String. As palavras de
--tamanho par devem ser excluídas da resposta.
vetorStringSemPar :: [String] -> [Int]
vetorStringSemPar vetor = [length x | x <- vetor, mod (length x) 2 /= 0]


--2.5) Escreva a função head como composição de duas outras.


--2.6) Faça uma função que receba uma String e retorne True se esta for um
--palíndromo; caso contrário, False.
palidromo :: String -> Bool
palidromo p = p == (reverse p)


--2.7 Faça uma função que receba um inteiro e retorne uma
--tupla, contendo: o dobro deste número na primeira coordenada, o
--triplo na segunda, o quádruplo na terceira e o quíntuplo na quarta.
tupla :: Int -> (Int, Int, Int, Int)
tupla n = (n * 2, n * 3, n * 4, n * 5)