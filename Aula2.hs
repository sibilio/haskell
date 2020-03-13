module Aula2 where

data Dia = Domingo | Segunda | Terça | Quarta | Quinta | Sexta | Sábado deriving (Enum, Eq, Show, Ord)

-- Pattern Matching: descontroi os tipos, ou seja, expõe seu valor de modo a criar
-- maneiras diferentes de se manipular uma função. Nesse caso, usa-se o Pattern Matching como um case.
agenda :: Dia -> String
agenda Segunda = "Dia de tristeza"
agenda Terça  = "Dia do peixao na liberta"
agenda Quarta = "Dia de Haskell"
agenda Sexta = "Dia da maldade"
agenda _ = "Dia que ninguem liga"

agenda2 :: Dia -> Dia -> String
agenda2 Segunda _ = "Aqui e um teste de haskell"
agenda2 _ _ = "Vale qualquer coisa"

-- 1) Crie uma função toDia que transforma um inteiro em Dia. Sendo 1 para Aomingo
-- toDia é uma função parcial, não tem uma cobertura para todos os valores de entrada do Pattern Matching
toDia :: Int -> Either String Dia
toDia 1 = Right Domingo
toDia 2 = Right Segunda
toDia 3 = Right Terça
toDia 4 = Right Quarta
toDia 5 = Right Quinta
toDia 6 = Right Sexta
toDia 7 = Right Sábado
toDia _ = Left "Erro"

-- 2) Crie uma função toNum que converte Dia em inteiro onde 1 é Domingo
toNum :: Dia -> Int
toNum Domingo = 1
toNum Segunda = 2
toNum Terça = 3
toNum Quarta = 4
toNum Quinta = 5
toNum Sexta = 6
toNum Sábado = 7

-- 3) Faça o tipo Day contento os valores Sunday, ..., Saturday e crie uma função chamada traduzir que recebe um Dia e retorna um Day
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
traduzir :: Day -> Dia
traduzir Sunday = Domingo
traduzir Monday = Segunda
traduzir Tuesday = Terça
traduzir Wednesday = Quarta
traduzir Thursday = Quinta
traduzir Friday = Sexta
traduzir Saturday = Sábado

data Mes = Jan | Fev | Mar | Abr | Mai | Jun | Jul | Ago | Set | Out | Nov | Dez deriving Show

data Calendario = Calendario Int Dia Mes deriving Show

proxMes :: Calendario -> Calendario
proxMes (Calendario num dia Jan) = Calendario num dia Fev
proxMes (Calendario num dia Fev) = Calendario num dia Mar