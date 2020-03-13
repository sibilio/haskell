-- 3.1) 
--Crie o tipo Pergunta com os values constructors Sim
-- ou Nao .
data Pergunta = Sim | Nao deriving (Show, Eq)

-- Faça as funções seguintes, determinando seus tipos
-- explicitamente:
-- pergNum: recebe via parâmetro uma Pergunta .
-- Retorna 0 para Nao e 1 para Sim .
pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

-- listPergs : recebe via parâmetro uma lista de
-- Perguntas , e retorna 0 s e 1 s correspondentes aos
-- constructores contidos na lista.
listPergs :: [Pergunta] -> [Int]
listPergs p = [pergNum x | x <- p]

-- and' : recebe duas Perguntas como parâmetro e
-- retorna a tabela verdade do and lógico, usando
-- Sim como verdadeiro e Nao como falso.
and' :: Pergunta -> Pergunta -> Pergunta
and' Sim Sim = Sim
and' Sim Nao = Nao
and' Nao Sim = Nao
and' Nao Nao = Nao

-- or' : idem ao anterior, porém deve ser usado o ou
-- lógico.
or' :: Pergunta -> Pergunta -> Pergunta
or' Sim Sim = Sim
or' Sim Nao = Sim
or' Nao Sim = Sim
or' Nao Nao = Nao

-- not' : idem aos anteriores, porém usando o not
-- lógico.
not' :: Pergunta -> Pergunta
not' Sim = Nao
not' Nao = Sim

-- Converte boolean para Pergunta
pergBool :: Bool -> Pergunta
pergBool True = Sim
pergBool False = Nao

andMelhorado :: Pergunta -> Pergunta -> Pergunta
andMelhorado x y = pergBool (x == Sim && y == Sim)

orMelhorado :: Pergunta -> Pergunta -> Pergunta
orMelhorado x y = pergBool (x == Sim || y == Sim)

-- 3.2) Faça o tipo Temperatura que pode ter valores Celsius ,
-- Farenheit ou Kelvin
data Temperatura = Celsius | Farenheit | Kelvin deriving (Show, Eq)

-- Implemente as funções:
-- converterCelsius: recebe um valor double e uma
-- temperatura, e faz a conversão para Celsius.
converterCelsius :: Double -> Temperatura -> Double
converterCelsius t Farenheit = (t - 32) * (5 / 9)
converterCelsius t Kelvin = t - 273.15
converterCelsius t Celsius = t

-- converterKelvin : recebe um valor Double e uma
-- temperatura, e faz a conversão para Kelvin.
converterKelvin :: Double -> Temperatura -> Double
converterKelvin t Celsius = t + 273.15
converterKelvin t Farenheit = ((t - 32) * 5 / 9) + 273.15
converterKelvin t Kelvin = t

-- converterFarenheit : recebe um valor doubleuma temperatura, e faz a conversão para Farenheit.
converterFarenheit :: Double -> Temperatura -> Double
converterFarenheit t Celsius = (t * 9 / 5) + 32
converterFarenheit t Farenheit = t
converterFarenheit t Kelvin = ((t - 273.15) * 9 / 5) + 32
