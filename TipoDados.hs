module TipoDados where

data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo

agenda :: Dia -> String
agenda Domingo = "Assistir TV"
agenda Sabado = "Festa"
agenda _ = "Trabalho"

data Pessoa = Fisica String Int | Juridica String

pessoa :: Pessoa -> (String, String)
pessoa (Fisica x y) = ("Nome: " ++ x, "Idade: " ++ show y)
pessoa (Juridica x) = ("Nome: " ++ x, "nao tem idade")

data Ponto = Ponto {xval, yval :: Double} deriving Show

distOrig p = sqrt(xval p**2 + yval p**2)