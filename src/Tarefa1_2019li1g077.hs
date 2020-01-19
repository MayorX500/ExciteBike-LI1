module Tarefa1_2019li1g077 where

import LI11920
import System.Random


-- * Testes

-- | Testes unitários da Tarefa 1.

-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(2,5,1),(3,5,1),(1,77,8),(2,77,8),(2,77,12),(10,445,213353657),(2,1,8),(4,0,3)]

-- * Funções pré-definidas da Tarefa 1.
-- | Funçao que devolve uma lista de Int's Aleatória

geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.
-- | Função que gera o Mapa

gera :: Int -> Int -> Int -> Mapa
gera 0 _ _ = []
gera np 1 s = [Recta Terra 0]: (gera (np-1) 1 s)
gera np c s = helptRackMap (splitList (c-1) (groupPair (geraAleatorios ((np *(c- 1))*2) s)))

-- | Função que agrupa em pares.

groupPair :: [Int] -> [(Int,Int)]
groupPair [x,y]  = [(x,y)]
groupPair (x:y:t) = ((x,y): groupPair t)

-- | Função que divide em pistas (de modod a obter o Mapa)

splitList :: Int -> [(Int,Int)] -> [[(Int,Int)]]
splitList n [] = []
splitList n l = (take n l) : splitList n (drop n l)

-- | Função que indica o tipo de chão de uma peça

fLoor :: Int -> Piso-> Piso
fLoor x lastF |x <= 1 = Terra
              |x <= 3 = Relva
              |x == 4 = Lama
              |x == 5 = Boost
              |x > 5 && x <= 9 = lastF

-- | adiciona o inicio a uma pista

tRack :: [(Int,Int)] -> Pista
tRack (x:y) = [Recta Terra 0] ++ helptRack Terra 0 (x:y)

-- | Troca a lista de Pares para uma pista

helptRack :: Piso -> Int -> [(Int,Int)] -> Pista
helptRack lastF h [] = []
helptRack lastF h ((x,y):xs) = if y <= 1
                                then (Rampa i h u): helptRack i u xs
                                else if y >= 2 && y <= 5
                                      then if h == 0
                                            then (Recta i h): helptRack i h xs
                                            else if (y - 1) <= h
                                                  then (Rampa i h d): (helptRack i d xs)
                                                  else (Rampa i h 0): helptRack i 0 xs
                                       else (Recta i h) : helptRack i h xs
                                                        where
                                                          u = (h  + (y + 1)) -- quanto sobe
                                                          d = (h - (y - 1)) -- quanto desce
                                                          i = (fLoor (x) lastF) --troca o chao com ajuda da fLoor

-- | efetua a funcao tRack a uma lista de listas de pares (listas de pistas)

helptRackMap :: [[(Int,Int)]] -> Mapa
helptRackMap [] = []
helptRackMap (h:t) = tRack h : helptRackMap t
