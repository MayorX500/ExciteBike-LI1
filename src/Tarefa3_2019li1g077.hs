{-

= Introdução
|
| A tarefa 3 tem como objetivo a partir de um mapa, diminuí-lo ao número mínimo de instruções que permitem construir o mapa.

= Objetivos
| |
| | * 1 - Determinar se as instruções horizontais eram menores que as verticais (e vice versa). 
| | * 2 - Executar as compressões segundo peças iguais ao longo de uma pista ou de um mapa.

= Conclusão
| Apesar de a taxa de compressão não ter sido muito elevada, pensamos que a ideia inicial foi boa mas a abordagem não foi apropriada mas ainda assim ainda conseguimos compimir bastante algumas pistas.

-}

-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g077 where

import LI11920
import Tarefa1_2019li1g077
-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [[[Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0]]
            ,[[Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Recta Lama 2,Rampa Relva 2 0,Recta Relva 0,Rampa Boost 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Boost 0,Rampa Boost 0 1,Recta Boost 1,Recta Terra 1,Recta Terra 1,Recta Relva 1],[Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Boost 0,Recta Terra 0,Recta Relva 0,Rampa Relva 0 2,Recta Relva 2,Rampa Terra 2 4,Rampa Terra 4 2,Recta Terra 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0,Recta Boost 0,Rampa Boost 0 1,Rampa Boost 1 0,Rampa Lama 0 2,Recta Relva 2,Rampa Boost 2 0,Recta Boost 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Lama 0,Recta Boost 0,Recta Lama 0]]
            ,[[Recta Terra 0,Recta Boost 0,Rampa Relva 0 2,Recta Boost 2,Recta Boost 2,Recta Boost 2,Rampa Terra 2 1,Recta Terra 1,Rampa Lama 1 0,Rampa Lama 0 2,Rampa Relva 2 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 1,Rampa Boost 1 0,Rampa Lama 0 1,Recta Lama 1,Rampa Lama 1 2,Rampa Lama 2 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Rampa Relva 0 1,Recta Boost 1,Rampa Boost 1 0],[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,Recta Lama 0,Recta Relva 0,Rampa Relva 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Rampa Terra 0 1,Recta Terra 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Rampa Boost 0 2,Rampa Relva 2 4,Recta Relva 4,Recta Relva 4,Recta Relva 4,Recta Relva 4,Rampa Relva 4 6,Rampa Relva 6 7,Recta Relva 7,Rampa Relva 7 5,Recta Terra 5,Rampa Terra 5 3,Rampa Relva 3 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Rampa Relva 0 2,Recta Relva 2,Rampa Relva 2 4,Rampa Terra 4 3,Recta Terra 3,Rampa Terra 3 1,Rampa Terra 1 0,Recta Lama 0,Recta Lama 0,Rampa Relva 0 2,Rampa Relva 2 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 1,Recta Lama 1,Rampa Lama 1 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Rampa Boost 0 1,Rampa Boost 1 0,Recta Boost 0,Recta Relva 0,Rampa Relva 0 1,Recta Relva 1,Rampa Terra 1 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Rampa Terra 1 3,Rampa Relva 3 1,Rampa Relva 1 0,Rampa Relva 0 2,Recta Terra 2,Rampa Lama 2 3,Rampa Relva 3 0,Rampa Lama 0 2,Rampa Lama 2 3,Rampa Relva 3 4,Recta Relva 4,Recta Relva 4,Rampa Lama 4 5,Rampa Lama 5 3]]
            ,[[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Lama 0,Recta Lama 0,Recta Lama 0],[Recta Terra 0,Recta Relva 0,Rampa Relva 0 2,Rampa Terra 2 0],[Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Rampa Terra 1 0],[Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Lama 0]]
            ,[[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]]]

-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
desconstroi :: Mapa -> Instrucoes
desconstroi mapa = if tamanhoInstrucoes (horizontal mapa) > tamanhoInstrucoes (vertical mapa)
  then vertical mapa
  else horizontal mapa

-- * Funções auxiliares da Tarefa 3.

-- | Transpose
-- Transposta de uma Matriz
transpose::[[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

-- | DropHead
-- remove o 1º elemento
dropHead :: Pista -> Pista
dropHead pista = drop 1 pista

-- * Funções para Horizontal

-- | TranslateTrack
-- Traduz uma Pista
translateTrack :: Int -> Pista -> Instrucoes
translateTrack _ [] = []
translateTrack g ((Recta x y):pecas) = (Anda [g] x):translateTrack g pecas
translateTrack g ((Rampa x i f):pecas) = if (i-f) > 0
                 then (Desce [g] x (i-f)):translateTrack g pecas
                 else (Sobe [g] x (f-i)):translateTrack g pecas

-- | HelptRack
-- Traduz uma pista sem o 1º elemento
helpTtRack :: Int -> Pista -> Instrucoes
helpTtRack g pista = translateTrack g (dropHead pista)

-- | TranslateMap
-- Traduz um mapa com a ajuda da helptRack
translateMap :: Int -> Mapa -> Instrucoes
translateMap _ [] = []
translateMap g (h:hs) = (helpTtRack g h) ++ translateMap (g+1) hs

-- |CountHowMany
-- verifica se existe a possibilidade de trocar uma ordem de pecas por repete numa pista
countHowMany :: Int -> Instrucoes -> (Int,Instrucao)
countHowMany i [x] = (i,x)
countHowMany i (h:y:ps)| h /= y = (i,h)
                       |h == y = countHowMany (i+1) (y:ps)
-- | CountHowManyTrack
--  a mesma coisa que o countHowMany mas para um mapa
countHowManyTrack :: Int -> Instrucoes -> [(Int,Instrucao)]
countHowManyTrack i [] = []
countHowManyTrack i [x] = [(i,x)]
countHowManyTrack i (h:t) = countHowMany i (h:t):(countHowManyTrack i (dropWhile (==h) t))

-- | AddRepete
-- Utiliza o resultado da countHowManyTrack para addicionar repetes numa lista
addRepete :: [(Int,Instrucao)] -> Instrucoes
addRepete [] = []
addRepete ((x,h):t)|x > 1 = [Repete x [h]] ++ addRepete t
                   |otherwise = [h] ++ addRepete t

-- * Funçoes para vertical

-- | translateTranspose
-- transforma uma pista numa lista de instruções mas do trasnposto do mapa
translateTranspose :: Int -> Pista -> Instrucoes
translateTranspose _ [] = []
translateTranspose g ((Recta x y):pecas) = (Anda [g] x):translateTranspose (g+1) pecas
translateTranspose g ((Rampa x i f):pecas) = if (i-f) > 0
                 then (Desce [g] x (i-f)):translateTranspose (g+1) pecas
                 else (Sobe [g] x (f-i)):translateTranspose (g+1) pecas

-- | translateTransposeMap
-- transforma uma Mapa numa lista de instruções mas do trasnposto do mapa
translateTransposeMap :: Int -> Int -> Mapa -> Instrucoes
translateTransposeMap _ _ [] = []
translateTransposeMap g a (h:hs) = (translateTranspose g h) ++ translateTransposeMap (a) (a) hs

-- | transposeMap
-- faz a transposta do mapa sem a 1ªa peça de cada pista (peca inicial [Recta Terra 0])
transposeMap :: Mapa -> Mapa
transposeMap (h:t) = transpose (map dropHead (h:t))

-- | transpose2Int
-- Transforma uma lista de Instrucoes em uma lista de 4 Ints (para facil comparaçao( ([(Instrução,pista,Terreno,altura)]) ))
transpose2Int :: Instrucoes -> Int -> [(Int,Int,Int,Int)]
transpose2Int [] _ = []
transpose2Int ((Anda [g] x):ps) num = (1,g,b,num):transpose2Int ps num
                                     where
                                       b=if x == Terra
                                         then 1
                                         else if x == Relva
                                           then 2
                                           else if x == Lama
                                             then 3
                                             else if x == Boost
                                               then 4
                                               else 5
transpose2Int ((Desce [g] x h):ps) num = (2,g,b,(h)):transpose2Int ps (num)
                                        where
                                          b=if x == Terra
                                            then 1
                                            else if x == Relva
                                              then 2
                                              else if x == Lama
                                                then 3
                                                else if x == Boost
                                                  then 4
                                                  else 5
transpose2Int ((Sobe [g] x h):ps) num = (3,g,b,(h)):transpose2Int ps (num)
                                        where
                                          b=if x == Terra
                                            then 1
                                            else if x == Relva
                                              then 2
                                              else if x == Lama
                                                then 3
                                                else if x == Boost
                                                  then 4
                                                  else 5

-- | ordVertical
-- Verifica se instruções em pistas diferentes sao as mesmas e junta as
ordVertical :: [(Int,Int,Int,Int)] -> [(Int,[Int],Int,Int)]
ordVertical [] = []
ordVertical [(x,y,z,d)] = [(x,[y],z,d)]
ordVertical ((x,y,z,d):(x1,y1,z1,d1):xs) | x==x1 && x == 1 && z == z1 && d == d1 = ((x,pos,z,d)):ordVertical (xs)
                                         |otherwise = (x,[y],z,d):ordVertical ((x1,y1,z1,d1):xs)
                                         where
                                           pos = [y,y1]

-- | ordVertical2nd
-- Verifica se instruções em pistas diferentes sao as mesmas e junta as
ordVertical2nd :: [(Int,[Int],Int,Int)] -> [(Int,[Int],Int,Int)]
ordVertical2nd [] = []
ordVertical2nd [(x,y,z,d)] = [(x,y,z,d)]
ordVertical2nd ((x,y,z,d):(x1,y1,z1,d1):xs) | x==x1 && x == 1 && z == z1 && d == d1 && helpOrdVertical y y1 == False = ((x,pos,z,d)):ordVertical2nd (xs)
                                        |otherwise = (x,y,z,d):ordVertical2nd ((x1,y1,z1,d1):xs)
                                        where
                                          pos = y ++ y1

-- | ordVertical3rd
-- Verifica se instruções em pistas diferentes sao as mesmas e junta as
ordVertical3rd :: [(Int,[Int],Int,Int)] -> [(Int,[Int],Int,Int)]
ordVertical3rd [] = []
ordVertical3rd [(x,y,z,d)] = [(x,y,z,d)]
ordVertical3rd ((x,y,z,d):(x1,y1,z1,d1):xs) | x==x1 && x == 1 && z == z1 && d == d1 && helpOrdVertical y y1 == False = ((x,pos,z,d)):ordVertical3rd (xs)
                                            |otherwise = (x,y,z,d):ordVertical3rd ((x1,y1,z1,d1):xs)
                                              where
                                              pos = y ++ y1

-- | ordVertical4th
-- Verifica se instruções em pistas diferentes sao as mesmas e junta as
ordVertical4th :: [(Int,[Int],Int,Int)] -> [(Int,[Int],Int,Int)]
ordVertical4th [] = []
ordVertical4th [(x,y,z,d)] = [(x,y,z,d)]
ordVertical4th ((x,y,z,d):(x1,y1,z1,d1):xs) | x==x1 && x == 1 && z == z1 && d == d1 && helpOrdVertical y y1 == False = ((x,pos,z,d)):ordVertical4th (xs)
                                            |otherwise = (x,y,z,d):ordVertical4th ((x1,y1,z1,d1):xs)
                                              where
                                               pos = y ++ y1

-- | helpOrdVertical
--
helpOrdVertical :: [Int] -> [Int] -> Bool
helpOrdVertical [] l = False
helpOrdVertical (x:xs) l = elem x l || helpOrdVertical xs l

-- | countHowManyVertical
-- verifica se existe a possibilidade de trocar uma ordem de pecas por repete numa pista (Modo Vertical)
countHowManyVertical :: Int -> [(Int,[Int],Int,Int)] -> (Int,(Int,[Int],Int,Int))
countHowManyVertical i [x] = (i,x)
countHowManyVertical i ((x,pos,y,d):(x1,pos1,y1,d1):ps)| (x,pos,y,d) /= (x1,pos1,y1,d1) = (i,(x,pos,y,d))
                       |(x,pos,y,d) == (x1,pos1,y1,d1) = countHowManyVertical (i+1) ((x1,pos1,y1,d1):ps)

-- | countHowManyTrackVertical
-- A Aplicação da Funçao acima indicada a uma lista de Instrucoes que traduzem um mapa
countHowManyTrackVertical :: Int -> [(Int,[Int],Int,Int)] -> [(Int,(Int,[Int],Int,Int))]
countHowManyTrackVertical i [] = []
countHowManyTrackVertical i [x] = [(i,x)]
countHowManyTrackVertical i (h:t) = countHowManyVertical i (h:t):(countHowManyTrackVertical i (dropWhile (==h) t))

-- | retranslateMap
-- transforma a lista de Ints de volta a instruções
retranslateMap :: [(Int,(Int,[Int],Int,Int))] -> [(Int,Instrucao)]
retranslateMap [] = []
retranslateMap ((i,(x,pos,y,d)):xs) | x == 1 = (i,(Anda pos translateFloor)):retranslateMap xs
                                    | x == 2 = (i,(Desce pos translateFloor d)):retranslateMap xs
                                    | x == 3 = (i,(Sobe pos translateFloor d)):retranslateMap xs
                                    where
                                      translateFloor = if y == 1
                                        then Terra
                                        else if y == 2
                                          then Relva
                                          else if y == 3
                                            then Lama
                                            else if y == 4
                                              then Boost
                                              else Cola

-- | addRepeateVertical
-- Addiciona os repetes com o resultado da funçao countHowManyTrackVertical
addRepeateVertical :: [(Int,Instrucao)] -> Instrucoes
addRepeateVertical [] = []
addRepeateVertical ((x,h):t)|x > 1 = [Repete x [h]] ++ addRepeateVertical t
                   |otherwise = [h] ++ addRepeateVertical t

-- | Vertical
-- tradução pelos padroes Verticais
vertical :: Mapa -> Instrucoes
vertical mapa = addRepeateVertical (retranslateMap (countHowManyTrackVertical 1(ordVertical4th (ordVertical3rd (ordVertical2nd (ordVertical (transpose2Int (translateTransposeMap 0 0 (transposeMap mapa)) 0)))))))

-- | Horizontal
-- tradução pelos padroes Horizontais
horizontal :: Mapa -> Instrucoes
horizontal mapa = addRepete (countHowManyTrack 1 (translateMap 0 mapa))
