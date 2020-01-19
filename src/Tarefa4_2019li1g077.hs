-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g077 where

import LI11920
import Tarefa0_2019li1g077
import Tarefa2_2019li1g077

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).

testmap4 = [[Recta Terra 0,Recta Relva 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Relva 0],
           [Recta Terra 0,Recta Terra 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Relva 0,Recta Lama 0,Recta Lama 0,Recta Relva 0,Recta Lama 0,Recta Boost 0],
           [Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Lama 0,Rampa Lama 0 1,Recta Relva 1,Rampa Relva 1 3,Recta Terra 3,Rampa Lama 3 0,Recta Lama 0],
           [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Relva 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,Recta Cola 0]]


testesT4 :: [(Double,Mapa,Jogador)]
testesT4 =  [(0.2 , testmap4 , (Jogador 0 5.5 5 5 (Chao True)))
            ,(0.4 , testmap4 , (Jogador 0 5.5 5 5 (Chao True)))
            ,(0.25 , testmap4 , (Jogador 2 2.2 5 5 (Chao True)))
            ,(0.8 , testmap4 , (Jogador 1 5.5 5 5 (Chao True)))
            ,(2.0 , testmap4 , (Jogador 1 2.7 5 5 (Chao True)))
            ,(0.2 , testmap4 , (Jogador 3 4.5 5 5 (Chao True)))
            ,(0.4 , testmap4 , (Jogador 0 3.9 5 5 (Chao True)))
            ,(0.2 , testmap4 , (Jogador 0 0 5 5 (Chao True)))]


-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera t mapa j = mudaMudaMuda j mapa t

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t m j = duality j m t


pecaAtual :: (Int,Int) -> Mapa -> Peca
pecaAtual (x1,y1) mapa = encontraPosicaoMatriz (x1,y1) mapa

statusPlayer :: Jogador -> EstadoJogador
statusPlayer (Jogador _ _ _ _ e) = e

anulaNegativa :: Double -> Double
anulaNegativa x = if x < 0
                then 0
                else x

descobrePiso :: Peca -> Piso
descobrePiso ( Recta x _ ) = x
descobrePiso ( Rampa x _ _ ) = x

mudaMudaMuda :: Jogador -> Mapa -> Double -> Jogador
mudaMudaMuda j@(Jogador p d v c e) mapa t = case statusPlayer j of
  (Ar x y g) -> (Jogador p d (anulaNegativa (calculaVelocidade j (descobrePiso (pecaAtual (whereBoy j) mapa )) t)) c (Ar x y (calculaGravidade j t)))
  (Morto x) -> (Jogador p d 0 c e)
  (Chao x) -> (Jogador p d (anulaNegativa (calculaVelocidade j (descobrePiso (pecaAtual (whereBoy j) mapa )) t)) c e)

calculaVelocidade :: Jogador -> Piso -> Double -> Double
calculaVelocidade (Jogador _ _ v _ (Ar _ _ _)) _ t = v -(( (0.125) * v) * t)
calculaVelocidade (Jogador _ _ v _ (Chao e)) piso t = v + (accelMota - (atrito piso) * v) * t
                                               where
                                                accelMota = if v < 2 && e then 1 else 0

atrito :: Piso -> Double
atrito piso = case piso of Terra -> 0.25
                           Relva -> 0.75
                           Lama  -> 1.50
                           Boost -> (-0.50)
                           Cola  -> 3.00

calculaGravidade :: Jogador -> Double -> Double
calculaGravidade (Jogador _ _ _ _ (Ar _ _ g)) t = g+(1*t)

vetorGravidade :: Jogador -> Vetor
vetorGravidade (Jogador _ _ _ _ (Ar _ _ g )) = (Polar g 270)

pontoPlayer :: Jogador -> Ponto
pontoPlayer (Jogador _ d _ _ (Ar x _ _)) = (Cartesiano d x)


pontosRampa :: Peca -> Jogador -> Reta
pontosRampa (Recta _ y) (Jogador _ d _ _ _) = ((Cartesiano (fromIntegral(floor d)) (fromIntegral y)),(Cartesiano ((fromIntegral(floor d))+1) (fromIntegral y)))
pontosRampa (Rampa _ i f) (Jogador _ d _ _ _) = ((Cartesiano (fromIntegral(floor d)) (fromIntegral i)),(Cartesiano ((fromIntegral(floor d))+1) (fromIntegral f)))

vecSpeed :: Jogador -> Peca -> Double -> Vetor
vecSpeed j p t = multiplicaVetor t $ Polar (velocidadeJogador j) (anguloRampa p)

whereisEndPiece :: Jogador -> (Double,Double)
whereisEndPiece (Jogador _ d _ _ _) | isInt d = (d+1,1000)
                                    | otherwise = ((fromIntegral (floor d))+1 , 1000)

isInt ::Double -> Bool
isInt x = x == fromInteger (round x)

buildThatWall :: (Double,Double) -> Reta
buildThatWall (x,y) = ((Cartesiano x (-10)),(Cartesiano x y))

airWalk :: Jogador -> Double -> Vetor
airWalk (Jogador _ _ v _ (Ar _ inc g)) t = multiplicaVetor t $ somaVetores (Polar v inc) (Polar g (-90))

-- |go2Ramp
-- verifica se o jogador fica abaixo do chao

go2Ramp :: Reta -> Reta -> Reta -> Ponto
go2Ramp (ca1@(Cartesiano x1 y1), ca2@(Cartesiano x2 y2)) (ca3@(Cartesiano x3 y3), ca4@(Cartesiano x4 y4)) (ca5@(Cartesiano x5 y5), ca6@(Cartesiano x6 y6))
  | intersetam (ca1, ca2) (ca5, ca6) = intersecao (ca1, ca2) (ca5, ca6)
  | intersetam (ca1, ca2) (ca3, ca4) = intersecao (ca1, ca2) (ca3, ca4)
  | otherwise = ca2

makeCartesianoGreatAgain :: Ponto -> Jogador -> Jogador
makeCartesianoGreatAgain (Cartesiano x1 y1) (Jogador p _ v c (Ar _ y g)) = Jogador p x1 v c (Ar y1 y g)

putinRamp :: Jogador -> Peca -> Ponto -> Jogador
putinRamp j peca ponto = placeInFloor (makeCartesianoGreatAgain ponto j) peca

gimiCoordinates :: Jogador -> Peca -> Ponto
gimiCoordinates (Jogador p d v c e) peca = (Cartesiano d $ tallBoy (soX (Jogador p d v c e)) peca)

goes2End :: Jogador -> Peca -> Double -> Ponto
goes2End j@(Jogador p d v c e) peca1 t = if intersetam ((gimiCoordinates j peca1) ,(somaVetores (gimiCoordinates j peca1) (vecSpeed j peca1 t))) (buildThatWall (whereisEndPiece j))
  then intersecao ((gimiCoordinates j peca1) , (somaVetores (gimiCoordinates j peca1) (vecSpeed j peca1 t))) (buildThatWall (whereisEndPiece j))
  else (somaVetores (gimiCoordinates j peca1) (vecSpeed j peca1 t))

placeInFloor :: Jogador -> Peca -> Jogador
placeInFloor j@(Jogador p d v c (Ar x y g)) ra1@(Rampa c1 i f) | (tallBoy (soX j) ra1) == x && (abs((anguloRampa ra1)-(y))) < 45 = (Jogador p d v c (Chao False))
                                                               | (tallBoy (soX j) ra1) /= x = j
                                                               | otherwise = (Jogador p d 0 c (Morto 2))
placeInFloor j@(Jogador p d v c (Ar x y g)) (Recta _ ce) | (round x) == ce && abs y < 45 =(Jogador p d v c (Chao False))
                                                         | (round x) /= ce = j
                                                         | (round x) == ce = j
                                                         | otherwise = (Jogador p d 0 c (Morto 3))

givedistance :: Ponto -> Double
givedistance (Cartesiano x _) = x

giveheight :: Ponto -> Double
giveheight (Cartesiano _ y) = y

-- |isKill
-- verifica se o jogador continua morto

isKill :: Jogador -> Double -> Jogador
isKill (Jogador p d v c (Morto x)) t | (x-t) <= 0 = (Jogador p d 0 c (Chao False))
                                     | otherwise = (Jogador p d 0 c (Morto (x-t)))

whereBoyAfter :: Jogador -> (Int,Int)
whereBoyAfter (Jogador x y _ _ _) = (x,((floor y)+1))

stillRamp :: Jogador -> Peca -> Peca  -> Double -> Jogador
stillRamp j@(Jogador p d v c e) ra1@(Rampa p1 i1 f1) (Rampa _ _ f2) t
    | giveheight (goes2End j ra1 t) > fromIntegral f2 && givedistance (goes2End j ra1 t) == fromIntegral(ceiling (givedistance (goes2End j ra1 t))) = Jogador p ((givedistance (goes2End j ra1 t ))) v c (e)
    | giveheight (goes2End j ra1 t) < fromIntegral f2 || givedistance (goes2End j ra1 t) /= fromIntegral(ceiling (givedistance (goes2End j ra1 t))) = Jogador p ((givedistance (goes2End j ra1 t ))) v c e
stillRamp j@(Jogador p d v c e) ra1@(Rampa p1 i1 f1) (Recta p2 f2) t
    | i1 < f1 = Jogador p ((givedistance (goes2End j ra1 t ))) v c e
    | otherwise = Jogador p ((givedistance (goes2End j ra1 t ))) v c e
stillRamp (Jogador p d v c e) (Recta p1 f1) (Recta p2 f2) t = Jogador p ((givedistance (goes2End (Jogador p d v c e) (Recta p1 f1) t ))) v c e

stillRamp j@(Jogador p d v c e)  re1@(Recta p1 f1) (Rampa p2 i2 f2) t
    | f1 > f2 = Jogador p ((givedistance (goes2End j re1 t ))) v c e
    | otherwise = Jogador p ((givedistance (goes2End j re1 t ))) v c e

duality :: Jogador -> Mapa -> Double -> Jogador
duality j@(Jogador p d v c e) mapa t = case statusPlayer j of
  (Ar x y g) -> putinRamp j (pecaAtual (whereBoy j) mapa) $ go2Ramp ((pontoPlayer j),(somaVetores (pontoPlayer j) (airWalk j t))) (pontosRampa (pecaAtual (whereBoy j) mapa)j) $ buildThatWall (whereisEndPiece j)
  (Morto x)  -> isKill j t
  (Chao x)   -> stillRamp j (pecaAtual (whereBoy j) mapa ) (pecaAtual (whereBoyAfter j) mapa ) t
