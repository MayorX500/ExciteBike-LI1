module Tarefa0_2019li1g077 where

data Ponto = Cartesiano Double Double | Polar Double Angulo
  deriving (Show)

type Angulo = Double

type Vetor = Ponto

somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = Cartesiano (x1 + x2) (y1 + y2)
somaVetores v1 v2 = somaVetores (polar2cart v1) (polar2cart v2)

rad2deg :: Double -> Double
rad2deg x = x * (180/pi)

deg2rad :: Double -> Double
deg2rad a = a * (pi/180)

polar2cart :: Vetor -> Vetor
polar2cart (Polar r a) = Cartesiano (r * cos (deg2rad a)) (r * sin (deg2rad a))
polar2cart c@(Cartesiano x y) = c

subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = Cartesiano (x1 - x2) (y1 - y2)
subtraiVetores v1 v2 = subtraiVetores (polar2cart v1) (polar2cart v2)

multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor a (Cartesiano x1 y1) = Cartesiano (x1 * a) (y1 * a)
multiplicaVetor b v1= multiplicaVetor b (polar2cart v1)

type Reta = (Ponto,Ponto)

intersetam :: Reta -> Reta -> Bool
intersetam ((Cartesiano x1 y1), (Cartesiano x2 y2)) ((Cartesiano x3 y3), (Cartesiano x4 y4)) = if ta >= 0 && ta <= 1 && tb >= 0 && tb <= 1
  then True
  else False
    where
      ta = ((y3-y4) * (x1-x3) + (x4-x3) * (y1-y3)) / ((x4-x3) * (y1-y2) - (x1-x2) * (y4-y3))
      tb = ((y1-y2) * (x1-x3) + (x2-x1) * (y1-y3)) / ((x4-x3) * (y1-y2) - (x1-x2) * (y4-y3))
intersetam (p1,p2) (p3,p4) = intersetam (polar2cart p1, polar2cart p2) (polar2cart p3, polar2cart p4)

intersecao :: Reta -> Reta -> Ponto
intersecao ((Cartesiano x1 y1), (Cartesiano x2 y2)) ((Cartesiano x3 y3), (Cartesiano x4 y4)) = (Cartesiano (x1 + ta * (x2-x1)) (y1 + ta * (y2-y1)))
  where
      ta = ((y3-y4) * (x1-x3) + (x4-x3) * (y1-y3)) / ((x4-x3) * (y1-y2) - (x1-x2) * (y4-y3))

intersecao (p1,p2) (p3,p4) = intersecao (polar2cart p1, polar2cart p2) (polar2cart p3, polar2cart p4)

eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido i l = i<length l && i>=0

type DimensaoMatriz = (Int,Int)
type PosicaoMatriz = (Int,Int)
type Matriz a = [[a]]

dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz [] = (0,0)
dimensaoMatriz ([]:xs) = (0,0)
dimensaoMatriz (m:n) = (length (m:n) , length m)

ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool
ePosicaoMatrizValida (x1,y1) a = x1 >= 0 && y1 >= 0 && x1 <= (length a)-1 && y1 <= (length (a !! 0))-1

normalizaAngulo :: Angulo -> Angulo
normalizaAngulo x = if  x > 0 && x < 360
  then x
  else if x < 0
    then normalizaAngulo (x + 360)
    else if x > 0
      then normalizaAngulo (x - 360)
      else x

encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 (h:t) = h
encontraIndiceLista i (h:t) |i < (length (h:t) -1) = encontraIndiceLista (i-1) t
                            |i >= (length (h:t) -1) = last (h:t)

atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista 0 a (h:t) = (a:t)
atualizaIndiceLista x a (h:t) = h:(atualizaIndiceLista (x-1) a t)

encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (x1,y1) m = encontraIndiceLista y1 (encontraIndiceLista x1 m)

atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (0,y1) a (x:y) = (atualizaIndiceLista y1 a x):y
atualizaPosicaoMatriz (x1,y1) a (x:y) |x1>=0 && y1>=0 = x:(atualizaPosicaoMatriz ((x1-1), y1) a y)
