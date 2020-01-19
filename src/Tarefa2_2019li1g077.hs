-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g077 where

import LI11920
import Tarefa0_2019li1g077

-- * Testes

-- | Mapa de Testes.
testmap = [[Recta Terra 0,Recta Relva 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Relva 0],
           [Recta Terra 0,Recta Terra 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Relva 0,Recta Lama 0,Recta Lama 0,Recta Relva 0,Recta Lama 0,Recta Boost 0],
           [Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Lama 0,Rampa Lama 0 1,Recta Relva 1,Rampa Relva 1 3,Recta Terra 3,Rampa Lama 3 0,Recta Lama 0],
           [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Relva 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,Recta Lama 0]]


-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 2.005 0 5 (Chao True))]})
           ,(0,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 6 0 5 (Chao False))]})
           ,(0,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 2 4.005 0 5 (Chao True))]})
           ,(1,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 3 5.9 0 5 (Chao True)),(Jogador 3 5.9 0 5 (Chao True))]})
           ,(0,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 1 0 5 (Chao True))]})
           ,(0,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 1 0 5 (Chao True))]})
           ,(0,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 2 6.1 0 0 (Chao True))]})
           ,(0,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 2 5.1 0 0 (Chao True))]})
           ,(0,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 3 1.5 0 0 (Chao False))]})
           ,(1,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 0 0 0 (Chao True)),(Jogador 1 1 1 1 (Chao True))]})
           ,(0,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 0 0 0 (Chao True))]})
           ,(0,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 1.5 2 2 (Chao True))]})
           ,(0,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 0 0 0 (Chao True))]})
           ,(0,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 6.1 0 0 (Morto 1.0))]})
           ,(0,(Movimenta C),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 2 6.1 0 0 (Ar 5 0 0))]})
           ,(1,(Movimenta B),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 2.005 0 5 (Chao True)),(Jogador 0 2.005 0 5 (Chao True))]})
           ,(0,(Movimenta B),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 3 0 5 (Chao True))]})
           ,(0,(Movimenta B),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 4.005 0 5 (Chao True))]})
           ,(0,(Movimenta B),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 3 2 0 5 (Chao False))]})
           ,(0,(Movimenta B),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 3 3 0 0 (Chao True))]})
           ,(1,(Movimenta B),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 0 0 0 (Chao True)),(Jogador 1 1 1 1 (Chao True))]})
           ,(0,(Movimenta B),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 3 0 0 0 (Chao True))]})
           ,(0,(Movimenta B),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 3 2.5 2 2 (Chao True))]})
           ,(0,(Movimenta B),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 0 0 0 (Chao True))]})
           ,(0,(Movimenta B),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 6.1 0 0 (Morto 1.0))]})
           ,(0,(Movimenta B),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 2 6.1 0 0 (Ar 5 0 0))]})
           ,(0,(Movimenta B),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 6 0 0 (Chao True))]})
           ,(1,(Movimenta D),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 1.5 0 5 (Ar 2 0 0)),(Jogador 0 1.5 0 5 (Ar 2 30 0))]})
           ,(0,(Movimenta D),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 1.5 0 5 (Ar 2 (-89) 0))]})
           ,(0,(Movimenta D),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 2 5.1 0 0 (Chao True))]})
           ,(0,(Movimenta D),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 3 1.5 0 0 (Chao False))]})
           ,(0,(Movimenta D),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 1 0 5 (Morto 1.0))]})
           ,(0,(Movimenta E),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 2.5 0 5 (Ar 5 89 0))]})
           ,(1,(Movimenta E),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 2.5 0 5 (Ar 5 0 0)),(Jogador 0 2.5 0 5 (Ar 5 30 0))]})
           ,(0,(Movimenta E),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 2 5.1 0 0 (Chao True))]})
           ,(0,(Movimenta E),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 3 1.5 0 0 (Chao False))]})
           ,(0,(Movimenta E),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 1 0 5 (Morto 1.0))]})
           ,(0,(Acelera),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 4 2 3 (Chao False))]})
           ,(1,(Acelera),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 4 2 3 (Chao False)),(Jogador 0 4 2 3 (Chao True))]})
           ,(0,(Acelera),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 1 0 5 (Morto 1.0))]})
           ,(0,(Acelera),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 1 0 5 (Ar 5 0 0))]})
           ,(0,(Desacelera),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 4 2 3 (Chao False))]})
           ,(1,(Desacelera),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 4 2 3 (Chao False)),(Jogador 0 4 2 3 (Chao True))]})
           ,(0,(Desacelera),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 1 0 5 (Morto 1.0))]})
           ,(0,(Desacelera),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 1 0 5 (Ar 5 0 0))]})
           ,(1,(Dispara),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 2.5 0 0 (Chao False)),(Jogador 0 2.5 0 0 (Chao True))]})
           ,(0,(Dispara),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 0 2.5 0 5 (Chao False))]})
           ,(0,(Dispara),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 2 0 5 (Morto 1.0))]})
           ,(0,(Dispara),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 2 0 5 (Ar 5 0 0))]})
           ,(0,(Dispara),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 0.5 0 5 (Chao True))]})
           ,(0,(Dispara),Estado {mapaEstado = testmap, jogadoresEstado = [(Jogador 1 2.5 0 5 (Chao True))]})]



-- * Funções principais da Tarefa 2.

-- | Dá a Altura na PosiçãoExata do Jogador

tallBoy :: Double -> Peca -> Double
tallBoy n (Recta _ a) = fromIntegral a
tallBoy n (Rampa _ i f) = let
  a = (fromIntegral (f-i))
  in (a * n) + (fromIntegral i)

-- | Funções auxiliares da funçãoo tallBoy

soX :: Jogador -> Double
soX (Jogador _ x _ _ _) = x
-- auxiliar
pBoy :: Double -> Double
pBoy x = x - (fromIntegral(floor x))

-- | Dá o angulo da rampa

anguloRampa :: Peca -> Double
anguloRampa (Recta _ i) = 0
anguloRampa (Rampa _ i f) = (atan (fromIntegral (f-i))*(180/pi))

-- | Muda o Angulo do Jogador (+15)

degUp :: Int -> Mapa -> [Jogador] -> [Jogador]
degUp 0 mapa ((Jogador p d v c (Ar a i g)):js) | i > 90 = degUp 0 mapa ((Jogador p d v c (Ar a (i - 180) g)):js)
                                               | i < (-90) = degUp 0 mapa ((Jogador p d v c (Ar a (i + 180) g)):js)
                                               | (i + 15) >= 90 = (Jogador p d v c (Ar a 90 g)):js
                                               | i >= (-90) = (Jogador p d v c (Ar a (i + 15) g)):js
degUp 0 mapa ((Jogador p d v c (Morto z)):js) = (Jogador p d v c (Morto z)):js
degUp 0 mapa ((Jogador p d v c (Chao z)):js) = (Jogador p d v c (Chao z)):js
degUp j mapa ((Jogador p d v c e):js) = (Jogador p d v c e): degUp (j-1) mapa js

-- | Muda o Angulo do Jogador (-15)

degDown :: Int -> Mapa -> [Jogador] -> [Jogador]
degDown 0 mapa ((Jogador p d v c (Ar a i g)):js) | i <  (-90) = degDown 0 mapa ((Jogador p d v c (Ar a (i + 180) g)):js)
                                                 | i >  90 = degDown 0 mapa ((Jogador p d v c (Ar a (i - 180) g)):js)
                                                 | (i-15) <= (-90) = (Jogador p d v c (Ar a (-90) g)):js
                                                 | i <= 90 = (Jogador p d v c (Ar a (i - 15) g)):js

degDown 0 mapa ((Jogador p d v c (Morto z)):js) = (Jogador p d v c (Morto z)):js
degDown 0 mapa ((Jogador p d v c (Chao z)):js) = (Jogador p d v c (Chao z)):js
degDown j mapa ((Jogador p d v c e):js) = (Jogador p d v c e): degDown (j-1) mapa js

-- | Dá a Posição do jogador (x,y)

whereBoy :: Jogador -> (Int,Int)
whereBoy (Jogador x y _ _ _) = (x,(floor y))

-- | last location of player
whereBoyB4 :: Jogador -> (Int,Int)
whereBoyB4 (Jogador x y _ _ _) = (x,((floor y)-1))

-- | Dá a Peça antes do jogador

pecaB4 :: (Int,Int) -> Mapa -> Peca
pecaB4 (x1,0) mapa = encontraPosicaoMatriz (x1,0) mapa
pecaB4 (x1,y1) mapa = encontraPosicaoMatriz ((x1),y1-1) mapa

-- | Dá a Peça da pista acima

pecaUp :: (Int,Int) -> Mapa -> Peca
pecaUp (0,y1) mapa = encontraPosicaoMatriz (0,y1) mapa
pecaUp (x1,y1) mapa = encontraPosicaoMatriz ((x1-1),y1) mapa

-- | auxiliares
pecaDown :: (Int,Int) -> Mapa -> Peca
pecaDown (x1,y1) mapa |(x1+1) <= length mapa = encontraPosicaoMatriz ((x1+1),y1) mapa
                      |otherwise = encontraPosicaoMatriz (x1,y1) mapa

-- | Dá um mapaa partir de um estado

mapEstado :: Estado -> Mapa
mapEstado (Estado mapa jogador) = mapa

-- | Dá os jogadores a partir de um estado

whichJogador :: Estado -> [Jogador]
whichJogador (Estado mapa (j:js)) = (j:js)

-- | auxiliares

uShallJoin :: Mapa -> [Jogador] -> Estado
uShallJoin mapa jog = (Estado {mapaEstado = mapa, jogadoresEstado = jog})

-- | Funçao Acelera

speedPlayer :: Int -> [Jogador] -> [Jogador]
speedPlayer 0 ((Jogador p d v c (Morto z)):js) = ((Jogador p d v c (Morto z)):js)
speedPlayer 0 ((Jogador p d v c (Ar a i g)):js) = ((Jogador p d v c (Ar a i g)):js)
speedPlayer 0 ((Jogador p d v c (Chao x)):js) | x == True = ((Jogador p d v c (Chao True)):js)
                                                | otherwise = ((Jogador p d v c (Chao True)):js)
speedPlayer i ((Jogador p d v c e):js) = (Jogador p d v c e): speedPlayer (i-1) js

-- | Funçao Desacelera
slowPlayer :: Int -> [Jogador] -> [Jogador]
slowPlayer 0 ((Jogador p d v c (Morto z)):js) = ((Jogador p d v c (Morto z)):js)
slowPlayer 0 ((Jogador p d v c (Ar a i g)):js) = ((Jogador p d v c (Ar a i g)):js)
slowPlayer 0 ((Jogador p d v c (Chao x)):js) | x == False = ((Jogador p d v c (Chao False)):js)
                                             | otherwise = ((Jogador p d v c (Chao False)):js)
slowPlayer i ((Jogador p d v c e):js) = (Jogador p d v c e): slowPlayer (i-1) js

-- | Função Dispara
glueMinus :: Int -> Mapa -> [Jogador] -> [Jogador]
glueMinus 0 mapa ((Jogador p f v c (Morto 1)):js) = ((Jogador p f v c (Morto 1)):js)
glueMinus 0 mapa ((Jogador p d v c (Ar a k g)):js) = ((Jogador p d v c (Ar a k g)):js)
glueMinus 0 mapa ((Jogador p d v c (Chao x)):js) =
  let b4 = pecaB4 (whereBoy (Jogador p d v c (Chao x))) mapa
  in
    case b4  of
      (Recta ç y) | c > 0 && d > 1 -> ((Jogador p d v (c-1) (Chao x)):js)
      (Rampa ç i f) | c > 0 && d > 1 -> ((Jogador p d v (c-1) (Chao x)):js)
      otherwise -> ((Jogador p d v c (Chao x)):js)
glueMinus i mapa ((Jogador p d v c e):js) = (Jogador p d v c e):glueMinus (i-1) mapa js

-- | Troca o chao quando se efectua o disparo

swapFloor :: Int -> Mapa -> [Jogador] -> Mapa
swapFloor 0 mapa ((Jogador p d v c (Morto t)):js) = mapa
swapFloor 0 mapa ((Jogador p d v c (Ar a k g)):js) = mapa
swapFloor 0 mapa ((Jogador p d v c (Chao x)):js) =
  let b4 = pecaB4 (whereBoy (Jogador p d v c (Chao x))) mapa
  in
    case b4  of
      (Recta ç y) | c > 0 && d > 1 -> atualizaPosicaoMatriz (whereBoyB4 (Jogador p d v c (Chao x))) (Recta Cola y) mapa
      (Rampa ç i f) | c > 0 && d > 1 -> atualizaPosicaoMatriz (whereBoyB4 (Jogador p d v c (Chao x))) (Rampa Cola i f) mapa
      otherwise -> mapa
swapFloor i mapa ((Jogador p d v c e):js) = swapFloor (i-1) mapa js

-- | Funçao [Movimento C] (Up)

trackUp :: Int -> Mapa -> [Jogador] -> [Jogador]
trackUp 0 mapa ((Jogador p d v c (Morto z)):js) = ((Jogador p d v c (Morto z)):js)
trackUp 0 mapa ((Jogador p d v c (Ar a i g)):js) = ((Jogador p d v c (Ar a i g)):js)
trackUp 0 mapa ((Jogador p d v c e):js) | ePosicaoMatrizValida (whereBoy (Jogador (p - 1) d v c e)) mapa == False = (Jogador p d v c e):js
                                        |(a1-a2) > 0.2 = (Jogador (p - 1) d v c (Ar {inclinacaoJogador = w, gravidadeJogador = 0, alturaJogador = a1})):js
                                        |(a1-a2) < (-0.2) = (Jogador p d 0 c (Morto 1)):js
                                        |(a1-a2) >= (-0.2) && (a1-a2) <= 0.2 = (Jogador (p - 1) d v c e):js

                                          where
                                            a1 = tallBoy (pBoy (soX (Jogador p d v c e))) (encontraPosicaoMatriz (whereBoy (Jogador p d v c e)) mapa)
                                            a2 = tallBoy (pBoy (soX (Jogador p d v c e))) (pecaUp (whereBoy (Jogador p d v c e)) mapa)
                                            w = anguloRampa (encontraPosicaoMatriz (whereBoy (Jogador p d v c e)) mapa)
trackUp i mapa ((Jogador p d v c e):js) = (Jogador p d v c e): trackUp (i-1) mapa (js)

-- | Funçao [Movimenta B] (Down)

trackDown :: Int -> Mapa -> [Jogador] -> [Jogador]
trackDown 0 mapa ((Jogador p d v c (Morto z)):js) = ((Jogador p d v c (Morto z)):js)
trackDown 0 mapa ((Jogador p d v c (Ar a i g)):js) = ((Jogador p d v c (Ar a i g)):js)
trackDown 0 mapa ((Jogador p d v c e):js) |ePosicaoMatrizValida (whereBoy (Jogador (p + 1) d v c e)) mapa == False = (Jogador p d v c e):js
                                          |(a1-a2) > 0.2 = (Jogador (p + 1) d v c (Ar {inclinacaoJogador = w, gravidadeJogador = 0, alturaJogador = a1})):js
                                          |(a1-a2) < (-0.2) = (Jogador p d 0 c (Morto {timeoutJogador = 1})):js
                                          |(a1-a2) >= (-0.2) && (a1-a2) <= 0.2 = (Jogador (p + 1) d v c e):js
                                            where
                                              a1 = tallBoy (pBoy (soX (Jogador p d v c e))) (encontraPosicaoMatriz (whereBoy (Jogador p d v c e)) mapa)
                                              a2 = tallBoy (pBoy (soX (Jogador p d v c e))) (pecaDown (whereBoy (Jogador p d v c e)) mapa)
                                              w = anguloRampa (encontraPosicaoMatriz (whereBoy (Jogador p d v c e)) mapa)
trackDown i mapa ((Jogador p d v c e):js) = (Jogador p d v c e): trackDown (i-1) mapa (js)

-- | Efetua uma jogada.

jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.

jogada i move stat |move == (Movimenta C) = uShallJoin (mapEstado stat) (trackUp i (mapEstado stat) (whichJogador stat))
                   |move == (Movimenta B) = uShallJoin (mapEstado stat) (trackDown i (mapEstado stat) (whichJogador stat))
                   |move == (Movimenta E) = uShallJoin (mapEstado stat) (degUp i (mapEstado stat) (whichJogador stat))
                   |move == (Movimenta D) = uShallJoin (mapEstado stat) (degDown i (mapEstado stat) (whichJogador stat))
                   |move == Dispara = uShallJoin (swapFloor i (mapEstado stat) (whichJogador stat)) (glueMinus i (mapEstado stat) (whichJogador stat))
                   |move == Acelera = uShallJoin (mapEstado stat) (speedPlayer i (whichJogador stat))
                   |move == Desacelera = uShallJoin (mapEstado stat) (slowPlayer i (whichJogador stat))
