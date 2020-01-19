{-

= Introdução
|
| A tarefa 6 tem como objetivo criar um 'bot', de maneira a que as decisões por ele tomadas resultem no mínimo de tempo que ele demora a percorrer a pista.

= Objetivos
| |
| | * 1 - Se o 'bot' tiver velocidade 0 então tem de Acelerar. 
| | * 2 - Se o 'bot' estiver morto não faz nada.
| | * 3 - Se o 'bot' estiver numa peça de cola ou a peça à sua frente for cola então muda para a pista mais eficiente (se possível).
| | * 4 - Se o 'bot' tiver outro jogador na peça antes dele, ou quando passar por um boost então vai disparar.
| | * 5 - Se o 'bot' estiver no Ar vai ajustar a sua inclinação de modo a alcançar a maior distância ppossível
| | * 6 - Se o 'bot' estiver no Chão vai optar pela pista mais eficiente.
! ! * 7 - Se o 'bot' não obedecer a nenhum dos casos previstos então vai Acelerar


= Conclusão
|
| Ao ver alguns torneios realizados pelo 'bot' verificamos que o 'bot' já se desviava das peças de pior atrito em alguns casos, disparava nas situações esperadas e ajustava o seu ângulo quando estava no ar. 
| Enfrentamos algumas dificuldades no ângulo do 'bot' no ar e quando o este se encontrava nas pistas da extremidade do mapa não tomava as melhores decisões, mas conseguimos ultrapassar essas dificuldades.
| Em suma, apesar dos problemas enfrentados, achamos que temos um 'bot' bom que possivelmente pode só não estar tão bom no ângulo do ar mais eficiente. 

-}

-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g077 where

import LI11920
import Tarefa0_2019li1g077
import Tarefa2_2019li1g077
import Tarefa4_2019li1g077


-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot i (Estado mapa lstjog)  | (velocidadeJogador j)==0 = Just Acelera 
                            | checkMorte j = Nothing  --Se o 'bot' estiver morto não faz nada
                            | evitaCola atritoAtual || evitaCola atritoAtualup25 = moveEvitaCola j mapa
                            | (checkCola j) && prj && pertoSuficiente || eliminaCola==(-0.50) = Just Dispara -- Se tem disparos e tem jogadores na mesma pista atrás do 'bot' ou se a peça atrás é um Boost então dispara
                            | testaAr j = ajustaAngulo j mapa -- Se o 'bot' estiver no ar ajusta a sua inclinação se necessário
                            | testaChao j = moveEficiente j mapa
                            | otherwise = Just Acelera
                         where
                            j = lstjog !! i
                            prBot = j:(ajusta j lstjog)
                            prj = potencialAtaque $ distEpista prBot
                            eliminaCola = findAtrito $ pecaB4 posAtual mapa
                            pertoSuficiente = procuraAtras (distEpista [j]) $ procuraMsmPista $ distEpista prBot
                            altCertaUp = (abs(a1-a2))<=0.2
                            altCertaDown = (abs(a1-a3))<=0.2
                            atritoAtual = findAtrito $ pecaAtual posAtual mapa
                            atritoAtualup25 = findAtrito $ pecaAtual (up25 j) mapa
                            atritoUp = findAtrito $ pecaUp posAtual mapa
                            atritoAbaixo = findAtrito $ pecaDown posAtual mapa
                            posAtual = whereBoy j
                            atritoAtualNext = findAtrito $ pecaAtual posAtualNext mapa
                            atritoUpNext = findAtrito $ pecaUp posAtualNext mapa
                            atritoAbaixoNext = findAtrito $ pecaDown posAtualNext mapa
                            posAtualNext = whereBoyAfter j
                            a1 = tallBoy (pBoy (soX j)) $ encontraPosicaoMatriz posAtual mapa
                            a2 = tallBoy (pBoy (soX j)) $ pecaUp posAtual mapa
                            a3 = tallBoy (pBoy (soX j)) $ pecaDown posAtual mapa

-- | checkMorte
-- Testa se o jogador está morto
checkMorte :: Jogador -> Bool
checkMorte (Jogador _ _ _ _ (Morto _)) = True
checkMorte _ = False

-- | evitaCola
-- Verifica se a peça seguinte é cola
evitaCola :: Double -> Bool
evitaCola potencialCola | (potencialCola==3.00) = True
                        | otherwise = False

-- | ajusta
-- Traz o bot para primeiro elemento da lista de jogadores
ajusta :: Jogador -> [Jogador] -> [Jogador]
ajusta _ [] = []
ajusta j (hjs:tjs) | (j==hjs) = (ajusta j tjs)
                   | otherwise = (hjs:(ajusta j tjs))

-- | checkCola
-- Verifica se o jogador tem disparos disponíveis
checkCola :: Jogador -> Bool
checkCola (Jogador _ _ _ c _) | c>0 = True
                              | otherwise = False

-- | distEpista
-- Tranforma uma lista de jogadores numa lista de pares com a informação da distância e da pista
distEpista :: [Jogador] -> [(Double,Int)]
distEpista [] = []
distEpista ((Jogador p d _ _ _):js) = (d,p):(distEpista js)

-- | potencialAtaque
-- Verifica se a pista dos outros jogadores é a mesma que a do 'bot'
potencialAtaque :: [(Double,Int)] -> Bool
potencialAtaque [(d,p)] = False
potencialAtaque ((d,p):(d1,p1):js)  | p==p1 = True
                                    | otherwise = potencialAtaque ((d,p):js)

-- | procuraMsmPista
-- Dá uma lista de jogadores que estão na mesma pista que o 'bot'
procuraMsmPista :: [(Double,Int)] -> [(Double,Int)]
procuraMsmPista [(_,_)] = []
procuraMsmPista ((d,p):(d1,p1):js) | p==p1 = (d1,p1) : procuraMsmPista ((d,p):js)
                                   | otherwise = procuraMsmPista ((d,p):js)

-- | procuraAtras
-- Decide se vale a pena disparar consoante a distância dos jogadores atrás do 'bot'
procuraAtras :: [(Double,Int)] -> [(Double,Int)] -> Bool
procuraAtras [(_,_)] [] = False
procuraAtras [(d,p)] ((d1,p1):js) | ((d-d1)<2.9)&&(d-d1>=0) = True
                                  | otherwise = procuraAtras [(d,p)] js

-- | findAtrito
-- Descobre o atrito de um peça
findAtrito :: Peca -> Double
findAtrito (Recta piso _) = atrito piso
findAtrito (Rampa piso _ _) = atrito piso

-- | testaAr
-- Testa se o jogador está no ar
testaAr :: Jogador -> Bool
testaAr (Jogador _ _ _ _ (Ar _ _ _)) = True
testaAr _ = False

-- | testaChao
-- Testa se o jogador está no ar
testaChao :: Jogador -> Bool
testaChao (Jogador _ _ _ _ (Chao _)) = True
testaChao _ = False

-- | ajustaAngulo
-- decide se o jogador se deve inclinar para ajustar o seu ângulo ao ângulo da peça que está a sobrevoar
ajustaAngulo :: Jogador -> Mapa -> Maybe Jogada
ajustaAngulo j@(Jogador p d v c (Ar a inc g)) mapa  |checkAterra eTempo && abs(angAjuste)<7.5 = Nothing
                                                    |checkAterra eTempo && angAjuste<=(-7.5) = Just (Movimenta E)
                                                    |checkAterra eTempo && angAjuste>=7.5 = Just (Movimenta D)
                                                    |otherwise = angPlanar j
                                                    where
                                                        tempoAjuste = (abs(angPeca)/15)*0.2
                                                        angPeca = anguloRampa (pecaAtual (whereBoy j) mapa)
                                                        angAjuste = inc - angPeca
                                                        eTempo = passo tempoAjuste mapa j
                                                        angPlanar j | inc>=7.5   = Just (Movimenta D)
                                                                    | inc<(-7.5) = Just (Movimenta E)
                                                                    | otherwise  = Nothing
-- | checkAterra
-- Testa se o jogador aterrou numa pista
checkAterra :: Jogador -> Bool
checkAterra j@(Jogador _ _ _ _ e) | (testaAr j)==False = True
                                  | otherwise = False

-- | up25
-- Dá a posição do jogador com um increto de 0.25 à distância
up25 :: Jogador -> (Int,Int)
up25 (Jogador p d _ _ _) = (p,(floor (d + 0.25)))


-- | moveEvitaCola
-- Sabendo que está ou tem uma peça do tipo Cola à sua frente tenta evitar essa peça  
moveEvitaCola :: Jogador -> Mapa -> Maybe Jogada
moveEvitaCola j mapa  | (pistaJogador j)==0 && altCertaDown = Just (Movimenta B)
                      | (pistaJogador j)==(length mapa)-1 && altCertaUp = Just (Movimenta C)
                      | (pistaJogador j)==(length mapa)-1 || (pistaJogador j)==0 = Just Acelera
                      | altCertaUp && atritoUp<atritoAbaixo = Just (Movimenta C)
                      | altCertaDown = Just (Movimenta B)
                      | altCertaUp = Just (Movimenta C)
                      | otherwise = Just Acelera
                      where
                        posAtual = whereBoy j
                        altCertaDown = (abs(a1-a3))<=0.2
                        altCertaUp = (abs(a1-a2))<=0.2
                        a1 = tallBoy (pBoy (soX j)) $ encontraPosicaoMatriz posAtual mapa
                        a2 = tallBoy (pBoy (soX j)) $ pecaUp posAtual mapa
                        a3 = tallBoy (pBoy (soX j)) $ pecaDown posAtual mapa
                        atritoUp = findAtrito $ pecaUp posAtual mapa
                        atritoAbaixo = findAtrito $ pecaDown posAtual mapa

-- | moveEficiente
-- Faz o jogador optar pela pista que tem melhor atrito 
moveEficiente :: Jogador -> Mapa -> Maybe Jogada
moveEficiente j mapa
  | (pistaJogador j)==0 && atritoAbaixo<atritoAtual && altCertaDown = Just (Movimenta B)
  | (pistaJogador j)==(length mapa)-1 && atritoUp<atritoAtual && altCertaUp = Just (Movimenta C)
  | (pistaJogador j)==(length mapa)-1 || (pistaJogador j)==0 = Just Acelera
  | altCertaUp && atritoUp<atritoAtual && atritoUp<atritoAbaixo = Just (Movimenta C)
  | altCertaUp && atritoUp<atritoAtual && atritoAbaixo==atritoUp = if (checkFrente atritoAbaixoNext atritoUpNext)==(Just (Movimenta B)) && altCertaDown then Just (Movimenta B)
                                                                        else checkFrente atritoAbaixoNext atritoUpNext
  | altCertaDown && atritoAbaixo<atritoAtual = Just (Movimenta B)
  | altCertaUp && atritoUp==atritoAtual = checkFrenteUp atritoAtualNext atritoUpNext
  | altCertaDown && atritoAbaixo==atritoAtual = checkFrenteDown atritoAbaixoNext atritoAtualNext
  | otherwise = Just Acelera
  where
    altCertaUp = (abs(a1-a2))<=0.2
    altCertaDown = (abs(a1-a3))<=0.2
    atritoAtual = findAtrito $ pecaAtual posAtual mapa
    atritoUp = findAtrito $ pecaUp posAtual mapa
    atritoAbaixo = findAtrito $ pecaDown posAtual mapa
    posAtual = whereBoy j
    atritoAtualNext = findAtrito $ pecaAtual posAtualNext mapa
    atritoUpNext = findAtrito $ pecaUp posAtualNext mapa
    atritoAbaixoNext = findAtrito $ pecaDown posAtualNext mapa
    posAtualNext = whereBoyAfter j
    a1 = tallBoy (pBoy (soX j)) $ encontraPosicaoMatriz posAtual mapa
    a2 = tallBoy (pBoy (soX j)) $ pecaUp posAtual mapa
    a3 = tallBoy (pBoy (soX j)) $ pecaDown posAtual mapa


-- | checkFrente
-- Decide a jogada mais favorável se receber os atritos das peças de cima e de baixo seguintes ao jogador 
checkFrente :: Double -> Double -> Maybe Jogada
checkFrente a b = if a<b then Just (Movimenta B)
                    else if b<a then Just (Movimenta C)
                        else Just Acelera

-- |checkFrenteUp
-- Decide a jogada mais favorável se receber os atritos seguintes das peças atual e acima seguintes ao jogador
checkFrenteUp :: Double -> Double -> Maybe Jogada
checkFrenteUp a b = if b<a then Just (Movimenta C)
                      else Just Acelera 

-- | checkFrenteDown
-- Decide a jogada mais favorável se receber os atritos seguintes das peças atual e acima seguintes ao jogador
checkFrenteDown :: Double -> Double -> Maybe Jogada
checkFrenteDown a b = if a<b then Just (Movimenta B)
                        else Just Acelera 
