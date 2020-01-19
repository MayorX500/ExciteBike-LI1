{- |
= Introdução
|
|A Tarefa5 consiste na implementação da interface gráfica e Input/Output, enquanto que nas restantes tarefas foi
|desenvolvido o codigo por detrás do jogo

= Objetivos
| |
| | * 1 - Desenhar o Mapa;
| | * 2 - Desenhar os Jogadores em relação ao mapa (coordenadas);
| | * 3 - Registar os Inputs do Jogador;
| | * 4 - Definir Menus;
| | * 5 - Animar o mapa (o jogador fica no mesmo local e o mapa é que se move);
| | * 6 - Implementar as tarefas 2, 4 e 6;
! ! * 7 - Retoques finais.


= Discussão e conclusão
Gostamos bastante da forma como o nosso jogo ficou, apesar de não ficar um jogo completamente funcional.
 O haskell não é uma linguagem ideal para a criação jogos, mesmo com a lib gloss não se compara a um Engine próprio
para o desenvolvimento de jogos.
Apesar de não ter sido possivel concluir a interface grafica do jogo, esta tarefa foi bastante divertida de realizar
, uma vez que pudemos dar asas à nossa imaginação e criar algo que se possa apreciar.
Foi uma das nossas tarefas preferidas de fazer, precisamente por causa disso. A parte mais difícil foi ter que aprender
 a usar o gloss, que é um pouco diferente do que tínhamos feito com haskell até ao momento, mas não foi muito difícil
 apanhar-lhe o jeito, e a partir daí tornou-se fácil de usar.
-}




-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where

import AuxGloss
import Maps
import LI11920
import Tarefa0_2019li1g077
import Tarefa1_2019li1g077
import Tarefa2_2019li1g077
import Tarefa3_2019li1g077
import Tarefa4_2019li1g077
import Tarefa6_2019li1g077
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
  --  Fundos
  fundoInvertido <- loadBMP "Gloss/Menu/Background/Reverse.bmp"
  fundoNormal <- loadBMP "Gloss/Menu/Background/Normal.bmp"
  fundoWinner <- loadBMP "Gloss/Menu/Background/FundoWinner.bmp"
  gameLogo <- loadBMP "Gloss/Menu/Buttons/ExiteBike.bmp"
  keysHelpforeground <- loadBMP "Gloss/Menu/Background/helpKeys.bmp"
  creditsForeground <- loadBMP "Gloss/Menu/Background/CreditsForeground.bmp"
  --  Menu
    -- Buttons
  cpu1 <- loadBMP "Gloss/Menu/Buttons/BOT-1.bmp"
  cpu2 <- loadBMP "Gloss/Menu/Buttons/BOT-2.bmp"
  cpu3 <- loadBMP "Gloss/Menu/Buttons/BOT-3.bmp"
  creditsPic <- loadBMP "Gloss/Menu/Buttons/Credits.bmp"
  goB <- loadBMP "Gloss/Menu/Buttons/Go.bmp"
  mapPic <- loadBMP "Gloss/Menu/Buttons/Map.bmp"
  otherPic <- loadBMP "Gloss/Menu/Buttons/Other.bmp"
  playPic <- loadBMP "Gloss/Menu/Buttons/Play.bmp"
  playerPic <- loadBMP "Gloss/Menu/Buttons/Player.bmp"
  exitPic <- loadBMP "Gloss/Menu/Buttons/Exit.bmp"
  source_codePic <- loadBMP "Gloss/Menu/Buttons/Source-Code.bmp"
  helpPic <- loadBMP "Gloss/Menu/Buttons/Help.bmp"
  playAgainPic <- loadBMP "Gloss/Menu/Buttons/Play-Again.bmp"
  quitB <- loadBMP "Gloss/Menu/Buttons/Back.bmp"
  uno <- loadBMP "Gloss/Menu/Buttons/1.bmp"
  dos <- loadBMP "Gloss/Menu/Buttons/2.bmp"
  tres <- loadBMP "Gloss/Menu/Buttons/3.bmp"
  colasNumero <- loadBMP "Gloss/Menu/Buttons/Colas.bmp"
  pausePic <- loadBMP "Gloss/Menu/Buttons/Pausa.bmp"
  menuPics <- loadBMP "Gloss/Menu/Buttons/Menu.bmp"
    --  MapNumbers
  map1Pic <- loadBMP "Gloss/Menu/Buttons/MapN/1.bmp"
  map2Pic <- loadBMP "Gloss/Menu/Buttons/MapN/2.bmp"
  map3Pic <- loadBMP "Gloss/Menu/Buttons/MapN/3.bmp"
  map4Pic <- loadBMP "Gloss/Menu/Buttons/MapN/4.bmp"
  map5Pic <- loadBMP "Gloss/Menu/Buttons/MapN/5.bmp"
  map6Pic <- loadBMP "Gloss/Menu/Buttons/MapN/6.bmp"
  map7Pic <- loadBMP "Gloss/Menu/Buttons/MapN/7.bmp"
  map8Pic <- loadBMP "Gloss/Menu/Buttons/MapN/8.bmp"
  map9Pic <- loadBMP "Gloss/Menu/Buttons/MapN/9.bmp"
  map10Pic <- loadBMP "Gloss/Menu/Buttons/MapN/10.bmp"
  map11Pic <- loadBMP "Gloss/Menu/Buttons/MapN/11.bmp"
  map12Pic <- loadBMP "Gloss/Menu/Buttons/MapN/12.bmp"
  map13Pic <- loadBMP "Gloss/Menu/Buttons/MapN/13.bmp"
  map14Pic <- loadBMP "Gloss/Menu/Buttons/MapN/14.bmp"
  map15Pic <- loadBMP "Gloss/Menu/Buttons/MapN/15.bmp"

  --  Jogadores
  p1 <-  loadBMP "Gloss/Players/Red.bmp"
  p2 <- loadBMP "Gloss/Players/Blue.bmp"
  p3 <- loadBMP "Gloss/Players/Green.bmp"
  p4 <- loadBMP "Gloss/Players/Yellow.bmp"
  p5 <- loadBMP "Gloss/Players/AE86.bmp"
  p6 <- loadBMP "Gloss/Players/Delorean.bmp"
  p7 <- loadBMP "Gloss/Players/Nissan.bmp"
  p8 <- loadBMP "Gloss/Players/Mystery.bmp"
  pm  <- loadBMP "Gloss/Players/Morto.bmp"
  --  Pecas
    --  Rectas
  retaT <- loadBMP "Gloss/Straight/Recta_Terra_W.bmp"
  retaL <- loadBMP "Gloss/Straight/Recta_Lama_W.bmp"
  retaC <- loadBMP "Gloss/Straight/Recta_Cola_W.bmp"
  retaR <- loadBMP "Gloss/Straight/Recta_Relva_W.bmp"
  retaB <- loadBMP "Gloss/Straight/Recta_Boost_W.bmp"
    --  Rampa Up
  ut1 <- loadBMP "Gloss/Ramp/Up/Rampa_Terra_1_W.bmp"
  ul1 <- loadBMP "Gloss/Ramp/Up/Rampa_Lama_1_W.bmp"
  uc1 <- loadBMP "Gloss/Ramp/Up/Rampa_Cola_1_W.bmp"
  ur1 <- loadBMP "Gloss/Ramp/Up/Rampa_Relva_1_W.bmp"
  ub1 <- loadBMP "Gloss/Ramp/Up/Rampa_Boost_1_W.bmp"
  ut2 <- loadBMP "Gloss/Ramp/Up/Rampa_Terra_2_W.bmp"
  ul2 <- loadBMP "Gloss/Ramp/Up/Rampa_Lama_2_W.bmp"
  uc2 <- loadBMP "Gloss/Ramp/Up/Rampa_Cola_2_W.bmp"
  ur2 <- loadBMP "Gloss/Ramp/Up/Rampa_Relva_2_W.bmp"
  ub2 <- loadBMP "Gloss/Ramp/Up/Rampa_Boost_2_W.bmp"
  ut3 <- loadBMP "Gloss/Ramp/Up/Rampa_Terra_3_W.bmp"
  ul3 <- loadBMP "Gloss/Ramp/Up/Rampa_Lama_3_W.bmp"
  uc3 <- loadBMP "Gloss/Ramp/Up/Rampa_Cola_3_W.bmp"
  ur3 <- loadBMP "Gloss/Ramp/Up/Rampa_Relva_3_W.bmp"
  ub3 <- loadBMP "Gloss/Ramp/Up/Rampa_Boost_3_W.bmp"
  ut4 <- loadBMP "Gloss/Ramp/Up/Rampa_Terra_4_W.bmp"
  ul4 <- loadBMP "Gloss/Ramp/Up/Rampa_Lama_4_W.bmp"
  uc4 <- loadBMP "Gloss/Ramp/Up/Rampa_Cola_4_W.bmp"
  ur4 <- loadBMP "Gloss/Ramp/Up/Rampa_Relva_4_W.bmp"
  ub4 <- loadBMP "Gloss/Ramp/Up/Rampa_Boost_4_W.bmp"
  ut5 <- loadBMP "Gloss/Ramp/Up/Rampa_Terra_5_W.bmp"
  ul5 <- loadBMP "Gloss/Ramp/Up/Rampa_Lama_5_W.bmp"
  uc5 <- loadBMP "Gloss/Ramp/Up/Rampa_Cola_5_W.bmp"
  ur5 <- loadBMP "Gloss/Ramp/Up/Rampa_Relva_5_W.bmp"
  ub5 <- loadBMP "Gloss/Ramp/Up/Rampa_Boost_5_W.bmp"
    --  Rampa Down
  dt1 <- loadBMP "Gloss/Ramp/Down/Rampa_Terra_1_F_W.bmp"
  dl1 <- loadBMP "Gloss/Ramp/Down/Rampa_Lama_1_F_W.bmp"
  dc1 <- loadBMP "Gloss/Ramp/Down/Rampa_Cola_1_F_W.bmp"
  dr1 <- loadBMP "Gloss/Ramp/Down/Rampa_Relva_1_F_W.bmp"
  db1 <- loadBMP "Gloss/Ramp/Down/Rampa_Boost_1_F_W.bmp"
  dt2 <- loadBMP "Gloss/Ramp/Down/Rampa_Terra_2_F_W.bmp"
  dl2 <- loadBMP "Gloss/Ramp/Down/Rampa_Lama_2_F_W.bmp"
  dc2 <- loadBMP "Gloss/Ramp/Down/Rampa_Cola_2_F_W.bmp"
  dr2 <- loadBMP "Gloss/Ramp/Down/Rampa_Relva_2_F_W.bmp"
  db2 <- loadBMP "Gloss/Ramp/Down/Rampa_Boost_2_F_W.bmp"
  dt3 <- loadBMP "Gloss/Ramp/Down/Rampa_Terra_3_F_W.bmp"
  dl3 <- loadBMP "Gloss/Ramp/Down/Rampa_Lama_3_F_W.bmp"
  dc3 <- loadBMP "Gloss/Ramp/Down/Rampa_Cola_3_F_W.bmp"
  dr3 <- loadBMP "Gloss/Ramp/Down/Rampa_Relva_3_F_W.bmp"
  db3 <- loadBMP "Gloss/Ramp/Down/Rampa_Boost_3_F_W.bmp"
  dt4 <- loadBMP "Gloss/Ramp/Down/Rampa_Terra_4_F_W.bmp"
  dl4 <- loadBMP "Gloss/Ramp/Down/Rampa_Lama_4_F_W.bmp"
  dc4 <- loadBMP "Gloss/Ramp/Down/Rampa_Cola_4_F_W.bmp"
  dr4 <- loadBMP "Gloss/Ramp/Down/Rampa_Relva_4_F_W.bmp"
  db4 <- loadBMP "Gloss/Ramp/Down/Rampa_Boost_4_F_W.bmp"
  dt5 <- loadBMP "Gloss/Ramp/Down/Rampa_Terra_5_F_W.bmp"
  dl5 <- loadBMP "Gloss/Ramp/Down/Rampa_Lama_5_F_W.bmp"
  dc5 <- loadBMP "Gloss/Ramp/Down/Rampa_Cola_5_F_W.bmp"
  dr5 <- loadBMP "Gloss/Ramp/Down/Rampa_Relva_5_F_W.bmp"
  db5 <- loadBMP "Gloss/Ramp/Down/Rampa_Boost_5_F_W.bmp"

  playIO window (black) fr (estadoGlossInicial ([fundoInvertido,fundoNormal,fundoWinner,gameLogo,keysHelpforeground,creditsForeground]
                                                 ,[cpu1,cpu2,cpu3,creditsPic,goB,mapPic,otherPic,playPic,playAgainPic,playerPic,exitPic,source_codePic,helpPic,quitB,uno,dos,tres,colasNumero,pausePic,menuPics,map1Pic,map2Pic,map3Pic,map4Pic,map5Pic,map6Pic,map7Pic,map8Pic,map9Pic,map10Pic,map11Pic,map12Pic,map13Pic,map14Pic,map15Pic]
                                                 ,[p1,p2,p3,p4,p5,p6,p7,p8,pm]
                                                 ,[retaT,retaL,retaC,retaR,retaB,ut1,ul1,uc1,ur1,ub1,ut2,ul2,uc2,ur2,ub2,ut3,ul3,uc3,ur3,ub3,ut4,ul4,uc4,ur4,ub4,ut5,ul5,uc5,ur5,ub5,dt1,dl1,dc1,dr1,db1,dt2,dl2,dc2,dr2,db2,dt3,dl3,dc3,dr3,db3,dt4,dl4,dc4,dr4,db4,dt5,dl5,dc5,dr5,db5])
                                                 ) drawEstadoGloss reageKeyGloss reageTimeGloss
-- * Funções Principais da Tarefa5

-- | drawEstado
-- desenha o estado em que o jogo se encontra (definido na AuxGloss)
drawEstado :: GameMode -> Textures -> [Picture]
drawEstado (GM (Estado m j) ppic GameMenu _ _) textures |isOver j m = drawWinner (GM (Estado m j) ppic GameOver NothingB []) textures
                                                       |otherwise = concat (justLayer (gLayers(drawMap m (sX) (height) mapTextures 0 ++ drawPlayers j 0 m ppic [deadTexture])))
                                                        where
                                                          (backgroundPic,menuPic,playerPics,mapTextures) =  textures
                                                          deadTexture = last playerPics
                                                          sX = startingX (head j)

drawEstado game@(GM _ _ GameOver _ _) textures = drawWinner game textures
drawEstado game@(GM _ _ PlayerPicMenu _ _) textures = drawPlayerMenu game textures
drawEstado game@(GM _ _ MapMenu _ _) textures = drawMapMenu game textures
drawEstado game@(GM _ _ OtherMenu _ _) textures = drawOther game textures
drawEstado game@(GM _ _ MainMenu _ _) textures = drawMainMenu game textures
drawEstado game@(GM _ _ BotMenu _ _) textures = drawBotMenu game textures
drawEstado game@(GM _ _ CreditsMenu _ _) textures = drawCredits game textures
drawEstado game@(GM _ _ PauseMenu _ _) textures = drawPausa game textures
drawEstado game@(GM _ _ KeysMenu _ _) textures = drawKeyHelp game textures

-- | drawMainMenu
-- desenha o jogo no menu principal
drawMainMenu :: GameMode -> Textures -> [Picture]
drawMainMenu (GM _ _ MainMenu Play _) textures = [Translate 0 0 (fundoInvertido)] ++ [scale 1.2 1.2 (Translate 0 0 (playPic))] ++ [scale 2 2 (Translate 0 160 (gameLogo))] ++ [Translate 0 (-125) (otherPic)] ++ [Translate 0 (-250) (exitPic)]
                                                  where
                                                    (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                    (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                    (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                    (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                    (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawMainMenu (GM _ _ MainMenu Other _) textures = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 0 (playPic)] ++ [scale 2 2 (Translate 0 160 (gameLogo))] ++ [scale 1.2 1.2 (Translate 0 (-125) (otherPic))] ++ [Translate 0 (-250) (exitPic)]
                                                  where
                                                    (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                    (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                    (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                    (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                    (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawMainMenu (GM _ _ MainMenu Exit _) textures = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 0 (playPic)] ++ [scale 2 2 (Translate 0 160 (gameLogo))] ++ [Translate 0 (-125) (otherPic)] ++ [scale 1.2 1.2 (Translate 0 (-250) (exitPic))]
                                                  where
                                                    (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                    (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                    (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                    (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                    (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawMainMenu (GM _ _ MainMenu _ _) textures = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 0 (playPic)] ++ [scale 2 2 (Translate 0 160 (gameLogo))] ++ [Translate 0 (-125) (otherPic)] ++ [Translate 0 (-250) (exitPic)]
                                                  where
                                                    (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                    (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                    (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                    (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                    (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures
-- | drawPlayerMenu
-- Desenha o jogo no menu de escolha de avatar do jogador
drawPlayerMenu :: GameMode -> Textures -> [Picture]
drawPlayerMenu (GM _ ((0,i,playerPic):t) PlayerPicMenu Player _) textures = [Translate 0 0 (fundoInvertido)] ++ [scale 1.2 1.2 (Translate 0 (-100) (playerPic))]
                                                                                                            ++ [Translate 0 300 (playerPics !! i)] ++ [Translate 0 (-220) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                                                                            where
                                                                                                              (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                                                              (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                                                              (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                                                              (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                                                              (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawPlayerMenu (GM _ ((0,i,playerPic):t) PlayerPicMenu Back _) textures = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (-100) (playerPic)]
                                                                                                          ++ [Translate 0 300 (playerPics !! i)] ++ [Translate 0 (-220) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                                                                          where
                                                                                                            (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                                                            (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                                                            (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                                                            (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                                                            (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawPlayerMenu (GM _ ((0,i,playerPic):t) PlayerPicMenu Play _) textures = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (-100) (playerPic)]
                                                                                                          ++ [Translate 0 300 (playerPics !! i)] ++ [scale 1.2 1.2 (Translate 0 (-220) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                                                                          where
                                                                                                            (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                                                            (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                                                            (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                                                            (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                                                            (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawPlayerMenu (GM _ ((0,i,playerPic):t) PlayerPicMenu _ _) textures = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (-100) (playerPic)]
                                                                                                        ++ [Translate 0 300 (playerPics !! i)] ++ [Translate 0 (-220) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                                                                        where
                                                                                                          (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                                                          (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                                                          (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                                                          (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                                                          (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures


-- |drawBotMenu
--Desenha o jogo no menu de escolha de avatar dos Bots
drawBotMenu :: GameMode -> Textures -> [Picture]
drawBotMenu (GM _ ((0,i,playerPic0):(1,b1,playerPic1):(2,b2,playerPic2):(3,b3,playerPic3):[]) BotMenu CPU1 _) textures = [Translate 0 0 (fundoInvertido)]
                                                                                                                          ++ [Translate (-600) (300) (playerPics !! b1)]
                                                                                                                          ++ [Translate (0) (300) (playerPics !! b2)]
                                                                                                                          ++ [Translate (600) (300) (playerPics !! b3)]
                                                                                                                          ++ [scale 1.3 1.3 (Translate (-600) (80) (cpu1))]
                                                                                                                          ++ [Translate (0) (80) (cpu2)]
                                                                                                                          ++ [Translate (600) (80) (cpu3)]
                                                                                                                          ++ [Translate (-300) (-150) (goB)]
                                                                                                                          ++ [Translate (300) (-150) (quitB)]
                                                                                              where
                                                                                                (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                                                (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                                                (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                                                (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                                                (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawBotMenu (GM _ ((0,i,playerPic0):(1,b1,playerPic1):(2,b2,playerPic2):(3,b3,playerPics):[]) BotMenu CPU2 _) textures = [Translate 0 0 (fundoInvertido)]
                                                                                                                          ++ [Translate (-600) (300) (playerPics !! b1)]
                                                                                                                          ++ [Translate (0) (300) (playerPics !! b2)]
                                                                                                                          ++ [Translate (600) (300) (playerPics !! b3)]
                                                                                                                          ++ [Translate (-600) (80) (cpu1)]
                                                                                                                          ++ [scale 1.3 1.3 (Translate (0) (80) (cpu2))]
                                                                                                                          ++ [Translate (600) (80) (cpu3)]
                                                                                                                          ++ [Translate (-300) (-150) (goB)]
                                                                                                                          ++ [Translate (300) (-150) (quitB)]
                                                                                              where
                                                                                                (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                                                (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                                                (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                                                (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                                                (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawBotMenu (GM _ ((0,i,playerPic0):(1,b1,playerPic1):(2,b2,playerPic2):(3,b3,playerPics):[]) BotMenu CPU3 _) textures = [Translate 0 0 (fundoInvertido)]
                                                                                                                          ++ [Translate (-600) (300) (playerPics !! b1)]
                                                                                                                          ++ [Translate (0) (300) (playerPics !! b2)]
                                                                                                                          ++ [Translate (600) (300) (playerPics !! b3)]
                                                                                                                          ++ [Translate (-600) (80) (cpu1)]
                                                                                                                          ++ [Translate (0) (80) (cpu2)]
                                                                                                                          ++ [scale 1.3 1.3 (Translate (600) (80) (cpu3))]
                                                                                                                          ++ [Translate (-300) (-150) (goB)]
                                                                                                                          ++ [Translate (300) (-150) (quitB)]
                                                                                              where
                                                                                                (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                                                (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                                                (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                                                (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                                                (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawBotMenu (GM _ ((0,i,playerPic0):(1,b1,playerPic1):(2,b2,playerPic2):(3,b3,playerPics):[]) BotMenu Play _) textures = [Translate 0 0 (fundoInvertido)]
                                                                                                                          ++ [Translate (-600) (300) (playerPics !! b1)]
                                                                                                                          ++ [Translate (0) (300) (playerPics !! b2)]
                                                                                                                          ++ [Translate (600) (300) (playerPics !! b3)]
                                                                                                                          ++ [Translate (-600) (80) (cpu1)]
                                                                                                                          ++ [Translate (0) (80) (cpu2)]
                                                                                                                          ++ [Translate (600) (80) (cpu3)]
                                                                                                                          ++ [scale 1.5 1.5 (Translate (-300) (-150) (goB))]
                                                                                                                          ++ [Translate (300) (-150) (quitB)]
                                                                                              where
                                                                                                (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                                                (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                                                (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                                                (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                                                (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures


drawBotMenu (GM _ ((0,i,playerPic0):(1,b1,playerPic1):(2,b2,playerPic2):(3,b3,playerPics):[]) BotMenu Back _) textures = [Translate 0 0 (fundoInvertido)]
                                                                                                                          ++ [Translate (-600) (300) (playerPics !! b1)]
                                                                                                                          ++ [Translate (0) (300) (playerPics !! b2)]
                                                                                                                          ++ [Translate (600) (300) (playerPics !! b3)]
                                                                                                                          ++ [Translate (-600) (80) (cpu1)]
                                                                                                                          ++ [Translate (0) (80) (cpu2)]
                                                                                                                          ++ [Translate (600) (80) (cpu3)]
                                                                                                                          ++ [Translate (-300) (-150) (goB)]
                                                                                                                          ++ [scale 1.5 1.5 (Translate (300) (-150) (quitB))]
                                                                                              where
                                                                                                (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                                                (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                                                (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                                                (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                                                (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawBotMenu (GM _ ((0,i,playerPic0):(1,b1,playerPic1):(2,b2,playerPic2):(3,b3,playerPics):[]) BotMenu _ _) textures = [Translate 0 0 (fundoInvertido)]
                                                                                                                          ++ [Translate (-600) (300) (playerPics !! b1)]
                                                                                                                          ++ [Translate (0) (300) (playerPics !! b2)]
                                                                                                                          ++ [Translate (600) (300) (playerPics !! b3)]
                                                                                                                          ++ [Translate (-600) (80) (cpu1)]
                                                                                                                          ++ [Translate (0) (80) (cpu2)]
                                                                                                                          ++ [Translate (600) (80) (cpu3)]
                                                                                                                          ++ [Translate (-300) (-150) (goB)]
                                                                                                                          ++ [Translate (300) (-150) (quitB)]
                                                                                          where
                                                                                            (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                                            (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                                            (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                                            (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                                            (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

-- | drawMapMenu
-- Desenha o jogo no menu de escolha de mapa
drawMapMenu :: GameMode -> Textures -> [Picture]
drawMapMenu (GM (Estado m j) _ MapMenu MapSelect _) textures |m == map1 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map1Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map2 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map2Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map3 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map3Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map4 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map4Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map5 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map5Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map6 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map6Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map7 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map7Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map8 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map8Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map9 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map9Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map10 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map10Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map11 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map11Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map12 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map12Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map13 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map13Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map14 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map14Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map15 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map15Pic] ++ [scale 1.2 1.2 (Translate 0 0 (mapPic))] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             where
                                                               (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                               (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                               (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                               (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                               (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawMapMenu (GM (Estado m j) _ MapMenu Play _) textures |m == map1 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map1Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map2 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map2Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map3 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map3Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map4 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map4Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map5 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map5Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map6 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map6Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map7 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map7Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map8 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map8Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map9 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map9Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map10 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map10Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map11 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map11Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map12 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map12Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map13 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map13Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map14 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map14Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map15 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map15Pic] ++ [Translate 0 0 (mapPic)] ++ [scale 1.2 1.2 (Translate 0 (-200) (goB))] ++ [Translate 0 (-400) (quitB)]
                                                                where
                                                                  (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                  (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                  (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                  (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                  (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawMapMenu (GM (Estado m j) _ MapMenu Back _) textures |m == map1 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map1Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                             |m == map2 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map2Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                             |m == map3 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map3Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                             |m == map4 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map4Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                             |m == map5 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map5Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                             |m == map6 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map6Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                             |m == map7 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map7Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                             |m == map8 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map8Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                             |m == map9 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map9Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                             |m == map10 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map10Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                             |m == map11 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map11Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                             |m == map12 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map12Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                             |m == map13 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map13Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                             |m == map14 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map14Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                             |m == map15 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map15Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [scale 1.2 1.2 (Translate 0 (-400) (quitB))]
                                                              where
                                                                (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawMapMenu (GM (Estado m j) _ MapMenu _ _) textures |m == map1 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map1Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map2 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map2Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map3 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map3Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map4 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map4Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map5 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map5Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map6 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map6Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map7 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map7Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map8 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map8Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map9 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map9Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map10 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map10Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map11 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map11Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map12 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map12Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map13 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map13Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map14 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map14Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                             |m == map15 = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) map15Pic] ++ [Translate 0 0 (mapPic)] ++ [Translate 0 (-200) (goB)] ++ [Translate 0 (-400) (quitB)]
                                                                 where
                                                                   (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                   (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                   (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                   (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                   (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

-- | drawOther
-- Desenha o jogo no menu de opções
drawOther :: GameMode -> Textures -> [Picture]
drawOther (GM (Estado m j) _ OtherMenu SC _) textures = [Translate 0 0 (fundoInvertido)] ++ [scale 1.2 1.2 (Translate 0 (250) (source_codePic))] ++ [(Translate 0 (90) (helpPic))] ++ [Translate 0 (-90) (creditsPic)] ++ [Translate 0 (-250) (quitB)]
                                                          where
                                                            (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                            (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                            (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                            (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                            (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawOther (GM (Estado m j) _ OtherMenu KeysHelp _) textures = [Translate 0 0 (fundoInvertido)] ++ [(Translate 0 (250) (source_codePic))] ++ [scale 1.2 1.2 (Translate 0 (90) (helpPic))] ++ [Translate 0 (-90) (creditsPic)] ++ [Translate 0 (-250) (quitB)]
                                                                where
                                                                  (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                  (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                  (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                  (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                  (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawOther (GM (Estado m j) _ OtherMenu Credits _) textures = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) (source_codePic)] ++ [(Translate 0 (90) (helpPic))] ++ [scale 1.2 1.2 (Translate 0 (-90) (creditsPic))] ++ [Translate 0 (-250) (quitB)]
                                                              where
                                                                (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawOther (GM (Estado m j) _ OtherMenu Back _) textures = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) (source_codePic)] ++ [(Translate 0 (90) (helpPic))] ++ [Translate 0 (-90) (creditsPic)] ++ [scale 1.2 1.2 (Translate 0 (-250) (quitB))]
                                                            where
                                                              (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                              (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                              (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                              (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                              (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawOther (GM (Estado m j) _ OtherMenu _ _) textures = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (250) (source_codePic)] ++ [(Translate 0 (90) (helpPic))] ++ [Translate 0 (-90) (creditsPic)] ++ [Translate 0 (-250) (quitB)]
                                                          where
                                                            (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                            (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                            (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                            (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                            (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

-- |drawCredits
--Desenha o jogo nos creditos
drawCredits :: GameMode -> Textures -> [Picture]
drawCredits (GM (Estado m j) _ CreditsMenu ExitCredits _) textures = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (-100) (creditsForeground)] ++ [scale 1.2 1.2 (Translate 0 (-360) (quitB))]
                                                                      where
                                                                        (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                        (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                        (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                        (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                        (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawCredits (GM (Estado m j) _ CreditsMenu _ _) textures = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (-100) (creditsForeground)] ++ [(Translate 0 (-360) (quitB))]
                                                            where
                                                              (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                              (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                              (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                              (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                              (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

-- |drawWinner
-- Mostra o carro vencedor depois de um jogo completo
drawWinner :: GameMode -> Textures -> [Picture]
drawWinner g@(GM (Estado m j) _ GameOver PlayAgain _) textures = [Translate 0 0 (fundoWinner)] ++ [Translate 0 0 (giveWinner g)] ++ [scale 1.2 1.2 (Translate (-100) (-150) (playAgainPic))] ++ [Translate (100) (-150) (quitB)]
                                                                  where
                                                                    (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                    (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                    (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                    (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                    (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawWinner g@(GM (Estado m j) _ GameOver Back _) textures = [Translate 0 0 (fundoWinner)] ++ [Translate 0 0 (giveWinner g)] ++ [Translate (-100) (-150) (playAgainPic)] ++ [scale 1.2 1.2 (Translate (100) (-150) (quitB))]
                                                                  where
                                                                    (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                    (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                    (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                    (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                    (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawWinner g@(GM (Estado m j) _ GameOver _ _) textures = [Translate 0 0 (fundoWinner)] ++ [Translate 0 0 (giveWinner g)] ++ [Translate (-100) (-150) (playAgainPic)] ++ [Translate (100) (-150) (quitB)]
                                                                where
                                                                  (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                  (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                  (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                  (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                  (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

-- |drawWinner
--Mostra o menu das teclas de condução
drawHelp :: GameMode -> Textures -> [Picture]
drawHelp (GM _ _ KeysMenu Back _) textures = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (0) (keysHelpforeground)] ++ [scale 1.2 1.2 (Translate 400 (-130) (quitB))]
                                                where
                                                  (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                  (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                  (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                  (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                  (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures
drawHelp (GM _ _ KeysMenu _ _) textures = [Translate 0 0 (fundoInvertido)] ++ [Translate 0 (0) (keysHelpforeground)] ++ [(Translate 400 (-130) (quitB))]
                                            where
                                              (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                              (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                              (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                              (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                              (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

-- | drawPausa
--Desenha o jogo em pausa
drawPausa :: GameMode -> Textures -> [Picture]
drawPausa g@(GM (Estado m j) _ PauseMenu Back _) textures = [Translate 0 0 (fundoNormal)] ++ [scale 1.2 1.2 (Translate (0) (250) (quitB))] ++ [Translate 0 0 (menuPics)] ++ [Translate (0) (-250) (exitPic)] ++ [Translate (0) (350) (pausePic)]
                                                                  where
                                                                    (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                    (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                    (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                    (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                    (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawPausa g@(GM (Estado m j) _ PauseMenu Menu _) textures = [Translate 0 0 (fundoNormal)] ++ [Translate (0) (250) (quitB)] ++ [scale 1.2 1.2 (Translate 0 0 (menuPics))] ++ [Translate (0) (-250) (exitPic)] ++ [Translate (0) (350) (pausePic)]
                                                                  where
                                                                    (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                    (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                    (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                    (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                    (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawPausa g@(GM (Estado m j) _ PauseMenu Exit _) textures =[Translate 0 0 (fundoNormal)] ++ [Translate (0) (250) (quitB)] ++ [Translate 0 0 (menuPics)] ++ [scale 1.2 1.2 (Translate (0) (-250) (exitPic))] ++ [Translate (0) (350) (pausePic)]
                                                                where
                                                                  (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                  (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                  (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                  (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                  (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures
drawPausa g@(GM (Estado m j) _ PauseMenu _ _) textures =[Translate 0 0 (fundoNormal)] ++ [Translate (0) (250) (quitB)] ++ [Translate 0 0 (menuPics)] ++ [Translate (0) (-250) (exitPic)] ++ [Translate (0) (350) (pausePic)]
                                                                where
                                                                  (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                  (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                  (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                  (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                  (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures


drawKeyHelp :: GameMode -> Textures -> [Picture]
drawKeyHelp (GM (Estado m j) _ KeysMenu Back _) textures = [Translate 0 0 (fundoInvertido)] ++ [(scale 1.5 1.5 (Translate 0 (0) (keysHelpforeground)))] ++ [scale 1.2 1.2 (Translate (-200) (-250) (quitB))]
                                                                      where
                                                                        (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                                        (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                                        (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                                        (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                                        (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawKeyHelp (GM (Estado m j) _ KeysMenu _ _) textures = [Translate 0 0 (fundoInvertido)] ++ [(scale 1.5 1.5 (Translate 0 (0) (keysHelpforeground)))] ++ [Translate (-200) (-250) (quitB)]
                                                            where
                                                              (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                                              (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                                              (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                                              (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                                              (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures



-- | drawPlayers
--Desenha os jogadores no mapa
drawPlayers :: [Jogador] -- ^ Lista de Jogadores (Max4)
              -> Int -- ^ O Indice do Jogador a ser desenhado (drawPlayer)
              -> Mapa -- ^ Mapa do Jogo
              -> [Ppic] -- ^ Lista das Escolhas de imagem dos Jogadores (Bots are sequencial)
              -> [Picture] -- ^ Imagem do Player Morto
              -> [Layer] -- ^ A imagem de todos os jogadores e o nº de colas de cada 1
drawPlayers (h:t) n m ((player,indice,li):p) dm = drawPlayer h n (player,indice,li) dm (pecaAtual (whereBoy h) m) : drawPlayers t (n + 1) m p dm
drawPlayers [] _ _ _ _ = []


-- Desenha um jogador
drawPlayer :: Jogador -- ^ Jogador a "desenhar"
            -> Int -- ^ Nº do Jogador
            -> Ppic -- ^ Escolha de imagem do Jogador
            -> [Picture] -- ^ Imagem do Jogador Mort0
            -> Peca -- ^ Peca em qual o Jogador se encontra no mapa
            -> Layer -- ^ Jogador desenhado
drawPlayer (Jogador p d v c e) n (player,indice,li) [dm] peca = case e of
  (Chao _)   -> (p,([Translate (xC) (yC) (rotate degC (scale 0.5 0.5 (li !! indice))),
                 Translate (gX) (gY) (scale 1 1 (Text (show d))),
                 Translate (pPX) (pPY) (scale 1 1 (Text (show (n+1))))]))
                 where
                   -- xC = chaoX p d peca
                   -- xA = arX p d peca a
                   -- yC = chaoY p d peca
                   -- yA = arY p d peca a
                   xC = (width)
                   xA = (width)
                   yC = (yChao p d peca )
                   degC = degChao p d peca
                   gX = posGlueX n
                   gY = posGlueY n
                   pPX = posPlayerX n
                   pPY = posPlayerY n

  (Ar a i g) -> (p,([Translate (xA) (yA) (rotate (-(realToFrac i))  (scale 0.5 0.5 (li !! indice))),
                 Translate (gX) (gY) (scale 1 1 (Text (show d))),
                 Translate (pPX) (pPY) (scale 1 1 (Text (show (n+1))))]))
                 where
                   -- xC = chaoX p d peca
                   -- xA = arX p d peca a
                   -- yC = chaoY p d peca
                   -- yA = arY p d peca a
                   xC = (width)
                   xA = (width)
                   yC = (yChao p d peca )
                   yA = (yAr a p )
                   degC = degChao p d peca
                   gX = posGlueX n
                   gY = posGlueY n
                   pPX = posPlayerX n
                   pPY = posPlayerY n

  (Morto _)  -> (p,([Translate (xC) (yC) (rotate degC (scale 0.35 0.35 dm)),
                 Translate (gX) (gY) (scale 1 1 (Text (show d))),
                 Translate (pPX) (pPY) (scale 1 1 (Text (show (n+1))))]))
                 where
                   -- xC = chaoX p d peca
                   -- xA = arX p d peca a
                   -- yC = chaoY p d peca
                   -- yA = arY p d peca a
                   xC = (width)
                   xA = (width)
                   yC = (height + pieceMid)
                   yA = (height + pieceMid)
                   degC = degChao p d peca
                   gX = posGlueX n
                   gY = posGlueY n
                   pPX = posPlayerX n
                   pPY = posPlayerY n


drawMap :: Mapa -> Float -> Float -> [Picture] -> Int -> [Layer]
drawMap [] _ _ _ n = [(n,[])]
drawMap (h:t) x y pic n = (n,(drawTrack h x y pic)) : (drawMap t x (y - pieceLenght) pic (n+1))

drawTrack :: Pista -> Float -> Float -> [Picture] -> [Picture]
drawTrack [] _ _ _ = []
drawTrack (h:t) x y pic = [drawPeca h x y pic] ++ (drawTrack t (x + pieceSize) y pic)

drawPeca :: Peca -> Float -> Float -> [Picture] -> Picture
drawPeca p x y pic = case p of
  (Recta a b) -> drawPecaAux1 p x y (listfrom2 pic 0 4)
  (Rampa a i f) -> if i<f then drawPecaAux2 p x y (listfrom2 pic 5 29)
                    else drawPecaAux3 p x y (listfrom2 pic 30 54)

drawPecaAux1 :: Peca -> Float -> Float -> [Picture] -> Picture
drawPecaAux1 (Recta a b) x y (retaT:retaL:retaC:retaR:retaB:[]) = case a of Terra -> Translate x ((y + ((realToFrac b) *pieceSize)) - (6*pieceLenght)) retaT
                                                                            Lama  -> Translate x ((y + ((realToFrac b) *pieceSize)) - (6*pieceLenght)) retaL
                                                                            Cola  -> Translate x ((y + ((realToFrac b) *pieceSize)) - (6*pieceLenght)) retaC
                                                                            Relva -> Translate x ((y + ((realToFrac b) *pieceSize)) - (6*pieceLenght)) retaR
                                                                            Boost -> Translate x ((y + ((realToFrac b) *pieceSize)) - (6*pieceLenght)) retaB
drawPecaAux2 :: Peca -> Float -> Float -> [Picture] -> Picture
drawPecaAux2 (Rampa a i f) x y pic = case a of Terra -> auxTerraU x y i f (eInList [0,5,10,15,20] pic)
                                               Lama  -> auxLamaU x y i f (eInList [1,6,11,16,21] pic)
                                               Cola  -> auxColaU x y i f (eInList [2,7,12,17,22] pic)
                                               Relva -> auxRelvaU x y i f (eInList [3,8,13,18,23] pic)
                                               Boost -> auxBoostU x y i f (eInList [4,9,14,19,24] pic)

auxTerraU :: Float -> Float -> Int -> Int -> [Picture] -> Picture
auxTerraU x y i f (ut1:ut2:ut3:ut4:ut5:[]) = case (f-i) of 1 -> Translate x (y+((realToFrac i)*pieceSize)) ut1
                                                           2 -> Translate x (y+((realToFrac i)*pieceSize)) ut2
                                                           3 -> Translate x (y+((realToFrac i)*pieceSize)) ut3
                                                           4 -> Translate x (y+((realToFrac i)*pieceSize)) ut4
                                                           5 -> Translate x (y+((realToFrac i)*pieceSize)) ut5


auxLamaU :: Float -> Float -> Int -> Int -> [Picture] -> Picture
auxLamaU x y i f (ul1:ul2:ul3:ul4:ul5:[]) = case (f-i) of 1 -> Translate x (y+((realToFrac i)*pieceSize)) ul1
                                                          2 -> Translate x (y+((realToFrac i)*pieceSize)) ul2
                                                          3 -> Translate x (y+((realToFrac i)*pieceSize)) ul3
                                                          4 -> Translate x (y+((realToFrac i)*pieceSize)) ul4
                                                          5 -> Translate x (y+((realToFrac i)*pieceSize)) ul5

auxColaU :: Float -> Float -> Int -> Int -> [Picture] -> Picture
auxColaU x y i f (uc1:uc2:uc3:uc4:uc5:[]) = case (f-i) of 1 -> Translate x (y+((realToFrac i)*pieceSize)) uc1
                                                          2 -> Translate x (y+((realToFrac i)*pieceSize)) uc2
                                                          3 -> Translate x (y+((realToFrac i)*pieceSize)) uc3
                                                          4 -> Translate x (y+((realToFrac i)*pieceSize)) uc4
                                                          5 -> Translate x (y+((realToFrac i)*pieceSize)) uc5


auxRelvaU :: Float -> Float -> Int -> Int -> [Picture] -> Picture
auxRelvaU x y i f (ur1:ur2:ur3:ur4:ur5:[]) = case (f-i) of 1 -> Translate x (y+((realToFrac i)*pieceSize)) ur1
                                                           2 -> Translate x (y+((realToFrac i)*pieceSize)) ur2
                                                           3 -> Translate x (y+((realToFrac i)*pieceSize)) ur3
                                                           4 -> Translate x (y+((realToFrac i)*pieceSize)) ur4
                                                           5 -> Translate x (y+((realToFrac i)*pieceSize)) ur5

auxBoostU :: Float -> Float -> Int -> Int -> [Picture] -> Picture
auxBoostU x y i f (ub1:ub2:ub3:ub4:ub5:[]) = case (f-i) of 1 -> Translate x (y+((realToFrac i)*pieceSize)) ub1
                                                           2 -> Translate x (y+((realToFrac i)*pieceSize)) ub2
                                                           3 -> Translate x (y+((realToFrac i)*pieceSize)) ub3
                                                           4 -> Translate x (y+((realToFrac i)*pieceSize)) ub4
                                                           5 -> Translate x (y+((realToFrac i)*pieceSize)) ub5

drawPecaAux3 :: Peca -> Float -> Float -> [Picture] -> Picture
drawPecaAux3 (Rampa a i f) x y pic = case a of Terra -> auxTerraD x y i f (eInList [0,5,10,15,20] pic)
                                               Lama  -> auxLamaD x y i f (eInList [1,6,11,16,21] pic)
                                               Cola  -> auxColaD x y i f (eInList [2,7,12,17,22] pic)
                                               Relva -> auxRelvaD x y i f (eInList [3,8,13,18,23] pic)
                                               Boost -> auxBoostD x y i f (eInList [4,9,14,19,24] pic)

auxTerraD :: Float -> Float -> Int -> Int -> [Picture] -> Picture
auxTerraD x y i f (dt1:dt2:dt3:dt4:dt5:[]) = case (f-i) of (-1) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dt1
                                                           (-2) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dt2
                                                           (-3) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dt3
                                                           (-4) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dt4
                                                           (-5) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dt5


auxLamaD :: Float -> Float -> Int -> Int -> [Picture] -> Picture
auxLamaD x y i f (dl1:dl2:dl3:dl4:dl5:[]) = case (f-i) of (-1) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dl1
                                                          (-2) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dl2
                                                          (-3) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dl3
                                                          (-4) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dl4
                                                          (-5) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dl5

auxColaD :: Float -> Float -> Int -> Int -> [Picture] -> Picture
auxColaD x y i f (dc1:dc2:dc3:dc4:dc5:[]) = case (f-i) of (-1) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dc1
                                                          (-2) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dc2
                                                          (-3) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dc3
                                                          (-4) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dc4
                                                          (-5) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dc5


auxRelvaD :: Float -> Float -> Int -> Int -> [Picture] -> Picture
auxRelvaD x y i f (dr1:dr2:dr3:dr4:dr5:[]) = case (f-i) of (-1) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dr1
                                                           (-2) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dr2
                                                           (-3) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dr3
                                                           (-4) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dr4
                                                           (-5) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) dr5

auxBoostD :: Float -> Float -> Int -> Int -> [Picture] -> Picture
auxBoostD x y i f (db1:db2:db3:db4:db5:[]) = case (f-i) of (-1) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) db1
                                                           (-2) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) db2
                                                           (-3) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) db3
                                                           (-4) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) db4
                                                           (-5) -> Translate (x+pieceSize) (y+((realToFrac f)*pieceSize)) db5


reageKey :: Event -> GameMode -> GameMode
-- GameMenu
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic GameMenu NothingB []) = (GM (jogada 0 (Movimenta C) stat) pic GameMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic GameMenu NothingB []) = (GM (jogada 0 (Movimenta B) stat) pic GameMenu NothingB [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM stat pic GameMenu NothingB []) = (GM (jogada 0 (Movimenta E) stat) pic GameMenu NothingB [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM stat pic GameMenu NothingB []) = (GM (jogada 0 (Movimenta D) stat) pic GameMenu NothingB [])
reageKey (EventKey (Char 'w') Down _ _) (GM stat pic GameMenu NothingB []) = (GM (jogada 0 (Acelera) stat) pic GameMenu NothingB [])
reageKey (EventKey (Char 's') Down _ _) (GM stat pic GameMenu NothingB []) = (GM (jogada 0 (Desacelera) stat) pic GameMenu NothingB [])
reageKey (EventKey (Char 'a') Down _ _) (GM stat pic GameMenu NothingB []) = (GM (jogada 0 (Dispara) stat) pic GameMenu NothingB [])
reageKey (EventKey (Char 'p') Down _ _) (GM stat pic GameMenu NothingB []) = (GM stat pic PauseMenu NothingB [])
-- MainMenu
  -- NothingB
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic MainMenu NothingB []) = (GM stat pic MainMenu Play [])
reageKey (EventKey (SpecialKey _) Down _ _) s@(GM stat pic MainMenu NothingB []) = s
reageKey (EventKey (Char _) Down _ _) s@(GM stat pic MainMenu NothingB []) = s
  -- Play
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic MainMenu Play []) = (GM stat pic MainMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic MainMenu Play []) = (GM stat pic MainMenu Other [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic MainMenu Play []) = (GM stat pic PlayerPicMenu NothingB [])
  -- Other
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic MainMenu Other []) = (GM stat pic MainMenu Play [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic MainMenu Other []) = (GM stat pic MainMenu Exit [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic MainMenu Other []) = (GM stat pic OtherMenu NothingB [])
  -- Exit
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic MainMenu Exit []) = (GM stat pic MainMenu Other [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic MainMenu Exit []) = (GM stat pic MainMenu NothingB [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic MainMenu Exit []) = undefined -- (definir na função reageKeyGloss)
-- PlayerPicMenu
  --NothingB
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic PlayerPicMenu NothingB []) = (GM  stat pic PlayerPicMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic PlayerPicMenu NothingB []) = (GM  stat pic PlayerPicMenu Player [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM stat pic PlayerPicMenu NothingB []) = (GM  stat pic PlayerPicMenu NothingB [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM stat pic PlayerPicMenu NothingB []) = (GM  stat pic PlayerPicMenu NothingB [])
  --Player
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic PlayerPicMenu Player []) = (GM  stat pic PlayerPicMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic PlayerPicMenu Player []) = (GM  stat pic PlayerPicMenu Play [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM stat ((0,0,list):t) PlayerPicMenu Player []) = (GM  stat ((0,7,list):t) PlayerPicMenu Player [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM stat ((0,x,list):t) PlayerPicMenu Player []) = (GM  stat ((0,x-1,list):t) PlayerPicMenu Player [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM stat ((0,7,list):t) PlayerPicMenu Player []) = (GM  stat ((0,0,list):t) PlayerPicMenu Player [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM stat ((0,x,list):t) PlayerPicMenu Player []) = (GM  stat ((0,x+1,list):t) PlayerPicMenu Player [])
  -- Play
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic PlayerPicMenu Play []) = (GM  stat pic PlayerPicMenu Player [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic PlayerPicMenu Play []) = (GM  stat pic PlayerPicMenu Back [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic PlayerPicMenu Play []) = (GM stat pic BotMenu NothingB [])
  -- Back
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic PlayerPicMenu Back []) = (GM  stat pic PlayerPicMenu Play [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic PlayerPicMenu Back []) = (GM  stat pic PlayerPicMenu NothingB [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic PlayerPicMenu Back []) = (GM stat pic MainMenu NothingB [])
-- BotMenu
  -- NothingB
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic BotMenu NothingB []) = (GM  stat pic BotMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic BotMenu NothingB []) = (GM  stat pic BotMenu CPU1 [])
  -- CPU1
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic BotMenu CPU1 []) = (GM  stat pic BotMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic BotMenu CPU1 []) = (GM  stat pic BotMenu CPU2 [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM stat (p:(1,0,list):t) BotMenu CPU1 []) = (GM  stat (p:(1,7,list):t) BotMenu CPU1 [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM stat (p:(1,x,list):t) BotMenu CPU1 []) = (GM  stat (p:(1,x-1,list):t) BotMenu CPU1 [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM stat (p:(1,7,list):t) BotMenu CPU1 []) = (GM  stat (p:(1,0,list):t) BotMenu CPU1 [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM stat (p:(1,x,list):t) BotMenu CPU1 []) = (GM  stat (p:(1,x+1,list):t) BotMenu CPU1 [])
  -- CPU2
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic BotMenu CPU2 []) = (GM  stat pic BotMenu CPU1 [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic BotMenu CPU2 []) = (GM  stat pic BotMenu CPU3 [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM stat (p:c1:(2,0,list):t) BotMenu CPU2 []) = (GM  stat (p:c1:(2,7,list):t) BotMenu CPU2 [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM stat (p:c1:(2,x,list):t) BotMenu CPU2 []) = (GM  stat (p:c1:(2,x-1,list):t) BotMenu CPU2 [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM stat (p:c1:(2,7,list):t) BotMenu CPU2 []) = (GM  stat (p:c1:(2,0,list):t) BotMenu CPU2 [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM stat (p:c1:(2,x,list):t) BotMenu CPU2 []) = (GM  stat (p:c1:(2,x+1,list):t) BotMenu CPU2 [])
  -- CPU3
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic BotMenu CPU3 []) = (GM  stat pic BotMenu CPU2 [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic BotMenu CPU3 []) = (GM  stat pic BotMenu Play [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM stat (p:c1:c2:(3,0,list):[]) BotMenu CPU3 []) = (GM  stat (p:c1:c2:(3,7,list):[]) BotMenu CPU3 [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM stat (p:c1:c2:(3,x,list):[]) BotMenu CPU3 []) = (GM  stat (p:c1:c2:(3,x-1,list):[]) BotMenu CPU3 [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM stat (p:c1:c2:(3,7,list):[]) BotMenu CPU3 []) = (GM  stat (p:c1:c2:(3,0,list):[]) BotMenu CPU3 [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM stat (p:c1:c2:(3,x,list):[]) BotMenu CPU3 []) = (GM  stat (p:c1:c2:(3,x+1,list):[]) BotMenu CPU3 [])
  -- Play
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic BotMenu Play []) = (GM  stat pic BotMenu CPU3 [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic BotMenu Play []) = (GM  stat pic BotMenu Back [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic BotMenu Play []) = (GM stat pic MapMenu NothingB [])
  -- Back
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic BotMenu Back []) = (GM  stat pic BotMenu Play [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic BotMenu Back []) = (GM  stat pic BotMenu NothingB [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic BotMenu Back []) = (GM stat pic PlayerPicMenu NothingB [])
-- MapMenu
  -- NothingB
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic MapMenu NothingB []) = (GM  stat pic MapMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic MapMenu NothingB []) = (GM  stat pic MapMenu MapSelect [])
  -- MapSelect
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic MapMenu MapSelect []) = (GM  stat pic MapMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic MapMenu MapSelect []) = (GM  stat pic MapMenu Play [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM (Estado map2 j) pic MapMenu MapSelect []) = (GM  (Estado map1 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM (Estado map3 j) pic MapMenu MapSelect []) = (GM  (Estado map2 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM (Estado map4 j) pic MapMenu MapSelect []) = (GM  (Estado map3 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM (Estado map5 j) pic MapMenu MapSelect []) = (GM  (Estado map4 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM (Estado map6 j) pic MapMenu MapSelect []) = (GM  (Estado map5 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM (Estado map7 j) pic MapMenu MapSelect []) = (GM  (Estado map6 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM (Estado map8 j) pic MapMenu MapSelect []) = (GM  (Estado map7 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM (Estado map9 j) pic MapMenu MapSelect []) = (GM  (Estado map8 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyLeft) Down _ _) (GM (Estado map10 j) pic MapMenu MapSelect []) = (GM  (Estado map9 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM (Estado map1 j) pic MapMenu MapSelect []) = (GM  (Estado map2 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM (Estado map2 j) pic MapMenu MapSelect []) = (GM  (Estado map3 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM (Estado map3 j) pic MapMenu MapSelect []) = (GM  (Estado map4 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM (Estado map4 j) pic MapMenu MapSelect []) = (GM  (Estado map5 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM (Estado map5 j) pic MapMenu MapSelect []) = (GM  (Estado map6 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM (Estado map6 j) pic MapMenu MapSelect []) = (GM  (Estado map7 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM (Estado map7 j) pic MapMenu MapSelect []) = (GM  (Estado map8 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM (Estado map8 j) pic MapMenu MapSelect []) = (GM  (Estado map9 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM (Estado map9 j) pic MapMenu MapSelect []) = (GM  (Estado map10 j) pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyRight) Down _ _) (GM (Estado map10 j) pic MapMenu MapSelect []) = (GM  (Estado map1 j) pic MapMenu MapSelect [])
  -- Play
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic MapMenu Play []) = (GM  stat pic MapMenu MapSelect [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic MapMenu Play []) = (GM  stat pic MapMenu Back [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic MapMenu Play []) = (GM stat pic GameMenu NothingB [])
  -- Back
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic MapMenu Back []) = (GM  stat pic MapMenu Play [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic MapMenu Back []) = (GM  stat pic MapMenu NothingB [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic MapMenu Back []) = (GM stat pic BotMenu NothingB [])
-- OtherMenu
  -- NothingB
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic OtherMenu NothingB []) = (GM  stat pic OtherMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic OtherMenu NothingB []) = (GM  stat pic OtherMenu SC [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic OtherMenu NothingB []) = (GM stat pic BotMenu NothingB [])
  -- SC
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic OtherMenu SC []) = (GM  stat pic OtherMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic OtherMenu SC []) = (GM  stat pic OtherMenu KeysHelp [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic OtherMenu SC []) = (GM stat pic SCMenu NothingB [])
  -- KeysHelp
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic OtherMenu KeysHelp []) = (GM  stat pic OtherMenu SC [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic OtherMenu KeysHelp []) = (GM  stat pic OtherMenu Credits [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic OtherMenu KeysHelp []) = (GM stat pic KeysMenu NothingB [])
  -- Credits
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic OtherMenu Credits []) = (GM  stat pic OtherMenu KeysHelp [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic OtherMenu Credits []) = (GM  stat pic OtherMenu Back [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic OtherMenu Credits []) = (GM stat pic CreditsMenu NothingB [])
  -- Back
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic OtherMenu Back []) = (GM  stat pic OtherMenu Credits [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic OtherMenu Back []) = (GM  stat pic OtherMenu NothingB [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic OtherMenu Back []) = (GM stat pic MainMenu NothingB [])
-- CreditsMenu
  -- NothingB
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic CreditsMenu NothingB []) = (GM  stat pic CreditsMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic CreditsMenu NothingB []) = (GM  stat pic CreditsMenu ExitCredits [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic CreditsMenu NothingB []) = (GM stat pic CreditsMenu NothingB [])
  -- ExitCredits
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic CreditsMenu ExitCredits []) = (GM  stat pic CreditsMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic CreditsMenu ExitCredits []) = (GM  stat pic CreditsMenu NothingB [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic CreditsMenu ExitCredits []) = (GM stat pic OtherMenu NothingB [])
-- SCMenu
  -- NothingB
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic SCMenu NothingB []) = (GM  stat pic SCMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic SCMenu NothingB []) = (GM  stat pic SCMenu Menu [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic SCMenu NothingB []) = (GM stat pic OtherMenu NothingB [])
  -- Menu
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic SCMenu Menu []) = (GM  stat pic SCMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic SCMenu Menu []) = (GM  stat pic SCMenu Exit [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic SCMenu Menu []) = (GM stat pic MainMenu NothingB [])
  -- Exit
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic SCMenu Exit []) = (GM  stat pic SCMenu Menu [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic SCMenu Exit []) = (GM  stat pic SCMenu NothingB [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic SCMenu Exit []) =  undefined -- (definir na função reageKeyGloss)
-- GameOver
  -- NothingB
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic GameOver NothingB []) = (GM  stat pic GameOver NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic GameOver NothingB []) = (GM  stat pic GameOver PlayAgain [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic GameOver NothingB []) = (GM stat pic GameOver NothingB [])
  -- PlayAgain
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic GameOver PlayAgain []) = (GM  stat pic GameOver NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic GameOver PlayAgain []) = (GM  stat pic GameOver Menu [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM (Estado m [(Jogador p d v c e),(Jogador p1 d1 v1 c1 e1),(Jogador p2 d2 v2 c2 e2),(Jogador p3 d3 v3 c3 e3)]) pic GameOver PlayAgain []) = (GM (Estado m [(Jogador 0 0 0 3 (Ar 2 0 0)),(Jogador 1 0 0 3 (Ar 2 0 0)),(Jogador 2 0 0 3 (Ar 2 0 0)),(Jogador 3 0 0 3 (Ar 2 0 0))]) pic GameMenu NothingB [])
  -- Menu
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic GameOver Menu []) = (GM  stat pic GameOver PlayAgain [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic GameOver Menu []) = (GM  stat pic GameOver NothingB [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic GameOver Menu []) = (GM stat pic MainMenu NothingB [])
-- PauseMenu
  -- NothingB
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic PauseMenu NothingB []) = (GM  stat pic PauseMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic PauseMenu NothingB []) = (GM  stat pic PauseMenu Back [])
  -- Back
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic PauseMenu Back []) = (GM  stat pic PauseMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic PauseMenu Back []) = (GM  stat pic PauseMenu Menu [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic PauseMenu Back []) = (GM stat pic GameMenu NothingB [])
  -- Menu
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic PauseMenu Menu []) = (GM  stat pic PauseMenu Back [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic PauseMenu Menu []) = (GM  stat pic PauseMenu Exit [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM (Estado m [(Jogador 0 _ _ _ (Chao False))]) pic PauseMenu Menu []) = (GM (Estado m [(Jogador 0 0 0 3 (Chao False))]) pic MainMenu NothingB [])
  -- Exit
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic PauseMenu Exit []) = (GM  stat pic PauseMenu Menu [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic PauseMenu Exit []) = (GM  stat pic PauseMenu NothingB [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic PauseMenu Exit []) = undefined -- definir na reageKeyGloss
-- KeysHelp
  -- NothingB
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic KeysMenu NothingB []) = (GM  stat pic CreditsMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic KeysMenu NothingB []) = (GM  stat pic KeysMenu Back [])
  -- Back
reageKey (EventKey (SpecialKey KeyUp) Down _ _) (GM stat pic KeysMenu Back []) = (GM  stat pic KeysMenu NothingB [])
reageKey (EventKey (SpecialKey KeyDown) Down _ _) (GM stat pic KeysMenu Back []) = (GM  stat pic KeysMenu NothingB [])
reageKey (EventKey (SpecialKey KeyEnter) Down _ _) (GM stat pic KeysMenu Back []) = (GM stat pic OtherMenu NothingB [])

-- Ignore the rest
reageKey _ s = s

reageTime :: Float -> GameMode -> GameMode
reageTime t (GM (Estado m (j:js)) pic GameMenu a []) = (GM (Estado m ((passo (realToFrac t) m j) : js)) pic GameMenu a [])
reageTime t e = e

-- gloss

estadoGlossInicial :: Textures -> EstadoGloss
estadoGlossInicial textures = ((GM (Estado map1 [(Jogador 0 0 0 3 (Ar 2 0 0 ))]) [(0,0,playerPics),(1,1,playerPics),(2,2,playerPics),(3,3,playerPics)] MainMenu NothingB []),textures,(0,2728))
                              where
                                (backgroundPic,menuPic,playerPics,mapTextures) = textures
                                (fundoInvertido:fundoNormal:fundoWinner:gameLogo:keysHelpforeground:creditsForeground:[]) = backgroundPic
                                (cpu1:cpu2:cpu3:creditsPic:goB:mapPic:otherPic:playPic:playAgainPic:playerPic:exitPic:source_codePic:helpPic:quitB:uno:dos:tres:colasNumero:pausePic:menuPics:map1Pic:map2Pic:map3Pic:map4Pic:map5Pic:map6Pic:map7Pic:map8Pic:map9Pic:map10Pic:map11Pic:map12Pic:map13Pic:map14Pic:map15Pic:[]) = menuPic
                                (p1:p2:p3:p4:p5:p6:p7:p8:pm:[]) = playerPics
                                (retaT:retaL:retaC:retaR:retaB:ut1:ul1:uc1:ur1:ub1:ut2:ul2:uc2:ur2:ub2:ut3:ul3:uc3:ur3:ub3:ut4:ul4:uc4:ur4:ub4:ut5:ul5:uc5:ur5:ub5:dt1:dl1:dc1:dr1:db1:dt2:dl2:dc2:dr2:db2:dt3:dl3:dc3:dr3:db3:dt4:dl4:dc4:dr4:db4:dt5:dl5:dc5:dr5:db5:[]) = mapTextures

drawEstadoGloss :: EstadoGloss -> IO (Picture)
drawEstadoGloss (e,li,(i,j)) = do
                              let deg = scale 0.4 0.4 (Pictures (drawBG ++ drawmad))
                                          where
                                            (backgroundPic,menuPic,playerPics,mapTextures) = li
                                            drawmad =(drawEstado e li )
                                            bg = encontraIndiceLista 1 backgroundPic
                                            bgF = encontraIndiceLista 0 backgroundPic
                                            drawBG = [Translate i 0 bg] ++ [Translate j 0 bgF]
                              return deg



reageKeyGloss :: Event -> EstadoGloss -> IO (EstadoGloss)
reageKeyGloss ev (e,li,(i,j)) = do
                                 let rkg = (reageKey ev e, li,(i,j))
                                 return rkg

reageTimeGloss :: Float -> EstadoGloss -> IO (EstadoGloss)
reageTimeGloss t (e,li,(i,j)) =do
                                let rtg = ((reageTime t e),li,(i - frame,j - frame))
                                return rtg

fr :: Int
fr = 120

window :: Display
window = FullScreen


-- Funçoes Por Definir
  -- chaoX , chaoY , arX , arY , posColaX , posColaY , posPlayerX , posPlayerY , -degChao , -degAr
  -- startingX , startingYA , startingYC , drawWinner , drawPlayerMenu , drawMapMenu , drawOther , drawMainMenu , drawBotMenu , drawCredits
