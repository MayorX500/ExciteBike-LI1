module AuxGloss where

-- import Constroi -- ^ For Future implementation of Custom Maps
import LI11920
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Tarefa0_2019li1g077
import Tarefa2_2019li1g077
import Data.Function (on)
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)

data GameMode = GM
  {actualState :: Estado -- ^ The game's "Estado" {(Estado Mapa [Jogadores])}
  ,playerImage :: [Ppic] -- ^ A List of (Int,Int,[Pictures]) that tell the Image of which Player / Bot {(PlayerIndex,PictureIndex,[PlayerPictures])}
  ,currentMenu :: Menu -- ^ Reports the "menu/state" the game is on (Main Menu, Game Menu, etc)
  ,selectedOption :: Option -- ^ Tells which button is being highlighted
  ,customMap :: [Instrucao] -- ^ For Future implementation of Custom Maps
  } deriving (Show)

data Menu
    = MainMenu | PlayerPicMenu | BotMenu
    | MapMenu | OtherMenu | GameOver
    | CreditsMenu | PauseMenu | GameMenu
    |SCMenu | KeysMenu
    deriving (Eq,Show)

data Option
    = Play | Other | Exit                       -- MainMenu
    | Player | Back -- | Play               -- PlayerPicMenu
    | CPU1 | CPU2 | CPU3 -- | Play | Back   -- BotMenu
    | MapSelect -- | Play | Back            -- MapMenu
    | SC | KeysHelp | Credits -- | Back     -- OtherMenu
    | PlayAgain | Menu                      -- GameOver
    | ExitCredits                           -- CreditsMenu
 -- | Back | Menu | Exit                    -- PauseMenu
 -- | Menu | Exit                           -- SCMenu
 -- | Back                                  -- KeysMenu
    | NothingB                              -- Just Nothing Highlighted
    deriving (Eq,Show)

type Textures = ([Picture] -- ^ Backgrounds
                ,[Picture] -- ^ Menu
                ,[Picture] -- ^ Players
                ,[Picture]) -- ^ Map Textures

type EstadoGloss = (GameMode,Textures,(Float,Float))

type Ppic = (Int -- ^ Player Index
            ,Int -- ^ Picture Index
            ,[Picture]) -- ^ Pictures available for character selection

type Layer = (Int             -- ^ Drawn Layer Index
              ,[Picture])     -- ^ Drawn Picture

-- | PlaceHolders / Constants

width :: Float -- ^ Starting point of the first track in the X axis
width = (-550)
height :: Float -- ^ Starting point of the first track in the Y axis
height = (-225)
pieceMid :: Float -- ^ Mid width of the drawn piece
pieceMid = (59.5)
pieceSize :: Float -- ^ Lenght of the drawn track (X axis)
pieceSize = 300
pieceLenght :: Float -- ^ Width of the drawn track (Y Axix)
pieceLenght = 119
frame :: Float -- ^ Background moving speed
frame = 0.15

-- * Functions for Tarefa5_2019li1g077

-- | startingX
startingX :: Jogador -> Float
startingX (Jogador p d v c e) = width - ((realToFrac d)*pieceSize)

-- | startingYC
startingYC :: Int -> Double -> Mapa -> Float
startingYC p d mapa = (height + ((realToFrac p)*pieceLenght)) --((height - ((realToFrac (tallBoy (pBoy d) (encontraPosicaoMatriz (p, floor d) mapa )))* pieceSize)) + ((realToFrac p)*pieceLenght))

startingYA :: Int -> Double -> Double -> Mapa -> Float
startingYA p d a mapa = (height + ((realToFrac p)*pieceLenght)) --((height - ((realToFrac (a))* pieceSize)) + ((realToFrac p)*pieceLenght))

posGlueX :: Int -> Float
posGlueX x |x == 0 = (-500)
           |x == 1 = 500
           |x == 2 = (-500)
           |x == 3 = 500

posPlayerX:: Int -> Float
posPlayerX x |x == 0 = (-500)
             |x == 1 = 500
             |x == 2 = (-500)
             |x == 3 = 500

posGlueY :: Int -> Float
posGlueY x |x == 0 = (200)
             |x == 1 = (200)
             |x == 2 = (-200)
             |x == 3 = (-200)

posPlayerY:: Int -> Float
posPlayerY x |x == 0 = (200)
             |x == 1 = (200)
             |x == 2 = (-200)
             |x == 3 = (-200)

degChao :: Int -> Double -> Peca -> Float
degChao p d peca = (-(realToFrac (anguloRampa peca)))

yChao::Int -> Double -> Peca -> Float
yChao p d peca = (height + ((realToFrac (tallBoy (pBoy d) peca)) * pieceSize) - ((realToFrac p) * pieceLenght)) + pieceMid

yAr :: Double -> Int -> Float
yAr a p = (height + ((realToFrac a) * pieceSize) - ((realToFrac p) * pieceLenght)) + pieceMid

-- * Auxiliar Functions
matrixEnd :: [Jogador] -> Mapa -> [Bool]
matrixEnd (h:t) m = [ePosicaoMatrizValida (whereBoy h) m] ++ matrixEnd t m
matrixEnd [] m = []

checkFalse :: [Bool] -> Bool
checkFalse (h:t) |h = checkFalse t
                 |otherwise = True
checkFalse [] = False

isOver :: [Jogador] -> Mapa -> Bool
isOver j m = checkFalse (matrixEnd j m)

givePictureWinner :: [Ppic] -> Int -> Ppic
givePictureWinner (h:t) 0 = h
givePictureWinner (h:t) x = givePictureWinner t (x-1)

giveP :: Ppic -> Picture
giveP (_,i,l) = l!!i

giveWinner :: GameMode -> Picture
giveWinner (GM (Estado _ j) lpic _ _ _) = giveP (givePictureWinner lpic (giveBestPlayer j))

giveBestPlayer :: [Jogador] -> Int
giveBestPlayer j = giveInt (whichIsMax $ giveDistances j) (giveDistances j) 0

giveDistances :: [Jogador] -> [Double]
giveDistances (j:js) = [distanciaJogador (j)] ++ giveDistances js
giveDistances [] = []

whichIsMax :: [Double] -> Double
whichIsMax (h:x:t) = if h > x
  then whichIsMax (h:t)
  else whichIsMax (x:t)
whichIsMax [x] = x

giveInt :: Double -> [Double] -> Int -> Int
giveInt a (h:t) d |a == h = d
                  |otherwise = giveInt a t (d+1)
giveInt a [] d = undefined
-- | Gives a list from the Index a to Index b
listfrom2 :: [a] -- ^ Give a List
            -> Int -- ^ Give the starting position
            -> Int -- ^ Give the stoping position
            -> [a] -- ^ Recieve a List with the elements from the first Int to the second Int
listfrom2 [] _ _ = []
listfrom2 (h:t) 0 y |(y-1) >= 0 = h : (listfrom2 t 0 (y-1))
                    |otherwise = [h]
listfrom2 (h:t) x y = listfrom2 t (x-1) (y-1)

-- | Gives a list with the elements in the positions given
eInList :: [Int] -- ^ Give a Int List (Positions)
          -> [a] -- ^ Give a List
          -> [a] -- ^ Recive a List of Elements that were in the positions given
eInList [] _ = []
eInList [x] [] = []
eInList (h:t) l = (l !! h) : (eInList t l)

gLayers :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
gLayers = map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst)
          . sortBy (comparing fst)

justLayer :: (Eq a, Ord a) => [(a, [b])] -> [b]
justLayer ((n,pic):t) =pic ++ justLayer t
justLayer [] = []
