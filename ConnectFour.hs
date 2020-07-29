import Game
import Piece
import Data.List (delete, (\\))
import System.Random (getStdGen,randoms)
import Data.Maybe (Maybe)

data ConnectFour = ConnectFour Int [Piece]

boardX :: Int
boardX = 8
boardY :: Int
boardY = 7

instance Game ConnectFour where
    current (ConnectFour c _) = c

    winner (ConnectFour c pc) mvs
        |any (testfinal pc) [(p,x,y,k) | (p,x,y,k) <- pc , p==c]     = Just c
        |any (testfinal pc) [(p,x,y,k) | (p,x,y,k) <- pc , p==(1-c)] = Just (1 - c)
        |null mvs                                                    = error "empate"
        |otherwise                                                   = Nothing
        
    movements (ConnectFour c pc) = let
        pc1 = [(p,x,y,k) | (p,x,y,k) <- pc , y==6]
        pc2 = [x | (p,x,y,k) <- pc1]
        noneFullList = [1..7] \\ pc2
        pc3 = [ (n,maxelem n pc) | n<-noneFullList  ]
        possible = map (\(x,y) -> (c,x,y,tag c)) pc3
        in [(moveName (x,y-1) (x,y) , ConnectFour (1-c) (extra:pc)) | extra@(p,x,y,k) <- possible]


--Las siguientes funciones se usan para la condición de victoria
testhorizontal :: [Piece] -> Piece -> Bool
testhorizontal xs (p,x,y,k) = if elem (p,x,y,k) xs && elem (p,x+1,y,k) xs && elem (p,x+2,y,k) xs && elem (p,x+3,y,k) xs then True else False

testvertical :: [Piece] -> Piece -> Bool
testvertical xs (p,x,y,k) = if elem (p,x,y,k) xs && elem (p,x,y+1,k) xs && elem (p,x,y+2,k) xs && elem (p,x,y+3,k) xs then True else False

testdiagonalp :: [Piece] -> Piece -> Bool
testdiagonalp xs (p,x,y,k) = if elem (p,x,y,k) xs && elem (p,x+1,y+1,k) xs && elem (p,x+2,y+2,k) xs && elem (p,x+3,y+3,k) xs then True else False

testdiagonaln :: [Piece] -> Piece -> Bool
testdiagonaln xs (p,x,y,k) = if elem (p,x,y,k) xs && elem (p,x+1,y-1,k) xs && elem (p,x+2,y-2,k) xs && elem (p,x+3,y-3,k) xs then True else False

testfinal :: [Piece] -> Piece -> Bool
testfinal xs p = if testhorizontal xs p && testvertical xs p && testdiagonalp xs p && testdiagonaln xs p then True else False

--Estado inicial
connectFourini :: ConnectFour
connectFourini = ConnectFour 0 []

--Asignación de carácter correspondiente según jugador
tag :: Int -> Char
tag 0 = 'R'
tag 1 = 'B'
tag a = error "nunca debería pasar"

--Función para obtener la altura usada de una columna del tablero
maxelem :: Int -> [Piece] ->Int
maxelem n xs= let
    mystack  = filter (\(p,x,y,k) -> x==n) xs
    maxy     = maximum . (\as -> 0:as) $  map (\(p,x,y,k) -> y) mystack
    in maxy+1

instance Show ConnectFour where
    show (ConnectFour _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'R') -> "R "
            Just (_,_,_,'B') -> "B "
            Nothing          -> "□ "
        in drawBoard (boardX,boardY) draw


main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    -- Crear jugadores
    let player0 = cpuRand "Son"
    let player1 = cpuRand "Father" 
    -- Jugar
    execute connectFourini [player0,player1] seed