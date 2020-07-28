-- Damas.hs contiene la lógica específica del juego Damas Inglesas
import Game
import Piece

import System.Random (getStdGen,randoms)

-- Se crea el tipo Damas.

data Damas = Damas Int [Piece] -- jugador actual y lista de piezas

-- Se define el tamaño del tablero

boardX :: Int
boardX = 8
boardY :: Int
boardY = 8

-- Se hace que Damas sea instancia de Game y se definen las funciones necesarias.

instance Game Damas where
    current (Damas c _) = c

    winner (Damas c pc) mvs
        -- Si el jugador no tiene movimientos, gana el otro. Incluye Si fueron capturadas todas las piezas
        | null mvs                                                   = Just (1 - c)
        -- En todo otro caso se sigue jugando
        | otherwise                           = Nothing

    movements st@(Damas c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

-- se implementa la misma funcion utilizada en el proyecto Java para saber si la pieza es propia o enemiga
isInside :: [Piece] -> (Int,Int) -> Int
isInside pcs (x,y) = case pieceAt (x,y) pcs of
    Just(j,_,_,_) -> j
    Nothing       -> -1


-- pieceMoves genera los movimientos posibles para los dos tipos de piezas del juego, Peón y King.

pieceMoves :: Damas -> Piece -> [(String,Damas)]
pieceMoves (Damas c oldPcs) (p,x,y,k) = let
    
    -- Al iniciar el turno convierte los Peones que hayan llegado al otro extremo en Reinas
    pcs = map makeQueen oldPcs 
    -- Movimientos posibles dependiendo del tipo de pieza. De la misma forma que se trabajó en el proyecto Java, se especifica el movimiento para cada lado de la pieza
    steps
        -- Pawn Piece Player0
        | k=='P' && p==0  = (if pieceAt(x+1,y-1) pcs   == Nothing then [(x+1,y-1)] else [])++ -- Mov Up-R 
                            (if pieceAt(x-1,y-1) pcs   == Nothing then [(x-1,y-1)] else [])++ -- Mov Up-L
                            (if isInside pcs (x+1,y-1) == 1       then [(x+2,y-2)] else [])++ -- Cap Up-R
                            (if isInside pcs (x-1,y-1) == 1       then [(x-2,y-2)] else [])   -- Cap Up-L



        -- Pawn Piece Player1                
        | k=='P' && p==1  = (if pieceAt(x-1,y+1) pcs == Nothing then [(x-1,y+1)] else [])++ -- Mov Down-R
                            (if pieceAt(x+1,y+1) pcs == Nothing then [(x+1,y+1)] else [])++ -- Mov Down-L
                            (if isInside pcs (x-1,y+1) == 0     then [(x-2,y+2)] else [])++ -- Cap Up-R
                            (if isInside pcs (x+1,y+1) == 0     then [(x+2,y+2)] else [])   -- Cap Up-L

        -- King Piece Both Players
        | k=='Q'  = takeWhileMod (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x+b,y-b) | b <- [1..7] ]++ -- Mov Up-R
                    takeWhileMod (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x-b,y-b) | b <- [1..7] ]++ -- Mov Up-L
                    takeWhileMod (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x+b,y+b) | b <- [1..7] ]++ -- Mov Down-R
                    takeWhileMod (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x-b,y+b) | b <- [1..7] ]   -- Mov Down-L
      
    -- Checkear si es posible moverse a una posición
    isValid (xf,yf) = pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY

    -- Generar los posibles movimientos
    steps2 = filter isValid steps 

    in [(moveName (x,y) (xf,yf), Damas (1-c) (capture (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2]

    


-- Peon se transforma en Reina si llega al lado contrario
makeQueen :: Piece -> Piece
makeQueen (p,x,y,k)
    | k == 'P' && p == 0 && y == 0          = (p,x,y,'Q') -- Pawn->Queen player0
    | k == 'P' && p == 1 && y == boardY-1   = (p,x,y,'Q') -- Pawn->Queen player1 
    | otherwise                             = (p,x,y,k)

-- takeWhile modificado
takeWhileMod :: (a -> Bool) -> [a] -> [a]
takeWhileMod p [] = []
takeWhileMod p (x:xs) = 
   if p x
   then x : takeWhileMod p xs
   else [x]

--La pieza capturada corresponde a la que está entre la posicion incial y la posición final, y se manda fuera del tablero
capture :: (Int,Int) -> (Int,Int) -> [Piece] -> [Piece]
capture (xi,yi) (xf,yf) pcs = let
    pieces = [if 2*x == (xi+xf) && 2*y == (yi+yf) then (p,90,10,k)
             else (p,x,y,k)| (p,x,y,k) <- pcs]
    
    in [if (x,y)==(xi,yi) then (p,xf,yf,k) else (p,x,y,k) | (p,x,y,k) <- pieces]

-- Inicialización del juego.

damasIni :: Damas               -- Piezas CPU
damasIni = Damas 0 [(1,1,0,'P'),(1,3,0,'P'),(1,5,0,'P'),(1,7,0,'P'),
                    (1,0,1,'P'),(1,2,1,'P'),(1,4,1,'P'),(1,6,1,'P'),
                    (1,1,2,'P'),(1,3,2,'P'),(1,5,2,'P'),(1,7,2,'P'),
                                -- Piezas Jugador
                    (0,0,5,'P'),(0,2,5,'P'),(0,4,5,'P'),(0,6,5,'P'),
                    (0,1,6,'P'),(0,3,6,'P'),(0,5,6,'P'),(0,7,6,'P'),
                    (0,0,7,'P'),(0,2,7,'P'),(0,4,7,'P'),(0,6,7,'P')]
                    
                                -- Test Pieces
                   --[(0,3,6,'P'),(1,2,5,'P')]


-- Se define como se transforma un Damas a String.

instance Show Damas where
    show (Damas _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (0,_,_,'P') -> "⛂ "
            Just (1,_,_,'P') -> "⛀ "
            Just (0,_,_,'Q') -> "⛃ "
            Just (1,_,_,'Q') -> "⛁ "
            Nothing          -> if (x+y) `mod` 2 == 0 then "■ " else "□ "
        in drawBoard (boardX,boardY) draw


-- Main.

main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    -- Crear jugadores
    let player0 = human "Usted" 
    let player1 = cpuRand "CPU"
    -- Jugar
    execute damasIni [player0,player1] seed


