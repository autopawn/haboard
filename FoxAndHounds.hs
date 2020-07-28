-- FoxAndHounds.hs contiene la lógica específica del juego Fox and Hounds

import Game
import Piece

import System.Random (getStdGen,randoms)

-- Se crea el tipo FoxAndHounds.

data FoxAndHounds = FoxAndHounds Int [Piece] -- jugador actual y lista de piezas

-- Se define el tamaño del tablero

boardX :: Int
boardX = 8
boardY :: Int
boardY = 8

-- Se hace que FoxAndHounds sea instancia de Game y se definen las funciones necesarias.

instance Game FoxAndHounds where
    current (FoxAndHounds c _) = c

    winner (FoxAndHounds c pc) mvs
        -- Si el jugador no tiene movimientos, gana el otro
        | null mvs                            = Just (1 - c)
        -- Si el jugador 0 (Fox) llega a y = 0, gana
        | any (\(p,x,y,k) -> p==0 && y==0) pc = Just 0
        -- En todo otro caso se sigue jugando
        | otherwise                           = Nothing

    movements st@(FoxAndHounds c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

{-
    pieceMoves genera los movimientos posibles para los dos tipos de piezas del juego.
    Recibe un FoxAndHounds y una pieza.
    Retorna el un movimiento, consistente de su nombre y el estado que genera.
-}

pieceMoves :: FoxAndHounds -> Piece -> [(String,FoxAndHounds)]
pieceMoves (FoxAndHounds c pcs) (p,x,y,k) = let
    -- Posiciones posibles dependiendo del tipo de pieza
    steps
        | k=='H'  = [(x+1,y+1),(x-1,y+1)]
        | k=='F'  = [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)]
    -- Checkear si es posible moverse a una posición
    isValid (xf,yf) =
        pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY
    -- Generar los posibles movimientos
    steps2 = filter isValid steps
    in [(moveName (x,y) (xf,yf), FoxAndHounds (1-c) (movePiece (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2]

-- Inicialización del juego.

foxAndHoundsIni :: FoxAndHounds
foxAndHoundsIni = FoxAndHounds 0 [(0,0,7,'F'),(1,1,0,'H'),(1,3,0,'H'),(1,5,0,'H'),(1,7,0,'H')]

-- Se define como se transforma un FoxAndHounds a String.

instance Show FoxAndHounds where
    show (FoxAndHounds _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'H') -> "🄷 "
            Just (_,_,_,'F') -> "🄵 "
            Nothing          -> if (x+y) `mod` 2 == 0 then "■ " else "□ "
        in drawBoard (boardX,boardY) draw

{-
    foxAndHoundsEval corresponde a la función de evaluación que usa cpuEval.
    Recibe un estado del jeugo.
    Retorna la evaluación del estado.
-}

foxAndHoundsEval :: FoxAndHounds -> Float
foxAndHoundsEval (FoxAndHounds c pcs) = let
    fI = fromIntegral
    fox@(_,fx,fy,_) = head (filter (\(p,_,_,_) -> p == 0) pcs)
    houndsum = sum [if y>=fy then 0.5 else 0.05 * abs (fI x - fI fx) | (p,x,y,k) <- pcs, p==1]
    in (houndsum + 7 - fI fy) * (if c==0 then 1.0 else -1.0)

-- Main.

main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    -- Jugar
    configAndExecute foxAndHoundsIni seed "foxAndHounds" --Ejecutamos y configuramos el juego


