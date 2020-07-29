-- HareAndHounds.hs contiene la lógica específica del juego Hare and Hounds

import Game
import PieceHare

import System.Random (getStdGen,randoms)

-- Se crea el tipo HareAndHounds.

data HareAndHounds = HareAndHounds Int [Piece] -- jugador actual y lista de piezas

-- Se define el tamaño del tablero

boardX :: Int
boardX = 3
boardY :: Int
boardY = 5



-- Se hace que HareAndHounds sea instancia de Game y se definen las funciones necesarias.

instance Game HareAndHounds where
    current (HareAndHounds c _) = c

    winner (HareAndHounds c pc) mvs
        -- Si el jugador no tiene movimientos, gana el otro
        | null mvs                            = Just (1 - c)
        -- Si el jugador 0 (Hare) llega a y = 0, gana
        | any (\(p,x,y,k) -> p==0 && y==0) pc = Just 0
        -- En todo otro caso se sigue jugando
        | otherwise                           = Nothing

    movements st@(HareAndHounds c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

{-
    pieceMoves genera los movimientos posibles para los dos tipos de piezas del juego.
    Recibe un HareAndHounds y una pieza.
    Retorna el un movimiento, consistente de su nombre y el estado que genera.
-}

pieceMoves :: HareAndHounds -> Piece -> [(String,HareAndHounds)]
pieceMoves (HareAndHounds c pcs) (p,x,y,k) = let
    {- Posiciones posibles dependiendo del tipo de pieza, y dependiendo de la posición,
    descubrí que al sumar las coordenadas, y al calcular su módulo de 2, aquellas que daban 0 limitaban el movimiento impidiendo diagonalidad,
    por otro lado, aquellas que no daban 0, daban movimiento total (con tal que no fuera fuera del tablero)
  -}
    steps
        | k=='F' && (x+y)`mod`2 == 0 = [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]
        | k=='F' && (x+y)`mod`2 /= 0 = [(x+1,y),(x,y+1),(x+1,y+1),(x-1,y),(x,y-1),(x-1,y-1),(x+1,y-1),(x-1,y+1)]

        | k=='H' && (x+y)`mod`2 == 0 = [(x+1,y),(x,y+1),(x-1,y)]
        | k=='H' && (x+y)`mod`2 /= 0 = [(x+1,y),(x,y+1),(x+1,y+1),(x-1,y),(x-1,y+1)]

        | otherwise = []
    -- Checkear si es posible moverse a una posición, como HareAndHounds no tiene un tablero rectangular, tuve que "cortar los bordes"
    isValid (xf,yf) =
        pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY && (xf,yf) /= (0,0) && (xf,yf) /= (2,0) && (xf,yf) /= (0,4) && (xf,yf) /= (2,4)
    -- Generar los posibles movimientos`elem`
    steps2 = filter isValid steps
    in [(moveName (x,y) (xf,yf), HareAndHounds (1-c) (movePiece (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2]
-- Inicialización del juego.

hareAndHoundsIni :: HareAndHounds
hareAndHoundsIni = HareAndHounds 0 [(0,1,4,'F'),(1,1,0,'H'),(1,0,1,'H'),(1,2,1,'H')]

-- Se define como se transforma un HareAndHounds a String. Como el tablero es diferente, tuve que modificar Piece.hs (ahora llamada PieceHare.hs), y a la vez, quitar la condicion de los espacios diagonales blancos

instance Show HareAndHounds where
    show (HareAndHounds _ pcs) = let
        draw (x,y) = case pieceAt (y,x) pcs of
            Just (_,_,_,'H') -> "☉ "
            Just (_,_,_,'F') -> "◉ "
            Nothing          -> "▓ "
        in drawBoard (boardY,boardX) draw

{-
    hareAndHoundsEval corresponde a la función de evaluación que usa cpuEval.
    Recibe un estado del juego.
    Retorna la evaluación del estado.
-}

hareAndHoundsEval :: HareAndHounds -> Float
hareAndHoundsEval (HareAndHounds c pcs) = let
    fI = fromIntegral
    hare@(_,fx,fy,_) = head (filter (\(p,_,_,_) -> p == 0) pcs)
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
    -- Crear jugadores
    let player0 = human "Hare"
    let player1 = cpuEval "Hounds" hareAndHoundsEval
    -- Jugar
    execute hareAndHoundsIni [player0,player1] seed
