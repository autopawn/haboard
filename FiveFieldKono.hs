-- FiveFieldKono.hs contiene la lógica específica del juego Five Field Kono

import Game
import Piece

import System.Random (getStdGen,randoms)

-- Se crea el tipo FiveFieldKono.

-- ++ Ahora FiveFieldKono tiene jugador, numero de turno y una lista de piezas ++
data FiveFieldKono = FiveFieldKono Int Int [Piece]

boardX :: Int
boardX = 5
boardY :: Int
boardY = 5

{-
    inStartPos sirve para determinar si una coordenada corresponde a una posción inicial o no.
    Recibe una coordenada.
    Retorna un Int que dice si la posición inicial corresponde al jugador 1, 0 o si no es una
    posición inicial.
-}

inStartPos :: (Int,Int) -> Int
inStartPos (x,y)
    | y==0                         = 1
    | y==1 && (x==0 || x==bx-1)    = 1
    | y==by-1                      = 0
    | y==by-2 && (x==0 || x==bx-1) = 0
    | otherwise                    = -1
    where
    bx = boardX
    by = boardY

-- Se hace que FiveFieldKono sea instancia de Game y se definen las funciones necesarias.

instance Game FiveFieldKono where
    current (FiveFieldKono c _ _) = c
    winner (FiveFieldKono c turn pc) mvs
        -- ++ Si jugador 0 tiene sus espacios usados y el turno es mayor a 4, pierde ++
        | turn > 8 && all (\(x,y) -> pieceAt (x,y) pc /= Nothing) [(0,3),(0,4),(1,4),(2,4),(3,4),(4,3),(4,4)] = Just 1
        -- ++ Si jugador 1 tiene sus espacios usados y el turno es mayor a 4, pierde ++
        | turn > 9 && all (\(x,y) -> pieceAt (x,y) pc /= Nothing) [(0,1),(0,0),(1,0),(2,0),(3,0),(4,0),(4,1)] = Just 0
        -- Si el jugador actual no tiene movimientos, gana el otro
        | null mvs = Just (1 - c)
        -- Si todas las piezas del jugador0 están en las posiciones iniciales del jugador1, gana
        | all (\(p,x,y,k) -> inStartPos (x,y) == 1) (playerPieces pc 0) = Just 0
        -- Si todas las piezas del jugador1 están en las posiciones iniciales del jugador0, gana
        | all (\(p,x,y,k) -> inStartPos (x,y) == 0) (playerPieces pc 1) = Just 1
        -- Si ninguna se cumple, se sigue jugando
        | otherwise = Nothing

    movements st@(FiveFieldKono c _ pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

{-
    pieceMoves genera los movimientos posibles para una pieza.
    Recibe un juego FiveFieldKono y una pieza.
    Retorna un movimiento, que consiste de un comando y el nuevo estado del juego.
-}
-- ++ Los movimientos tambien tienen un contador de turno aumentado en 1
pieceMoves :: FiveFieldKono -> Piece -> [(String,FiveFieldKono)]
pieceMoves (FiveFieldKono c turn pcs) (p,x,y,k) = let
    -- Posibles posiciones a las que moverse
    steps = [(x-1,y-1),(x-1,y+1),(x+1,y-1),(x+1,y+1)]
    -- Checkear si es posible moverse a una posición
    isValid (xf,yf) =
        pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY
    -- Generar los posibles movimientos
    steps2 = filter isValid steps
    in [(moveName (x,y) (xf,yf), FiveFieldKono (1-c) (turn+1) (movePiece (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2]

-- Se define como se transforma un FiveFieldKono a String.

instance Show FiveFieldKono where
    show (FiveFieldKono c turn pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (0,_,_,_) -> "⚉ "
            Just (1,_,_,_) -> "⚇ "
            Nothing
                | inStartPos (x,y) == 0 -> "◡ "
                | inStartPos (x,y) == 1 -> "◠ "
                | otherwise             -> "  "
        -- ++ Aqui se concadena el turno actual, al string de drawboard. ++
        extra = if turn >= 8 then ": Sudden Death!" else ""
        turno = if (c == 0) then "Turno: " ++ show (div turn 2) ++ extra else ""
        in drawBoard (boardX,boardY) draw ++ turno

-- Inicialización del juego.
-- ++ Inicializamos con turno = 0 ++
fiveFieldKonoIni :: FiveFieldKono
fiveFieldKonoIni = FiveFieldKono 0 0 [(p,x,y,'K') |
    x<-[0..boardX-1], y<-[0..boardY-1], let p = inStartPos (x,y), p>=0]

-- Main.

main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    -- Crear jugadores
    let player0 = human "Walter White"
    let player1 = cpuRand "Jack Black"
    --Jugar
    execute fiveFieldKonoIni [player0,player1] seed
