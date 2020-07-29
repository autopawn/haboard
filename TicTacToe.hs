-- TicTacToe.hs contiene la lógica específica del juego Tic Tac Toe
import Game
import Piece
import System.Random (getStdGen,randoms)

-- Se crea el tipo TicTacToe.

data TicTacToe = TicTacToe Int [Piece] -- jugador actual y lista de piezas

-- Se define el tamaño del tablero

boardX :: Int
boardX = 3
boardY :: Int
boardY = 3

-- Se hace que TicTacToe sea instancia de Game y se definen las funciones necesarias.

instance Game TicTacToe where
    current (TicTacToe c _) = c

    winner (TicTacToe c pc) mvs
    -- Checkear si el player 0 ganó
        |(any (\(p,x,y,k) -> p == 0 && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == 0 && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == 0 && x == 2 && y== 2) pc )||
         (any (\(p,x,y,k) -> p == 0 && x == 0 && y== 2) pc && any (\(p,x,y,k) -> p == 0 && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == 0 && x == 2 && y== 0) pc )||       
         (any (\(p,x,y,k) -> p == 0 && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == 0 && x == 1 && y== 0) pc && any (\(p,x,y,k) -> p == 0 && x == 2 && y== 0) pc )||
         (any (\(p,x,y,k) -> p == 0 && x == 0 && y== 1) pc && any (\(p,x,y,k) -> p == 0 && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == 0 && x == 2 && y== 1) pc )||
         (any (\(p,x,y,k) -> p == 0 && x == 0 && y== 2) pc && any (\(p,x,y,k) -> p == 0 && x == 1 && y== 2) pc && any (\(p,x,y,k) -> p == 0 && x == 2 && y== 2) pc )||
         (any (\(p,x,y,k) -> p == 0 && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == 0 && x == 0 && y== 1) pc && any (\(p,x,y,k) -> p == 0 && x == 0 && y== 2) pc )||
         (any (\(p,x,y,k) -> p == 0 && x == 1 && y== 0) pc && any (\(p,x,y,k) -> p == 0 && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == 0 && x == 1 && y== 2) pc )||
         (any (\(p,x,y,k) -> p == 0 && x == 2 && y== 0) pc && any (\(p,x,y,k) -> p == 0 && x == 2 && y== 1) pc && any (\(p,x,y,k) -> p == 0 && x == 2 && y== 2) pc )
                                    = Just 0
    -- Checkear si el player 1 ganó
        |(any (\(p,x,y,k) -> p == 1 && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == 1 && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == 1 && x == 2 && y== 2) pc )||
        (any (\(p,x,y,k) -> p == 1 && x == 0 && y== 2) pc && any (\(p,x,y,k) -> p == 1 && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == 1 && x == 2 && y== 0) pc )||       
        (any (\(p,x,y,k) -> p == 1 && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == 1 && x == 1 && y== 0) pc && any (\(p,x,y,k) -> p == 1 && x == 2 && y== 0) pc )||
        (any (\(p,x,y,k) -> p == 1 && x == 0 && y== 1) pc && any (\(p,x,y,k) -> p == 1 && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == 1 && x == 2 && y== 1) pc )||
        (any (\(p,x,y,k) -> p == 1 && x == 0 && y== 2) pc && any (\(p,x,y,k) -> p == 1 && x == 1 && y== 2) pc && any (\(p,x,y,k) -> p == 1 && x == 2 && y== 2) pc )||
        (any (\(p,x,y,k) -> p == 1 && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == 1 && x == 0 && y== 1) pc && any (\(p,x,y,k) -> p == 1 && x == 0 && y== 2) pc )||
        (any (\(p,x,y,k) -> p == 1 && x == 1 && y== 0) pc && any (\(p,x,y,k) -> p == 1 && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == 1 && x == 1 && y== 2) pc )||
        (any (\(p,x,y,k) -> p == 1 && x == 2 && y== 0) pc && any (\(p,x,y,k) -> p == 1 && x == 2 && y== 1) pc && any (\(p,x,y,k) -> p == 1 && x == 2 && y== 2) pc )
                                    = Just 1
    -- Si un player se queda sin movimientos y nadie gano, entonces el tablero esta lleno y es un empate.
        | null mvs                  = Just 2
    -- Si nadie ha ganado y no hay empate, se sigue jugando.
        | otherwise                 = Nothing

    movements st@(TicTacToe c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

-- Se define como se transforma un TicTacToe a String.

instance Show TicTacToe where
    show (TicTacToe _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (0,_,_,_) -> "X "
            Just (1,_,_,_) -> "O "
            Nothing        -> "□ "
        in drawBoard (boardX,boardY) draw


pieceMoves :: TicTacToe -> Piece -> [(String,TicTacToe)]
pieceMoves (TicTacToe c pcs) (p,x,y,k) = let
    -- Posibles posiciones a las que moverse
    steps = [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]
    -- Checkear si es posible moverse a una posición
    isValid (xf,yf) =
       pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY
    -- Generar los posibles movimientos
    steps2 = filter isValid steps
    -- Se agregaron las condiciones para que el movimiento de una pieza solo pueda provenir de fuera del tablero, esto permite que una pieza solo pueda ser puesta 1 sola vez en el tablero como si se tratara de una marca.
    in [(moveName (x,y) (xf,yf), TicTacToe (1-c) (putMark (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2, (x,y) /= (0,0),(x,y) /= (0,1),(x,y) /= (0,2),(x,y) /= (1,0),(x,y) /= (1,1),(x,y) /= (1,2),(x,y) /= (2,0),(x,y) /= (2,1),(x,y) /= (2,2)]

{- Prototipo de funciones que no fueron utilizadas

cpuBestMove :: TicTacToe -> Piece -> [(String,TicTacToe)]
cpuBestMove (TicTacToe c pcs) (p,x,y,k)
    |(cpuFirstMove (TicTacToe c pcs) (p,x,y,k) == Just 0) = [(moveName (x,y) (1,1), TicTacToe (1-c) (putMark (4,0) (1,1) pcs))]
    |(cpuFirstMove (TicTacToe c pcs) (p,x,y,k) == Just 1) = [(moveName (x,y) (0,0), TicTacToe (1-c) (putMark (4,0) (0,0) pcs))]

cpuFirstMove :: TicTacToe -> Piece -> Maybe Int
cpuFirstMove (TicTacToe c pcs) (p,x,y,k)
    -- Si el jugador toma una esquina, la CPU marca el centro del tablero.
    |((pieceAt (0,0) pcs /= Nothing) || (pieceAt (2,0) pcs /= Nothing) || (pieceAt (0,2) pcs /= Nothing) || (pieceAt (2,2) pcs /= Nothing)) = Just 0
    --[(moveName (x,y) (1,1), TicTacToe (1-c) (putMark (4,0) (1,1) pcs))]
    -- Si el jugador toma el centro del tablero, la CPU toma la esquina (0,0)
    |((pieceAt (0,0) pcs == Nothing) && (pieceAt (2,0) pcs == Nothing) && (pieceAt (0,2) pcs == Nothing) || (pieceAt (2,2) pcs == Nothing)) = Just 1
    -- [(moveName (x,y) (0,0), TicTacToe (1-c) (putMark (4,0) (0,0) pcs))]
    |otherwise = Nothing 

cpuWinMove :: TicTacToe -> Piece -> [(String,TicTacToe)]
cpuWinMove (TicTacToe c pcs) (p,x,y,k) = 
    [(moveName (x,y) (i,j), TicTacToe (1-c) (putMark (4,y) (i,j) pcs)) | i <- [0..3], j <- [0..3], pieceAt (i,j) pcs == Nothing]

cpuDontLoseMove :: TicTacToe -> Piece -> [(String,TicTacToe)]
cpuDontLoseMove (TicTacToe c pcs) (p,x,y,k) = 
    [(moveName (x,y) (i,j), TicTacToe (c) (putMark (4,y) (i,j) pcs)) | i <- [0..3], j <- [0..3], pieceAt (i,j) pcs == Nothing]

isEnemyFirstMove :: TicTacToe -> (Int, Int) -> Bool
isEnemyFirstMove (TicTacToe c pc) (xf,yf)
        --If enemy takes edge CPU takes middle
        |(any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 0) pc && pieceAt (1,0) pc == Nothing && pieceAt (2,0) pc == Nothing && pieceAt (0,1) pc == Nothing && pieceAt (1,1) pc == Nothing && pieceAt (2,1) pc == Nothing && pieceAt (0,2) pc == Nothing && pieceAt (1,2) pc == Nothing && pieceAt (2,2) pc == Nothing) || 
         (any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 0) pc && pieceAt (1,0) pc == Nothing && pieceAt (0,0) pc == Nothing && pieceAt (0,1) pc == Nothing && pieceAt (1,1) pc == Nothing && pieceAt (2,1) pc == Nothing && pieceAt (0,2) pc == Nothing && pieceAt (1,2) pc == Nothing && pieceAt (2,2) pc == Nothing) || 
         (any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 2) pc && pieceAt (1,0) pc == Nothing && pieceAt (2,0) pc == Nothing && pieceAt (0,1) pc == Nothing && pieceAt (1,1) pc == Nothing && pieceAt (2,1) pc == Nothing && pieceAt (0,2) pc == Nothing && pieceAt (1,2) pc == Nothing && pieceAt (0,0) pc == Nothing) || 
         (any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 2) pc && pieceAt (1,0) pc == Nothing && pieceAt (2,0) pc == Nothing && pieceAt (0,1) pc == Nothing && pieceAt (1,1) pc == Nothing && pieceAt (2,1) pc == Nothing && pieceAt (0,0) pc == Nothing && pieceAt (1,2) pc == Nothing && pieceAt (2,2) pc == Nothing) = True
        --Otherwise CPU takes an edge
        |otherwise = False
-}

isEnemyPlayerWin :: TicTacToe -> (Int, Int) -> Bool
isEnemyPlayerWin (TicTacToe c pc) (xf,yf)
        --Diagonal 1
        |(any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 1) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 2) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 2) pc) = True
        --Diagonal 2
        |(any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 0) pc && any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 1) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 0) pc && any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 2) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 2) pc) = True
        --Fila 0
        |(any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 0) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 0) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 0) pc && any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 0) pc) = True
        --Fila 1
        |(any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 1) pc && any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 1) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 1) pc && any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 1) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 1) pc) = True
        --Fila 2
        |(any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 2) pc && any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 2) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 2) pc && any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 2) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 2) pc && any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 2) pc) = True
        --Columna 0
        |(any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 1) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 2) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 1) pc && any (\(p,x,y,k) -> p == 1-c && x == 0 && y== 2) pc) = True
        --Columna 1
        |(any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 0) pc && any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 1) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 0) pc && any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 2) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == 1-c && x == 1 && y== 2) pc) = True
        --Columna 2
        |(any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 0) pc && any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 1) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 0) pc && any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 2) pc)||
         (any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 1) pc && any (\(p,x,y,k) -> p == 1-c && x == 2 && y== 2) pc) = True
        |otherwise = False

isCurrentPlayerWin :: TicTacToe -> (Int, Int) -> Bool
isCurrentPlayerWin (TicTacToe c pc) (xf,yf)
        --Diagonal 1
        |(any (\(p,x,y,k) -> p == c && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == c && x == 1 && y== 1) pc)||
         (any (\(p,x,y,k) -> p == c && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == c && x == 2 && y== 2) pc)||
         (any (\(p,x,y,k) -> p == c && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == c && x == 2 && y== 2) pc) = True
        --Diagonal 2
        |(any (\(p,x,y,k) -> p == c && x == 2 && y== 0) pc && any (\(p,x,y,k) -> p == c && x == 1 && y== 1) pc)||
         (any (\(p,x,y,k) -> p == c && x == 2 && y== 0) pc && any (\(p,x,y,k) -> p == c && x == 0 && y== 2) pc)||
         (any (\(p,x,y,k) -> p == c && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == c && x == 0 && y== 2) pc) = True
        --Fila 0
        |(any (\(p,x,y,k) -> p == c && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == c && x == 1 && y== 0) pc)||
         (any (\(p,x,y,k) -> p == c && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == c && x == 2 && y== 0) pc)||
         (any (\(p,x,y,k) -> p == c && x == 1 && y== 0) pc && any (\(p,x,y,k) -> p == c && x == 2 && y== 0) pc) = True
        --Fila 1
        |(any (\(p,x,y,k) -> p == c && x == 0 && y== 1) pc && any (\(p,x,y,k) -> p == c && x == 1 && y== 1) pc)||
         (any (\(p,x,y,k) -> p == c && x == 0 && y== 1) pc && any (\(p,x,y,k) -> p == c && x == 2 && y== 1) pc)||
         (any (\(p,x,y,k) -> p == c && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == c && x == 2 && y== 1) pc) = True
        --Fila 2
        |(any (\(p,x,y,k) -> p == c && x == 0 && y== 2) pc && any (\(p,x,y,k) -> p == c && x == 1 && y== 2) pc)||
         (any (\(p,x,y,k) -> p == c && x == 0 && y== 2) pc && any (\(p,x,y,k) -> p == c && x == 2 && y== 2) pc)||
         (any (\(p,x,y,k) -> p == c && x == 1 && y== 2) pc && any (\(p,x,y,k) -> p == c && x == 2 && y== 2) pc) = True
        --Columna 0
        |(any (\(p,x,y,k) -> p == c && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == c && x == 0 && y== 1) pc)||
         (any (\(p,x,y,k) -> p == c && x == 0 && y== 0) pc && any (\(p,x,y,k) -> p == c && x == 0 && y== 2) pc)||
         (any (\(p,x,y,k) -> p == c && x == 0 && y== 1) pc && any (\(p,x,y,k) -> p == c && x == 0 && y== 2) pc) = True
        --Columna 1
        |(any (\(p,x,y,k) -> p == c && x == 1 && y== 0) pc && any (\(p,x,y,k) -> p == c && x == 1 && y== 1) pc)||
         (any (\(p,x,y,k) -> p == c && x == 1 && y== 0) pc && any (\(p,x,y,k) -> p == c && x == 1 && y== 2) pc)||
         (any (\(p,x,y,k) -> p == c && x == 1 && y== 1) pc && any (\(p,x,y,k) -> p == c && x == 1 && y== 2) pc) = True
        --Columna 2
        |(any (\(p,x,y,k) -> p == c && x == 2 && y== 0) pc && any (\(p,x,y,k) -> p == c && x == 2 && y== 1) pc)||
         (any (\(p,x,y,k) -> p == c && x == 2 && y== 0) pc && any (\(p,x,y,k) -> p == c && x == 2 && y== 2) pc)||
         (any (\(p,x,y,k) -> p == c && x == 2 && y== 1) pc && any (\(p,x,y,k) -> p == c && x == 2 && y== 2) pc) = True
        |otherwise = False

ticTacToeEval :: TicTacToe -> Float
ticTacToeEval (TicTacToe c pcs) = let
    o@(_,ox,oy,_) = head (filter (\(p,_,_,_) -> p == 0) pcs)
    value = sum [if isCurrentPlayerWin (TicTacToe c pcs) (xf,yf) then 0.5
            else if isEnemyPlayerWin (TicTacToe c pcs) (xf,yf) then 0.5
            else 0.05 |  (p,xf,yf,k) <- pcs, p==1]
    in value * (if c == 1 then 1.0 else -1.0)

-- Inicialización del juego.

ticTacToeIni :: TicTacToe
ticTacToeIni = TicTacToe 0 [(0,3,0,'X'),(0,3,1,'X'),(0,3,2,'X'),(0,3,3,'X'),(0,3,4,'X'),(1,4,0,'O'),(1,4,1,'O'),(1,4,2,'O'),(1,4,3,'O'),(1,4,4,'O')]

-- Main.

main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    -- Crear jugadores
    let player0 = human "Crosses"
    let player1 = cpuEval "Noughts" ticTacToeEval
    -- Jugar
    execute ticTacToeIni [player0,player1] seed