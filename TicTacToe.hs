import Game
import Piece

import System.Random (getStdGen,randoms)

data TicTacToe = TicTacToe Int [Piece]

boardX :: Int
boardX = 3
boardY :: Int
boardY = 3

instance Game TicTacToe where
    current (TicTacToe c _) = c

    winner (TicTacToe c pc) mvs
        -- Chequea todos los eventos donde se pueda ganar en el tablero
        | length (filter (\(p,x,y,k) -> x == y ) (playerPieces pc 0)) == 3 = Just 0
        | length (filter (\(p,x,y,k) -> (x == 2 && y == 0) || (x == 1 && y == 1) || (x == 0 && y == 2) ) (playerPieces pc 0)) == 3 = Just 0
        | length (filter (\(p,x,y,k) -> (x == 0 && y == 0) || (x == 0 && y == 1) || (x == 0 && y == 2) ) (playerPieces pc 0)) == 3 = Just 0
        | length (filter (\(p,x,y,k) -> (x == 1 && y == 0) || (x == 1 && y == 1) || (x == 1 && y == 2) ) (playerPieces pc 0)) == 3 = Just 0
        | length (filter (\(p,x,y,k) -> (x == 2 && y == 0) || (x == 2 && y == 1) || (x == 2 && y == 2) ) (playerPieces pc 0)) == 3 = Just 0
        | length (filter (\(p,x,y,k) -> (x == 0 && y == 0) || (x == 1 && y == 0) || (x == 2 && y == 0) ) (playerPieces pc 0)) == 3 = Just 0
        | length (filter (\(p,x,y,k) -> (x == 0 && y == 1) || (x == 1 && y == 1) || (x == 2 && y == 1) ) (playerPieces pc 0)) == 3 = Just 0
        | length (filter (\(p,x,y,k) -> (x == 0 && y == 2) || (x == 1 && y == 2) || (x == 2 && y == 2) ) (playerPieces pc 0)) == 3 = Just 0
        | length (filter (\(p,x,y,k) -> x == y ) (playerPieces pc 1)) == 3 = Just 1
        | length (filter (\(p,x,y,k) -> (x == 2 && y == 0) || (x == 1 && y == 1) || (x == 0 && y == 2) ) (playerPieces pc 1)) == 3 = Just 1
        | length (filter (\(p,x,y,k) -> (x == 0 && y == 0) || (x == 0 && y == 1) || (x == 0 && y == 2) ) (playerPieces pc 1)) == 3 = Just 1
        | length (filter (\(p,x,y,k) -> (x == 1 && y == 0) || (x == 1 && y == 1) || (x == 1 && y == 2) ) (playerPieces pc 1)) == 3 = Just 1
        | length (filter (\(p,x,y,k) -> (x == 2 && y == 0) || (x == 2 && y == 1) || (x == 2 && y == 2) ) (playerPieces pc 1)) == 3 = Just 1
        | length (filter (\(p,x,y,k) -> (x == 0 && y == 0) || (x == 1 && y == 0) || (x == 2 && y == 0) ) (playerPieces pc 1)) == 3 = Just 1
        | length (filter (\(p,x,y,k) -> (x == 0 && y == 1) || (x == 1 && y == 1) || (x == 2 && y == 1) ) (playerPieces pc 1)) == 3 = Just 1
        | length (filter (\(p,x,y,k) -> (x == 0 && y == 2) || (x == 1 && y == 2) || (x == 2 && y == 2) ) (playerPieces pc 1)) == 3 = Just 1
        -- Si no hay mas movimientos y ya no se ganó, entonces se empata.
        | null mvs = Just 2
        | otherwise = Nothing

    movements st@(TicTacToe c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

pieceMoves :: TicTacToe -> Piece -> [(String,TicTacToe)]
pieceMoves (TicTacToe c pcs) (p,x,y,k) = let
    -- Posiciones posibles dependiendo del tipo de pieza
    steps
        | k=='O'  = [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2),
                    (5,5),(5,6),(5,7),(5,8),(5,9)]
        | k=='X'  = [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2),
                    (4,5),(4,6),(4,7),(4,8),(4,9)]
    -- Checkear si es posible moverse a una posición
    isValid (xf,yf) =
        pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY
    -- Generar los posibles movimientos
    steps2 = filter isValid steps
    -- Chequear si es posible mover la pieza dado que tenemos piezas fuera del tablero
    isMovable (x, y) =
        pieceAt (x, y) pcs /= Nothing && 0 <= x && x > boardX && 0 <= y && y > boardY

    steps3 = filter isMovable steps

    in remDup [(newMoveName (xf, yf), TicTacToe (1-c) (movePiece (x, y) (xf,yf) pcs)) | (xf,yf) <- steps2, (x,y) <- steps3]


ticTacToeIni :: TicTacToe
ticTacToeIni = TicTacToe 0 [(0,5,5,'O'),(0,5,6,'O'),(0,5,7,'O'),(0,5,8,'O'),(0,5,9,'O'),
                            (1,4,5,'X'),(1,4,6,'X'),(1,4,7,'X'),(1,4,8,'X'),(1,4,9,'X')]

-- Nueva función para el nombre del movimiento, con la idea de dar solo el nombre de la coordenada
newMoveName :: (Int, Int) -> String
newMoveName (xf,yf) = let
    cxf = ['a'..] !! xf
    syf = show yf
    in (cxf:syf)

-- Función que remueve duplicados de listas (no funciona en este caso dado que los estados del tablero son distintos)
remDup :: [(String,TicTacToe)] -> [(String,TicTacToe)]
remDup [] = []
remDup (x:xs) = x : (remDup (remove x xs))
    where
        remove :: (String,TicTacToe) -> [(String,TicTacToe)] -> [(String,TicTacToe)]
        remove x [] = []
        remove x (y:ys)
            | (fst x == fst y) = remove y ys
            | otherwise = y:(remove x ys) 

-- Función que muestra el tablero
instance Show TicTacToe where
    show (TicTacToe _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'O') -> "O "
            Just (_,_,_,'X') -> "X "
            Nothing          -> "  "
        in drawBoard (boardX,boardY) draw


-- Función que chequea si al oponente le falta 1 espacio para ganar y lo evita
checkWin :: TicTacToe -> (Int, Int) -> Bool
checkWin (TicTacToe c pc) (fx,fy)
        | (length (filter (\(p,x,y,k) -> x == y ) (playerPieces pc 0)) == 2) && (fx == fy) = True
        | (length (filter (\(p,x,y,k) -> (x == 2 && y == 0) || (x == 1 && y == 1) || (x == 0 && y == 2) ) (playerPieces pc 0)) == 2) && ((fx == 2 && fy == 0) || (fx == 1 && fy == 1) || (fx == 0 && fy == 2) )  = True
        | (length (filter (\(p,x,y,k) -> (x == 0 && y == 0) || (x == 0 && y == 1) || (x == 0 && y == 2) ) (playerPieces pc 0)) == 2) && fx == 0 = True
        | (length (filter (\(p,x,y,k) -> (x == 1 && y == 0) || (x == 1 && y == 1) || (x == 1 && y == 2) ) (playerPieces pc 0)) == 2) && fx == 1 = True
        | (length (filter (\(p,x,y,k) -> (x == 2 && y == 0) || (x == 2 && y == 1) || (x == 2 && y == 2) ) (playerPieces pc 0)) == 2) && fx == 2 = True
        | (length (filter (\(p,x,y,k) -> (x == 0 && y == 0) || (x == 1 && y == 0) || (x == 2 && y == 0) ) (playerPieces pc 0)) == 2) && fy == 0 = True
        | (length (filter (\(p,x,y,k) -> (x == 0 && y == 1) || (x == 1 && y == 1) || (x == 2 && y == 1) ) (playerPieces pc 0)) == 2) && fy == 1 = True
        | (length (filter (\(p,x,y,k) -> (x == 0 && y == 2) || (x == 1 && y == 2) || (x == 2 && y == 2) ) (playerPieces pc 0)) == 2)&& fy == 2 = True
        | otherwise = False

-- AI, igual que la de game, se pensó en cambiar algo pero finalmente se matuvo igual

cpuTicTacToe :: String -> (TicTacToe -> Float) -> Player TicTacToe
cpuTicTacToe name eval = let
    pickCommand st mvs r = do
        let me = current st
        -- Función de evaluación para cualquier estado s, negada si cambia el jugador
        let eval2 s = if current s == me then eval s else (-1)*eval s
        -- Elegir el movimiento mejor evaluado
        let bestis = argmaxs (map (\(cmd,s) -> eval2 s) mvs)
        -- Elegir aleatoriamente uno de los mejores movimientos
        let pick = bestis !! (r `mod` length bestis)
        return (mvs !! pick)
    in Player name pickCommand

{-
    Función de evaluación que usa la cpu del TicTacToe
-}
ticTacToeEval :: TicTacToe -> Float
ticTacToeEval (TicTacToe c pcs) = let
    -- Tomamos la posición de una de las fichas del oponente
    o@(_,ox,oy,_) = head (filter (\(p,_,_,_) -> p == 0) pcs)
    -- Analizamos casos, tratando de asegurar esquinas y evitar que el otro jugador gane
    value = sum [if (ox == 0 && oy == 0) && (cx == 0 && cy == 2) then 0.5
            else if (ox == 2 && oy == 0) && (cx == 2 && cy == 2) then 0.5
            else if (ox == 1 && oy == 1) && (cx == 0 && cy == 0) then 0.5
            else if checkWin (TicTacToe c pcs) (cx,cy) then 0.5
            else 0.05 |  (p,cx,cy,k) <- pcs, p==1]
    in value * (if c == 1 then 1.0 else -1.0)

main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    -- Crear jugadores
    let player0 = human "Un Pibe"
    let player1 = cpuTicTacToe "Otro Pibe" ticTacToeEval 
    --Jugar
    execute ticTacToeIni [player0,player1] seed