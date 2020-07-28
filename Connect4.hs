import Game
import Piece

import System.Random (getStdGen,randoms)

data Connect4 = Connect4 Int [Piece]

boardX :: Int
boardX = 7
boardY :: Int
boardY = 6

-- Antes de instanciar el juego, debemos proceder a hacer las condiciones de linea horizontal, vertical y diagonales.
checkWin :: [Piece] -> Bool
checkWin pc = let
    -- Dada una pieza, busca si en las siguientes 4 posiciones en la direccion correspondiente hay mas piezas del mismo tipo. (Es decir, del mismo jugador)
    horz = map(\(p,x,y,k) -> elem (p,x,y+1,k) pc && elem (p,x,y+2,k) pc && elem (p,x,y+3,k) pc) pc
    vert = map(\(p,x,y,k) -> elem (p,x+1,y,k) pc && elem (p,x+2,y,k) pc && elem (p,x+3,y,k) pc) pc
    diag1 = map(\(p,x,y,k) -> elem (p,x+1,y+1,k) pc && elem (p,x+2,y+2,k) pc && elem (p,x+3,y+3,k) pc) pc
    diag2 = map(\(p,x,y,k) -> elem (p,x-1,y+1,k) pc && elem (p,x-2,y+2,k) pc && elem (p,x-3,y+3,k) pc) pc
    in any(\t->t==True) (horz ++ vert ++ diag1 ++ diag2)

instance Game Connect4 where
    current (Connect4 c _) = c

    winner (Connect4 c pc) mvs
        -- Si el jugador actual no tiene movimientos, gana el otro. Pues no tener piezas => no tener movimientos.
        | null mvs     = Just (1-c)
        -- Condiciones de victoria con checkWin
        | checkWin pc1 = Just 0
        | checkWin pc2 = Just 1
        -- Si ninguna se cumple, se sigue jugando
        | otherwise    = Nothing
        where pc1 = [(p,x,y,k) | (p,x,y,k) <- pc , k == 'R']
              pc2 = [(p,x,y,k) | (p,x,y,k) <- pc , k == 'A']

    movements st@(Connect4 c pc) = moves st

-- Funcion para escalar a la pieza libre mas baja de la columna para moverse, dado a que las piezas caen con gravedad.
climb :: (Int,Int) -> [Piece] -> Int
climb (x,y) pcs = if pieceAt (x,y) pcs /= Nothing then climb (x,y-1) pcs else y

moves :: Connect4 -> [(String,Connect4)]
moves (Connect4 c pcs) = let
    -- Base del tableto
    board = [x | x <- [0..(boardX-1)]]
    -- Checkear si la columna esta libre, basta con que la fila superior tenga un espacio para estar libre.
    isValid xf = pieceAt (xf,0) pcs == Nothing
    -- Generar los posibles movimientos
    steps2 = filter isValid board
    k = if c==0 then 'R' else 'A'
    in [(newMoveName x, Connect4 (1-c) (newPiece (c,x,k) pcs)) | x <- steps2]

-- Imprime solo las columnas libres, dado a que no se requiere eje vertical, las piezas caen.
newMoveName :: Int -> String
newMoveName xi = let
    cxi = ['a'..] !! xi
    in [cxi]

-- Trepa el tablero desde abajo y añade la pieza en el primer espacio libre que encuentre.
newPiece :: (Int,Int,Char) -> [Piece] -> [Piece]
newPiece (c,x,k) pcs = let
    y = climb (x,boardY-1) pcs
    in [(c,x,y,k)]++pcs

instance Show Connect4 where
    show (Connect4 _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'R') -> "R "
            Just (_,_,_,'A') -> "A "
            Nothing          -> "□ " 
        in drawBoard (boardX,boardY) draw

connect4Ini :: Connect4
connect4Ini = Connect4 0 []

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
    execute connect4Ini [player0,player1] seed