import Game
import Piece

import System.Random (getStdGen,randoms)

--
data ConnectFour = ConnectFour Int [Piece]

-- Sets the size of the board
boardX::Int
boardX = 7
boardY::Int
boardY = 6

-- Chequeamos si el jugador ha ganado, similar a la implementacion de otros compañeros, pero aprovechamos el filtro
-- y la recursividad para hacerlo mas eficiente
checkCols :: (Int,Int) -> Int  -> [Piece] -> Bool
checkCols (x,y) c pcs 
        -- Si termina la ultima columna y no hay ganador, retorna falso
        |x == 8 = False
        -- Si al terminar la columna hay ganador retorna verdadero
        |any (\t -> t == True) consecPcs = True

        -- si aun no termina de revisar todas las columnas llamamos recursivamenta a la funcion
        |otherwise = checkCols (x+1,0) c pcs
        where
            filteredPcs = filter (\(j,px,py,char)-> j==c && px == x) pcs
            consecPcs = map (\(j,px,py,char) -> elem (j,px,py+1,char) pcs && elem (j,px,py+2,char) pcs && elem (j,px,py+3,char) pcs) pcs

checkRows :: (Int,Int) -> Int  -> [Piece] -> Bool
checkRows (x,y) c pcs 
        -- Si termina la ultima fila y no hay ganador retorna falso, termina la recursion
        |y == 7 = False
        -- Si al terminar la fila hay ganador retorna verdadero, termina la recursion
        |any (\t -> t == True) consecPcs = True

        -- si aun no termina de revisar todas las filas llamamos recursivamenta a la funcion
        |otherwise = checkCols (0,y+1) c pcs
        where
            filteredPcs = filter (\(j,px,py,char)-> j==c && px == x) pcs
            consecPcs = map (\(j,px,py,char) -> elem (j,px+1,py,char) pcs && elem (j,px+2,py,char) pcs && elem (j,px+3,py,char) pcs) pcs

-- para las diagonales abandonamos la recursion pues no se me ocurre como recorrer recursivamente las diagonales en estos momentos
checkLDiag :: (Int,Int) -> Int  -> [Piece] -> Bool
checkLDiag (x,y) c pcs 
        |any (\t -> t == True) consecPcs = True
        |otherwise = False
        where
            filteredPcs = filter (\(j,px,py,char)-> j==c) pcs
            consecPcs = map (\(j,px,py,char) -> elem (j,px+1,py+1,char) pcs && elem (j,px+2,py+2,char) pcs && elem (j,px+3,py+3,char) pcs) pcs
checkRDiag :: (Int,Int) -> Int  -> [Piece] -> Bool
checkRDiag (x,y) c pcs 
        |any (\t -> t == True) consecPcs = True
        |otherwise = False
        where
            filteredPcs = filter (\(j,px,py,char)-> j==c) pcs
            consecPcs = map (\(j,px,py,char) -> elem (j,px-1,py+1,char) pcs && elem (j,px-2,py+2,char) pcs && elem (j,px-3,py+3,char) pcs) pcs

-- checkForPlayer evalua todas las funciones de checkeo para un jugador 
checkForPlayer :: Int -> [Piece] -> Bool -- Jugador-> piezas -> resultado
checkForPlayer j pcs = checkCols (0,0) j pcs || checkRows (0,0) j pcs || checkLDiag (0,0) j pcs || checkRDiag (0,0) j pcs   


instance Game ConnectFour where 
    current (ConnectFour c _) = c

    winner (ConnectFour c pc) mvs 
        |null mvs = Just (3) --Si se agotan los movimientos nadie gana
        |checkForPlayer c pc = Just (c)
        |checkForPlayer (1-c) pc = Just (1-c)
        |otherwise                = Nothing


    movements st@(ConnectFour c pc) = let
        playerpcs = filter(\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs




--Añade una pieza al tablero
addPiece :: (Int,Int,Int) -> [Piece] -> [Piece]
addPiece (x,y,j) pcs
    |j == 0 = pcs ++ [(0,x,y,'A')]
    |j == 1 = pcs ++ [(1,x,y,'B')]


{-
     pieceMoves genera los movimientos posibles para los dos tipos de piezas del juego.
     Recibe un ConnectFour y una pieza.
     Retorna el un movimiento, consistente de su nombre y el estado que genera.
 -}




pieceMoves :: ConnectFour -> Piece -> [(String,ConnectFour)]
pieceMoves (ConnectFour c pcs) (p,x,y,k) = let
    -- Le pasamos todos las coordenadas, este es el producto cartesiano de las dimensiones
    steps = [(x,y) | x <- [0,1,2,3,4,5,6], y <- [0,1,2,3,4,5]]
     -- Verificamos que haya una pieza abajo de la posicion o sea la primera pieza
    isTop (xf,yf) = (not (pieceAt (xf,yf+1) pcs == Nothing) )  || (yf == 5)
    -- Verificamos que la posicion sea valida
    isValid (xf,yf) = pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY && isTop (xf,yf)
    steps2 = filter isValid steps
    in [(moveName (x,y) (xf,yf), ConnectFour (1-c) (addPiece (xf,yf,c) pcs)) | (xf,yf) <- steps2]


-- se inicializa el tablero vacio
connectFourIni :: ConnectFour
connectFourIni = ConnectFour 0 [(0,0,5,'A'),(1,6,5,'B')]

-- se define como se transforma un ConnectFour a String

instance Show ConnectFour where
    show (ConnectFour _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'A') -> "◯ "
            Just (_,_,_,'B') -> "● "
            Nothing          -> "⧇ "
        in drawBoard (boardX,boardY) draw


-- ConnectFour no necesita una funcion de evaluacion

main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    -- Crear jugadores
    let player0 = cpuRand "A"
    let player1 = cpuRand "B" 
    -- Jugar
    execute connectFourIni [player0,player1] seed




