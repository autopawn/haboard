import Game
import Piece

import System.Random (getStdGen,randoms)

data PegSolitaire = PegSolitaire Int [Piece]

boardX :: Int
boardX = 7
boardY :: Int
boardY = 7

instance Game PegSolitaire where
    current (PegSolitaire c _) = c

    winner (PegSolitaire c pc) mvs
        -- Si el jugador se queda sin movimientos, el jugador pierde
        | (length pc) == 1  = Just 0
        -- Si queda solo una pieza en el tablero, el jugador gana
        | null mvs          = Just 1
        -- En otro caso se sigue jugando
        | otherwise      = Nothing

    movements st@(PegSolitaire c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

{-
    validPosition sirve para verificar si las coordenadas de una pieza están dentro del tablero (puesto que no es cuadrado)
    Recibe una posición (x,y)
    Retorna True si está dentro del tablero, False si no lo está
-}
validPosition :: (Int,Int) -> Bool
validPosition (x,y) = x >= 0 && y >= 0 && x <= 6 && y <= 6 &&
                ((x >= 2 && x <= 4) || (y >= 2 && y <= 4))
    
{-  
    movePeg tiene una estructura similar a movePiece, pero con la distinción que la pieza que se "come" no se agrega a la lista de
    piezas actualizada. Se crea para no modificar movePiece que tiene una estructura más general
    Recibe una posición inicial, posición final y la lista de piezas
    Retorna la lista de piezas actualizada sin las piezas que se van comiendo
-}

movePeg :: (Int,Int) -> (Int,Int) -> [Piece] -> [Piece]
movePeg (xi,yi) (xf,yf) pcs =
    [if (x,y)==(xi,yi) then (p,xf,yf,k) else (p,x,y,k) | (p,x,y,k) <- pcs, (x,y) /= ((xi+xf) `div` 2,(yi+yf) `div` 2)]

pieceMoves :: PegSolitaire -> Piece -> [(String,PegSolitaire)]
pieceMoves (PegSolitaire c pcs) (p,x,y,k) = let
    steps 
    -- Existe solo una pieza en PegSolitaire, y se puede mover dos coordenadas en forma horizontal y vertical
        |k=='P' = [(x+2,y),(x-2,y),(x,y+2),(x,y-2)] 
    {-
        Las condiciones de movimiento son: que en la posición a la que se mueve la pieza este vacía,
        la posición a la que se mueve esté dentro del tablero (verificado con validPosition),
        y que la pieza que se va a "comer" exista, en la posición del medio a la que se mueve la pieza
    -}
    isValid (xf,yf) =
        pieceAt (xf,yf) pcs == Nothing && validPosition(xf,yf) && pieceAt((x+xf) `div` 2,(y+yf) `div` 2) pcs /= Nothing
    -- Se generan los movimientos
    steps2 = filter isValid steps
    in [(moveName (x,y) (xf,yf), PegSolitaire 0 (movePeg (x,y) (xf,yf) pcs))| (xf,yf) <- steps2]


-- Inicilización del juego con listas por secciones, que después son concatenadas para formar la lista de piezas inicial

pegSolitaireIni :: PegSolitaire
pegSolitaireIni = let
    p1 = [(0,x,0,'P')| x <- [2..4]]
    p2 = [(0,x,1,'P')| x <- [2..4]]
    p3 = [(0,x,2,'P')| x <- [0..6]]
    p4 = [(0,x,3,'P')| x <- [0..2]]
    p5 = [(0,x,3,'P')| x <- [4..6]]
    p6 = [(0,x,4,'P')| x <- [0..6]]
    p7 = [(0,x,5,'P')| x <- [2..4]]
    p8 = [(0,x,6,'P')| x <- [2..4]]
    initial = p1 ++ p2 ++ p3 ++ p4 ++ p5 ++ p6 ++ p7 ++ p8
    in PegSolitaire 0 initial

{-
    Transformación del juego a String. En la casilla donde haya una pieza, se representará esta como "𐤈",
    mientras que donde no hayan piezas (y esté dentro del tablero) se representará con un punto ".", y fuera del
    tablero con espacios en blanco
-} 

instance Show PegSolitaire where
    show (PegSolitaire _ pcs) = let
        draw (x,y) = case pieceAt(x,y) pcs of
            Just(_,_,_,'P') -> "P "
            Nothing         -> if validPosition(x,y) then ". " else "  "
        in drawBoard (boardX,boardY) draw

main :: IO Int
main = do
    gen <- getStdGen
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    -- Crear jugadores. Se crean dos jugadores humanos, pero el segundo nunca juega
    let player0 = human "Chupete Suazo"
    let player1 = human "a"
    -- Jugar
    execute pegSolitaireIni [player0,player1] seed