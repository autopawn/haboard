

import Game
import Piece

import System.Random (getStdGen,randoms)

-- Se crea el tipo Ajedrez.

data Ajedrez = Ajedrez Int [Piece] -- jugador actual y lista de piezas

-- Se define el tamaño del tablero

boardX :: Int
boardX = 8
boardY :: Int
boardY = 8

-- Se hace que Ajedrez sea instancia de Game y se definen las funciones necesarias.

instance Game Ajedrez where
    current (Ajedrez c _) = c

    winner (Ajedrez c pc) mvs
        -- La condicion de victoria es si alguno de los jugadores deja sin piezas al otro, o alguno se queda sin movimientos
        | (filter (\(p,x,y,k) -> x < 8) (playerPieces pc 0)) == []   = Just 1
        | (filter (\(p,x,y,k) -> x < 8) (playerPieces pc 1)) == []   = Just 0
        | null mvs                            = Just (1 - c)
        | otherwise                           = Nothing

    movements st@(Ajedrez c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

{-
    pieceMoves considera los movimentos posibles de todas sus piezas, ademas de las que incluyen posibilidad de captura
-}

pieceMoves :: Ajedrez -> Piece -> [(String,Ajedrez)]
pieceMoves (Ajedrez c pcs) (p,x,y,k) = let
    -- Posiciones posibles dependiendo del tipo de pieza, la logica de los movimientos es la misma utilizada en el proyecto de Java
    steps
        | k=='P' && p==0  = (if pieceAt(x,y-1) pcs == Nothing then [(x,y-1)]else []) ++ ( if y==6 then [(x,y-2)] else []) ++ (filter (\(a,b) -> pieceAt (a,b) pcs /= Nothing) [(x+b,y-1) |  b <- [-1,1] ] )
        | k=='P' && p==1  = (if pieceAt(x,y+1) pcs == Nothing then [(x,y+1)]else []) ++ ( if y==1 then [(x,y+2)] else []) ++ (filter (\(a,b) -> pieceAt (a,b) pcs /= Nothing) [(x+b,y+1) |  b <- [-1,1] ] )      
        | k=='R'  = takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x,y-b) |  b <- [1..7] ] ++ 
                    takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x,y+b) |  b <- [1..7] ] ++
                    takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x-b,y) |  b <- [1..7] ] ++
                    takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x+b,y) |  b <- [1..7] ] 

        | k=='k'  = [(x+a,y+b) | a <- [-1,1], b <- [-2,2]] ++ [(x+a,y+b) | a <- [-2,2], b <- [-1,1]]
        | k=='B'  = takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x+b,y+b) |  b <- [1..7] ] ++ 
                    takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x+b,y-b) |  b <- [1..7] ] ++
                    takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x-b,y+b) |  b <- [1..7] ] ++
                    takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x-b,y-b) |  b <- [1..7] ]
        | k=='Q'  = takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x,y-b) |  b <- [1..7] ] ++ 
                    takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x,y+b) |  b <- [1..7] ] ++
                    takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x-b,y) |  b <- [1..7] ] ++
                    takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x+b,y) |  b <- [1..7] ] ++
                    takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x+b,y+b) |  b <- [1..7] ] ++ 
                    takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x+b,y-b) |  b <- [1..7] ] ++
                    takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x-b,y+b) |  b <- [1..7] ] ++
                    takeWhileOneMore (\(a,b) -> pieceAt (a,b) pcs == Nothing) [(x-b,y-b) |  b <- [1..7] ]
        | k=='K'  = [(x+a,y+b) | a <- [-1..1], b <- [-1..1]]
    
    
    -- La idea fue pasar los steps por 2 filtros distintos "isValid" y "posCapture", para despues unir las listas
    
    -- Filtro isValid, dejará en la lista los movimientos que en la posicion futura no exista ninguna pieza y quede dentro del tablero
    isValid (xf,yf) =
        pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY 
    
    steps_2 = filter isValid steps                           -- Se aplica el filtro

    -- Filtro posCapture (posible captura), dejará pasar las posiciones en que si hay algo, pero ese algo es una pieza del adversario
    posCapture (xf,yf) =
        pieceAt (xf,yf) pcs /= Nothing &&  extractFirst(pieceAt (xf,yf) pcs ) /= c
    

    steps_3 = filter posCapture steps                         -- Se aplica el filtro

    posMoves = steps_2 ++ steps_3                           -- la lista de movimientos posibles considera los movimientos normales y los con posibilidad de captura

    
    in [(moveName (x,y) (xf,yf), Ajedrez (1-c) (moveOrCapture (x,y) (xf,yf) pcs)) | (xf,yf) <- posMoves]


-- Funcion creada para retornar el valor de p, utilizada en filtro posCapture
extractFirst :: Maybe Piece -> Int
extractFirst (Just(p,_,_,_)) = p

-- Funcion simil a takewhile, pero envez de detenerse cuando la condicion es false, tambien considera esa opcion (la primera), por eso se llama takewhile"OneMore"
-- Esta funcion la saque de internet 
takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []


{- Esta funcion fue creada en reemplazo de movePiece, y efectua las siguientes tareas:
- Toma las piezas capturadas (que se encontraban en la posicion final (xf,yf)), y las mueve a una zona de captura, las piezas en estado capturadas del player 0, se mueven a (50,10)
y las del player 1 a (90,10), posiciones fueron escogidas al azar.
- Si en el movimiento un peon llega al borde contrario, se convierte en reina, cambiando el valor de k por 'Q'
- finalmente efectua los movimientos de posiciones iniciales a finales y retorna la lista
-}
moveOrCapture :: (Int,Int) -> (Int,Int) -> [Piece] -> [Piece]
moveOrCapture (xi,yi) (xf,yf) pcs = let
    piezas = [if (x,y) == (xf,yf)  && p == 1 then (p,90,10,k) else if ((x,y) == (xf,yf))  && (p == 0) then (p,50,10,k) else  (p,x,y,k)| (p,x,y,k) <- pcs] -- Transporta piezas capturadas
    piezas2 = [if k == 'P' && (y == 0 || y == 7) then (p,x,y,'Q') else (p,x,y,k) | (p,x,y,k) <- piezas]                     -- Si un peon llega al otro lado se transforma en reina
    in [if (x,y)==(xi,yi) then (p,xf,yf,k) else (p,x,y,k) | (p,x,y,k) <- piezas2]

-- Inicialización del juego, disposicion inicial del tablero.

ajedrezIni :: Ajedrez
ajedrezIni = Ajedrez 0 [(0,0,6,'P'),(0,1,6,'P'),(0,2,6,'P'),(0,3,6,'P'),(0,4,6,'P'),(0,5,6,'P'),(0,6,6,'P'),(0,7,6,'P'),
                        (1,0,1,'P'),(1,1,1,'P'),(1,2,1,'P'),(1,3,1,'P'),(1,4,1,'P'),(1,5,1,'P'),(1,6,1,'P'),(1,7,1,'P'),
                        (0,0,7,'R'),(0,7,7,'R'),(0,1,7,'k'),(0,6,7,'k'),(0,2,7,'B'),(0,5,7,'B'),(0,3,7,'Q'),(0,4,7,'K'),
                        (1,0,0,'R'),(1,7,0,'R'),(1,1,0,'k'),(1,6,0,'k'),(1,2,0,'B'),(1,5,0,'B'),(1,3,0,'Q'),(1,4,0,'K')]



-- Se define como se transforma un Ajedrez a String.

instance Show Ajedrez where
    show (Ajedrez _ pcs) = let
        pieces2 =   pcs  ++ capturedPieces pcs 50 ++ capturedPieces pcs 90
        draw (x,y) = case pieceAt (x,y) pieces2 of
            Just (0,_,_,'P') -> "♙ "
            Just (1,_,_,'P') -> "♟ "
            Just (0,_,_,'R') -> "♖ "
            Just (1,_,_,'R') -> "♜ "
            Just (0,_,_,'k') -> "♘ "
            Just (1,_,_,'k') -> "♞ "
            Just (0,_,_,'B') -> "♗ "
            Just (1,_,_,'B') -> "♝ "
            Just (0,_,_,'Q') -> "♕ "
            Just (1,_,_,'Q') -> "♛ "
            Just (0,_,_,'K') -> "♔ "
            Just (1,_,_,'K') -> "♚ "
            Just (2,_,_,'K') -> "♚ "      
            Nothing          -> if (x+y) `mod` 2 == 0 && x < boardX  then "■ " else if x >= boardX then " " else "□ "
        in drawBoard (boardX,boardY) draw 

{-
    AjedrezEval corresponde a la función de evaluación que usa cpuEval.
    Recibe un estado del jeugo.
    Retorna la evaluación del estado.
-}

{- Esta funcion, en conjunto con addZip, retornan las piezas capturadas pero efectua la siguiente logica:
Todas las piezas capturadas quedan en una posicion fija luego de pasar por moveOrCapture, por ejemplo para el player 0 en
(50,10), entonces esta funcion toma todos los que estan en x=50, y les asigna valores correlativos sumandole el numero que entrega zip +1,
por ejemplo si estaban las siguientes capturadas (0,50,10,'P') (0,50,10,'P') (0,50,10,'Q') (0,50,10,'B'), devolverá la siguiente lista:
[(0,51,10,'P'),(0,52,10,'P'),(0,53,10,'Q'),(0,54,10,'B')], lo anterior para aprovechar el uso de draw.

 -}
capturedPieces :: [Piece] -> Int -> [Piece]
capturedPieces pcs z = let
    filtered = filter (\(p,x,y,k) -> x == z) pcs  
    ziped = zip [0..] filtered
    in addZip ziped 

addZip :: [(Int,(Int,Int,Int,Char))] -> [(Int,Int,Int,Char)]
addZip new = [(p,x+m+1,y,k) | (m,(p,x,y,k)) <- new] 
    


-- Main.

main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    -- Crear jugadores
    let player0 = human "Son"
    let player1 = cpuRand "Father"
    -- Jugar
    execute ajedrezIni [player0,player1] seed


