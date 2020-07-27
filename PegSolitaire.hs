-- PegSolitaire.hs contiene la lógica específica del PegSolitaire

import Game
import Piece

import System.Random (getStdGen,randoms)

-- Se crea el tipo PegSolitaire.

data PegSolitaire = PegSolitaire Int [Piece] -- jugador actual y lista de piezas

-- Se define el tamaño del tablero

boardX :: Int
boardX = 7
boardY :: Int
boardY = 7

{- Se define inBoard que valida una pieza acorde a su posición.
   Es decir, debe estar en el tablero para tomar el valor de True, si no, entonces retorna False
-}
inBoard :: (Int,Int) -> Bool
inBoard (x,y)
    |x>=2 && x<=4 && y>=0 && y<=1 = True
    |x>=0 && x<=6 && y>=2 && y<=4 = True
    |x>=2 && x<=4 && y>=5 && y<=6 = True
    |otherwise                    = False


-- Se hace que PegSolitaire sea instancia de Game y se definen las funciones necesarias.

instance Game PegSolitaire where
    current (PegSolitaire c _) = c

    winner (PegSolitaire c pc) mvs
        -- Si queda solo una pieza, entonces el jugador gana
        | (length pc) == 1         = Just 0
        -- Si el jugador no tiene movimientos, entonces pierde
        | null mvs                 = Just 1
        -- En todo otro caso se sigue jugando
        | otherwise                           = Nothing

    movements st@(PegSolitaire c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

{-
    pieceMoves genera los movimientos posibles para los dos tipos de piezas del juego.
    Recibe un PegSolitaire y una pieza.
    Retorna el un movimiento, consistente de su nombre y el estado que genera.
-}

pieceMoves :: PegSolitaire -> Piece -> [(String,PegSolitaire)]
pieceMoves (PegSolitaire c pcs) (p,x,y,k) = let
    -- Posiciones posibles dependiendo del tipo de pieza
    steps
        | k=='P'  = [(x+2,y),(x-2,y),(x,y+2),(x,y-2)]
    {- Checkear si es posible moverse a una posición
       Un movimiento es válido si la posición inicial y final de la pieza pertenecen al tablero, si la posición final del movimiento
       no contiene una pieza y si entre medio de ambas posiciones exista una pieza. -}
    isValid (xf,yf) =
        pieceAt (xf,yf) pcs == Nothing  && inBoard(x,y) && inBoard(xf,yf) && pieceAt ((x+xf) `div` 2,(y+yf) `div` 2) pcs /= Nothing

    -- Generar los posibles movimientos
    steps2 = filter isValid steps
    in [(moveName (x,y) (xf,yf), PegSolitaire 0 (movePiece (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2]

-- Inicialización del juego junto a las piezas (32 piezas)

pegSolitaireIni :: PegSolitaire
pegSolitaireIni = PegSolitaire 0 [(0,0,2,'P'),(0,0,3,'P'),(0,0,4,'P'),(0,1,2,'P'),(0,1,3,'P'),(0,1,4,'P'),(0,2,0,'P'),(0,2,1,'P'),(0,2,2,'P'),(0,2,3,'P'),(0,2,4,'P'),(0,2,5,'P'),(0,2,6,'P'),(0,3,0,'P'),(0,3,1,'P'),(0,3,2,'P'),(0,3,4,'P'),(0,3,5,'P'),(0,3,6,'P'),(0,4,0,'P'),(0,4,1,'P'),(0,4,2,'P'),(0,4,3,'P'),(0,4,4,'P'),(0,4,5,'P'),(0,4,6,'P'),(0,5,2,'P'),(0,5,3,'P'),(0,5,4,'P'),(0,6,2,'P'),(0,6,3,'P'),(0,6,4,'P')]


-- Se define como se transforma un PegSolitaire a String.

instance Show PegSolitaire where
    show (PegSolitaire _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'P') -> "■ " --Representación ASCII de la pieza
            Nothing          -> if x > 1 && x < 5 then "□ " else 
                                                                if y>1 && y<5 then "□ " else "  " --Representación de lo que es o no tablero
        in drawBoard (boardX,boardY) draw

{-
    PegSolitaireEval corresponde a la función de evaluación que usa cpuEval.
    Recibe un estado del jeugo.
    Retorna la evaluación del estado.
-}


-- Main.

main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    -- Crear jugadores, ambos del tipo terminal, pero en realidad solo juega una persona
    let player0 = human "1"
    let player1 = human "2"
    -- Jugar
    execute pegSolitaireIni [player0,player1] seed


