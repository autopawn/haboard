-- HareAndHounds.hs contiene la lógica de Hare and Hounds

import Game
import Piece

import System.Random (getStdGen,randoms)

-- Se crea el tipo HareAndHounds.

data HareAndHounds = HareAndHounds Int [Piece] -- jugador actual y lista de piezas

-- Se define el tamaño del tablero

boardX :: Int
boardX = 9
boardY :: Int
boardY = 5

-- Se hace que HareAndHounds sea instancia de Game y se definen las funciones necesarias.

instance Game HareAndHounds where

    -- Jugador actual

    current (HareAndHounds c _) = c

    winner (HareAndHounds c pc) mvs
        -- Si todos los Dog pasaron al Rabbit, gana el Rabbit (jugador 0 siempre es Rabbit y jugador 1 siempre es Dog)
        | filter (\(p,x,_,_) -> p == 1 && (fst (getPos ( pc !! 0 )) - x) >= 0) pc == [] = Just 0

        -- Si un jugador no tiene movimientos, gana el otro, esto pasa cuando los perros llegan a la posición más
        -- a la izquierda posible, o cuando el conejo es atrapado.
        | null mvs                            = Just (1 - c)

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
    -- Posiciones posibles dependiendo del tipo de pieza (Dog, Rabbit)
    steps
    ---- Movimientos del Rabbit dependiendo de en que posición se encuentre.
        | k=='R' && x `elem` [2,6] && y == 0 = [(x-2,y),(x-2,y+2),(x,y+2),(x+2,y),(x+2,y+2)]                             -- Posicion 1
        | k=='R' && x == 4         && y == 0 = [(x-2,y),(x,y+2),(x+2,y)]                                                 -- Posicion 6
        | k=='R' && x `elem` [2,6] && y == 2 = [(x-2,y),(x,y+2),(x,y-2),(x+2,y)]                                         -- Posicion 3
        | k=='R' && x `elem` [4,8] && y == 2 = [(x-2,y),(x-2,y+2),(x-2,y-2),(x,y+2),(x,y-2),(x+2,y),(x+2,y-2),(x+2,y+2)] -- Posicion 4
        | k=='R' && x `elem` [2,6] && y == 4 = [(x-2,y),(x-2,y-2),(x,y-2),(x+2,y-2),(x+2,y)]                             -- Posicion 2
        | k=='R' && x == 4         && y == 4 = [(x-2,y),(x,y-2),(x+2,y)]                                                 -- Posicion 5
    ---- Movimientos del Dog dependiendo de en que posición se encuentre.
        | k=='D' && x `elem` [2,6] && y == 0 = [(x+2,y),(x+2,y+2),(x,y+2)]                   -- Posicion 1
        | k=='D' && x == 4         && y == 0 = [(x+2,y),(x,y+2)]                             -- Posicion 6
        | k=='D' && x `elem` [2,6] && y == 2 = [(x+2,y),(x,y+2),(x,y-2)]                     -- Posicion 3
        | k=='D' && x `elem` [0,4] && y == 2 = [(x+2,y),(x+2,y+2),(x+2,y-2),(x,y+2),(x,y-2)] -- Posicion 4
        | k=='D' && x `elem` [2,6] && y == 4 = [(x+2,y),(x+2,y-2),(x,y-2)]                   -- Posicion 2
        | k=='D' && x == 4         && y == 4 = [(x+2,y),(x,y-2)]                             -- Posicion 5
    ---- Cualquier caso en que no hayan movimientos válidos
        | otherwise = []
    -- Checkear si es posible moverse a una posición de steps
    isValid (xf,yf)
        | xf `elem` [0,8]   && yf == 2           && pieceAt (xf,yf) pcs == Nothing = True
        | xf `elem` [2,4,6] && yf `elem` [0,2,4] && pieceAt (xf,yf) pcs == Nothing = True
        | otherwise                                                                = False
    -- Generar los posibles movimientos
    steps2 = filter isValid steps

    in [(moveName (x,y) (xf,yf), HareAndHounds (1-c) (movePiece (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2]

-- Estado inicial del juego

hareAndHoundsIni :: HareAndHounds
hareAndHoundsIni = HareAndHounds 1 [(0,8,2,'R'),(1,0,2,'D'),(1,2,0,'D'),(1,2,4,'D')]

-- Se define como se transforma un HareAndHounds a String.

instance Show HareAndHounds where
    show (HareAndHounds _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            -- Si en la posición hay una pieza, se imprime el caracter representativo de la pieza,
            Just (_,_,_,'D') -> "🄳 "
            Just (_,_,_,'R') -> "🅁 "
            Nothing
                -- Posiciones donde se puede mover
                |x `elem` [0,8] && y == 2                                             -> "O "
                |x `elem` [2,4,6] && y `elem` [0,2,4]                                 -> "O "
                -- Otras posiciones ("caminos")
                |x `elem` [2,4,6] && y `elem` [1,3]                                   -> "| "
                |(x `elem` [1,5] && y == 1) || (x `elem` [3,7] && y == 3)             -> "/ "
                |(x `elem` [3,7] && y == 1) || (x `elem` [1,5] && y == 3)             -> "\\ "
                |(x `elem` [1,3,5,7] && y == 2) || (x `elem` [3,5] && y `elem` [0,4]) -> "- "
                |otherwise                                                            -> "  "
        in drawBoard (boardX,boardY) draw

{-
    hareAndHoundsEval corresponde a la función de evaluación que usa cpuEval.
    Recibe un estado del juego.
    Retorna la evaluación del estado.
-}

hareAndHoundsEval :: HareAndHounds -> Float
hareAndHoundsEval (HareAndHounds c pcs) = let
    fI = fromIntegral
    fox@(_,fx,fy,_) = head (filter (\(p,_,_,_) -> p == 0) pcs)
    houndsum = sum [if y>=fy then 0.5 else 0.05 * abs (fI x - fI fx) | (p,x,y,k) <- pcs, p==1]
    in (houndsum + 7 - fI fy) * (if c==0 then 1.0 else -1.0)

-- Main.

main :: IO Int
main = do

    -- Se imprime en pantalla una breve introducción al juego y a sus reglas.

    putStrLn "¡Bienvenido!, ingrese su nombre:\n"

    name <- getLine

    putStrLn ("\n" ++ name ++ ", ahora elija su personaje, hay dos tipos de personaje:")
    putStrLn ("\n---- Dog: Jugará con tres piezas de tipo Dog (o Hound), solo podrá moverse hacia la derecha, y deberá arrinconar al otro jugador que será de tipo Rabbit (o Hare).")
    putStrLn ("\n---- Rabbit: Jugará con una sola pieza de tipo Rabbit (o Hare) que puede moverse en cualquier dirección siempre y cuando un Dog no esté tapando la pasada, su misión será pasar las líneas enemigas para escaparse.")
    putStrLn ("\nEscriba D o R para jugar como Dog o Rabbit respectivamente, si escribe cualquier otra cosa, jugará contra si mismo:\n")

    personaje <- getLine

    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed

    -- Crear jugadores
    let player0 |personaje == "D" || personaje == "d" = cpuEval "Maquina" hareAndHoundsEval
                |personaje == "R" || personaje == "r" = human name
                |otherwise = human (name ++ " 1")

    let player1 |personaje == "D" || personaje == "d"  = human name
                |personaje == "R" || personaje == "r" = cpuEval "Maquina" hareAndHoundsEval
                |otherwise = human (name ++ " 2")

    -- Jugar
    execute hareAndHoundsIni [player0,player1] seed
