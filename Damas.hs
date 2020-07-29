-- Chess.hs contiene la lógica específica del juego Ajedrez

import Game
import Piece

import System.Random (getStdGen,randoms)

-- Se crea el tipo Chess.

data Damas = Damas Int [Piece] -- jugador actual y lista de piezas

-- Se define el tamaño del tablero

boardX :: Int
boardX = 8
boardY :: Int
boardY = 8
-- Se hace que Damas sea instancia de Game y se definen las funciones necesarias.

instance Game Damas where
    current (Damas c _) = c


    winner (Damas c pc) mvs
        -- Si el jugador actual no tiene movimientos, gana el otro
        | null mvs                                   = Just (1 - c)
        -- Si ninguna se cumple, se sigue jugando
        | otherwise                                  = Nothing
        

    movements st@(Damas c pc) = let
        playerpcs = filter (\(p,x,y,k)  -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

pieceMoves :: Damas -> Piece -> [(String,Damas)]
pieceMoves (Damas c pcs) (p,x,y,k) = let
    move
        | k=='R' =  let
            r1 = arribaIzquierda (Damas c pcs) (p,x,y,k) 1 [1..7]
            r2 = arribaDerecha (Damas c pcs) (p,x,y,k) 1 [1..7]
            r3 = abajoDerecha (Damas c pcs) (p,x,y,k) 1 [1..7]
            r4 = abajoIzquierda (Damas c pcs) (p,x,y,k) 1 [1..7]
            in r1++r2++r3++r4
        | k=='F' = let
            f = simple (Damas c pcs) (p,x,y,k) [1,-1]
            in f
    in move

--------------------------------------------------------------------------------------------------------------------------------
arribaIzquierda :: Damas -> Piece -> Int -> [Int] ->[(String,Damas)]
-- Sin posibilidad de movimiento
arribaIzquierda _ _ 0 _   = []
-- Lista de movimientos terminada
arribaIzquierda _ _ _ []  = []
-- Posibles movimientos
arribaIzquierda (Damas c pcs) (p,x,y,k) 1 (l:ls) = let
    mov
        -- Posición de llegada dentro del tablero
        | dentro (x-l,y-l) (boardX,boardY) = case pieceAt (x-l,y-l) pcs of
            -- Lugar de llegada vacio
            Nothing -> let
                lista = [(moveName (x,y) (x-l,y-l), Damas (1-c) (movePiece (x,y) (x-l,y-l) pcs))]
                in lista ++ arribaIzquierda (Damas c pcs) (p,x,y,k) 1 ls
            -- Existe una pieza en la posición de llegada, pero la condición es si es del jugador actual
            Just (z,_,_,_) -> if playerAt pcs (x-l,y-l) == c then arribaIzquierda (Damas c pcs) (p,x,y,k) 0 ls
                -- La pieza es del otro jugador
                else let
                    aux = removePiece (x-l,y-l) pcs
                    lista = [(moveName (x,y) (x-l-1,y-l-1), Damas (1-c) (movePiece (x,y) (x-l-1,y-l-1) aux))]
                    in lista ++ arribaIzquierda (Damas c pcs) (p,x,y,k) 0 ls
        -- Posición de llegada fuera del tablero
        | otherwise = arribaIzquierda (Damas c pcs) (p,x,y,k) 0 ls
    in mov

arribaDerecha :: Damas -> Piece -> Int -> [Int] ->[(String,Damas)]
arribaDerecha _ _ 0 _   = []
arribaDerecha _ _ _ []  = []
arribaDerecha (Damas c pcs) (p,x,y,k) 1 (l:ls) = let
    mov
        | dentro (x+l,y-l) (boardX,boardY) = case pieceAt (x+l,y-l) pcs of
            Nothing -> let
                lista = [(moveName (x,y) (x+l,y-l), Damas (1-c) (movePiece (x,y) (x+l,y-l) pcs))]
                in lista ++ arribaDerecha (Damas c pcs) (p,x,y,k) 1 ls
            Just (z,_,_,_) -> if playerAt pcs (x+l,y-l) == c then arribaDerecha (Damas c pcs) (p,x,y,k) 0 ls
                else let
                    aux = removePiece (x+l,y-l) pcs
                    lista = [(moveName (x,y) (x+l+1,y-l-1), Damas (1-c) (movePiece (x,y) (x+l+1,y-l-1) aux))]
                    in lista ++ arribaDerecha (Damas c pcs) (p,x,y,k) 0 ls
        | otherwise = arribaDerecha (Damas c pcs) (p,x,y,k) 0 ls
    in mov

abajoDerecha :: Damas -> Piece -> Int -> [Int] ->[(String,Damas)]
abajoDerecha _ _ 0 _   = []
abajoDerecha _ _ _ []  = []
abajoDerecha (Damas c pcs) (p,x,y,k) 1 (l:ls) = let
    mov
        | dentro (x-l,y+l) (boardX,boardY) = case pieceAt (x-l,y+l) pcs of
            Nothing -> let
                lista = [(moveName (x,y) (x-l,y+l), Damas (1-c) (movePiece (x,y) (x-l,y+l) pcs))]
                in lista ++ abajoDerecha (Damas c pcs) (p,x,y,k) 1 ls
            Just (z,_,_,_) -> if playerAt pcs (x-l,y+l) == c then abajoDerecha (Damas c pcs) (p,x,y,k) 0 ls
                else let
                    aux = removePiece (x-l,y+l) pcs
                    lista = [(moveName (x,y) (x-l-1,y+l+1), Damas (1-c) (movePiece (x,y) (x-l-1,y+l+1) aux))]
                    in lista ++ abajoDerecha (Damas c pcs) (p,x,y,k) 0 ls
        | otherwise = abajoDerecha (Damas c pcs) (p,x,y,k) 0 ls
    in mov

abajoIzquierda :: Damas -> Piece -> Int -> [Int] ->[(String,Damas)]
abajoIzquierda _ _ 0 _   = []
abajoIzquierda _ _ _ []  = []
abajoIzquierda (Damas c pcs) (p,x,y,k) 1 (l:ls) = let
    mov
        | dentro (x+l,y+l) (boardX,boardY) = case pieceAt (x+l,y+l) pcs of
            Nothing -> let
                lista = [(moveName (x,y) (x+l,y+l), Damas (1-c) (movePiece (x,y) (x+l,y+l) pcs))]
                in lista ++ abajoIzquierda (Damas c pcs) (p,x,y,k) 1 ls
            Just (z,_,_,_) -> if playerAt pcs (x+l,y+l) == c then abajoIzquierda (Damas c pcs) (p,x,y,k) 0 ls
                else let
                    aux = removePiece (x+l,y+l) pcs
                    lista = [(moveName (x,y) (x+l+1,y+l+1), Damas (1-c) (movePiece (x,y) (x+l+1,y+l+1) aux))]
                    in lista ++ abajoIzquierda (Damas c pcs) (p,x,y,k) 0 ls
        | otherwise = abajoIzquierda (Damas c pcs) (p,x,y,k) 0 ls
    in mov

simple :: Damas -> Piece -> [Int] -> [(String,Damas)]
-- Lista de movimientos terminada
simple _ _ [] = []
-- Posibles movimientos
simple (Damas c pcs) (p,x,y,k) (l:ls) = let
    mov
        -- Posicion de llegada dentro del tablero, que exista un enemigo en el movimiento y que la siguiente esté vacia
        | (dentro (x+(2*l),y+(2*(2*c-1))) (boardX,boardY) == True && pieceAt (x+(2*l),y+(2*(2*c-1))) pcs == Nothing && playerAt pcs (x+l,y+(2*c-1)) /= c && pieceAt (x+l,y+(2*c-1)) pcs /= Nothing) = let
            mova
                -- Posicion de llegada corresponde al origen del enemigo, por si puede transformarse en reina
                |(y+(2*(2*c-1))==0 || y+(2*(2*c-1))==7) = let
                    list1 = removePiece (x,y) pcs
                    list2 = removePiece (x+l,y+(2*c-1)) list1
                    list3 = addQueen list2 (c,x+(2*l),y+(2*(2*c-1)),'R')
                    in [(moveName (x,y) (x+(2*l),y+(2*(2*c-1))), Damas (1-c) list3)] ++ simple (Damas c pcs) (p,x,y,k) ls
                |otherwise = let
                    list1 = removePiece (x+l,y+(2*c-1)) pcs
                    list2 = [(moveName (x,y) (x+(2*l),y+(2*(2*c-1))), Damas (1-c) (movePiece (x,y) (x+(2*l),y+(2*(2*c-1))) list1))]
                    in list2 ++ simple (Damas c pcs) (p,x,y,k) ls
            in mova
        -- Posición de llegada dentro del tablero, pero esta solo revisa si la de llegada esta vacía para un movimiento sin captura
        | (dentro (x+l,y+(2*c-1)) (boardX,boardY) == True && pieceAt (x+l,y+(2*c-1)) pcs == Nothing) = let
            mova
                -- Posicion de llegada corresponde al origen del enemigo, por si puede transformarse en reina
                |(y+(2*c-1) == 0 || y+(2*c-1) == 7) = let
                    list1 = removePiece (x,y) pcs
                    list2 = addQueen list1 (c,x+l,7*c,'R')
                    in [(moveName (x,y) (x+l,7*c), Damas (1-c) list2)] ++ simple (Damas c pcs) (p,x,y,k) ls
                |otherwise = let
                    list1 = [(moveName (x,y) (x+l,y+(2*c-1)), Damas (1-c) (movePiece (x,y) (x+l,y+(2*c-1)) pcs))]
                    in list1 ++ simple (Damas c pcs) (p,x,y,k) ls
            in mova
        -- Fuera del tablero
        | otherwise = simple (Damas c pcs) (p,x,y,k) ls
    in mov
--------------------------------------------------------------------------------------------------------------------------------

damasIni :: Damas
damasIni = let
    -- Pawns
    p0 = [(0,0,7,'F'),(0,2,7,'F'),(0,4,7,'F'),(0,6,7,'F'),(0,1,6,'F'),(0,3,6,'F'),(0,5,6,'F'),(0,7,6,'F'),(0,0,5,'F'),(0,2,5,'F'),(0,4,5,'F'),(0,6,5,'F')]
    p1 = [(1,1,0,'F'),(1,3,0,'F'),(1,5,0,'F'),(1,7,0,'F'),(1,0,1,'F'),(1,2,1,'F'),(1,4,1,'F'),(1,6,1,'F'),(1,1,2,'F'),(1,3,2,'F'),(1,5,2,'F'),(1,7,2,'F')]
    -- p3 = [(0,4,3,'R'),(1,3,2,'F'),(1,5,2,'F'),(1,3,4,'F'),(1,5,4,'F')] -- Distrución para probar mov de la Reina
    -- p4 = [(0,3,2,'F'),(1,2,1,'F'),(1,4,1,'F')] -- Distribución para probar si las fichas al capturar un enemigo y llegar al origen de este se transforma y ademas, se elimina el esa ficha enemiga
    -- p5 =  [(0,2,1,'F'),(1,3,4,'F')] -- Distribución para probar si las fichas sin capturar se pueden transformar en reinas
    -- Puede probar esas distribuciones
    estIni = p0++p1
    in Damas 0 estIni

-- Se define como se transforma un Damas a String.

instance Show Damas where
    show (Damas _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (0,_,_,'R') -> "⛁ "
            Just (1,_,_,'R') -> "⛃ "
            Just (0,_,_,'F') -> "⛀ "
            Just (1,_,_,'F') -> "⛂ "
            Nothing          -> if (x+y) `mod` 2 == 0 then "■ " else "□ "
        in drawBoard (boardX,boardY) draw


-- MarribaIzquierdan.

main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    -- Crear jugadores
    let player0 = human "ReyArturo"
    let player1 = cpuRand "PrincipeCharles"
    -- Jugar
    execute damasIni [player0,player1] seed