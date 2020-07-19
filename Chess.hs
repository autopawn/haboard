-- Chess.hs contiene la lógica específica del juego Ajedrez

import Game
import Piece

import System.Random (getStdGen,randoms)

-- Se crea el tipo Chess.

data Chess = Chess Int [Piece] -- jugador actual y lista de piezas

-- Se define el tamaño del tablero

boardX :: Int
boardX = 8
boardY :: Int
boardY = 8

-- Se hace que Chess sea instancia de Game y se definen las funciones necesarias.

instance Game Chess where
    current (Chess c _) = c

    -- Debido a que no hay condiciones de victoria, siempre continuara el juego
    winner (Chess _ _) _ = Nothing

    movements st@(Chess c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

{-
    pieceMoves genera los movimientos posibles para los tipos de piezas del juego.
    Recibe un Chess y una pieza.
    Retorna una lista con sus movimientos posibles, consistiendo en un comando y el estado que genera.
-}

pieceMoves :: Chess -> Piece -> [(String,Chess)]
pieceMoves (Chess c pcs) (p,x,y,k) = let
    -- Posiciones posibles dependiendo del tipo de pieza
    movs
        -- Bishop
        | k=='B' = let
            b_1 = up_left (Chess c pcs) (p,x,y,k) 1 [1..7]
            b_2 = up_right (Chess c pcs) (p,x,y,k) 1 [1..7]
            b_3 = down_left (Chess c pcs) (p,x,y,k) 1 [1..7]
            b_4 = down_right (Chess c pcs) (p,x,y,k) 1 [1..7]
            in b_1++b_2++b_3++b_4
        -- Knight
        | k=='H' = let
            h_1 = up_1 (Chess c pcs) (p,x,y,k) [2,-2]
            h_2 = up_2 (Chess c pcs) (p,x,y,k) [1,-1]
            h_3 = down_1 (Chess c pcs) (p,x,y,k) [2,-2]
            h_4 = down_2 (Chess c pcs) (p,x,y,k) [1,-1]
            in h_1++h_2++h_3++h_4
        -- Rook
        | k=='R' = let
            r_1 = up (Chess c pcs) (p,x,y,k) 1 [1..7]
            r_2 = down (Chess c pcs) (p,x,y,k) 1 [1..7]
            r_3 = left (Chess c pcs) (p,x,y,k) 1 [1..7]
            r_4 = right (Chess c pcs) (p,x,y,k) 1 [1..7]
            in r_1++r_2++r_3++r_4
        -- Queen
        | k=='Q' =  let
            q_1 = up_left (Chess c pcs) (p,x,y,k) 1 [1..7]
            q_2 = up_right (Chess c pcs) (p,x,y,k) 1 [1..7]
            q_3 = down_left (Chess c pcs) (p,x,y,k) 1 [1..7]
            q_4 = down_right (Chess c pcs) (p,x,y,k) 1 [1..7]
            q_5 = up (Chess c pcs) (p,x,y,k) 1 [1..7]
            q_6 = down (Chess c pcs) (p,x,y,k) 1 [1..7]
            q_7 = left (Chess c pcs) (p,x,y,k) 1 [1..7]
            q_8 = right (Chess c pcs) (p,x,y,k) 1 [1..7]
            in q_1++q_2++q_3++q_4++q_5++q_6++q_7++q_8
        -- King
        | k=='K' = let
            k_1 = up_left (Chess c pcs) (p,x,y,k) 1 [1]
            k_2 = up_right (Chess c pcs) (p,x,y,k) 1 [1]
            k_3 = down_left (Chess c pcs) (p,x,y,k) 1 [1]
            k_4 = down_right (Chess c pcs) (p,x,y,k) 1 [1]
            k_5 = up (Chess c pcs) (p,x,y,k) 1 [1]
            k_6 = down (Chess c pcs) (p,x,y,k) 1 [1]
            k_7 = left (Chess c pcs) (p,x,y,k) 1 [1]
            k_8 = right (Chess c pcs) (p,x,y,k) 1 [1]
            in k_1++k_2++k_3++k_4++k_5++k_6++k_7++k_8
        -- Pawn
        | k=='P' = let
            p_1 = pbasic (Chess c pcs) (p,x,y,k) 1 [1,2]
            p_2 = pdiagonal (Chess c pcs) (p,x,y,k) [1,-1]
            in p_1++p_2
    in movs

--------------------------------------------------------------------------------------------------------------------------------
-- MOVIMIENTOS A LO LARGO DE 8 DIRECCIONES

{-
    up_left es una funcion que retorna una lista de tuplas con un comando y un estado del juego actualizado.
    Recibe un estado Chess, una pieza, un entero 1 para comenzar a iterar y una lista de enteros con la cantidad
    de movimientos maximos posibles en esta direccion.
    Retorna los movimientos posibles en la direccion diagonal hacia arriba a la izquierda.
-}
up_left :: Chess -> Piece -> Int -> [Int] ->[(String,Chess)]
-- HAY UNA PIEZA BLOQUEANDO EL CAMINO
up_left _ _ 0 _   = []
-- SE ACABARON LOS MOVIMIENTOS MAXIMOS EN ESTA DIRECCION
up_left _ _ _ []  = []
up_left (Chess c pcs) (p,x,y,k) 1 (l:ls) = let
    mov
        -- EN CASO DE QUE LA POSICION ESTE DENTRO DEL TABLERO
        | isInside (x-l,y-l) (boardX,boardY) = case pieceAt (x-l,y-l) pcs of
            -- LA CASILLA ESTA VACIA
            Nothing -> let
                lista = [(moveName (x,y) (x-l,y-l), Chess (1-c) (movePiece (x,y) (x-l,y-l) pcs))]
                in lista ++ up_left (Chess c pcs) (p,x,y,k) 1 ls
            -- EN LA CASILLA HAY UN REY
            Just (_,_,_,'K') -> up_left (Chess c pcs) (p,x,y,k) 0 ls
            -- EN LA CASILLA HAY UNA PIEZA | LA PIEZA ES DEL JUGADOR ACTUAL
            Just (z,_,_,_) -> if playerAt pcs (x-l,y-l) == c then up_left (Chess c pcs) (p,x,y,k) 0 ls
                -- LA PIEZA ES DEL JUGADOR RIVAL
                else let
                    aux = removePiece (x-l,y-l) pcs
                    lista = [(moveName (x,y) (x-l,y-l), Chess (1-c) (movePiece (x,y) (x-l,y-l) aux))]
                    in lista ++ up_left (Chess c pcs) (p,x,y,k) 0 ls
        -- EN CASO DE QUE LA POSICION ESTE FUERA DEL TABLERO
        | otherwise = up_left (Chess c pcs) (p,x,y,k) 0 ls
    in mov

{-
    up_right es una funcion que retorna una lista de tuplas con un comando y un estado del juego actualizado.
    Recibe un estado Chess, una pieza, un entero 1 para comenzar a iterar y una lista de enteros con la cantidad
    de movimientos maximos posibles en esta direccion.
    Retorna los movimientos posibles en la direccion diagonal hacia arriba a la derecha.
-}
up_right :: Chess -> Piece -> Int -> [Int] ->[(String,Chess)]
-- HAY UNA PIEZA BLOQUEANDO EL CAMINO
up_right _ _ 0 _   = []
-- SE ACABARON LOS MOVIMIENTOS MAXIMOS EN ESTA DIRECCION
up_right _ _ _ []  = []
up_right (Chess c pcs) (p,x,y,k) 1 (l:ls) = let
    mov
        -- EN CASO DE QUE LA POSICION ESTE DENTRO DEL TABLERO
        | isInside (x+l,y-l) (boardX,boardY) = case pieceAt (x+l,y-l) pcs of
            -- LA CASILLA ESTA VACIA
            Nothing -> let
                lista = [(moveName (x,y) (x+l,y-l), Chess (1-c) (movePiece (x,y) (x+l,y-l) pcs))]
                in lista ++ up_right (Chess c pcs) (p,x,y,k) 1 ls
            -- EN LA CASILLA HAY UN REY
            Just (_,_,_,'K') -> up_right (Chess c pcs) (p,x,y,k) 0 ls
            -- EN LA CASILLA HAY UNA PIEZA | LA PIEZA ES DEL JUGADOR ACTUAL
            Just (z,_,_,_) -> if playerAt pcs (x+l,y-l) == c then up_right (Chess c pcs) (p,x,y,k) 0 ls
                -- LA PIEZA ES DEL JUGADOR RIVAL
                else let
                    aux = removePiece (x+l,y-l) pcs
                    lista = [(moveName (x,y) (x+l,y-l), Chess (1-c) (movePiece (x,y) (x+l,y-l) aux))]
                    in lista ++ up_right (Chess c pcs) (p,x,y,k) 0 ls
        -- EN CASO DE QUE LA POSICION ESTE FUERA DEL TABLERO
        | otherwise = up_right (Chess c pcs) (p,x,y,k) 0 ls
    in mov

{-
    down_left es una funcion que retorna una lista de tuplas con un comando y un estado del juego actualizado.
    Recibe un estado Chess, una pieza, un entero 1 para comenzar a iterar y una lista de enteros con la cantidad
    de movimientos maximos posibles en esta direccion.
    Retorna los movimientos posibles en la direccion diagonal hacia abajo a la izquierda.
-}
down_left :: Chess -> Piece -> Int -> [Int] ->[(String,Chess)]
-- HAY UNA PIEZA BLOQUEANDO EL CAMINO
down_left _ _ 0 _   = []
-- SE ACABARON LOS MOVIMIENTOS MAXIMOS EN ESTA DIRECCION
down_left _ _ _ []  = []
down_left (Chess c pcs) (p,x,y,k) 1 (l:ls) = let
    mov
        -- EN CASO DE QUE LA POSICION ESTE DENTRO DEL TABLERO
        | isInside (x-l,y+l) (boardX,boardY) = case pieceAt (x-l,y+l) pcs of
            -- LA CASILLA ESTA VACIA
            Nothing -> let
                lista = [(moveName (x,y) (x-l,y+l), Chess (1-c) (movePiece (x,y) (x-l,y+l) pcs))]
                in lista ++ down_left (Chess c pcs) (p,x,y,k) 1 ls
            -- EN LA CASILLA HAY UN REY
            Just (_,_,_,'K') -> down_left (Chess c pcs) (p,x,y,k) 0 ls
            -- EN LA CASILLA HAY UNA PIEZA | LA PIEZA ES DEL JUGADOR ACTUAL
            Just (z,_,_,_) -> if playerAt pcs (x-l,y+l) == c then down_left (Chess c pcs) (p,x,y,k) 0 ls
                -- LA PIEZA ES DEL JUGADOR RIVAL
                else let
                    aux = removePiece (x-l,y+l) pcs
                    lista = [(moveName (x,y) (x-l,y+l), Chess (1-c) (movePiece (x,y) (x-l,y+l) aux))]
                    in lista ++ down_left (Chess c pcs) (p,x,y,k) 0 ls
        -- EN CASO DE QUE LA POSICION ESTE FUERA DEL TABLERO
        | otherwise = down_left (Chess c pcs) (p,x,y,k) 0 ls
    in mov

{-
    down_right es una funcion que retorna una lista de tuplas con un comando y un estado del juego actualizado.
    Recibe un estado Chess, una pieza, un entero 1 para comenzar a iterar y una lista de enteros con la cantidad
    de movimientos maximos posibles en esta direccion.
    Retorna los movimientos posibles en la direccion diagonal hacia abajo a la derecha.
-}
down_right :: Chess -> Piece -> Int -> [Int] ->[(String,Chess)]
-- HAY UNA PIEZA BLOQUEANDO EL CAMINO
down_right _ _ 0 _   = []
-- SE ACABARON LOS MOVIMIENTOS MAXIMOS EN ESTA DIRECCION
down_right _ _ _ []  = []
down_right (Chess c pcs) (p,x,y,k) 1 (l:ls) = let
    mov
        -- EN CASO DE QUE LA POSICION ESTE DENTRO DEL TABLERO
        | isInside (x+l,y+l) (boardX,boardY) = case pieceAt (x+l,y+l) pcs of
            -- LA CASILLA ESTA VACIA
            Nothing -> let
                lista = [(moveName (x,y) (x+l,y+l), Chess (1-c) (movePiece (x,y) (x+l,y+l) pcs))]
                in lista ++ down_right (Chess c pcs) (p,x,y,k) 1 ls
            -- EN LA CASILLA HAY UN REY
            Just (_,_,_,'K') -> down_right (Chess c pcs) (p,x,y,k) 0 ls
            -- EN LA CASILLA HAY UNA PIEZA | LA PIEZA ES DEL JUGADOR ACTUAL
            Just (z,_,_,_) -> if playerAt pcs (x+l,y+l) == c then down_right (Chess c pcs) (p,x,y,k) 0 ls
                -- LA PIEZA ES DEL JUGADOR RIVAL
                else let
                    aux = removePiece (x+l,y+l) pcs
                    lista = [(moveName (x,y) (x+l,y+l), Chess (1-c) (movePiece (x,y) (x+l,y+l) aux))]
                    in lista ++ down_right (Chess c pcs) (p,x,y,k) 0 ls
        -- EN CASO DE QUE LA POSICION ESTE FUERA DEL TABLERO
        | otherwise = down_right (Chess c pcs) (p,x,y,k) 0 ls
    in mov

{-
    up es una funcion que retorna una lista de tuplas con un comando y un estado del juego actualizado.
    Recibe un estado Chess, una pieza, un entero 1 para comenzar a iterar y una lista de enteros con la cantidad
    de movimientos maximos posibles en esta direccion.
    Retorna los movimientos posibles en la direccion hacia arriba.
-}
up :: Chess -> Piece -> Int -> [Int] ->[(String,Chess)]
-- HAY UNA PIEZA BLOQUEANDO EL CAMINO
up _ _ 0 _   = []
-- SE ACABARON LOS MOVIMIENTOS MAXIMOS EN ESTA DIRECCION
up _ _ _ []  = []
up (Chess c pcs) (p,x,y,k) 1 (l:ls) = let
    mov
        -- EN CASO DE QUE LA POSICION ESTE DENTRO DEL TABLERO
        | isInside (x,y-l) (boardX,boardY) = case pieceAt (x,y-l) pcs of
            -- LA CASILLA ESTA VACIA
            Nothing -> let
                lista = [(moveName (x,y) (x,y-l), Chess (1-c) (movePiece (x,y) (x,y-l) pcs))]
                in lista ++ up (Chess c pcs) (p,x,y,k) 1 ls
            -- EN LA CASILLA HAY UN REY
            Just (_,_,_,'K') -> up (Chess c pcs) (p,x,y,k) 0 ls
            -- EN LA CASILLA HAY UNA PIEZA | LA PIEZA ES DEL JUGADOR ACTUAL
            Just (z,_,_,_) -> if playerAt pcs (x,y-l) == c then up (Chess c pcs) (p,x,y,k) 0 ls
                -- LA PIEZA ES DEL JUGADOR RIVAL
                else let
                    aux = removePiece (x,y-l) pcs
                    lista = [(moveName (x,y) (x,y-l), Chess (1-c) (movePiece (x,y) (x,y-l) aux))]
                    in lista ++ up (Chess c pcs) (p,x,y,k) 0 ls
        -- EN CASO DE QUE LA POSICION ESTE FUERA DEL TABLERO
        | otherwise = up (Chess c pcs) (p,x,y,k) 0 ls
    in mov

{-
    down es una funcion que retorna una lista de tuplas con un comando y un estado del juego actualizado.
    Recibe un estado Chess, una pieza, un entero 1 para comenzar a iterar y una lista de enteros con la cantidad
    de movimientos maximos posibles en esta direccion.
    Retorna los movimientos posibles en la direccion hacia abajo.
-}
down :: Chess -> Piece -> Int -> [Int] ->[(String,Chess)]
-- HAY UNA PIEZA BLOQUEANDO EL CAMINO
down _ _ 0 _   = []
-- SE ACABARON LOS MOVIMIENTOS MAXIMOS EN ESTA DIRECCION
down _ _ _ []  = []
down (Chess c pcs) (p,x,y,k) 1 (l:ls) = let
    mov
        -- EN CASO DE QUE LA POSICION ESTE DENTRO DEL TABLERO
        | isInside (x,y+l) (boardX,boardY) = case pieceAt (x,y+l) pcs of
            -- LA CASILLA ESTA VACIA
            Nothing -> let
                lista = [(moveName (x,y) (x,y+l), Chess (1-c) (movePiece (x,y) (x,y+l) pcs))]
                in lista ++ down (Chess c pcs) (p,x,y,k) 1 ls
            -- EN LA CASILLA HAY UN REY
            Just (_,_,_,'K') -> down (Chess c pcs) (p,x,y,k) 0 ls
            -- EN LA CASILLA HAY UNA PIEZA | LA PIEZA ES DEL JUGADOR ACTUAL
            Just (z,_,_,_) -> if playerAt pcs (x,y+l) == c then down (Chess c pcs) (p,x,y,k) 0 ls
                -- LA PIEZA ES DEL JUGADOR RIVAL
                else let
                    aux = removePiece (x,y+l) pcs
                    lista = [(moveName (x,y) (x,y+l), Chess (1-c) (movePiece (x,y) (x,y+l) aux))]
                    in lista ++ down (Chess c pcs) (p,x,y,k) 0 ls
        -- EN CASO DE QUE LA POSICION ESTE FUERA DEL TABLERO
        | otherwise = down (Chess c pcs) (p,x,y,k) 0 ls
    in mov

{-
    left es una funcion que retorna una lista de tuplas con un comando y un estado del juego actualizado.
    Recibe un estado Chess, una pieza, un entero 1 para comenzar a iterar y una lista de enteros con la cantidad
    de movimientos maximos posibles en esta direccion.
    Retorna los movimientos posibles en la direccion izquierda.
-}
left :: Chess -> Piece -> Int -> [Int] ->[(String,Chess)]
-- HAY UNA PIEZA BLOQUEANDO EL CAMINO
left _ _ 0 _   = []
-- SE ACABARON LOS MOVIMIENTOS MAXIMOS EN ESTA DIRECCION
left _ _ _ []  = []
left (Chess c pcs) (p,x,y,k) 1 (l:ls) = let
    mov
        -- EN CASO DE QUE LA POSICION ESTE DENTRO DEL TABLERO
        | isInside (x-l,y) (boardX,boardY) = case pieceAt (x-l,y) pcs of
            -- LA CASILLA ESTA VACIA
            Nothing -> let
                lista = [(moveName (x,y) (x-l,y), Chess (1-c) (movePiece (x,y) (x-l,y) pcs))]
                in lista ++ left (Chess c pcs) (p,x,y,k) 1 ls
            -- EN LA CASILLA HAY UN REY
            Just (_,_,_,'K') -> left (Chess c pcs) (p,x,y,k) 0 ls
            -- EN LA CASILLA HAY UNA PIEZA | LA PIEZA ES DEL JUGADOR ACTUAL
            Just (z,_,_,_) -> if playerAt pcs (x-l,y) == c then left (Chess c pcs) (p,x,y,k) 0 ls
                -- LA PIEZA ES DEL JUGADOR RIVAL
                else let
                    aux = removePiece (x-l,y) pcs
                    lista = [(moveName (x,y) (x-l,y), Chess (1-c) (movePiece (x,y) (x-l,y) aux))]
                    in lista ++ left (Chess c pcs) (p,x,y,k) 0 ls
        -- EN CASO DE QUE LA POSICION ESTE FUERA DEL TABLERO
        | otherwise = left (Chess c pcs) (p,x,y,k) 0 ls
    in mov

{-
    right es una funcion que retorna una lista de tuplas con un comando y un estado del juego actualizado.
    Recibe un estado Chess, una pieza, un entero 1 para comenzar a iterar y una lista de enteros con la cantidad
    de movimientos maximos posibles en esta direccion.
    Retorna los movimientos posibles en la direccion derecha.
-}
right :: Chess -> Piece -> Int -> [Int] ->[(String,Chess)]
-- HAY UNA PIEZA BLOQUEANDO EL CAMINO
right _ _ 0 _   = []
-- SE ACABARON LOS MOVIMIENTOS MAXIMOS EN ESTA DIRECCION
right _ _ _ []  = []
right (Chess c pcs) (p,x,y,k) 1 (l:ls) = let
    mov
        -- EN CASO DE QUE LA POSICION ESTE DENTRO DEL TABLERO
        | isInside (x+l,y) (boardX,boardY) = case pieceAt (x+l,y) pcs of
            -- LA CASILLA ESTA VACIA
            Nothing -> let
                lista = [(moveName (x,y) (x+l,y), Chess (1-c) (movePiece (x,y) (x+l,y) pcs))]
                in lista ++ right (Chess c pcs) (p,x,y,k) 1 ls
            -- EN LA CASILLA HAY UN REY
            Just (_,_,_,'K') -> right (Chess c pcs) (p,x,y,k) 0 ls
            -- EN LA CASILLA HAY UNA PIEZA | LA PIEZA ES DEL JUGADOR ACTUAL
            Just (z,_,_,_) -> if playerAt pcs (x+l,y) == c then right (Chess c pcs) (p,x,y,k) 0 ls
                -- LA PIEZA ES DEL JUGADOR RIVAL
                else let
                    aux = removePiece (x+l,y) pcs
                    lista = [(moveName (x,y) (x+l,y), Chess (1-c) (movePiece (x,y) (x+l,y) aux))]
                    in lista ++ right (Chess c pcs) (p,x,y,k) 0 ls
        -- EN CASO DE QUE LA POSICION ESTE FUERA DEL TABLERO
        | otherwise = right (Chess c pcs) (p,x,y,k) 0 ls
    in mov

-- FIN DE MOVIMIENTOS A LO LARGO DE 8 DIRECCIONES
--------------------------------------------------------------------------------------------------------------------------------
-- MOVIMIENTOS DEL CABALLO

{-
    up_1 es una funcion que retorna una lista de tuplas con un comando y un estado del juego actualizado.
    Recibe un estado Chess, una pieza y una lista de enteros con los movimientos relativos en x.
    Retorna los movimientos posibles en la direccion: 1 hacia arriba y 2 hacia los lados.
-}
up_1 :: Chess -> Piece -> [Int] -> [(String,Chess)]
-- EN CASO DE QUE SE ACABEN LOS MOVIMIENTOS RELATIVOS EN X
up_1 _ _ [] = []
up_1 (Chess c pcs) (p,x,y,k) (l:ls) = let
    mov
        -- EN CASO DE QUE LA POSICION ESTE DENTRO DEL TABLERO
        | isInside (x+l,y-1) (boardX,boardY) = case pieceAt (x+l,y-1) pcs of
            -- LA CASILLA ESTA VACIA
            Nothing -> let
                lista = [(moveName (x,y) (x+l,y-1), Chess (1-c) (movePiece (x,y) (x+l,y-1) pcs))]
                in lista ++ up_1 (Chess c pcs) (p,x,y,k) ls
            -- EN LA CASILLA HAY UN REY
            Just (_,_,_,'K') -> up_1 (Chess c pcs) (p,x,y,k) ls
            -- EN LA CASILLA HAY UNA PIEZA | LA PIEZA ES DEL JUGADOR ACTUAL
            Just (z,_,_,_) -> if playerAt pcs (x+l,y-1) == c then up_1 (Chess c pcs) (p,x,y,k) ls
                -- LA PIEZA ES DEL JUGADOR RIVAL
                else let
                    aux = removePiece (x+l,y-1) pcs
                    lista = [(moveName (x,y) (x+l,y-1), Chess (1-c) (movePiece (x,y) (x+l,y-1) aux))]
                    in lista ++ up_1 (Chess c pcs) (p,x,y,k) ls
        -- EN CASO DE QUE LA POSICION ESTE FUERA DEL TABLERO
        | otherwise = up_1 (Chess c pcs) (p,x,y,k) ls
    in mov

{-
    up_2 es una funcion que retorna una lista de tuplas con un comando y un estado del juego actualizado.
    Recibe un estado Chess, una pieza y una lista de enteros con los movimientos relativos en x.
    Retorna los movimientos posibles en la direccion: 2 hacia arriba y 1 hacia los lados.
-}
up_2 :: Chess -> Piece -> [Int] -> [(String,Chess)]
-- EN CASO DE QUE SE ACABEN LOS MOVIMIENTOS RELATIVOS EN X
up_2 _ _ [] = []
up_2 (Chess c pcs) (p,x,y,k) (l:ls) = let
    mov
        -- EN CASO DE QUE LA POSICION ESTE DENTRO DEL TABLERO
        | isInside (x+l,y-2) (boardX,boardY) = case pieceAt (x+l,y-2) pcs of
            -- LA CASILLA ESTA VACIA
            Nothing -> let
                lista = [(moveName (x,y) (x+l,y-2), Chess (1-c) (movePiece (x,y) (x+l,y-2) pcs))]
                in lista ++ up_2 (Chess c pcs) (p,x,y,k) ls
            -- EN LA CASILLA HAY UN REY
            Just (_,_,_,'K') -> up_2 (Chess c pcs) (p,x,y,k) ls
            -- EN LA CASILLA HAY UNA PIEZA | LA PIEZA ES DEL JUGADOR ACTUAL
            Just (z,_,_,_) -> if playerAt pcs (x+l,y-2) == c then up_2 (Chess c pcs) (p,x,y,k) ls
                -- LA PIEZA ES DEL JUGADOR RIVAL
                else let
                    aux = removePiece (x+l,y-2) pcs
                    lista = [(moveName (x,y) (x+l,y-2), Chess (1-c) (movePiece (x,y) (x+l,y-2) aux))]
                    in lista ++ up_2 (Chess c pcs) (p,x,y,k) ls
        -- EN CASO DE QUE LA POSICION ESTE FUERA DEL TABLERO
        | otherwise = up_2 (Chess c pcs) (p,x,y,k) ls
    in mov

{-
    down_1 es una funcion que retorna una lista de tuplas con un comando y un estado del juego actualizado.
    Recibe un estado Chess, una pieza y una lista de enteros con los movimientos relativos en x.
    Retorna los movimientos posibles en la direccion: 1 hacia abajo y 2 hacia los lados.
-}
down_1 :: Chess -> Piece -> [Int] -> [(String,Chess)]
-- EN CASO DE QUE SE ACABEN LOS MOVIMIENTOS RELATIVOS EN X
down_1 _ _ [] = []
down_1 (Chess c pcs) (p,x,y,k) (l:ls) = let
    mov
        -- EN CASO DE QUE LA POSICION ESTE DENTRO DEL TABLERO
        | isInside (x+l,y+1) (boardX,boardY) = case pieceAt (x+l,y+1) pcs of
            -- LA CASILLA ESTA VACIA
            Nothing -> let
                lista = [(moveName (x,y) (x+l,y+1), Chess (1-c) (movePiece (x,y) (x+l,y+1) pcs))]
                in lista ++ down_1 (Chess c pcs) (p,x,y,k) ls
            -- EN LA CASILLA HAY UN REY
            Just (_,_,_,'K') -> down_1 (Chess c pcs) (p,x,y,k) ls
            -- EN LA CASILLA HAY UNA PIEZA | LA PIEZA ES DEL JUGADOR ACTUAL
            Just (z,_,_,_) -> if playerAt pcs (x+l,y+1) == c then down_1 (Chess c pcs) (p,x,y,k) ls
                -- LA PIEZA ES DEL JUGADOR RIVAL
                else let
                    aux = removePiece (x+l,y+1) pcs
                    lista = [(moveName (x,y) (x+l,y+1), Chess (1-c) (movePiece (x,y) (x+l,y+1) aux))]
                    in lista ++ down_1 (Chess c pcs) (p,x,y,k) ls
        -- EN CASO DE QUE LA POSICION ESTE FUERA DEL TABLERO
        | otherwise = down_1 (Chess c pcs) (p,x,y,k) ls
    in mov

{-
    down_2 es una funcion que retorna una lista de tuplas con un comando y un estado del juego actualizado.
    Recibe un estado Chess, una pieza y una lista de enteros con los movimientos relativos en x.
    Retorna los movimientos posibles en la direccion: 2 hacia abajo y 1 hacia los lados.
-}
down_2 :: Chess -> Piece -> [Int] -> [(String,Chess)]
-- EN CASO DE QUE SE ACABEN LOS MOVIMIENTOS RELATIVOS EN X
down_2 _ _ [] = []
down_2 (Chess c pcs) (p,x,y,k) (l:ls) = let
    mov
        -- EN CASO DE QUE LA POSICION ESTE DENTRO DEL TABLERO
        | isInside (x+l,y+2) (boardX,boardY) = case pieceAt (x+l,y+2) pcs of
            -- LA CASILLA ESTA VACIA
            Nothing -> let
                lista = [(moveName (x,y) (x+l,y+2), Chess (1-c) (movePiece (x,y) (x+l,y+2) pcs))]
                in lista ++ down_2 (Chess c pcs) (p,x,y,k) ls
            -- EN LA CASILLA HAY UN REY
            Just (_,_,_,'K') -> down_2 (Chess c pcs) (p,x,y,k) ls
            -- EN LA CASILLA HAY UNA PIEZA | LA PIEZA ES DEL JUGADOR ACTUAL
            Just (z,_,_,_) -> if playerAt pcs (x+l,y+2) == c then down_2 (Chess c pcs) (p,x,y,k) ls
                -- LA PIEZA ES DEL JUGADOR RIVAL
                else let
                    aux = removePiece (x+l,y+2) pcs
                    lista = [(moveName (x,y) (x+l,y+2), Chess (1-c) (movePiece (x,y) (x+l,y+2) aux))]
                    in lista ++ down_2 (Chess c pcs) (p,x,y,k) ls
        -- EN CASO DE QUE LA POSICION ESTE FUERA DEL TABLERO
        | otherwise = down_2 (Chess c pcs) (p,x,y,k) ls
    in mov

-- FIN DE MOVIMIENTOS DEL CABALLO
--------------------------------------------------------------------------------------------------------------------------------
-- MOVIMIENTOS DEL PEON

{-
    pbasic es una funcion que retorna una lista de tuplas con un comando y un estado del juego actualizado.
    Recibe un estado Chess, una pieza, un entero que determina si se sigue iterando
    y una lista de enteros con los movimientos relativos en y.
    Retorna los movimientos basicos de un peon hacia el frente.
    NOTA: Debido a que el movimiento es distinto segun el color del peon:
    -> 2*c - 1 equivale a 1 si es negro (se mueve hacia abajo con el jugador 1)
        y equivale a -1 si es blanco (se mueve hacia arriba con el jugador 0).
    -> 6 - 5*c equivale a la coordenada y inicial del peon segun su color, y es utilizado para que en caso
        de que y cumpla con la equivalencia, se agregue el movimiento de dos casillas del peon.
    -> 7*c entrega la posicion donde ocurre el pawn promotion (con c numero del jugador).
-}
pbasic :: Chess -> Piece -> Int -> [Int] -> [(String,Chess)]
-- EN CASO DE QUE NO SE PUEDA SEGUIR MOVIENDO
pbasic _ _ 0 _ = []
-- EN CASO DE QUE SE ACABEN LOS MOVIMIENTOS HACIA EL FRENTE
pbasic _ _ _ [] = []
pbasic (Chess c pcs) (p,x,y,k) 1 (l:ls) = if (l==1 || (l==2 && y==6-5*c))
    -- INGRESA AL THEN EN LA PRIMERA ITERACION, PERO EL MOVIMIENTO DOBLE SOLO SI EL PEON NO HA SIDO MOVIDO
    then let
        mov
            -- EN CASO DE QUE LA POSICION ESTE DENTRO DEL TABLERO
            | isInside (x,y+(l*(2*c-1))) (boardX,boardY) = case pieceAt (x,y+(l*(2*c-1))) pcs of
                -- LA CASILLA ESTA VACIA
                -- IF y_final == y_pawn_promotion
                Nothing -> if (y+(l*(2*c-1)) == 7*c)
                    -- SI AVANZA A LA ULTIMA CASILLA, SE CAMBIA EL PEON POR UNA REINA
                    then let
                        aux = removePiece (x,y) pcs
                        aux2 = addQueen aux (c,x,7*c,'Q')
                        in [(moveName (x,y) (x,y+(l*(2*c-1))), Chess (1-c) aux2)]
                    -- SI NO SE AVANZA A LA ULTIMA CASILLA, SE HACE UN MOVIMIENTO NORMAL
                    else let
                        lista = [(moveName (x,y) (x,y+(l*(2*c-1))), Chess (1-c) (movePiece (x,y) (x,y+(l*(2*c-1))) pcs))]
                        in lista ++ pbasic (Chess c pcs) (p,x,y,k) 1 ls
                -- EN LA CASILLA HAY UNA PIEZA
                Just z -> pbasic (Chess c pcs) (p,x,y,k) 0 ls
            -- EN CASO DE QUE LA POSICION ESTE FUERA DEL TABLERO
            | otherwise = pbasic (Chess c pcs) (p,x,y,k) 0 ls
        in mov
    -- SI EL PEON YA FUE MOVIDO, NO PUEDE HACER EL MOVIMIENTO DOBLE
    else pbasic (Chess c pcs) (p,x,y,k) 0 ls

{-
    pdiagonal es una funcion que retorna una lista de tuplas con un comando y un estado del juego actualizado.
    Recibe un estado Chess, una pieza, un entero que determina si se sigue iterando
    y una lista de enteros con los movimientos relativos en x.
    Retorna los movimientos diagonales de un peon (en caso de captura).
    NOTA: Debido a que el movimiento es distinto segun el color del peon:
    -> 2*c - 1 equivale a 1 si es negro (se mueve hacia abajo con el jugador 1)
        y equivale a -1 si es blanco (se mueve hacia arriba con el jugador 0).
    -> 7*c entrega la posicion donde ocurre el pawn promotion (con c numero del jugador).
-}
pdiagonal :: Chess -> Piece -> [Int] -> [(String,Chess)]
-- EN CASO DE QUE SE ACABEN LOS MOVIMIENTOS EN DIAGONAL
pdiagonal _ _ [] = []
pdiagonal (Chess c pcs) (p,x,y,k) (l:ls) = let
    mov
        -- EN CASO DE QUE LA POSICION ESTE DENTRO DEL TABLERO
        | isInside (x+l,y+(2*c-1)) (boardX,boardY) = case pieceAt (x+l,y+(2*c-1)) pcs of
            -- LA CASILLA ESTA VACIA
            Nothing -> pdiagonal (Chess c pcs) (p,x,y,k) ls
            -- EN LA CASILLA HAY UN REY
            Just (_,_,_,'K') -> pdiagonal (Chess c pcs) (p,x,y,k) ls
            -- EN LA CASILLA HAY UNA PIEZA | LA PIEZA ES DEL JUGADOR ACTUAL
            Just (z,_,_,_) -> if playerAt pcs (x+l,y+(2*c-1)) == c then pdiagonal (Chess c pcs) (p,x,y,k) ls
                -- LA PIEZA ES DEL JUGADOR RIVAL
                else let
                    pp
                        -- y_final == y_pawn_promotion (OCURRE PAWN PROMOTION)
                        | y+(2*c-1) == 7*c   = let
                            -- SE ELIMINA EL PEON Y LA PIEZA CAPTURADA, LUEGO SE AñADE LA REINA
                            aux = removePiece (x+l,7*c) $ removePiece (x,y) pcs
                            aux2 = addQueen aux (c,x+l,7*c,'Q')
                            -- AGREGA EL MOVIMIENTO Y SE LLAMA RECURSIVAMENTE CON EL RESTO DE LA LISTA DE MOVS DIAGONALES
                            in [(moveName (x,y) (x+l,7*c), Chess (1-c) aux2)] ++ pdiagonal (Chess c pcs) (p,x,y,k) ls
                        -- MOVIMIENTO NORMAL SIN PAWN PROMOTION
                        | otherwise              = let
                            aux = removePiece (x+l,y+(2*c-1)) pcs
                            lista = [(moveName (x,y) (x+l,y+(2*c-1)), Chess (1-c) (movePiece (x,y) (x+l,y+(2*c-1)) aux))]
                            in lista ++ pdiagonal (Chess c pcs) (p,x,y,k) ls
                    in pp
        -- EN CASO DE QUE LA POSICION ESTE FUERA DEL TABLERO
        | otherwise = pdiagonal (Chess c pcs) (p,x,y,k) ls
    in mov

-- FIN DE MOVIMIENTOS DEL PEON
--------------------------------------------------------------------------------------------------------------------------------

-- Inicialización del juego.

chessIni :: Chess
chessIni = let
    -- Bishops
    b = [(0,2,7,'B'), (0,5,7,'B'), (1,2,0,'B'), (1,5,0,'B')]
    -- Knights
    h = [(0,1,7,'H'), (0,6,7,'H'), (1,1,0,'H'), (1,6,0,'H')]
    -- Rooks
    r = [(0,0,7,'R'), (0,7,7,'R'), (1,0,0,'R'), (1,7,0,'R')]
    -- Queens
    q = [(0,3,7,'Q'), (1,3,0,'Q')]
    -- Kings
    k = [(0,4,7,'K'), (1,4,0,'K')]
    -- Pawns
    p0 = [(0,x,6,'P') | x <- [0..7]]
    p1 = [(1,x,1,'P') | x <- [0..7]]
    -- Lista completa de piezas iniciales
    estIni = b++h++r++q++k++p0++p1
    in Chess 0 estIni

-- Se define como se transforma un Chess a String.

instance Show Chess where
    show (Chess _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (0,_,_,'B') -> "♗ "
            Just (1,_,_,'B') -> "♝ "
            Just (0,_,_,'H') -> "♘ "
            Just (1,_,_,'H') -> "♞ "
            Just (0,_,_,'R') -> "♖ "
            Just (1,_,_,'R') -> "♜ "
            Just (0,_,_,'Q') -> "♕ "
            Just (1,_,_,'Q') -> "♛ "
            Just (0,_,_,'K') -> "♔ "
            Just (1,_,_,'K') -> "♚ "
            Just (0,_,_,'P') -> "♙ "
            Just (1,_,_,'P') -> "♟ "
            Nothing          -> if (x+y) `mod` 2 == 0 then "□ " else "■ "
        in drawBoard (boardX,boardY) draw


-- Main.

main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    -- Crear jugadores
    let player0 = human "Charles"
    let player1 = cpuRand "Alexis"
    -- Jugar
    execute chessIni [player0,player1] seed
