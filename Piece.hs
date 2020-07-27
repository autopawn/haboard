-- Piece.hs contine la lógica para las piezas que se pueden definir con jugador, posición y tipo.

module Piece where

import Data.Maybe (Maybe)

{-
    Se hace el type synonym Piece que equivale a una tupla de cuatro elementos.
-}

type Piece = (Int,Int,Int,Int) -- (jugador, coordenada x, coordenada y, Hp unidad)


{-
    movePiece sirve para cambiar la posición de una pieza.
    Recibe una posición inicial, una posicipon final y la lista de piezas.
    Retorna la lista de piezas actualizada.
-}
movePiece :: (Int,Int) -> (Int,Int) -> [Piece] -> [Piece]
movePiece (xi,yi) (xf,yf) pcs =
    [if (x,y)==(xi,yi) then (p,xf,yf,k) else (p,x,y,k) | (p,x,y,k) <- pcs]

{-
    pieceAt sirve para saber que pieza está en una posición.
    Recibe una posición a checkear y la lista de piezas.
    Retorna una pieza o Nothing en caso de no haber una pieza en la posición consultada.
-}

pieceAt :: (Int,Int) -> [Piece] -> Maybe Piece
pieceAt (xi,yi) pcs = let
    pcs2 = filter (\(p,x,y,k) -> (x,y)==(xi,yi)) pcs
    in if null pcs2 then Nothing else Just (head pcs2)

{-
    moveName sirve para generar el string correspondiente a un movimiento.
    Recibe la posición inicial y la posición final.
    Retorna un String que que representa el movimiento.
-}

moveName :: (Int,Int) -> (Int,Int) -> String
moveName (xi,yi) (xf,yf) = let
    cxi = ['a'..] !! xi
    cxf = ['a'..] !! xf
    syi = show yi
    syf = show yf
    in (cxi:syi) ++ (cxf:syf)

{-
    alLado revisa si una pieza es vecina de otra del otro jugador
    si encuentra encuentra una, le quita 1 punto de hp, sino simplemente 
    no hace nada
    Retorna una Piece
-}

alLado :: Piece -> Piece -> Piece
alLado (pp,xx, yy,kk) (p,x,y,k)
    | abs(x-xx) + abs(y-yy) == 1 && pp /= p = (p,x,y,k-1)
    | otherwise                     = (p,x,y,k)

{- 
    punchNext recibe una Piece y le aplica la funcion alLado
    a todas las unidades de la [Piece] que recibe
-}

punchNext :: Piece -> [Piece] -> [Piece]
punchNext sq st = 
    map (alLado sq) st

{-
    removerItem revisa si hay una unidad con hp = 0
    si encuentra una, retorna la lista de unidades sin
    esa pieza
-}

removerItem :: [Piece] -> [Piece]
removerItem st = filter(\(p,x,y,k) -> 0 /= k) st

{-
    actualizarLista aplica las funciones removerItem y punchNext
    recibe una Piece y una lista de Pieces
    Retorna una lista de Pieces actualizada
-}

actualizarLista :: Piece -> [Piece] -> [Piece]
actualizarLista sq li =
    removerItem (punchNext sq li)

{-
    sumaHp y totalHp sirven para sumar el hp de todas las unidades
    de un jugador
    Retornan un Int
-}

sumaHp :: Piece -> Int
sumaHp (p,x,y,k) = k

totalHp :: [Piece] -> Int -> Int
totalHp st pla = sum(map sumaHp (filter(\(p,x,y,k) -> p == pla)st))

{-
    Las funciones soldados 1 y 0 reciben un [Int]
    Retornan una lista infinita de unidades de el jugador
    0 y 1.
-}

soldados0 :: [Int] -> [Piece]
soldados0 (x:y:rns) = (0,x `mod` 15, y `mod` 10, 3) : soldados0 rns

soldados1 :: [Int] -> [Piece]
soldados1 (x:y:rns) = (1,x `mod` 15, y `mod` 10, 3) : soldados1 rns

{-
    Posiciones 1 y 0 reciben un [Int] (lista de numeros infinitos)
    es la misma lista que reciben soldados 0 y 1 para revisar 
    las posiciones que tienen estas
    Retorna una lista de posiciones (x,y) de las unidades de el jugador
-}

posiciones0 :: [Int] -> [(Int,Int)]
posiciones0 (x:y:rns) = (x `mod` 15, y `mod` 10) : posiciones0 rns

posiciones1 :: [Int] -> [(Int,Int)]
posiciones1 (x:y:rns) = (x `mod` 15, y `mod` 10) : posiciones1 rns

{-
    Comparador toma dos listas de numeros infinitos, toma 4 posiciones de
    las listas y compara las primeras 4 posiciones y revisa si hay se repite
    alguna posicion entre las 2 listas.
    Retorna un True si encuentra una, si no va a retornar un False
-}

comparador :: [Int] -> [Int] -> Bool
comparador semilla0 semilla1
    | True `elem` map(`elem` (take 4 (posiciones0 semilla0))) (take 4 (posiciones1 semilla1))  = True
    | otherwise = False

{-
    unidades recibe una lista de Int infinitas, revisa si hay posiciones
    repetidas entre las listas de posiciones, si encuentra que es True, hace 
    un loop y bota 4 indices de las listas de Int infinitos.
    Retorna una lista de 8 unidades de los jugadores, 4 del jugador 0
    y 4 del jugador 1
-}

unidades :: [Int] -> [Int] -> [Piece]
unidades semilla0 semilla1
    | comparador semilla0 semilla1 == True = unidades (drop 4 semilla0) (drop 4 semilla1)
    | otherwise          = (take 4 (soldados0 semilla0)) ++ (take 4 (soldados1 semilla1))

{-
    playerPieces sirve para saber que piezas pertenecen al jugador.
    Recibe la lista de piezas y un Int correspondiente a la id del jugador.
    Retorna la lista de piezas del jugador.
-}

playerPieces :: [Piece] -> Int -> [Piece]
playerPieces pcs n = [pc | pc@(p,x,y,k) <- pcs, p==n]

{-
    drawBoard sirve para generar el String que se visualizará por pantalla.
    Recibe el tamaño del tablero y una función que códifica una celda a su 
    caracter correspondiente
    Retorna el String que representa el tablero.
-}

drawBoard :: (Int,Int) -> ((Int,Int) -> String) -> String
drawBoard (sizeX,sizeY) drawf = let
    -- Dibujar la primera línea
    drawLine (-1) = "\t" ++ concatMap (: " ") (take sizeX ['a'..]) ++ "\n"
    -- Dibujar el resto del tablero
    drawLine y    = show y ++ "\t" ++ concat [drawf (x,y) | x<-[0..sizeX-1]] ++ "\n"
    in concatMap drawLine [-1..sizeY-1]