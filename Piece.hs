-- Piece.hs contine la lógica para las piezas que se pueden definir con jugador, posición y tipo.

module Piece where

import Data.Maybe (Maybe)

{-
    Se hace el type synonym Piece que equivale a una tupla de cuatro elementos.
-}

type Piece = (Int,Int,Int,Char) -- (jugador, coordenada x, coordenada y, tipo)


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
    -- si y = -1, se crea la primera linea (la linea con carácteres, de lo contrario se crean las lineas verticales de numeros)
    drawLine (-1) = "\t" ++ concatMap (: " ") (take sizeX ['a'..]) ++ "\n"
    drawLine (0) = let
        drawLine2 (0) = show 0 ++ "\t" ++ "  "
        drawLine2 (1) = " "
        drawLine2 (2) = " " ++ drawf(2,0)
        drawLine2 (3) = "-"
        drawLine2 (4) = " " ++ drawf(4,0)
        drawLine2 (5) = "-"
        drawLine2 (6) = " " ++ drawf(6,0) ++ "\n"
        in concatMap drawLine2 [0..6]
    drawLine (1) = let
        drawLine2 (0) = show 1 ++ "\t" ++ " "
        drawLine2 (1) = " /"
        drawLine2 (2) = " |" 
        drawLine2 (3) = " \\" 
        drawLine2 (4) = " |" 
        drawLine2 (5) = " /"
        drawLine2 (6) = " |" 
        drawLine2 (7) = " \\\n" 
        in concatMap drawLine2 [0..7]
    drawLine (2) = let
        drawLine2 (0) = show 2 ++ "\t" ++ drawf(0,2)
        drawLine2 (1) = "-"
        drawLine2 (2) = " " ++ drawf(2,2)
        drawLine2 (3) = "-"
        drawLine2 (4) = " " ++ drawf(4,2)
        drawLine2 (5) = "-"
        drawLine2 (6) = " " ++ drawf(6,2)
        drawLine2 (7) = "-"
        drawLine2 (8) = " " ++ drawf (8,2) ++ "\n"
        in concatMap drawLine2 [0..8]
    drawLine (3) = let
        drawLine2 (0) = show 3 ++ "\t" ++ " "
        drawLine2 (1) = " \\"
        drawLine2 (2) = " |"
        drawLine2 (3) = " /" 
        drawLine2 (4) = " |" 
        drawLine2 (5) = " \\"
        drawLine2 (6) = " |" 
        drawLine2 (7) = " /" ++ "\n"
        in concatMap drawLine2 [0..7]
    drawLine (4) = let 
        drawLine2 (0) = show 4 ++ "\t" ++ "  "
        drawLine2 (1) = " "
        drawLine2 (2) = " " ++ drawf(2,4)
        drawLine2 (3) = "-"
        drawLine2 (4) = " " ++ drawf(4,4)
        drawLine2 (5) = "-"
        drawLine2 (6) = " " ++ drawf(6,4) ++ "\n"
        in concatMap drawLine2 [0..6]
    in concatMap drawLine [-1..sizeY-1]
