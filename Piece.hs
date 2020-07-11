module Piece where

import Data.Maybe (Maybe)

type Piece = (Int,Int,Int,Char) -- (player,x,y,kind)

movePiece :: (Int,Int) -> (Int,Int) -> [Piece] -> [Piece]
movePiece (xi,yi) (xf,yf) pcs =
    [if (x,y)==(xi,yi) then (p,xf,yf,k) else (p,x,y,k) | (p,x,y,k) <- pcs]

pieceAt :: (Int,Int) -> [Piece] -> Maybe Piece
pieceAt (xi,yi) pcs = let
    pcs2 = filter (\(p,x,y,k) -> (x,y)==(xi,yi)) pcs
    in if null pcs2 then Nothing else Just (head pcs2)

moveName :: (Int,Int) -> (Int,Int) -> String
moveName (xi,yi) (xf,yf) = let
    cxi = ['a'..] !! xi
    cxf = ['a'..] !! xf
    syi = show yi
    syf = show yf
    in (cxi:syi) ++ (cxf:syf)

playerPieces :: [Piece] -> Int -> [Piece]
playerPieces pcs n = [pc | pc@(p,x,y,k) <- pcs, p==n]

drawBoard :: (Int,Int) -> ((Int,Int) -> String) -> String
drawBoard (sizeX,sizeY) drawf = let
    drawLine (-1) = "\t" ++ concatMap (: " ") (take sizeX ['a'..]) ++ "\n"
    drawLine y    = show y ++ "\t" ++ concat [drawf (x,y) | x<-[0..sizeX-1]] ++ "\n"
    in concatMap drawLine [-1..sizeY-1]

