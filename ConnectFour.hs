import Game
import Piece
import System.Random (getStdGen,randoms)
-- Se crea el tipe connectFour
data ConnectFour = ConnectFour Int [Piece] -- jugador actual y lista de piezas
--Dimensiones del tablero
boardX :: Int
boardX = 7
boardY :: Int
boardY = 6
--Hace que ConnectFour sea instancia de game
instance Game ConnectFour where
    current (ConnectFour c _) = c
    winner (ConnectFour c pc) mvs
        -- Si el jugador no tiene movimientos, gana el otro
        | null mvs                                      = Just (1 - c)
        -- Ve las condiciones de victoria del juego
        |checkVert pceR ||checkHor pceR ||checkDiag pceR||checkDiag2 pceR= Just (0)
        |checkVert pceB ||checkHor pceB ||checkDiag pceB||checkDiag2 pceB = Just (1)
        -- En todo otro caso se sigue jugando
        | otherwise                           = Nothing
        where   pceR=[(p,x,y,t)|(p,x,y,t)<- pc,t =='R'] 
                pceB=[(p,x,y,t)|(p,x,y,t)<- pc,t =='B']
    movements st@(ConnectFour c pc) = possibleMoves st
--Funciones para verificar la condicion de victoria (No eficiente pero sirve)
checkVert :: [Piece] -> Bool
checkVert pces = let 
    x = map(\(p,x,y,t)-> elem (p,x,y+1,t) pces && elem (p,x,y+2,t) pces&& elem (p,x,y+3,t) pces ) pces
    in any(\x->x==True) x
checkHor :: [Piece] -> Bool
checkHor pces = let 
    x = map(\(p,x,y,t)-> elem (p,x+1,y,t) pces && elem (p,x+2,y,t) pces&& elem (p,x+3,y,t) pces ) pces
    in any(\x->x==True) x

checkDiag ::[Piece] -> Bool
checkDiag pces=let
    x = map(\(p,x,y,t)-> elem (p,x+1,y+1,t) pces&& elem (p,x+2,y+2,t) pces&& elem (p,x+3,y+3,t) pces ) pces
    in any(\x->x==True) x
checkDiag2 ::[Piece] -> Bool
checkDiag2 pces=let
    x = map(\(p,x,y,t)-> elem (p,x-1,y+1,t) pces&& elem (p,x-2,y+2,t) pces&& elem (p,x-3,y+3,t) pces ) pces
    in any(\x->x==True) x


possibleMoves :: ConnectFour -> [(String,ConnectFour)]
possibleMoves (ConnectFour c pcs) = let
    table=[(x,y) | x <- [0..(boardX-1)], y <- [0..(boardY-1)]]
    -- Ver las posiciones validas para poner una ficha
    isValid (xf,yf) =
        pieceAt (xf,yf) pcs == Nothing && (if yf /=0 then  pieceAt (xf,yf-1) pcs /= Nothing else True) 
    -- Generar los posibles movimientos
    mvs = filter isValid table
    kind= if c==0 then 'R' else 'B'
    in [(customMoveName (xf,yf), ConnectFour (1-c) (addPiece c (xf,yf) kind pcs) ) | (xf,yf) <- mvs]
--Muestra el nombre del movimiento
customMoveName :: (Int,Int) -> String
customMoveName (xf,yf) = let
    cxf = ['a'..] !! xf
    syf = show yf
    in (cxf:syf)
--Añade una pieza al tablero
addPiece :: Int->(Int,Int)->Char->[Piece]->[Piece]
addPiece c (xf,yf) kind pcs = [(c,xf,yf,kind)]++pcs
--Muestra el tablero por pantalla
instance Show ConnectFour where
    show (ConnectFour _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'R') -> "⚉ "
            Just (_,_,_,'B') -> "⚇ "
            Nothing          -> "□ " 
        in drawBoard (boardX,boardY) draw
--Inicializacion del juego
connectFourIni :: ConnectFour
connectFourIni = ConnectFour 0 []

main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    -- Crear jugadores
    let player0 = cpuRand "Son"
    let player1 = cpuRand "Father"
    -- Jugar
    execute connectFourIni [player0,player1] seed