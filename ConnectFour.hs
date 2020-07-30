import Game
import Piece

import System.Random (getStdGen,randoms)

-- Se crea el tipo ConnectFour.

data ConnectFour = ConnectFour Int [Piece] -- jugador actual y lista de piezas

-- Se define el tamaño del tablero

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
        -- Ve las condiciones de victoria del juego (donde 0 es jugador 1; 1 es jugador 2)

        -- Horizontal
        | (status)<-checkHorizontalBoth (pc,0),status == True = Just (0) 
        | (status)<-checkHorizontalBoth (pc,1),status == True = Just (1) 
        -- Vertical
        | (status)<-checkVerticalBoth (pc,0),status == True = Just (0) 
        | (status)<-checkVerticalBoth (pc,1),status == True = Just (1) 
        -- diagonal
        | (status)<-checkDiagonalUpperBoth (pc,0),status == True = Just (0) 
        | (status)<-checkDiagonalUpperBoth (pc,1),status == True = Just (1) 
        | (status)<-checkDiagonalUnderBoth (pc,0),status == True = Just (0) 
        | (status)<-checkDiagonalUnderBoth (pc,1),status == True = Just (1) 
 
        -- En todo otro caso se sigue jugando
        | otherwise                           = Nothing
    movements st@(ConnectFour c pc) = possibleMoves st



checkHorizontalBoth :: ([Piece],Int) -> (Bool)
checkHorizontalBoth (pces,pieces)  
     | pieces == 0 && checkOneHorizontal valueEvaluateFirst == True = (True)
     | pieces == 1 && checkOneHorizontal valueEvaluateSecond == True = (True)
     | otherwise =  False
     where  valueEvaluateFirst = filter (\(p,x,y,t) -> t == 'C') pces 
            valueEvaluateSecond = filter (\(p,x,y,t) -> t == 'F') pces 
       

checkVerticalBoth :: ([Piece],Int) -> (Bool)
checkVerticalBoth (pces,pieces)  
     | pieces == 0 && checkOneVertical valueEvaluateFirst == True = (True)
     | pieces == 1 && checkOneVertical valueEvaluateSecond == True = (True)
     | otherwise =  False
     where  valueEvaluateFirst = filter (\(p,x,y,t) -> t == 'C') pces 
            valueEvaluateSecond = filter (\(p,x,y,t) -> t == 'F') pces 
    

checkDiagonalUpperBoth :: ([Piece],Int) -> (Bool)
checkDiagonalUpperBoth (pces,pieces)  
     | pieces == 0 && checkDiagUpper valueEvaluateFirst == True = (True)
     | pieces == 1 && checkDiagUpper valueEvaluateSecond == True = (True)
     | otherwise =  False
     where  valueEvaluateFirst = filter (\(p,x,y,t) -> t == 'C') pces 
            valueEvaluateSecond = filter (\(p,x,y,t) -> t == 'F') pces

checkDiagonalUnderBoth :: ([Piece],Int) -> (Bool)
checkDiagonalUnderBoth (pces,pieces)  
     | pieces == 0 && checkDiagUnder valueEvaluateFirst == True = (True)
     | pieces == 1 && checkDiagUnder valueEvaluateSecond == True = (True)
     | otherwise =  False
     where  valueEvaluateFirst = filter (\(p,x,y,t) -> t == 'C') pces 
            valueEvaluateSecond = filter (\(p,x,y,t) -> t == 'F') pces


--Funciones especificas
checkOneHorizontal :: [Piece] -> Bool
checkOneHorizontal pces = let 
    x = map(\(p,x,y,t)-> elem (p,x+1,y,t) pces && elem (p,x+2,y,t) pces&& elem (p,x+3,y,t) pces ) pces
    in any(\x->x==True) x

checkOneVertical :: [Piece] -> Bool
checkOneVertical pces = let
    x = map(\(p,x,y,t)-> elem (p,x,y+1,t) pces && elem (p,x,y+2,t) pces&& elem (p,x,y+3,t) pces ) pces
    in any(\x->x==True) x

checkDiagUpper ::[Piece] -> Bool
checkDiagUpper pces=let
    result = filter (\(p,x,y,k) -> (p,x,y,k)==(p,x+1,y+1,k) && (p,x,y,k)==(p,x+2,y+2,k) && (p,x,y,k)==(p,x+3,y+3,k)) pces
    in if null result then False else True

checkDiagUnder ::[Piece] -> Bool
checkDiagUnder pces=let
    result = filter (\(p,x,y,k) -> (p,x,y,k)==(p,x-1,y-1,k) && (p,x,y,k)==(p,x-2,y-2,k) && (p,x,y,k)==(p,x-3,y-3,k)) pces
    in if null result then False else True


possibleMoves :: ConnectFour -> [(String,ConnectFour)]
possibleMoves (ConnectFour c pcs) = let
    -- Dimensiones del tablero 
    table=[(x,y) | x <- [0..6], y <- [0..5]]
    -- Posiciones validas 
    isValid (xp,yp) = pieceAt (xp,yp) pcs == Nothing && 0 <= xp && xp < 7 && (if 0 /= yp then pieceAt (xp,yp-1) pcs /= Nothing else True) && yp < 6 

    movement = filter isValid table
    in [(moveName  (xp,yp) (xp,yp), ConnectFour (1-c) (addPiece (xp,yp,c)  pcs) ) | (xp,yp) <- movement]


-- Se agrega una nueva pieza al tablero
addPiece :: (Int,Int,Int)->[Piece]->[Piece]
addPiece (xf,yf,c) pcs = let
    -- Reviso el jugador
    player = if c==0 then 'C' else 'F'
    -- genero una nueva pieza
    in [(c,xf,yf,player)]++pcs


--Muestra el tablero por pantalla
instance Show ConnectFour where
    show (ConnectFour _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'C') -> "⚫ "
            Just (_,_,_,'F') -> "🟠 "
            Nothing          -> "〇 " 
        in drawBoard (boardX,boardY) draw


-- Inicialización del juego
connectFourIni :: ConnectFour
connectFourIni = ConnectFour 0 []

-- Main.

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