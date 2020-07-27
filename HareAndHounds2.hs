
import Game
import Piece

import System.Random (getStdGen,randoms)

data HareAndHounds = HareAndHounds Int [Piece]
boardX :: Int
boardX = 5
boardY :: Int
boardY = 9
tablero :: (Int,Int)-> Int
tablero (x,y)
    |y==0 && x==0 = 1
    |y==0 && x==1 = 1
    |y==1 && x==0 = 1
    |y==0 && x==3 = 1
    |y==1 && x==4 = 1
    |y==0 && x==4 = 1
    |y==7 && x==0 = 1
    |y==8 && x==0 = 1
    |y==8 && x==1 = 1
    |y==8 && x==3 = 1
    |y==8 && x==4 = 1
    |y==7 && x==4 = 1
    |y>8         = 1
    |x>4         = 1
    |x `mod` 2==1 = 2
    |y `mod` 2==1 = 2
    |otherwise    = 0

grafo :: (Int,Int)-> Int
grafo (x,y)
    |tablero(x+1,y)==0 && tablero(x-1,y)==0     = 1
    |tablero(x,y+1)==0 && tablero(x,y-1)==0     = 2
    |tablero(x+1,y+1)==0 && tablero(x-1,y+1)==0 && tablero(x+1,y-1)==0 && tablero(x-1,y-1)==0 = 5
    |tablero(x+1,y-1)==0 && tablero(x-1,y+1)==0 = 3
    |tablero(x+1,y+1)==0 && tablero(x-1,y-1)==0 = 4
    |otherwise                                  = 6


instance Game HareAndHounds where
    current (HareAndHounds c _) = c

    winner (HareAndHounds c pc) mvs
        -- Si el jugador no tiene movimientos, gana el otro
        | null mvs                            = Just (1 - c)
        -- Si el jugador 0 (Fox) llega a y = 0, gana
        | any (\(p,x,y,k) -> p==0 && y==0) pc = Just 0
        -- En todo otro caso se sigue jugando
        | otherwise                           = Nothing

    movements st@(HareAndHounds c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

pieceMoves :: HareAndHounds -> Piece -> [(String,HareAndHounds)]
pieceMoves (HareAndHounds c pcs) (p,x,y,k) = let
    -- Posiciones posibles dependiendo del tipo de pieza
    steps
        | k=='H'  = [(x+2,y+2),(x-2,y+2),(x-2,y+0),(x+2,y+0)]
        | k=='F'  = [(x+2,y+2),(x-2,y+2),(x+2,y-2),(x-2,y-2),(x+2,y+0),(x+0,y+2),(x-2,y+0),(x+0,y-2)]
    -- Checkear si es posible moverse a una posición
    isValid (xf,yf) =
        pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY && tablero(xf,yf)==0
    -- Generar los posibles movimientos
    steps2 = filter isValid steps
    in [(moveName (x,y) (xf,yf), HareAndHounds (1-c) (movePiece (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2]

hareAndHoundsIni :: HareAndHounds
hareAndHoundsIni = HareAndHounds 0 [(0,2,8,'F'),(1,2,0,'H'),(1,0,2,'H'),(1,4,2,'H')]

instance Show HareAndHounds where
    show (HareAndHounds _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'H') -> "H "
            Just (_,_,_,'F') -> "F "
            Nothing          
                |tablero (x,y)== 1 ->"  "
                |tablero (x,y)== 0 ->"■ "
                |grafo(x,y)== 1    ->"- "
                |grafo(x,y)== 2    ->"| "
                |grafo(x,y)== 3    ->"/ "
                |grafo(x,y)== 4    ->"\\ "
                |grafo(x,y)== 5    ->if(x+y) `mod` 6 == 0 
                                        then do  
                                            "/ "  
                                        else do 
                                            "\\ "
                |tablero(x,y)== 2  ->"□ "
        in drawBoard (boardX,boardY) draw

hareAndHoundsEval :: HareAndHounds -> Float
hareAndHoundsEval (HareAndHounds c pcs) = let
    fI = fromIntegral
    hare@(_,fx,fy,_) = head (filter (\(p,_,_,_) -> p == 0) pcs)
    houndsum = sum [if y>=fy then 0.5 else 0.05 * abs (fI x - fI fx) | (p,x,y,k) <- pcs, p==1]
    in (houndsum + 7 - fI fy) * (if c==0 then 1.0 else -1.0)

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
    let player1 = cpuEval "Father" hareAndHoundsEval
    -- Jugar
    execute hareAndHoundsIni [player0,player1] seed


