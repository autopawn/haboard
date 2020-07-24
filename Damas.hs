{-# LANGUAGE ParallelListComp #-}
import Game
import Piece

import System.Random (getStdGen,randoms)

data Damas = Damas Int [Piece] 

boardX :: Int
boardX = 8
boardY :: Int
boardY = 8

-- decir que las lineas de los bordes son los lugares en donde las damas se transforman en reinas.
inStartPos :: (Int,Int) -> Int
inStartPos (x,y)
    | y==0                         = 1
    | y==by-1                      = 0
    | otherwise                    = -1
    where
    by = boardY

instance Game Damas where
    current (Damas c _) = c

    winner (Damas c pc) mvs
        -- Si el jugador actual no tiene movimientos, gana el otro
        | null mvs                                   = Just (1 - c)
        -- Si ninguna se cumple, se sigue jugando
        | otherwise                                  = Nothing

    movements st@(Damas c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

playerAt :: [Piece] -> (Int,Int) -> Int
playerAt pcs (x,y) = case pieceAt (x,y) pcs of
  Just (j,_,_,_) -> j
  Nothing        -> -1

-- Para matar una piesa la mando a un lugar donde no pueda ser vista,(en palabras mas simple, yeeteo la pieza a Boswania)
yeet :: Piece -> (Int,Int) -> [Piece] -> Piece
yeet (j,xi,yi,k) (xt,yt) pcs
    | pieceAt(xt,yt) pcs == Nothing          = (j,xt,yt,k)
    | otherwise                              = yeet (j,xi,yi,k) (xt+1,yt) pcs

eat :: (Int,Int) -> (Int,Int) -> [Piece] -> Piece -> Piece 
eat (xi,yi) (xf,yf) pcs pc@(p,x,y,j)
    | tx == a && ty == b                     = yeet pc (100,100) pcs
    | (x,y)==(xi,yi)                         = (p,xf,yf,j) 
    | otherwise                              = pc 
    where
    tx = fromIntegral x :: Float      
    ty = fromIntegral y :: Float
    a = fromIntegral (xi+xf)/2 :: Float
    b = fromIntegral (yi+yf)/2 :: Float

queen :: Piece -> Piece
queen (j,x,y,k) 
    | k == 'b' && inStartPos(x,y) == 0       = (j,x,y,'B')
    | k == 'n' && inStartPos(x,y) == 1       = (j,x,y,'N')
    | otherwise                              = (j,x,y,k)

pieceMoves :: Damas -> Piece -> [(String,Damas)]
pieceMoves (Damas c pc) (p,x,y,k) = let
    -- Chequea si ahy que coronar una piesa
    pcs = map queen pc   -- si lo hara despues del turno del rival pero no deveria de afectar ya que en el turno del jugador tendra su reina

    -- Posiciones posibles dependiendo del tipo de pieza
    steps                                                                                        
        | k=='b'  = [(if pieceAt(x+1,y+1) pcs == Nothing then (x+1,y+1) else if playerAt pcs (x+1,y+1) == p then (x+1,y+1) else (x+2,y+2)) , (if pieceAt(x-1,y+1) pcs == Nothing then (x-1,y+1) else if playerAt pcs (x-1,y+1) == p then (x-1,y+1) else (x-2,y+2))] --esto se ve horrible para solo 2 opciones :c
        | k=='n'  = [(if pieceAt(x+1,y-1) pcs == Nothing then (x+1,y-1) else if playerAt pcs (x+1,y-1) == p then (x+1,y-1) else (x+2,y-2)) , (if pieceAt(x-1,y-1) pcs == Nothing then (x-1,y-1) else if playerAt pcs (x-1,y-1) == p then (x-1,y+1) else (x-2,y-2))]
        | k=='B'  = [(if pieceAt(x+1,y-1) pcs == Nothing then (x+1,y-1) else if playerAt pcs (x+1,y-1) == p then (x+1,y-1) else (x+2,y-2)) , (if pieceAt(x-1,y-1) pcs == Nothing then (x-1,y-1) else if playerAt pcs (x-1,y-1) == p then (x-1,y+1) else (x-2,y-2)) , (if pieceAt(x+1,y+1) pcs == Nothing then (x+1,y+1) else if playerAt pcs (x+1,y+1) == p then (x+1,y+1) else (x+2,y+2)) , (if pieceAt(x-1,y+1) pcs == Nothing then (x-1,y+1) else if playerAt pcs (x-1,y+1) == p then (x-1,y+1) else (x-2,y+2))]
        | k=='N'  = [(if pieceAt(x+1,y-1) pcs == Nothing then (x+1,y-1) else if playerAt pcs (x+1,y-1) == p then (x+1,y-1) else (x+2,y-2)) , (if pieceAt(x-1,y-1) pcs == Nothing then (x-1,y-1) else if playerAt pcs (x-1,y-1) == p then (x-1,y+1) else (x-2,y-2)) , (if pieceAt(x+1,y+1) pcs == Nothing then (x+1,y+1) else if playerAt pcs (x+1,y+1) == p then (x+1,y+1) else (x+2,y+2)) , (if pieceAt(x-1,y+1) pcs == Nothing then (x-1,y+1) else if playerAt pcs (x-1,y+1) == p then (x-1,y+1) else (x-2,y+2))]

    {-
        Como las 4 linae de arribas se ven feas voy a explicar lo que hacen aca:
        Primero comprueban si hay una piesa en las diagonal que deverian de moverse
        en caso de que exitsa una piesa comprueva si la piesa es de su mismo equipo
        en caso de que no sea de su equipo puede saltar esa piesa
        en caso contrario se deja la coordenada original para que se filtre y borre
        luego se repite por cada diagonal en la que pueda moverse la piesa
    -}
   
    -- Checkear si es posible moverse a una posición
    isValid (xf,yf) =                                     
        pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY

    
    -- Generar los posibles movimientos
    steps2 = filter isValid steps
    in [(moveName (x,y) (xf,yf), Damas (1-c) (if abs(x-xf)==2 && abs(y-yf)==2 then map(eat (x,y) (xf,yf) pcs) pcs else (movePiece (x,y) (xf,yf) pcs))) |(xf,yf) <- steps2]



  
-- Inicialización del juego.

damasIni :: Damas
damasIni = Damas 0 [(0,0,7,'n'),(0,2,7,'n'),(0,4,7,'n'),(0,6,7,'n'),(0,7,6,'n'),(0,5,6,'n'),(0,3,6,'n'),(0,1,6,'n'),(0,0,5,'n'),(0,2,5,'n'),(0,4,5,'n'),(0,6,5,'n'),(1,1,0,'b'),(1,3,0,'b'),(1,5,0,'b'),(1,7,0,'b'),(1,0,1,'b'),(1,2,1,'b'),(1,4,1,'b'),(1,6,1,'b'),(1,7,2,'b'),(1,5,2,'b'),(1,3,2,'b'),(1,1,2,'b')]

-- Se define como se transforma un Damas a String.

instance Show Damas where
    show (Damas _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'b') -> "d "    -- Ⓓ
            Just (_,_,_,'n') -> "D "    -- 🅓
            Just (_,_,_,'B') -> "q "    -- Ⓠ
            Just (_,_,_,'N') -> "Q "    -- 🅠
            Nothing          -> if (x+y) `mod` 2 == 0 then "■ " else "□ "
        in drawBoard (boardX,boardY) draw
    

main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    -- Crear jugadores
    let player0 = human "Son"
    let player1 = human "Father" --foxAndHoundsEval
    -- Jugar
    execute damasIni [player0,player1] seed

