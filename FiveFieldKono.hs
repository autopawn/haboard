-- FiveFieldKono.hs contiene la lógica específica del juego Five Field Kono

import Game
import Piece

import System.Random (getStdGen,randoms)

-- Se crea el tipo FiveFieldKono.

data FiveFieldKono = FiveFieldKono Int [Piece]

boardX :: Int
boardX = 5
boardY :: Int
boardY = 5

{-
    inStartPos sirve para determinar si una coordenada corresponde a una posción inicial o no.
    Recibe una coordenada.
    Retorna un Int que dice si la posición inicial corresponde al jugador 1, 0 o si no es una
    posición inicial.
-}

inStartPos :: (Int,Int) -> Int
inStartPos (x,y)
    | y==0                         = 1
    | y==1 && (x==0 || x==bx-1)    = 1
    | y==by-1                      = 0
    | y==by-2 && (x==0 || x==bx-1) = 0
    | otherwise                    = -1
    where
    bx = boardX
    by = boardY

-- Se hace que FiveFieldKono sea instancia de Game y se definen las funciones necesarias.

instance Game FiveFieldKono where
    current (FiveFieldKono c _) = c

    winner (FiveFieldKono c pc) mvs
        -- Si el jugador actual no tiene movimientos, gana el otro
        | null mvs = Just (1 - c)
        -- Si todas las piezas del jugador0 están en las posiciones iniciales del jugador1, gana
        | all (\(p,x,y,k) -> inStartPos (x,y) == 1) (playerPieces pc 0) = Just 0
        -- Si todas las piezas del jugador1 están en las posiciones iniciales del jugador0, gana
        | all (\(p,x,y,k) -> inStartPos (x,y) == 0) (playerPieces pc 1) = Just 1
        -- Si ninguna se cumple, se sigue jugando
        | otherwise = Nothing

    movements st@(FiveFieldKono c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

{-
    pieceMoves genera los movimientos posibles para una pieza.
    Recibe un juego FiveFieldKono y una pieza.
    Retorna un movimiento, que consiste de un comando y el nuevo estado del juego.
-}

pieceMoves :: FiveFieldKono -> Piece -> [(String,FiveFieldKono)]
pieceMoves (FiveFieldKono c pcs) (p,x,y,k) = let
    -- Posibles posiciones a las que moverse
    steps = [(x-1,y-1),(x-1,y+1),(x+1,y-1),(x+1,y+1)]
    -- Checkear si es posible moverse a una posición
    isValid (xf,yf) =
        pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY
    -- Generar los posibles movimientos
    steps2 = filter isValid steps
    in [(moveName (x,y) (xf,yf), FiveFieldKono (1-c) (movePiece (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2]

-- Se define como se transforma un FiveFieldKono a String.

instance Show FiveFieldKono where
    show (FiveFieldKono _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (0,_,_,_) -> "⚉ "
            Just (1,_,_,_) -> "⚇ "
            Nothing
                | inStartPos (x,y) == 0 -> "◡ "
                | inStartPos (x,y) == 1 -> "◠ "
                | otherwise             -> "  "
        in drawBoard (boardX,boardY) draw

-- Inicialización del juego.

fiveFieldKonoIni :: FiveFieldKono
fiveFieldKonoIni = FiveFieldKono 0 [(p,x,y,'K') |
    x<-[0..boardX-1], y<-[0..boardY-1], let p = inStartPos (x,y), p>=0]
{-
    Esta funcion solo existe para evitarme un error, lo que hace no tiene nada relevante, incluso es una copia de 
    la funcion eval de fox and hounds. ya que al no tener una funcion eval propia tuve que "crear" una funcion para 
    que no me diera error, pero al momento de generar los jugadores no tomara encuenta esta funcion 
    y eligira una cpu random en su lugar.
-}
fiveFieldKonoEval :: FiveFieldKono -> Float
fiveFieldKonoEval (FiveFieldKono c pcs) = let
    fI = fromIntegral
    fox@(_,fx,fy,_) = head (filter (\(p,_,_,_) -> p == 0) pcs)
    houndsum = sum [if y>=fy then 0.5 else 0.05 * abs (fI x - fI fx) | (p,x,y,k) <- pcs, p==1]
    in (houndsum + 7 - fI fy) * (if c==0 then 1.0 else -1.0)

-- Main.

main :: IO Int
main = do
    {-
        Creo gen para poder usarla en la funcion fivefieldkono.
    -}
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    {-
        Se llama a la funcion que nos piden.
    -}
    configAndExecute fiveFieldKonoIni seed "five Fiel dKono" fiveFieldKonoEval
