import Game
import Piece

import System.Random(getStdGen, randoms)

data Simplewars = Simplewars Int [Piece] --jugador actual y lista de Pieces

boardX :: Int
boardY :: Int
boardX = 15
boardY = 10

-- Simplewars se hace como instancia de Game y se define lo nesesario para poder jugarlo

instance Game Simplewars where
    current(Simplewars c _) = c

    winner(Simplewars c pc) mvs
        --Si al jugador actual no le quedan movimientos, gana el otro jugador
        | null mvs = Just (1 - c)
        --En el otro caso, el Game continuara
        | otherwise = Nothing

    movements st@(Simplewars c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

{-
    pieceMoves genera los movimientos posibles para las unidades de los jugadores.
    Recibe un Simplewars y una pieza.
    Retorna el un movimiento, consistente de su nombre y el estado que genera.
-}

pieceMoves :: Simplewars -> Piece -> [(String, Simplewars)]
pieceMoves (Simplewars c pcs) (p,x,y,k) = let
    -- Posiciones disponibles para las unidades
    steps = [(x+a, y+b)| a <- [-4..4], b <- [-4..4], 0 < (abs(a) + abs(b)) && (abs(a) + abs(b)) <= 4]
        
    -- Comprueba si los movimientos son validos
    isValid (xf, yf) =
        pieceAt (xf, yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY

    -- Generar la lista de movimientos posibles para el jugador
    steps2 = filter isValid steps
    in [(moveName (x,y) (xf,yf), Simplewars (1-c) (actualizarLista (p,xf,yf,k) (movePiece (x,y) (xf,yf) pcs)))| (xf, yf) <- steps2]

-- Inicializacion del juego

simplewarsIni :: [Int] -> [Int] -> Simplewars
simplewarsIni num num2 = Simplewars 0 (unidades num num2)

-- Se define como se transforma un Simplewars a String.
instance Show Simplewars where

    show (Simplewars _ pcs) = let
        draw(x,y) = case pieceAt (x,y) pcs of 
            Just (0,_,_,_) -> "ⓢ "
            Just (1,_,_,_) -> "🅢 "
            Nothing        -> if(x+y) `mod` 2 == 0 then "■ " else "□ "
        in drawBoard (boardX, boardY) draw

{-
    simplewarsEval recibe el estado del juego actual y compara la suma
    de la vida de las unidades de los jugadores.
    Retorna un float de la evaluacion del estado.
-}

simplewarsEval :: Simplewars -> Float
simplewarsEval (Simplewars c pcs) = let
    fI = fromIntegral
    hpEne = totalHp pcs (1-c)
    hpJug = totalHp pcs c
    unitsum = hpJug - hpEne
    in fI(unitsum * (if c==(1-c) then -1 else 1))

-- Main

main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    -- Semillas especificas que se van a usar para generar las unidades de los jugadores
    let listas = randoms gen
    let listas1 = tail(randoms gen)
    putStrLn $ "Seed: " ++ show seed

    -- Crear jugadores
    let player0 = cpuEval "" simplewarsEval 
    let player1 = cpuEval "" simplewarsEval

    -- Jugar
    execute (simplewarsIni listas listas1) [player0,player1] seed