-- HareAndHounds.hs contiene la lógica específica del juego Hare and Hounds

import Game
import Piece

import System.Random (getStdGen,randoms)

-- Se crea el tipo Hare And Hounds

data HareAndHounds = HareAndHounds Int [Piece] -- jugador actual y lista de piezas

-- Se define el tamaño del tablero

boardX :: Int
boardX = 9
boardY :: Int
boardY = 5

-- Se hace que HareAndHounds sea instancia de Game y se definen las funciones necesarias.

instance Game HareAndHounds where
    current (HareAndHounds c _) = c

    winner (HareAndHounds c pc) mvs
        -- Si el jugador no tiene movimientos, gana el otro
        | null mvs                            = Just (1 - c)
        -- Si Hare (p=1) llega a (1,1) , gana
        | any (\(p,x,y,k) -> p==1 && x == 0 && y == 1) pc = Just 1
        -- Si Hare llega a una posición a la izquierda de todos los Hounds, entonces gana, ya que Hounds no pueden retroceder
        | any (\(p,x,y,k) -> p==1 && leftSide x pc) pc = Just 1
        -- En todo otro caso se sigue jugando
        | otherwise                           = Nothing

    movements st@(HareAndHounds c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

{-
    pieceMoves genera los movimientos posibles para los dos tipos de piezas del juego.
    Recibe un HareAndHounds y una pieza.
    Retorna el un movimiento, consistente de su nombre y el estado que genera.
-}

pieceMoves :: HareAndHounds -> Piece -> [(String,HareAndHounds)]
pieceMoves (HareAndHounds c pcs) (p,x,y,k) = let
    -- Posiciones posibles dependiendo del tipo de pieza
    steps
        | k=='C'  = [(x+2,y+2),(x+2,y-2),(x+2,y),(x,y-2), (x,y+2),(x-2,y), (x-2,y-2), (x-2,y+2)] -- El conejo puede moverse en todas las direcciones
        | k=='S'  = [(x+2,y+2),(x+2,y-2),(x+2,y),(x,y-2), (x,y+2)]
    -- Checkear si es posible moverse a una posición
    isValid (xf,yf) =
        {- Los valores (0,0), (0,4), (8,0), (8,4) no son válidos, pues serían "celdas vacías", por lo que tenemos que checkear que (xf,yf) no esté en la lista conformada
        por esos valores -}
        pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY && isInList (xf,yf) [(0,0), (0,4), (8,0), (8,4)] == False && diagonalRestriction (x,y) (xf,yf)
    -- Generar los posibles movimientos,
    steps2 = filter isValid steps
    in [(moveName (x,y) (xf,yf), HareAndHounds (1-c) (movePiece (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2]

-- Inicialización del juego.

hareAndHoundsIni :: HareAndHounds
hareAndHoundsIni = HareAndHounds 0 [(1,8,2,'C'),(0,0,2,'S'),(0,2,0,'S'),(0,2,4,'S')]

-- Se define como se transforma un HareAndHounds a String.

instance Show HareAndHounds where
    show (HareAndHounds _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'C') -> "🐇"
            Just (_,_,_,'S') -> "🐕"
            Nothing          -> if (x+y) `mod` 2 == 0 then "■ " else "□ "
        in drawBoard (boardX,boardY) draw

{-
    hareAndHoundsEval corresponde a la función de evaluación que usa cpuEval.
    Recibe un estado del juego.
    Retorna la evaluación del estado.
-}

hareAndHoundsEval :: HareAndHounds -> Float
hareAndHoundsEval (HareAndHounds c pcs) = let
    fI = fromIntegral
    hare@(_,fx,fy,_) = head (filter (\(p,_,_,_) -> p == 0) pcs)
    houndsum = sum [if y>=fy then 0.5 else 0.05 * abs (fI x - fI fx) | (p,x,y,k) <- pcs, p==1]
    in (houndsum + 7 - fI fy) * (if c==0 then 1.0 else -1.0)

-- Función que checkea si una tupla con las posiciones (x,y) está en una lista
isInList :: (Eq a) => (a,a) -> [(a,a)] -> Bool
isInList _ [] = False
isInList (t1,t2) (x:xs) = if x == (t1,t2) then True else isInList (t1,t2) xs

-- diagonalRestriction restringe los movimientos posibles de los Hounds y Hare, haciendo que no puedan ir de ciertas celdas a otras.
diagonalRestriction :: (Integral a,Eq a) => (a,a) -> (a,a) -> Bool
diagonalRestriction (xi,yi) (xf,yf) = if filter (==(xi,yi)) list == [] || filter (==(xf,yf)) list == [] then True else False
    where list = [(2,2),(4,0),(6,2),(4,4)]

-- chooseTeam retorna una lista dependiendo del equipo que elige el jugador en consola.
chooseTeam :: String -> String -> [Player HareAndHounds]
chooseTeam decision name
    | decision == "Hounds" = hound_decision
    | decision == "Hare" = hare_decision
    | otherwise = error "No se ha seleccionado una opción válida"
    where hare_decision = [cpuEval "CPU" hareAndHoundsEval, human name]
          hound_decision = [human name, cpuEval "CPU" hareAndHoundsEval]

leftSide :: Int -> [Piece] -> Bool
leftSide _ [] = True
-- Si cualquier Hound está a la izquierda del conejo, entonces retorna False inmediatamente
leftSide x ((_,x2,_,k):xs) = if  k == 'S' && x2 < x then False else leftSide x xs

-- Main.
main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    -- Menú Hare&Hounds
    putStrLn $ "Seed: " ++ show seed
    putStrLn "\t ====== HARE & HOUNDS ======\n\n"
    putStrLn "Hola Jugador!, ingrese su nombre: "
    nombre <- getLine
    putStrLn "\nElija un equipo: \n\t Hare \n\t Hounds\n"
    -- Obtención input usuario y creación de jugadores
    player_decision <- getLine 
    let result = chooseTeam player_decision nombre
    -- Jugar
    execute hareAndHoundsIni result seed


