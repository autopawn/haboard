-- HareAndHounds.hs contiene la lógica específica del juego Hare and Hounds

import Game
import Piece

import System.Random (getStdGen,randoms)

-- Se crea el tipo Hare And Hounds

data HareAndHounds = HareAndHounds Int [Piece] -- jugador actual y lista de piezas

-- Se define el tamaño del tablero

boardX :: Int
boardX = 5
boardY :: Int
boardY = 3

-- Se hace que FoxAndHounds sea instancia de Game y se definen las funciones necesarias.

instance Game HareAndHounds where
    current (HareAndHounds c _) = c

    winner (HareAndHounds c pc) mvs
        -- Si el jugador no tiene movimientos, gana el otro
        | null mvs                            = Just (1 - c)
        -- Si Hare (p=1) llega a (1,1) , gana
        | any (\(p,x,y,k) -> p==1 && x == 0 && y == 1) pc = Just 1
        -- En todo otro caso se sigue jugando
        | otherwise                           = Nothing

    movements st@(HareAndHounds c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

{-
    pieceMoves genera los movimientos posibles para los dos tipos de piezas del juego.
    Recibe un FoxAndHounds y una pieza.
    Retorna el un movimiento, consistente de su nombre y el estado que genera.
-}

pieceMoves :: HareAndHounds -> Piece -> [(String,HareAndHounds)]
pieceMoves (HareAndHounds c pcs) (p,x,y,k) = let
    -- Posiciones posibles dependiendo del tipo de pieza
    steps
        | k=='C'  = [(x+1,y+1),(x+1,y-1),(x+1,y),(x,y-1), (x,y+1),(x-1,y), (x-1,y-1), (x-1,y+1)] -- El conejo puede moverse en todas las direcciones
        | k=='S'  = [(x+1,y+1),(x+1,y-1),(x+1,y),(x,y-1), (x,y+1)]
    -- Checkear si es posible moverse a una posición
    isValid (xf,yf) =
        {- Los valores (0,0), (0,2), (4,0), (4,2) no son válidos, pues serían "celdas vacías", por lo que tenemos que checkear que (xf,yf) no esté en la lista conformada
        por esos valores -}
        pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY && isInList (xf,yf) [(0,0), (0,2), (4,0), (4,2)] == False && diagonalCell (x,y) (xf,yf) == False
    -- Generar los posibles movimientos,
    steps2 = filter isValid steps
    in [(moveName (x,y) (xf,yf), HareAndHounds (1-c) (movePiece (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2]

-- Inicialización del juego.

hareAndHoundsIni :: HareAndHounds
hareAndHoundsIni = HareAndHounds 0 [(1,4,1,'C'),(0,0,1,'S'),(0,1,0,'S'),(0,1,2,'S')]

-- Se define como se transforma un FoxAndHounds a String.

instance Show HareAndHounds where
    show (HareAndHounds _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'C') -> "🐇"
            Just (_,_,_,'S') -> "🐕"
            Nothing          -> if (x+y) `mod` 2 == 0 then "■ " else "□ "
        in drawBoard (boardX,boardY) draw

{-
    foxAndHoundsEval corresponde a la función de evaluación que usa cpuEval.
    Recibe un estado del juego.
    Retorna la evaluación del estado.
-}

hareAndHoundsEval :: HareAndHounds -> Float
hareAndHoundsEval (HareAndHounds c pcs) = let
    fI = fromIntegral
    fox@(_,fx,fy,_) = head (filter (\(p,_,_,_) -> p == 0) pcs)
    houndsum = sum [if y>=fy then 0.5 else 0.05 * abs (fI x - fI fx) | (p,x,y,k) <- pcs, p==1]
    in (houndsum + 7 - fI fy) * (if c==0 then 1.0 else -1.0)


-- Función que checkea si una tupla con las posiciones (x,y) está en una lista
isInList :: (Eq a) => (a,a) -> [(a,a)] -> Bool
isInList _ [] = False
isInList (t1,t2) (x:xs) = if x == (t1,t2) then True else isInList (t1,t2) xs

-- diagonalCell restringe los movimientos posibles de los Hounds y Hare, haciendo que no pueda ir de una celda par a otra par.
diagonalCell :: (Integral a,Eq a) => (a,a) -> (a,a) -> Bool
diagonalCell (xi,yi) (xf,yf) = 
    let suma_inicial = xi+yi
        suma_final = xf+yf
    in suma_inicial `mod` 2 == 0 && suma_final `mod` 2 == 0

-- chooseTeam retorna una lista dependiendo del equipo que elige el jugador en consola.
chooseTeam :: String -> String -> [Player HareAndHounds]
chooseTeam decision name
    | decision == "Hounds" = list1
    | decision == "Hare" = list2
    | otherwise = error "No se ha seleccionado una opción válida"
    where list2 = [cpuEval "CPU" hareAndHoundsEval, human name]
          list1 = [human name, cpuEval "CPU" hareAndHoundsEval]
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


