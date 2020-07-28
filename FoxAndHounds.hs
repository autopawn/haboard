-- FoxAndHounds.hs contiene la lógica específica del juego Fox and Hounds

import Game
import Piece


import System.Random (getStdGen,randoms)

-- Se crea el tipo FoxAndHounds.

data FoxAndHounds = FoxAndHounds Int [Piece] -- jugador actual y lista de piezas

-- Se define el tamaño del tablero

boardX :: (Integral a) => a
boardX = 8
boardY :: (Integral a) => a
boardY = 8

-- Se hace que FoxAndHounds sea instancia de Game y se definen las funciones necesarias.

instance Game FoxAndHounds where
    current (FoxAndHounds c _) = c

    winner (FoxAndHounds c pc) mvs
        -- Si el jugador no tiene movimientos, gana el otro
        | null mvs                            = Just (1 - c)
        -- Si el jugador 0 (Fox) llega a y = 0, gana
        | any (\(p,x,y,k) -> p==0 && y==0) pc = Just 0
        -- En todo otro caso se sigue jugando
        | otherwise                           = Nothing

    movements st@(FoxAndHounds c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc--p de la lista que se recorre tiene que ser igual al jugador
        in concatMap (pieceMoves st) playerpcs      --de la instancia de FoxAndHounds en la que esta
                                                    --CRea una lista con los movimientos solo del jugador de 
                                                    --esa instancia

{-
    pieceMoves genera los movimientos posibles para los dos tipos de piezas del juego.
    Recibe un FoxAndHounds y una pieza.
    Retorna el un movimiento, consistente de su nombre y el estado que genera.
-}
--recordar que cada estado del juego se va intercalando entre jugadores y la lista de piezas que posee
--el estado son la totalidad de las piezas que se van actualizando 
pieceMoves :: FoxAndHounds -> Piece -> [(String,FoxAndHounds)]
pieceMoves (FoxAndHounds c pcs) (p,x,y,k) = let
    -- Posiciones posibles dependiendo del tipo de pieza
    steps
        | k=='H'  = [(x+1,y+1),(x-1,y+1)]
        | k=='F'  = [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)]
    -- Checkear si es posible moverse a una posición
    --primero checkea si hay una pieza en la posicon (xf,yf) y luego revisa que el movimiento este
    --dentro del rango del tablero 8x8 (0->7)
    isValid (xf,yf) =
        pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY
    -- Generar un lista de los posibles movimientos
    steps2 = filter isValid steps
    --devuelve una lista que contiene tuplas con el movimiento(string) y el estado actualizado del juego
    --el estado se pasa al siguiente jugador con las piezas ya cambiadas(actualizadas)
    in [(moveName (x,y) (xf,yf), FoxAndHounds (1-c) (movePiece (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2]

-- Inicialización del juego.

foxAndHoundsIni :: FoxAndHounds
foxAndHoundsIni = FoxAndHounds 0 [(0,0,7,'F'),(1,1,0,'H'),(1,3,0,'H'),(1,5,0,'H'),(1,7,0,'H')]
--piezas iniciales partiendo con el jugador 0

-- Se define como se transforma un FoxAndHounds a String.

instance Show FoxAndHounds where
    show (FoxAndHounds _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'H') -> "🄷 "
            Just (_,_,_,'F') -> "🄵 "
            Nothing          -> if (x+y) `mod` 2 == 0 then "■ " else "□ "
        in drawBoard (boardX,boardY) draw

{-
    foxAndHoundsEval corresponde a la función de evaluación que usa cpuEval.
    Recibe un estado del juego.
    Retorna la evaluación del estado.
-}



foxAndHoundsEval :: FoxAndHounds -> Float
foxAndHoundsEval (FoxAndHounds c pcs) = let
    fox@(_,xi,yi,_) = head (filter (\(p,_,_,_) -> p == 0) pcs) --filtra solo las pieza del fox
    hounds_p = [(x, y) | (p,x,y,_) <- pcs, p == 1]--filtra para leer solo los hounds
    dist = map (bfs neighbour hounds_p (xi,yi)) [(0,1),(0,3),(0,5),(0,7)]
    in minimum(map (bfs neighbour hounds_p (xi,yi)) [(0,1),(0,3),(0,5),(0,7)]) * (if c==0 then 1.0 else -1.0)     
    --envia la posicion del fox y la lista con las posiciones de los hounds
    --esto lo que hace es tomar el minimo valor de la lista la cual contiene el camino mas corto para cada 
    --posicion terminal dentro del tablero, el proceso se hara para todos los nodos finales posibles



--bfs recibe una funcion que encuentra a los vecinos (hijos) del nodo, la posicion de los hounds, la posicion de fox(nodo inicial)
-- y la posicion del nodo final o meta; 
--retorna la distancia desde fox a la meta
--cabe mencionar que cuando llama a bfs', la tupla que envia contiene un Int que se usara para saber la profundidad del grafo
bfs :: (Integral a, Ord a) => ((a,a,a)->[(a,a)]->[(a,a,a)]->[(a,a,a)]->[(a,a,a)]) -> [(a,a)]->(a,a)->(a,a)->Float
bfs neighbour hounds_p (xi,yi) (xf,yf) = fromIntegral (bfs' neighbour hounds_p ([(xi,yi,0)],[],(xf,yf)))



--bfs' es una modificacion que recibe la funcion para encontrar los vecinos (hijos), la posicion de los hounds,
--tambien recibe una tupla con ([current],[children],(meta))
--devuelve la distancia desde fox hasta la meta
bfs' :: (Integral a, Ord a) => ((a,a,a)->[(a,a)]->[(a,a,a)]->[(a,a,a)]->[(a,a,a)]) -> [(a,a)] ->([(a,a,a)],[(a,a,a)],(a,a)) -> a
bfs' neighbour hounds_p ([], children, final) = bfs' neighbour hounds_p (children,[],final)
bfs' neighbour hounds_p ((xi,yi,n):current, children,(xf,yf)) =
    if xi == xf && yi == yf || null current && null children then n
    else bfs' neighbour hounds_p (current, children ++ (neighbour (xi,yi,n) hounds_p children current), (xf,yf))


--nota: en el apartado de la issue no consideraron que puede que se quede sin espacios el fox para moverse, por ello,
--lo que hice fue simplemente entregar el ultimo resultado de n (profundidad)


--recibe el nodo inicial(partida), una lista con la posicion de los hounds, los hijos y los nodos en la profundidad actual(current)
--devuelve los vecinos(hijos) de dicho nodo en una lista de tuplas tipo [(x,y,n)] con n profundidad
--La funcion comprueba que:
--1) los movimientos posibles (diagonales) esten dentro del tablero
--2) Los movimientos no se encuentran con hounds
--3) Los movimientos no estan ya en las listas anteriores
neighbour :: (Integral a, Ord a) => (a,a,a)->[(a,a)]->[(a,a,a)]->[(a,a,a)]->[(a,a,a)]
neighbour (xi,yi,n) hounds_p children current = let
    moves = [(movX, movY,n) | (movX,movY)<-map(\(x,y)->(x+xi,y+yi)) [(1,1),(-1,1),(1,-1),(-1,-1)], movX >= 0 && movX < boardX && movY >= 0 && movY < boardY]
    nodes = [(px,py,n+1) | (px,py,n) <- moves, (px,py,n) `notElem` children, (px,py,n) `notElem` current, (px,py) `notElem` hounds_p]
    in if null nodes then [] else nodes



    




-- Main.

main :: IO Int
main = do
    -- Inicialización del generador de números aleatorios
    gen <- getStdGen
    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    -- Crear jugadores
    let player0 = cpuEval "Son" foxAndHoundsEval     
    let player1 = cpuRand "Father"    
    -- Jugar
    execute foxAndHoundsIni [player0,player1] seed


