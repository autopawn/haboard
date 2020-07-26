-- Game.hs contiene la lógica general de los juegos.

module Game where

import Data.Maybe (Maybe)
import System.Random (mkStdGen,randoms,getStdGen)
import System.IO

{- 
    Se define la clase Game, para que algo sea instancia de Game
    debe tener definidas las siguientes funciones:
    - current que toma un Game y devuelve un Int (usualmente se refiere al jugador)
    - winner que toma un Game y una lista de movimientos y retorna un Just Int si 
      encuentra ganador o Nothing en caso contrario
    - movements que retorna una lista de movimientos
-}

class Game s where
    current   :: s -> Int
    winner    :: s -> [(String,s)] -> Maybe Int
    movements :: s -> [(String,s)]


{-
    Se define el tipo Player que requiere un String y una función para seleccionar 
    movimientos.
    La función toma el estado de un Game, una lista de movimientos, un Int y retorna
    un movimiento.
-}

data Player s = Player String (s -> [(String,s)] -> Int -> IO (String,s))

------------------------------------------

{-
    human corresponde al jugador humano.
    Su función para elegir movimientos pregunta por línea de comandos cual será el
    siguiente movimiento y comprueba su validez.
-}

human :: String -> Player s
human name = let
    askCommand st mvs r = do
        -- Imprimir movimientos disponibles
        print (map fst mvs)
        -- Recibir el comando
        putStrLn "Input:"
        cmd <- getLine
        -- Comprobar validez del comando y retornarlo de ser válido
        case lookup cmd mvs of
            Just st2 -> return (cmd,st2)
            Nothing  -> do
                putStrLn "Invalid command!"
                askCommand st mvs r
    in Player name askCommand

{-
    cpuRand corresponde a una CPU que elige un movimiento al azar de los movimientos 
    posibles de la CPU.
-}

cpuRand :: String -> Player s
cpuRand name = let
    pickCommand st mvs r =
        return (mvs !! (r `mod` length mvs))
    in Player name pickCommand

{-
    cpuEval es una CPU que usa una función de evaluación para encontrar un mejor
    movimiento que uno random.
    Requiere que además del String con el nombre, se le pase la función de evaluación
-}

-- Índices de los valores más grandes
argmaxs :: (Ord a) => [a] -> [Int]
argmaxs xs = [i | (x,i) <- zip xs [0..], x >= maximum xs]

cpuEval :: (Game s) => String -> (s -> Float) -> Player s
cpuEval name eval = let
    pickCommand st mvs r = do
        -- Encontrar que jugador soy
        let me = current st
        -- Función de evaluación para cualquier estado s, negada si cambia el jugador
        let eval2 s = if current s == me then eval s else (-1) * eval s
        -- Índices de los mejores movimientos
        let bestis = argmaxs (map (\(cmd,s) -> eval2 s) mvs)
        -- Elegir aleatoriamente uno de los mejores movimientos
        let pick = bestis !! (r `mod` length bestis)
        return (mvs !! pick)
    in Player name pickCommand


------------------------------------------


{-
    execute es la función que corre el juego.
    Requiere que s (el estado del juego) sea de clase Show y Game.
    Recibe un Game, una lista de jugadores y un Int que funciona como semilla aleatoria.
    Retorna un IO Int que corresponde al índice del jugador ganador
-}


configAndExecute :: (Show s, Game s) => String -> s -> IO Int
configAndExecute namegame s = do

    -- Inicialización del generador de números aleatorios
    gen <- getStdGen

    -- Semilla aleatoria que se usará para el juego
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed

    -- Crear jugadores Ingresando el nombre del jugador 1
    putStrLn "\nIngrese el nombre del jugador 1: "
    juga1 <- getLine
    
    --Selecciona el modo que sera el jugador, ya sea humano o del computador
    putStrLn "\nIngrese el modo del jugador 1: \n(1)cpuRand \n(2)human"
    modo1 <- getLine
    let player0 = if(modo1 == "1") 
                    then (cpuRand juga1) 
                    else (human juga1)

    -- Crear jugadores Ingresando el nombre del jugador 2
    putStrLn "\nIngrese el nombre del jugador 2: "
    juga2 <- getLine

    --Selecciona el modo que sera el jugador, ya sea humano o del computador
    putStrLn "\nIngrese el modo del jugador 1: \n(1)cpuRand \n(2)human"
    modo1 <- getLine
    let player1 = if(modo1 == "1") 
                    then (cpuRand juga2) 
                    else (human juga2)

    -- Hacemos un asignacion llamando a la funcion execute
    sum <- execute s [player0,player1] seed

    -- Agregamos la partida al final de un archivo (si no existe se crea)
    appendFile ("history - "++ namegame ++".txt") (juga1++" "++juga2++" "++show sum++"\n")
    return $ sum



execute :: (Show s, Game s) => s -> [Player s] -> Int -> IO Int
execute st players seed = do
    -- Generar una lista de números aleatorios
    let gen   = mkStdGen seed
    let rands = randoms gen
    -- Ejecutar el loop del juego
    loop st players rands

loop :: (Show s, Game s) => s -> [Player s] -> [Int] -> IO Int
loop st players (r:rs) = do
    print st
    let moves = movements st
    -- Comprobar si existe ganador
    let win = winner st moves
    case win of
        Just n -> do
            -- Terminar el juego
            let (Player name _) = players !! n
            putStrLn $ "Jugador"++show n++" "++name++" ganó!"
            return n
        Nothing -> do
            -- Continuar jugando
            let c = current st
            let (Player _ pchoice) = players !! c
            (cmd,st2) <- pchoice st moves r
            loop st2 players rs