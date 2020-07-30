-- Game.hs contiene la lógica general de los juegos.

module Game where

import Data.Maybe (Maybe)
import System.Random (mkStdGen,randoms)
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

{- Se crea un nuevo jugador.-}
new_player_1 ::  (Game s) => String-> String -> (s-> Float) -> String->  Player s
new_player_1 nombre tipo eval name_game
    |tipo == "3" = human nombre
    |tipo == "2" && name_game == "fox and hounds" = cpuEval nombre eval -- evita tomar una funcion cpuEval de five field kono,
    |otherwise = cpuRand nombre                                         -- que existe pero fue copiada de fox and hound solo para que no me lanzara error
                                                                        -- no pude soluciar el problema sin recurrir a ese trucazo...                                    
{- Esta funcion pide el tipo y evita que ingresen un tipo erroneo.-}


get_tipo :: IO String
get_tipo = do
    putStrLn "\n Ingrese tipo de jugador, 1 para cpuRand , 2 para cpuEval (solo en FoxAndHounds) y 3 para humano . \n"
    tipo <- getLine
    if (tipo == "1" || tipo == "2" || tipo == "3")
        then return tipo 
        else get_tipo
{-
    Funcion que nos piden, pide los nombres y tipos por consola y apartir de ello se encarga de generar
    los jugadores, sigue con el llamago a la funcion execute y crea/modifica el archivo historial que nos pidan.
-}
configAndExecute :: (Show s, Game s) => s -> Int -> String -> (s->Float) -> IO Int
configAndExecute s seed name_game eval = do

    putStrLn "\n Ingrese el nombre del jugador 1 \n"
    nombre_1 <- getLine
    tipo_1 <- get_tipo
    putStrLn "\n Ingrese el nombre del jugador 2 \n"
    nombre_2 <- getLine
    tipo_2 <- get_tipo

    let jugador_1 = new_player_1 nombre_1 tipo_1 eval name_game
    let jugador_2 = new_player_1 nombre_2 tipo_2 eval name_game

    let players = [jugador_1,jugador_2]

    {-
        Se ejecuta execute sin problemas y obtenemos el numero del jugador que gano.
    -}
    n <- execute s players seed
    
    let a = n+1

    let (Player name _) = players !! n

    putStrLn "\n Ingrese el nombre del historial donde se guardara:\n"
    n_historial <- getLine
    {-
        Se crea o modifica el historia, si esque este existe o no.
    -}
    appendFile (n_historial++".txt") (nombre_1 ++ "--" ++ nombre_2 ++ ".El jugador " ++ show a ++ " " ++ name ++ " gano en "++ name_game ++ "\n.")  

    return n