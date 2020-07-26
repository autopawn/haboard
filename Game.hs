-- Game.hs contiene la lógica general de los juegos.

module Game where

import Data.Maybe (Maybe)
import System.IO
import System.Random (mkStdGen,randoms)

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
    playerType verifica el tipo de jugador ingresado por input y
    entra la función correspondiente.
-}
playerType :: [String] -> Maybe (Player s)
playerType lt 
    | lt !! 0 == "humano"    = Just $ human (lt !! 1)   
    | lt !! 0 == "cpuRandom" = Just $ cpuRand (lt !! 1)
    --  lt !! 0 == "cpuEval"   = Just $ cpuEval (lt !! 1) foxAndHoundsEval  --No sirve
    | otherwise = Nothing

{-
    loser recibe un Int que es el índice del jugador ganador, 
    y entrega el índice del jugador perdedor. 
-}
loser :: Int -> Int
loser x = if x == 0 then 1 else 0

{- 
    configAndExecute es la funcion genérica que le permite al usuario seleccionar
    qué tipo de jugadores van a jugar de entre las opciones disponibles human y cpuEval
    dadas como argumento; y también consulta los nombres que se le darán a los jugadores seleccionados.
    Recibe s que es el estado del juego, debe ser de clase Show y Game.
    Recibe un Int que es la semilla aleatoria.
    Entrega un Int que es el índice del jugador que ganó.
-}
configAndExecute :: (Show s, Game s) => s -> Int -> IO Int
configAndExecute st0 seed0 = do
    putStrLn "Ingrese los usuarios que van a jugar.\nLas opciones son: humano y cpuRandom.\nEl formato de ingreso es: <tipo jugador0> <nombre jugador0> <tipo jugador1> <nombre jugador1>"
    --Input en el formato descrito.
    players0 <- getLine --players0 es una String. 
    let ltplayers = (words players0) --ltplayers es una lista: [<tipo jugador0> , <nombre jugador0> , ...]
    -- Se separa la lista en ambos jugadores.
    let ltplayer0 = [ltplayers !! 0, ltplayers !! 1]
    let ltplayer1 = [ltplayers !! 2, ltplayers !! 3]
    case (playerType ltplayer0) of
        Just player0 -> case (playerType ltplayer1) of
            --En caso que ambos jugadores fueran ingresados correctamente, se llama a la función execute.
            Just player1 -> execute st0 [player0,player1] seed0     
            Nothing -> putStrLn "Ingreso incorrecto jugador 1." >> return (-1) 
        Nothing -> putStrLn "Ingreso incorrecto jugador 0." >> return (-1)
    --Los return son necesarios dado que la función debe entregar un IO Int.
  

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
            let (Player name _) = players !! n -- name es el nombre del jugador ganador.
            putStrLn $ "Jugador "++show n++" "++name++" ganó!"            
            -- obtener el indice del jugador perdedor.
            let loserPlayer = loser n 
            let(Player name2 _) = players !! loserPlayer -- name2 es el nombre del jugador perdedor.            
            --Expliación de pprqué hacer el input aquí en el README.     
            putStrLn "Ingrese el nombre del archivo donde guardará el resultado:  " 
            namefile <- getLine --namefile es el String con el nombre del archivo que el usuario escribió.
            --Agregar línea al archivo con el formato: <nombre_J1> <nombre_J2> <jugador_ganador>
            appendFile namefile $ name++" "++name2++" "++name  
            return n
        Nothing -> do
            -- Continuar jugando
            let c = current st
            let (Player _ pchoice) = players !! c
            (cmd,st2) <- pchoice st moves r
            loop st2 players rs

