-- Game.hs contiene la lógica general de los juegos.

module Game where

import Data.Maybe (Maybe)
import System.Random (mkStdGen,randoms)
import System.IO
import System.Directory

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

{-
    configAndExecute es la función que corre el juego.
    Requiere que s (el estado del juego) sea de clase Show y Game.
    Recibe un Game, una lista de jugadores, un Int que funciona como semilla aleatoria
    y un String que permite idenficar que juego se está corriendo.
    Retorna un IO Int que corresponde al índice del jugador ganador.

    Permite el preguntar los nombres de los jugadores y el archivo historial.
    Además, por medio de otras funciones permite obtener de que tipo es el juego. Con
    este tipo y los nombres permite definir a los player0 y player1 y por ende players.
-}

configAndExecute :: (Show s, Game s) => s -> Int -> String -> IO Int
configAndExecute st seed nombre_juego = do
    -- Generar una lista de números aleatorios
    let gen   = mkStdGen seed
    let rands = randoms gen
    -- Solicitar datos de jugadores y de archivo historial
    putStrLn $ "Digite nombre del jugador 0"
    nombre0 <- getLine
    tipo0 <- pedir_tipo nombre_juego

    putStrLn $ "Digite nombre del jugador 1"
    nombre1 <- getLine
    tipo1 <- pedir_tipo nombre_juego

    let player0 = hacer_player tipo0 nombre0
    let player1 = hacer_player tipo1 nombre1

    let players = [player0,player1]

    putStrLn $ "Digite el nombre del archivo historial (Ejemplo: historial.txt)"
    historial <- getLine
    --Ejecutar el loop del juego
    loop2 st players rands nombre0 nombre1 historial

{-
    pedir_tipo es la funcion que gestiona a pedir_tipo_fox
    y pedir_tipo_five.
    Requiere de un String correspondiente al nombre del juego.
    Dependiendo del nombre del juego llamará a una de las funciones
    nombradas anteriormente.
-}

pedir_tipo :: String -> IO String
pedir_tipo nombreJuego
    |nombreJuego == "FoxAndHounds" = do
        pedir_tipo_fox
    |nombreJuego == "FiveFieldKono" = do
        pedir_tipo_five

{-
    pedir_tipo_fox es la funcion que pregunta el tipo del jugador
    para FonAndHounds, dando 3 opciones. En caso contrario entra
    en una recursión hasta que se digite uno de esos tipos.
    Retorna un IO String que corresponde al tipo del jugador.
-}

pedir_tipo_fox :: IO String
pedir_tipo_fox = do
    putStrLn $ "Digite el tipo del jugador (human, cpuRand o cpuEval)"
    tipo <- getLine
    if (tipo ==  "human" || tipo == "cpuRand" || tipo == "cpuEval")
        then return tipo
        else pedir_tipo_fox

{-
    pedir_tipo_five es la funcion que pregunta el tipo del jugador
    para FiveFieldKono, dando 2 opciones. En caso contrario entra
    en una recursión hasta que se digite uno de esos tipos.
    Retorna un IO String que corresponde al tipo del jugador.
-}

pedir_tipo_five :: IO String
pedir_tipo_five = do
    putStrLn $ "Digite el tipo del jugador (human o cpuRand)"
    tipo <- getLine
    if (tipo ==  "human" || tipo == "cpuRand")
        then return tipo
        else pedir_tipo_five

{-
    hacer_player es la función que crea al jugador.
    Requiere 2 String, uno correspondiente al tipo del jugador y otro
    correspondienteval nombre del jugador. Dependiendo del tipo, define
    al jugador.
    Retorna un Player s que corresponde al jugador. 
-}

hacer_player :: String -> String -> Player s
hacer_player tipo nombre
    |tipo == "human" = human nombre
    |tipo == "cpuRand" = cpuRand nombre
    |tipo == "cpuEval" = cpuRand nombre -- esto es mejor a nada
    -- |tipo == "cpuEval" = cpuEval nombre foxAndHoundEval

{-
    Se intentó que configAndExecute recibiera un Maybe (s -> Float), en el caso
    de FoxAndHounds se llamaría como:
    configAndExecute foxAndHoundsIni seed "FoxAndHounds" foxAndHoundsEval
    y en el caso de FiveFieldKono se llamaría como: 
    configAndExecute fiveFieldKonoIni seed "FiveFieldKono" Nothing

    De este modo al definir player0 y player1 se agregaría al llamado el Maybe (s -> Float)
    y la función hacer_player sería:

    hacer_player :: String -> String -> Maybe (s -> Float) -> Player s
    hacer_player st tipo nombre eval =
        case eval of
            Just foxAndHoundEval
                |tipo == "human" -> human nombre
                |tipo == "cpuRand" -> cpuRand nombre
                |tipo == "cpuEval" -> cpuEval nombre foxAndHoundEval
            Nothing
                |tipo == "human" -> human nombre
                |tipo == "cpuRand" -> cpuRand nombre


    Sin embargo no funcionó, por lo que se optó por usar cpuRand en el caso en que se llame
    a cpuEval para foxAndHounds, que es mejor a simplemente eliminar la opción de cpuEval
-}

{-
    loop2 es la función correspondiente al loop del juego, es similar a
    loop, pero esta además requiere 2 String correspondientes a los nombres
    de los jugadores y un FilePath correspondiente al nombre del archivo
    en que se guarda el historial de juegos.
    Retorna un Player s que corresponde al jugador. 
-}

loop2 :: (Show s, Game s) => s -> [Player s] -> [Int] -> String -> String -> FilePath -> IO Int
loop2 st players (r:rs) name0 name1 record = do
    print st
    let moves = movements st
    -- Comprobar si existe ganador
    let win = winner st moves
    case win of
        Just n -> do
            -- Terminar el juego
            let (Player name _) = players !! n
            putStrLn $ "Jugador"++show n++" "++name++" ganó!"
            --putStrLn $ name0 ++ " " ++ name1 ++ " " ++ name
            escribir_historial name0 name1 name record
            return n
        Nothing -> do
            -- Continuar jugando
            let c = current st
            let (Player _ pchoice) = players !! c
            (cmd,st2) <- pchoice st moves r
            loop2 st2 players rs name0 name1 record

{-
    escribir_historial es la función que escribe el historial
    Requiere 3 String, los 2 primeros corresponden a los nombres de
    los jugadores y el tercerocorresponde al nombre del ganador.
    Además, recibe un FilePath que corresponde al nombre del archivo
    en que se escribe el historial. (Notar que FilePath también es un
    String pero se diferenció para que quedara mas claro lo del historial)

    Retorna un IO (). 
-}

escribir_historial :: String -> String -> String -> FilePath -> IO ()
escribir_historial name0 name1 champion record = do
    fileExist <- doesFileExist record
    if fileExist
        then
            appendFile record (name0 ++ " " ++ name1 ++ " " ++ champion ++ "\n")
        else
            writeFile record (name0 ++ " " ++ name1 ++ " " ++ champion ++ "\n")