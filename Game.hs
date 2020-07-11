module Game where

import Data.Maybe (Maybe)
import System.Random (mkStdGen,randoms)

class Game s where
    current   :: s -> Int
    winner    :: s -> [(String,s)] -> Maybe Int
    movements :: s -> [(String,s)]

data Player s = Player String (s -> [(String,s)] -> Int -> IO (String,s))

------------------------------------------

human :: String -> Player s
human name = let
    askCommand st mvs r = do
        -- print available moves
        print (map fst mvs)
        -- receive input command
        putStrLn "Input:"
        cmd <- getLine
        --
        case lookup cmd mvs of
            Just st2 -> return (cmd,st2)
            Nothing  -> do
                putStrLn "Invalid command!"
                askCommand st mvs r
    in Player name askCommand

cpuRand :: String -> Player s
cpuRand name = let
    pickCommand st mvs r =
        return (mvs !! (r `mod` length mvs))
    in Player name pickCommand


-- indexes of the greatest values
argmaxs :: (Ord a) => [a] -> [Int]
argmaxs xs = [i | (x,i) <- zip xs [0..], x >= maximum xs]

cpuEval :: (Game s) => String -> (s -> Float) -> Player s
cpuEval name eval = let
    pickCommand st mvs r = do
        -- Find which player is me:
        let me = current st
        -- Evaluation function for any state s, negated if the player changes
        let eval2 s = if current s == me then eval s else (-1) * eval s
        -- Indexes of the best moves
        let bestis = argmaxs (map (\(cmd,s) -> eval2 s) mvs)
        -- Pick one of the best moves at random
        let pick = bestis !! (r `mod` length bestis)
        return (mvs !! pick)
    in Player name pickCommand


------------------------------------------

execute :: (Show s, Game s) => s -> [Player s] -> Int -> IO Int
execute st players seed = do
    let gen   = mkStdGen seed
    let rands = randoms gen
    loop st players rands

loop :: (Show s, Game s) => s -> [Player s] -> [Int] -> IO Int
loop st players (r:rs) = do
    print st
    let moves = movements st
    let win = winner st moves
    case win of
        Just n -> do
            -- Finish game
            let (Player name _) = players !! n
            putStrLn $ "Jugador"++show n++" "++name++" ganó!"
            return n
        Nothing -> do
            -- Continue playing
            let c = current st
            let (Player _ pchoice) = players !! c
            (cmd,st2) <- pchoice st moves r
            loop st2 players rs
