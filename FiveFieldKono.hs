import Game
import Piece

import System.Random (getStdGen,randoms)


data FiveFieldKono = FiveFieldKono Int [Piece]

-- (actually more than five if you want to change the board size)
boardX :: Int
boardX = 5
boardY :: Int
boardY = 5

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

instance Game FiveFieldKono where
    current (FiveFieldKono c _) = c

    winner (FiveFieldKono c pc) mvs
        -- If current player has no moves, the other wins
        | null mvs = Just (1 - c)
        -- If all player0 pieces are on player1 starting positions, he wins
        | all (\(p,x,y,k) -> inStartPos (x,y) == 1) (playerPieces pc 0) = Just 0
        -- If all player1 pieces are on player0 starting positions, he wins
        | all (\(p,x,y,k) -> inStartPos (x,y) == 0) (playerPieces pc 1) = Just 1
        -- Otherwise keep playing
        | otherwise = Nothing

    movements st@(FiveFieldKono c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

pieceMoves :: FiveFieldKono -> Piece -> [(String,FiveFieldKono)]
pieceMoves (FiveFieldKono c pcs) (p,x,y,k) = let
    steps = [(x-1,y-1),(x-1,y+1),(x+1,y-1),(x+1,y+1)]
    isValid (xf,yf) =
        pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY
    steps2 = filter isValid steps
    in [(moveName (x,y) (xf,yf), FiveFieldKono (1-c) (movePiece (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2]

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

fiveFieldKonoIni :: FiveFieldKono
fiveFieldKonoIni = FiveFieldKono 0 [(p,x,y,'K') |
    x<-[0..boardX-1], y<-[0..boardY-1], let p = inStartPos (x,y), p>=0]

main :: IO Int
main = do
    gen <- getStdGen
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    let player0 = cpuRand "Walter White"
    let player1 = cpuRand "Jack Black"
    execute fiveFieldKonoIni [player0,player1] seed
