import Game
import Piece

import System.Random (getStdGen,randoms)


data FoxAndHounds = FoxAndHounds Int [Piece] -- current player and list of pieces

boardX :: Int
boardX = 8
boardY :: Int
boardY = 8

instance Game FoxAndHounds where
    current (FoxAndHounds c _) = c

    winner (FoxAndHounds c pc) mvs
        -- If current player has no moves, the other wins
        | null mvs                            = Just (1 - c)
        -- If player 0 reached y=0, then he wins
        | any (\(p,x,y,k) -> p==0 && y==0) pc = Just 0
        -- Otherwise keep playing
        | otherwise                           = Nothing

    movements st@(FoxAndHounds c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

pieceMoves :: FoxAndHounds -> Piece -> [(String,FoxAndHounds)]
pieceMoves (FoxAndHounds c pcs) (p,x,y,k) = let
    steps
        | k=='H'  = [(x+1,y+1),(x-1,y+1)]
        | k=='F'  = [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)]
    isValid (xf,yf) =
        pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY
    steps2 = filter isValid steps
    in [(moveName (x,y) (xf,yf), FoxAndHounds (1-c) (movePiece (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2]

foxAndHoundsIni :: FoxAndHounds
foxAndHoundsIni = FoxAndHounds 0 [(0,0,7,'F'),(1,1,0,'H'),(1,3,0,'H'),(1,5,0,'H'),(1,7,0,'H')]


instance Show FoxAndHounds where
    show (FoxAndHounds _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'H') -> "🄷 "
            Just (_,_,_,'F') -> "🄵 "
            Nothing          -> if (x+y) `mod` 2 == 0 then "■ " else "□ "
        in drawBoard (boardX,boardY) draw

foxAndHoundsEval :: FoxAndHounds -> Float
foxAndHoundsEval (FoxAndHounds c pcs) = let
    fI = fromIntegral
    fox@(_,fx,fy,_) = head (filter (\(p,_,_,_) -> p == 0) pcs)
    houndsum = sum [if y>=fy then 0.5 else 0.05 * abs (fI x - fI fx) | (p,x,y,k) <- pcs, p==1]
    in (houndsum + 7 - fI fy) * (if c==0 then 1.0 else -1.0)

main :: IO Int
main = do
    gen <- getStdGen
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    let player0 = cpuRand "Son"
    let player1 = cpuEval "Father" foxAndHoundsEval
    execute foxAndHoundsIni [player0,player1] seed


