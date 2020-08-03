import Game
import Piece
import Data.List
import System.Random (getStdGen,randoms)

data Damas = Damas Int [Piece]

boardX :: Int
boardX = 8
boardY :: Int
boardY = 8

instance Game Damas where
    current (Damas c _) = c
    winner (Damas c pc) mvs
        | checkPieces c pc = Just (1-c)
        | checkPieces (1-c) pc = Just c
        | null mvs = Just (1-c)
        | otherwise = Nothing
    movements st@(Damas c pc) = let
        playerpcs = filter (\(p,x,y,k) -> p == c) pc
        in concatMap (pieceMoves st) playerpcs

checkPieces :: Int -> [Piece] -> Bool
checkPieces c xs = not (any (\(x,_,_,_) -> x == c) xs)

instance Show Damas where
    show (Damas _ pcs) = let
        draw (x,y) = case pieceAt (x,y) pcs of
            Just (_,_,_,'b') -> "b "
            Just (_,_,_,'w') -> "w "
            Just (_,_,_,'B') -> "B "
            Just (_,_,_,'W') -> "W "
            Nothing          -> if (x+y) `mod` 2 == 0 then "■ " else "□ "
        in drawBoard (boardX,boardY) draw

damasIni :: Damas
damasIni = let
    list0 = [(0,x,y,'w') | x <- [0..7], y <- [5..7], if even x then odd y else even y]
    list1 = [(1,x,y,'b') | x <- [0..7], y <- [0..2], if even x then odd y else even y]
    in Damas 0 (list0 ++ list1)

pieceMoves :: Damas -> Piece -> [(String,Damas)]
pieceMoves st@(Damas c pcs) pc@(p,x,y,k) = let
    eat = listEat pc st
    steps
        | length eat /= 0 = []
        | k == 'B' || k == 'W' =
            [(x+(if a>0 then a else a*(-1)),y+a) | a <- [-7..7]]++[(x+(if a>0 then a*(-1) else a),y+a) | a <- [-7..7]]
        | p == 0 = [(x+1,y-1),(x-1,y-1)]
        | p == 1 = [(x+1,y+1),(x-1,y+1)]
    isValid (xf,yf) =
        pieceAt (xf,yf) pcs == Nothing && 0 <= xf && xf < boardX && 0 <= yf && yf < boardY
    steps2 = filter isValid steps
    isValidEat (xf,yf,xd,yd) = isValid (xf,yf)
    eat2 = filter isValidEat eat
    getType mb = case mb of
        Just (_,_,_,t) ->t
        Nothing -> ' '
    deletePiece (xr,yr) pcs = let
        t = getType (pieceAt (xr,yr) pcs)
        in delete ((1-p),xr,yr,t) pcs
    in if length steps2 /= 0
        then [(moveName (x,y) (xf,yf), Damas (1-c) (checkQAndMove (x,y) (xf,yf) pcs)) | (xf,yf) <- steps2]
        else [(moveName (x,y) (xf,yf), Damas (1-c) (checkQAndMove (x,y) (xf,yf) (deletePiece (xd,yd) pcs))) | (xf,yf,xd,yd) <- eat2]


--movesQueen :: String -> Damas -> (Int,Int) -> [(Int,Int)]

checkQAndMove :: (Int,Int) -> (Int,Int) -> [Piece] -> [Piece]
checkQAndMove (xi,yi) (xf,yf) pcs = let
    pcs1 = movePiece (xi,yi) (xf,yf) pcs
    isQueen (p,x,y,k)
        | k == 'W' || k == 'B' = k
        | p == 1 && elem (x,y) (zip [0,2..7] (repeat 7)) = 'B'
        | p == 0 && elem (x,y) (zip [1,3..7] (repeat 0)) = 'W'
        | otherwise = k
    in [(p,x,y,isQueen (p,x,y,k)) | (p,x,y,k) <- pcs1]

listEat :: Piece -> Damas -> [(Int,Int,Int,Int)]
listEat pc@(p,x,y,k) st@(Damas c pcs) = let
    --stepsQueens =
    stepRigth
        | p == 0 && isEnemy st pc (x+1,y-1) && pieceAt (x+2,y-2) pcs == Nothing = [(x+2,y-2,x+1,y-1)]
        | p == 1 && isEnemy st pc (x+1,y+1) && pieceAt (x+2,y+2) pcs == Nothing = [(x+2,y+2,x+1,y+1)]
        | otherwise = []

    stepLeft
        | p == 0 && isEnemy st pc (x-1,y-1) && pieceAt (x-2,y-2) pcs == Nothing = [(x-2,y-2,x-1,y-1)]
        | p == 1 && isEnemy st pc (x-1,y+1) && pieceAt (x-2,y+2) pcs == Nothing = [(x-2,y+2,x-1,y+1)]
        | otherwise = []
    steps
        | null stepRigth && null stepLeft = []
        | null stepRigth = stepLeft
        | null stepLeft = stepRigth
        | otherwise = stepRigth++stepLeft
    in steps

isEnemy :: Damas -> Piece -> (Int,Int) -> Bool
isEnemy (Damas c pcs) (p1,_,_,_) (xi,yi) = let
    p2 = case pieceAt (xi,yi) pcs of
        Just (0,_,_,_) -> 0
        Just (1,_,_,_) -> 1
        Nothing -> -1
    in if p2 == -1 then False else p1 /= p2

main :: IO Int
main = do
    gen <- getStdGen
    let seed = head (randoms gen)
    putStrLn $ "Seed: " ++ show seed
    let player0 = cpuRand "Player1"
    let player1 = cpuRand "cpu"
    execute damasIni [player0,player1] seed
