module Move where
import Data
import Util
--import Board
import Data.Char 
import Data.List
import Data.Maybe
import Data.Either 


otherState :: PlayerState -> PlayerState
otherState state | state == PWhite = PBlack
                 | state == PBlack = PWhite

-- Get a list for all moves in a column/row compared to c
createMoveList :: Int -> [Int]
createMoveList c = (map (\x -> x - c) ([0..(c-1)] ++ [(c+1)..8]))

kingMoves :: PieceMoves
kingMoves = [(\(file, rank) -> (file + x, rank + y) ) | x <- [-1,0,1], y <- [-1, 0, 1]]


-- For Rooks, Bishops and the Queen, walkthrough the moves and if there is a friendly piece skip all further moves
-- possible idea, take 4 
-- walkThroughMoves :: 
-- walkThroughMoves = 
-- isMovableLine :: PlayerState -> ICoordinate -> ICoordinate -> ICoordinate
-- isMovableLine pstate src trgt = 

isRelated :: PieceMove -> PieceMove -> Bool
isRelated m v = if signum x1 == signum x2 && signum y1 == signum y2 then True else False  
            where (x1,y1) = m (0,0)
                  (x2,y2) = v (0,0)

movesToLines :: PieceMoves -> [PieceMoves]
movesToLines m = groupBy (\x y -> isRelated x y) (sort m)

-- Takes a line (from the bishop or rook) and returns the line with only possible moves
-- if sqr is enemy then [m] is last element 
isLineMovable :: PlayerState -> ICoordinate -> PieceMoves -> Board -> PieceMoves
isLineMovable state coor [] b = []
--isLineMovable state coor (_:m) b = if (isMovable state (m coor) b) then [m] else []
isLineMovable state coor (m:ms) b = if movable then if isEnemy then [m] 
                                                    else [m] ++ (isLineMovable state coor ms b)
                                    else []
                  where movable = isMovable state (m coor) b  
                        isEnemy = cmpSquareState (otherState state) (getSquareI coor b)

-- Filters all impossible moves
filterMoves :: Piece -> PieceMoves -> PlayerState -> ICoordinate -> Board -> PieceMoves
filterMoves Knight moves pstate coor board = filter (\x -> (isMovable pstate (x coor) board)) moves
filterMoves Rook moves pstate coor board   = concat (map (\x -> isLineMovable pstate coor x board) (movesToLines moves))
filterMoves Bishop moves pstate coor board = concat (map (\x -> isLineMovable pstate coor x board) (movesToLines moves)) 
filterMoves King moves pstate coor board   = filter (\x -> (isMovable pstate (x coor) board)) moves 

isMovable :: PlayerState -> ICoordinate -> Board -> Bool
isMovable pstate coor board = case sqr of Left err -> False 
                                          Right sqr -> if isFriendly then False else True 
      where sqr = getSquareI coor board
            isFriendly = cmpSquareState pstate (getSquareI coor board)

knightMoves :: ICoordinate -> PieceMoves
knightMoves (c, i) = concat [[\(file,rank) -> (file + x, rank + y)] ++ [\(file,rank) -> (file - x,rank + y)] 
                          ++ [\(file,rank) -> (file + x,rank - y)] ++ [\(file,rank) -> (file - x,rank - y)] | x <- [1,2], y <- [2,1], x /= y]
-- Todo: instead of rmdups, use guards in list comprehension
rookMoves :: ICoordinate -> PieceMoves
rookMoves (c, i) =(rmdups (concat [( [\(file, rank) -> (file + x, rank)] ++ [\(file, rank) -> (file, rank + y)] ) | 
                        x <- [x | x <- (createMoveList c) ], 
                        y <- [p | p <- (createMoveList i) ] ]))
    
bishopSort :: PieceMove -> PieceMove -> Ordering
bishopSort x f = compare ((fst l)+(snd l)) ((fst r)+(snd r))
            where l = x(0,0)
                  r = f(0,0)
bishopMoves :: ICoordinate -> PieceMoves
bishopMoves (c, i) =  sortBy bishopSort (concat [( [\(file, rank) -> (file + x, rank + x)] ++ [\(file, rank) -> (file + x, rank - x)] ) | 
                        x <- [x | x <- createMoveList c]])

queenMoves :: ICoordinate -> PieceMoves
queenMoves n = (rookMoves n) ++ (bishopMoves n)

cmpSquareState :: PlayerState -> Either Err Square -> Bool
cmpSquareState PBlack (Right (Black _)) = True
cmpSquareState PWhite (Right (White _)) = True
cmpSquareState _ _ = False 

-- todo: Generalize capLeft & capRight 
capLeft :: PlayerState -> ICoordinate -> Int -> Board -> PieceMoves
capLeft p (c,i) dir b  = if (cmpSquareState (otherState p) (getSquareI(c-dir,i+dir) b)) then [\(f,r) -> (f-dir,i+dir)] else [] 

capRight :: PlayerState -> ICoordinate -> Int -> Board -> PieceMoves
capRight p (c,i) dir b  = if (cmpSquareState (otherState p) (getSquareI(c+dir,i+dir) b)) then [\(f,r) -> (f+dir,i+dir)] else []

moveDouble :: ICoordinate -> Int -> PieceMoves
moveDouble (c,i) dir = if (dir == 1 && i == 1) || (dir == -1 && i == 6) then [\(f,r) -> (f,i+(2*dir))] else []

empty :: Either Err Square -> Bool
empty (Right Empty) = True
empty _     = False

pawnMoves :: PlayerState -> ICoordinate -> Board -> PieceMoves
pawnMoves p (c, i) b = (capLeft p (c,i) dir b ) ++ (capRight p (c,i) dir b) ++ (moveDouble (c,i) dir) ++ mov
            where isPlayer = cmpSquareState p (getSquareI (c,i) b)
                  dir = if isPlayer then (-1) else 1  -- Direction
                  mov = if (empty (getSquareI(c,i+dir) b)) then [\(f,r) -> (f,r+dir)] else []

-- tryMove :: (Coordinate -> Coordinate) -> 

-- isValid :: SquarePiece -> Coordinate -> Bool
-- isValid (p (sf, st)) (tf,tt) = case p of
--                                 | p == Rook = tryMove 

strToCoor :: Char -> Char -> Either Err Coordinate
strToCoor f r | file == Nothing || rank == Nothing = Left (InvalidErr "Did not find file or rank")
              | otherwise = Right (fromJust file, fromJust rank)
                        where rank = find(\x -> x == ((ord (toLower r))-48)) [0..8]
                              file = find (\x -> x == (toLower f)) ['a'..'h']

strToMove :: String -> Either Err Move
strToMove (s:tr) = if len == 3 then case coor1 of Left err   -> Left err
                                                  Right coor -> case piece of Left err -> Left err
                                                                              Right p -> Right (Move p coor)
                   else if len == 2 then case coor2 of Left err   -> Left err
                                                       Right coor -> Right (Move Pawn coor)
                        else Left (SyntaxErr "Expected 2 or 3 characters")
                  where len = length (s:tr)
                        coor1 = strToCoor (tr !! 0) (tr !! 1)
                        coor2 = strToCoor s (tr !! 0)
                        piece = getPiece s
                 
move :: PlayerState -> String -> Board -> Either Err Board
move pstate str board = case target of Left err -> Left err
                                       Right rtarget -> let source = findSource pstate rtarget board in 
                                                        case source of Left err -> Left err
                                                                       Right r  -> Right (moveTo r rtarget board)
                  where target = (strToMove str)
                        

coordinate :: Move -> Coordinate
coordinate (Move _ (c,i)) = (c,i)
-- move pstate str board = case source of
--                 Just t -> moveTo source target board
--                 Nothing -> Nothing 
--                 where source = findSource m board


moveTo :: Coordinate -> Move -> Board -> Board
moveTo (c, i) (Move p (c2, i2)) b | target == Empty   = replace (charToIndex c, intToIndex i) Empty (replace (charToIndex c2, intToIndex i2) piece b)
                                  | otherwise         = replace (charToIndex c, intToIndex i) Empty (replace (charToIndex c2, intToIndex i2) piece b)
                    where piece  = getSquare (c, i) b
                          target = getSquare(c2, i2) b


getMoves :: PlayerState -> SquarePiece -> Board -> [(ICoordinate -> ICoordinate)]
getMoves pstate (p, coor) b = case p of  King   -> filterMoves King   (kingMoves) pstate icoor b
                                         Knight -> filterMoves Knight (knightMoves (toIcoor coor)) pstate icoor b
                                         Queen  -> (getMoves pstate (p,coor) b) ++ (getMoves pstate (p,coor) b)
                                         Rook   -> filterMoves Rook   (rookMoves   (toIcoor coor)) pstate icoor b
                                         Bishop -> filterMoves Bishop (bishopMoves (toIcoor coor)) pstate icoor b 
                                         Pawn   -> pawnMoves pstate (toIcoor coor) b
                                    where icoor = (toIcoor coor)
-- foldr?
-- isBetween shoulde be 1 8
movesToICoors :: ICoordinate -> PieceMoves -> [ICoordinate]
movesToICoors (file,rank) [] = []
movesToICoors coor (f:fs) = filter (\(x,y) -> isBetween 0 7 x && isBetween 0 7 y) ( ([f coor] ++ movesToICoors coor fs))


findSource :: PlayerState -> Move -> Board -> Either Err Coordinate
findSource pstate (Move p (file,rank)) board = loopPieceMoves pstate board pcs (file,rank)
                                where pcs = filterPieces pstate p board


toIcoor :: Coordinate -> ICoordinate
toIcoor (f,r) = (charToIndex f, intToIndex r)


getSquare :: Coordinate -> Board -> Square
getSquare (c, i) b = (b !! (intToIndex i)) !! charToIndex c

findPiece :: Coordinate -> Board -> Maybe Piece
findPiece coor board = case sqr of 
                        Black p -> Just p
                        White p -> Just p
                        _       -> Nothing 
            where sqr = getSquare coor board

getSquarePiece :: Coordinate -> Board -> Maybe SquarePiece
getSquarePiece coor board = case piece of 
                               Just n  -> Just (n,coor)
                               Nothing -> Nothing 
                  where piece = findPiece coor board

getSquareI :: ICoordinate -> Board -> Either Err Square
getSquareI (c, i) b | c < 0 || c > 7 = Left (OutErr ("file " ++ [itsChr c]))
                    | i < 0 || i > 7 = Left (OutErr ("rank " ++ [itsChr i]))
                    | otherwise = Right ((b !! i) !! c)

-- Get all pieces from the board that have the color and piece
filterPieces :: PlayerState -> Piece -> Board -> [SquarePiece]
filterPieces pstate piece board = [(piece, (f,r)) | (p, (f,r)) <- (concat (coorBoard board)), case p of Black x -> isBlack pstate && x == piece
                                                                                                        White x -> isWhite pstate && x == piece
                                                                                                        otherwise -> False ]


coorBoard :: Board -> [[(Square, Coordinate)]]
coorBoard board = splitEvery 8 (zip (concat board) x)
                where x =  [(f,r) | r <- (reverse [1..8]), f <- ['a'..'h']]

loopMoves :: SquarePiece -> [(ICoordinate -> ICoordinate)] -> Coordinate -> Bool
loopMoves (pc, coor) [] target = False  
loopMoves (pc, coor) (m:ms) target = if m (toIcoor coor) == (toIcoor target) then True 
                                               else loopMoves (pc,coor) ms target

-- Brute force all filtered pieces  
loopPieceMoves :: PlayerState -> Board -> [SquarePiece] -> Coordinate -> Either Err Coordinate
loopPieceMoves pstate board [] target = Left (InvalidErr "Move not possible")
loopPieceMoves pstate board ((p,coor):cs) target = if moveFound then Right coor 
                                            else loopPieceMoves pstate board cs target 
                                where moveFound = loopMoves (p,coor) (getMoves pstate (p,coor) board) target 
