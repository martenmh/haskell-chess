module IO where
import Data
import Util
import Move
import Board
import System.Exit
import Data.Maybe


eval :: PlayerState -> String -> Board -> Either Err Board 
eval pstate str b = case board of Left err -> Left err
                                  Right bo -> Right bo
                        where board = move pstate str b

rmspace :: String -> String
rmspace [] = []
rmspace (s:tr) = if s == ' ' then [] ++ rmspace tr else [s] ++ rmspace tr 

turnError :: PlayerState -> Board -> Err -> IO Board
turnError pstate board err = do putBoard board (getErrMsg err)
                                playerTurn pstate board

playerTurn :: PlayerState -> Board -> IO Board
playerTurn pstate board = do x <- getLine
                             let ln = rmspace x
                             if length ln == 0 then 
                                     do putBoard board ""
                                        playerTurn pstate board 
                             else if ln == "q" then exitSuccess
                             else if length ln == 3 && head ln == 's' then -- ... Split into functions
                                     let c = strToCoor (ln !! 1) (ln !! 2) in 
                                     case c of Left err -> do turnError pstate board err
                                               Right coor -> do let sqrpc = getSquarePiece coor board in 
                                                                 case sqrpc of Nothing -> turnError pstate board (NoErr)
                                                                               Just p  ->  do putMoves pstate p board
                                                                                              playerTurn pstate board
                             else let b = (eval pstate ln board) in case b of
                                     Left err -> do putBoard board (getErrMsg err)
                                                    playerTurn pstate board
                                     Right bo -> return bo


aiTurn :: PlayerState -> Board -> IO Board
--aiTurn pstate 
aiTurn pstate board = playerTurn (otherState pstate) board -- 2 Player!


turn :: PlayerState -> PlayerState -> Board -> IO Board
turn pstate turnstate board = do clr
                                 putBoard board ""
                                 if pstate == turnstate then playerTurn pstate board
                                 else aiTurn pstate board 

-- find king location and see if there is any enemy that can capture it
haswon :: Board -> Bool
haswon b = False

clr = putStr "\ESC[2J"

start :: PlayerState -> Board -> IO()
start pstate board = do b <- turn pstate PWhite board
                        newboard <- turn pstate PBlack b
                        if haswon newboard then putChar 'a'
                        else start pstate newboard

run = do
        putStr "White or Black (w/b): "
        c <- getChar 
        if c == 'W' || c == 'w' then start PWhite genBoard
        else if c == 'B' || c == 'b' then start PBlack genBoard
        else do putChar '\n'
                run


putBoard :: Board -> String -> IO()
putBoard board err = do clr
                        putStr (unlines (interleave bar (map (\(x,y) -> showRow x y) (zip board (reverse [1..8])) )))
                        putStrLn ("   " ++ (innerleave "  " ( ['a'..'h'])))
                        if length err /= 0 then putStrLn err
                        else return () -- Does not short-circuit the function!
                        putChar ':'
                 where bar = "  " ++ replicate ((3*length (board !! 0))+1) '-'

putMoves :: PlayerState -> SquarePiece -> Board -> IO ()
--putMoves (p, (file,rank)) b = (movesToICoors (charToIndex file,rank) (getMoves (p, (file,rank))))
putMoves pstate (p, (file,rank)) b = putBoard (replaceMany (movesToICoors (charToIndex file, (intToIndex rank)) (getMoves pstate  (p, (file,rank)) b )) (ToMove Empty) b) ""

-- Debugging purposes
putMoves2 :: PlayerState -> SquarePiece-> PieceMoves -> IO ()
putMoves2 pstate (p, (file,rank)) moves = putBoard (replaceMany (movesToICoors (charToIndex file, (intToIndex rank)) moves) (ToMove Empty) genBoard) ""
