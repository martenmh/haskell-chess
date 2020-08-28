module Board where
import Data
import Util
import Move
import Data.Char

genBoard :: Board
genBoard = map (map Black) [back, front] ++ (replicate 4 empty) ++ map (map White) [front, (reverse back)]
                where back  = [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]
                      front = replicate 8 Pawn
                      empty = replicate 8 Empty

        
showSquare :: Square -> String
showSquare (Black x) = ['B', showPiece x]
showSquare (White x) = ['W', showPiece x]
showSquare (ToMove x) = " X"
showSquare Empty = "  "

showRow :: [Square] -> Int -> String
showRow n i = [chr(i+48), ' '] ++ concat (interleave "|" (map showSquare n))
        where bar = replicate 3 "|"




-- foldr?
replaceMany :: [ICoordinate] -> Square -> Board -> Board
replaceMany [] sq b = b
replaceMany [(f,r)] sq b = replace (f,r) sq b
replaceMany (x:xs) sq b = replaceMany xs sq nb
            where nb = replace x sq b
