{-# LANGUAGE FlexibleInstances #-}
module Data where
import Data.Char
import Data.List
import System.IO
import Util
import Data.Either

data Piece = King | Queen | Knight | Rook | Bishop | Pawn
    deriving(Show, Eq)

data Square = Empty | Black Piece | White Piece | ToMove Square | Capture Square
    deriving(Show, Eq)

type Board = [[Square]]
type PieceMoves = [PieceMove]
type PieceMove = (ICoordinate -> ICoordinate)

-- For rmdups
instance Eq PieceMove where
    lhs == rhs = (lhs(0,0)) == (rhs(0,0))
instance Ord PieceMove where
    compare (lhs) (rhs) = compare (lhs(0,0)) (rhs(0,0)) 
    

-- For debugging
instance Show PieceMove where
    show l = show (l (0,0))

type Coordinate = (Char, Int)
type ICoordinate = (Int, Int)
-- Algebraic notation:
data Move = Move Piece Coordinate
    deriving(Eq)
type SquarePiece = (Piece, Coordinate)

instance Show Move where
    show (Move x (c, i)) = showPiece x : c : chr(i+48) : []


data PlayerState = PBlack | PWhite
    deriving(Eq)

getPiece :: Char -> Either Err Piece
getPiece c = case toLower c of
                'k' -> Right King
                'q' -> Right Queen
                'r' -> Right Rook
                'n' -> Right Knight
                'b' -> Right Bishop
                _   -> Left (SyntaxErr ("There's no piece " ++ [c]))

getValue :: Piece -> Either Err Int
getValue p = case p of
                King -> Right 900
                Queen -> Right 90
                Rook -> Right 50
                Bishop -> Right 30
                Knight -> Right 30
                Pawn -> Right 10
                _    -> Left Err

-- Use unicode? ♚
showPiece :: Piece -> Char
showPiece p = case p of
                King -> 'K'
                Queen -> 'Q'
                Rook -> 'R'
                Knight -> 'N'
                Bishop -> 'B'
                Pawn -> ' '

-- Error Constructors 
data Err = Err | NoErr | InvalidErr String | SyntaxErr String | OutErr String
    deriving(Eq)


getErrMsg :: Err -> String
getErrMsg (InvalidErr str) = "Invalid Move: " ++ str ++ "."
getErrMsg (Err) = "An Error has occurred."
getErrMsg (NoErr) = ""
getErrMsg (SyntaxErr str) = "Syntax Error: " ++ str ++ "."
getErrMsg (OutErr str) = "" ++ str ++ "."

isWhite :: PlayerState -> Bool
isWhite pstate | pstate == PWhite = True
               | otherwise       = False 

isBlack :: PlayerState -> Bool
isBlack pstate = not (isWhite pstate)
