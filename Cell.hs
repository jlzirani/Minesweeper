module Cell (Cell(clicked, flagged, mined, nbrAdjBomb),isAdjToBomb,revealCell,convertToChar,createCell) where
import Data.Char

data Cell = Cell { clicked :: Bool, 
                   flagged :: Bool,
                   mined :: Bool,
                   nbrAdjBomb :: Int }

isAdjToBomb :: Cell -> Bool
isAdjToBomb = ((<) 0). nbrAdjBomb

convertToChar :: Cell -> Char
convertToChar Cell {clicked=False,flagged=False} = ' '
convertToChar Cell {clicked=False,flagged=True} = 'F'
convertToChar Cell {clicked=True, mined=False, nbrAdjBomb=nbrBombs} = intToDigit nbrBombs
convertToChar Cell {clicked=True, mined=True} = 'B'

revealCell :: Cell -> Char
revealCell Cell {mined=False, nbrAdjBomb=nbrBombs} = intToDigit nbrBombs
revealCell _ = 'B'

createCell :: (Bool,Bool,Bool,Int) -> Cell
createCell (isClicked, isFlagged, isMined, adjCell) = Cell { clicked = isClicked, flagged = isFlagged, mined = isMined, nbrAdjBomb = adjCell }
