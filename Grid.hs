module Grid(Grid, flipMarked, getCell, getCells, set, size, createGrid) where
import Cell
import Helper

-- data => Grid size nbrBombs cells 
data Grid = Grid (Int,Int) [[ Cell ]]

--------------------
-- Op on the Grid --
--------------------

-- get a cell
getCell :: Grid -> (Int, Int) -> Cell  
getCell (Grid _ list) (x,y) = (list !! x) !! y 

-- set a cill on the grid by recreating the grid
set :: Grid -> (Int, Int) -> Cell -> Grid
set (Grid (sizeX,sizeY) list) (x,y) val = Grid (sizeX, sizeY) $ [list !! i | i <- [0..(x-1)]] ++ [[(list !! x) !! i | i <- [0..(y-1)]] ++ [val] ++ [(list !! x) !! i | i <- [(y+1)..(sizeY-1)]]] ++ [list !! i | i <- [(x+1)..(sizeX-1)]]

-- get the [[ Cell ]]
getCells :: Grid -> [[ Cell ]]
getCells (Grid _ cells) = cells

-- get size
size :: Grid -> (Int, Int)
size (Grid gsize _) = gsize

getAdjCells :: Grid -> (Int, Int) -> [(Int, Int)]
getAdjCells grid (cX, cY) = let (sizeX, sizeY) = size grid
                            in [(x,y) | x <- [cX-1..cX+1], y <- [cY-1..cY+1], x >= 0, x < sizeX, y >= 0, y < sizeY]
-------------------
-- Creating Grid --
-------------------

-- create Basic Grid => We create a list of a list of cell, and each cell is not marked, not flagged, is bombed randomly and the number of bomb is not calculated yet.
createBasicGrid :: [Int] -> (Int, Int) -> [[ Cell ]]
createBasicGrid ra (sizeX,sizeY) = map (map (\x -> createCell (False, False, (mod x 10) == 0, 0))) $ splitEvery sizeY (take (sizeY*sizeX) ra)

-- We take the coordinates of all adjacent cell of the current cell, and we count the number of bombed adjacent cell.
calculateBombCell :: Grid -> (Int,Int) -> Int
calculateBombCell board coord = length $ filter (mined . (getCell board)) $ getAdjCells board coord

-- for each cell, we calculate the number of mined cell.
calculateBombGrid :: Grid -> Grid
calculateBombGrid grid = let (sizeX, sizeY) = size grid 
                         in Grid (sizeX,sizeY) [[(getCell grid (x,y)) {nbrAdjBomb=(calculateBombCell grid (x,y))} | y <- [0..sizeY-1]]| x<- [0..sizeX-1]]


-- Create a basic grid with only the bomb, and after that add the number of bomb on the adjacent cells
-- We also remove the the bomb of the coordinate "coord"
createGrid :: [Int] -> (Int,Int) -> (Int,Int) -> Grid
createGrid ra size coord = calculateBombGrid (set (Grid size (createBasicGrid ra size)) coord (createCell (False,False,False,0))) 

-----------------
-- flip marked --
----------------- 

setClickCell:: Grid -> (Int,Int) -> Grid
setClickCell grid coord = set grid coord $ (getCell grid coord) {clicked = True} 

-- Marked cells given by their coordinates
markedCells :: Grid -> [(Int,Int)] -> Grid
markedCells grid adjCells = foldl setClickCell grid adjCells 

-- Select all adj cells that is not marked, then marked them and propagate the marking
propagateAdjCells :: Grid -> (Int, Int) -> Grid
propagateAdjCells grid coord = let adjCells = filter (not . clicked .(getCell grid)) $ getAdjCells grid coord
                               in foldl (\nGrid nCoord -> propagateMarkedCell nGrid nCoord) (markedCells grid adjCells) adjCells 

-- Get the current Cell, stop the recursion if the cell is adjacent to one or more bomb, or it is a bomb
propagateMarkedCell :: Grid -> (Int,Int) -> Grid
propagateMarkedCell grid coord = let curCell = getCell grid coord
                                    in if (isAdjToBomb curCell) || (mined curCell)
                                          then grid
                                          else propagateAdjCells grid coord

-- Get the current cell, mark it, and then start the propagation
flipMarked :: Grid -> (Int,Int) -> Grid
flipMarked grid coord = let curCell = getCell grid coord
                        in if clicked curCell then grid else propagateMarkedCell (setClickCell grid coord) coord

