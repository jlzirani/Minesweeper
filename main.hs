import Project
import Cell
import Helper
import Grid
import System.Random

instance Board Grid where 
        -- initialize => create the grid and after, we start click on the right cell
	initialize seed size coord = click coord $ createGrid (randoms (mkStdGen seed) :: [Int]) size coord
	-- click :: Board b => (Int,Int) -> b -> b
	click coord b = if isInRange coord (size b) then flipMarked b coord else b
	--flag :: Board b => (Int,Int) -> b -> b
	flag coord b = if isInRange coord (size b) then set b coord $ (getCell b coord) {flagged=True} else b
        --won :: MineSweeper -> Bool
        won b = (==) 0 $ sum $ map (length.(filter (\x -> not $ (clicked x) || (flagged x && mined x)) )) (getCells b)
        --lost :: MineSweeper -> Bool
	lost b = (/=) 0 $ sum $ map (length.(filter (\x -> (clicked x && mined x)))) (getCells b)

instance Show Grid where
	--overriding show
	show grid = let (sizeX,sizeY) = size grid
                        convertFct = if (won grid) || (lost grid) then revealCell else convertToChar
                    in outterJoin (createLineSep sizeX) $ [outterJoin "|" [[convertFct y] | y <- x] | x <- getCells grid ]

main = top ( initialize :: Int -> (Int,Int) -> (Int,Int) -> Grid )
