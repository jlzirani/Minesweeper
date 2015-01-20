module Helper(join, isInRange, outterJoin, splitEvery, createLineSep) where

isInRange :: (Int,Int) -> (Int,Int) -> Bool
isInRange (cX,cY) (sX, sY) = cX >= 0 && cY >= 0 && cX < sX && cY < sY

join :: [Char] -> [[Char]] -> [Char]
join sep (x:[]) = x
join sep (x:xs) = x ++ sep ++ ( join sep xs )

outterJoin :: [Char] -> [[Char]] -> [Char]
outterJoin sep x = sep ++ (join sep x) ++ sep

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n a = 
          let (x,xs) = splitAt n a 
          in [x] ++ ( splitEvery n xs )

createLineSep :: Int -> [Char]
createLineSep size = "\n+" ++ (concat (replicate size "-+")) ++ "\n"


