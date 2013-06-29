{-
- 一歩進む
-}
forward :: Int -> Int -> [[a]] -> Bool
forward currentX currentY [[mazeMap]] = 

forwardNextBranch :: 

{-
- 行き止まりかどうか
-}
hasNext :: Int -> Int -> [[a]] -> Bool

{-
- 現在位置に訪問済みの箇所に目印をつける
-}
mark :: [[a]] -> [[a]]

countMarked :: [[Char]] -> Int
countMarked [] = 0
countMarked row:list = length (filter (eq '*') row) + countMarked list

