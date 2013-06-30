
type CountsMap = [(Int, Int)]

countCoinsOf :: Int -> Int
countCoinsOf amount = foldl (++) $ map (snd) (modAmount amount countsMap)
    where coinDefinitions = [1, 5, 10, 50, 100, 500]
          countsMap = reverse $ zip coinDefinitions [0..]

modAmount :: Int -> CountsMap -> CountsMap
modAmount _ [] = []
modAmount amount ((coin, count):countsMap)
    | coin == 0 = (coin, count) ++ (modAmount amount countsMap)
    | otherwise = (coin, count + used) ++ (modAmount (amount `mod` coin) countsMap)
    where used = amount `div` coin

