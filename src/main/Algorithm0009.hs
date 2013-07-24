import System.Environment

main = do args <- getArgs
          let count = head args
          let list = head $ tail args
          putStrLn $ show $ sortCow list
{- |
 - 指定された文字のリストをソートします。
 - 
 - リストの先頭要素と末尾要素を比較しながら、
 - 小さい値を持つ要素を先頭へと並べ替えていきます。
 -}
sortCow
    :: [Char]
    -> [Char]
sortCow [] = []
sortCow list 
    | needsLast == True = (last list) : sortCow (init list)
    | needsLast == False = (head list) : sortCow (tail list)
    where needsLast = isHeadLarger list

{- |
 - 指定されたリストの先頭と末尾の要素を比較し、
 - 先頭要素が大きいかどうかを判定します。
 -
 - * 判定のルールは以下の通り。
 - 1. 先頭 ＜ 末尾 : False
 - 2. 先頭 ＞ 末尾 : True
 - 3. 先頭 ＝ 末尾 : 先頭と末尾を除いたリストを再帰的に判定する
 -}
isHeadLarger
    :: [Char]
    -> Bool
isHeadLarger [] = False
isHeadLarger (end:[]) = False
isHeadLarger list 
    | headElm < lastElm = False
    | headElm > lastElm = True
    | otherwise = isHeadLarger (init $ tail list)
    where headElm = head list
          lastElm = last list


