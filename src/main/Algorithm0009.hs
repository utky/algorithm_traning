import System.Environment

main = do args <- getArgs
          let count = head args -- この要素数の引数要らない気がする
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
sortCow list =
    let (ahead, nextList) = popElement list
    in  ahead ++  sortCow nextList

{- |
 - 指定されたリストの先頭と末尾の要素を比較し、
 - より小さい方の要素をリストから切り出します。
 -
 - * 切り出しのルールは以下の通り。
 - 1. 先頭 ＜ 末尾 : 先頭を切り出す
 - 2. 先頭 ＞ 末尾 : 末尾を切り出す
 - 3. 先頭 ＝ 末尾 : 先頭も末尾も切り出す
 - 
 - * 返却値のタプルの定義は以下の通り。
 - 一番目の要素: 切り出された要素リスト(複数要素入る可能性がある)
 - 二番目の要素: 切り出された残りの要素が入ったリスト
 -}
popElement
    :: [Char]
    -> ([Char], [Char])
popElement [] = ([], [])
popElement (lst:[]) = (lst:[], [])
popElement list 
    | headElm < lastElm = (headElm:[], tail list)
    | headElm > lastElm = (lastElm:[], init list)
    | otherwise = (headElm:lastElm:[], init $ tail list)
    where headElm = head list
          lastElm = last list
