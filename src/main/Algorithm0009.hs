import System.Environment

main = do args <- getArgs
          let list = head args
          putStrLn $ show $ sortCow list

sortCow
    :: [Char]
    -> [Char]
sortCow [] = []
sortCow list =
    let (ahead, nextList) = popElement list
    in  ahead : sortCow nextList

popElement
    :: [Char]
    -> (Char, [Char])
popElement list 
    | headElm < lastElm = (headElm, tail list)
    | otherwise = (lastElm, init list)
    where headElm = head list
          lastElm = last list
