import System.Environment

main = do args <- getArgs
          let count = read (head args) :: Integer
          putStrLn $ show $ okometsubu count 

okometsubu :: Integer -> Integer
okometsubu 0 = 0
okometsubu 1 = 1
okometsubu n = okometsubu (n - 1) + okometsubu (n - 2)
