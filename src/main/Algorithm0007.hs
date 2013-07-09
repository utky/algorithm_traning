import System.Environment

main = do args <- getArgs
          let count = read (head args) :: Integer
          putStrLn $ show $ fibonacci count 

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

pucci :: Integer -> Integer
