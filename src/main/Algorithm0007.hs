import System.Environment

main = do args <- getArgs
          let count = read (head args) :: Integer
          let okometsubus = listFibonacci count
          putStrLn $ "How much grains of rice on N-th day? : " ++ (show $ last okometsubus)
          putStrLn $ "List the amount of rice per day. : " ++ (show okometsubus)

{- |
 - 指定された項までのフィボナッチ数列をリスト形式で取得します。
 -}
listFibonacci :: Integer -> [Integer]
listFibonacci 0 = (fibonacci 0) : []
listFibonacci n = listFibonacci (n - 1) ++ (fibonacci n) : []

{- |
 - フィボナッチ数列の第n項の数値を算出します。
 -
 - 太閤の提示した数式をほぼそのまま
 - Haskellの式として記述できてしまいます。
 - ( ﾟ∀ﾟ)o彡゜Haskell! Haskell!
 -}
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

