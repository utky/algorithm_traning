import System.Environment

main = do args <- getArgs
          let count = read (head args) :: Integer
          let okometsubus = listFibonacci count
          putStrLn $ "How much grains of rice on N-th day? : " ++ (show $ last okometsubus)
          putStrLn $ "List the amount of rice per day. : " ++ (show okometsubus)

{- |
 - �w�肳�ꂽ���܂ł̃t�B�{�i�b�`��������X�g�`���Ŏ擾���܂��B
 -}
listFibonacci :: Integer -> [Integer]
listFibonacci 0 = (fibonacci 0) : []
listFibonacci n = listFibonacci (n - 1) ++ (fibonacci n) : []

{- |
 - �t�B�{�i�b�`����̑�n���̐��l���Z�o���܂��B
 -
 - ���}�̒񎦂����������قڂ��̂܂�
 - Haskell�̎��Ƃ��ċL�q�ł��Ă��܂��܂��B
 - ( ߁��)o�c�KHaskell! Haskell!
 -}
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

