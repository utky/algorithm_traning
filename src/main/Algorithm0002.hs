{-
-
title: アルプロ002
==========
date: 2013-06-01 17:51
tags: [arpro]
categories: []
- - -

【アルプロ0002】足りるのか？ 
解き方は、いくつかあると思います。

問題 
整数a1,a2,a3...ai が与えられます。その中からいくつか(n)を選び、その和をちょうど k にすることができるか判定するプログラムを作成しなさい。整数aiは、それぞれ、一度しか使えません。


制約 
　1≦n≦20
　-10^8≦ai≦10^8
　-10^8≦k≦10^8


例 
  例1
    入力：
      k = 100
      n = 7
      ai = ( 50,3,8,9,10,1,2,20)
    出力：
      true
  例2
    入力：
      k = 100
      n = 6
      ai = ( 50,3,8,9,10,1,2,20)
    出力：
      false

-}

{-
- 
-}
isSummaryComposedOf :: Int -> Int -> [Int] -> String 
isSummaryComposedOf k n ai 
    | n < 1 || 20 < n = "n must be in range of (1 =< n =< 20)."
    | k < ((-10) ^ 8) || (10 ^ 8) < k = "k must be in range of (-10 ^ 8 =< k =< 10 ^ 8)."
    | minimum ai < ((-10) ^ 8) || (10 ^ 8) < maximum ai = "Element in ai must be in range of (-10 ^ 8 =< k =< 10 ^ 8)."
    | otherwise = show ((length (filter (isEqualsSummary k) (combinations n ai))) > 0)
 
{-
- 指定された数値が指定された配列の和と等しいかを判定します。
-}
isEqualsSummary :: Int -> [Int] -> Bool
isEqualsSummary k xs = (k == (summarize xs))

{- 
- 指定された数値リストの合計を取得します。
-
-}
summarize :: [Int] -> Int
summarize [] = 0
summarize (x:xs) = x + summarize xs

{- 
- 数値リストの組み合わせを二次元配列として生成します。
-}
combinations :: Int -> [Int] -> [[Int]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs 
