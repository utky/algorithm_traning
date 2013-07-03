{-
 - 有名な問題です。
 - アルゴリズムと言うより、クイズ的な問題かもしれません。
 - 
 - 
 - 問題 
 - n匹の蟻が、長さ L cm の棒の上を、それぞれ秒速1センチメートルで歩いています。棒の端まで行くと、蟻は棒から落ちて行きます。また、 2匹の蟻が出会うと、それぞれ反対方向に歩き始めます。私たちは棒にいるに蟻のスタート位置を知ることが出来ますが、どちらに歩き始めるかは分かりません。あなたの使命は、全ての蟻が棒から落ちる最長の時間と最短の時間を計算するプログラムを作ることです。
 - 
 - 
 - 入力 
 - 入力の最初は、棒の長さ L を指定してください。次に、棒の上に存在する蟻の数 n を。それから、棒の左端から測定したそれぞれの蟻の位置を与える n個の数値を続けます。全ての値は、整数で、1,000,000よりも大きくはありません。それぞれの値は、空白で区切ってください。
 - 
 - 
 - 出力 
 - 二つに値を一つのスペースで区切って出力します。
 - 一つ目の値は、最長の時間を。二つ目の値は、最短の時間を出力します。
 - 
 - 
 - 例 
 - 入力：10 3 1 5 9
 - 出力：9 5
 -}

{-
- 大事なこと
- 基本的に衝突は考えなくてよい。
-
- Aが衝突すると衝突先のBに位置と方向が伝播する。
- 結局Aが迎えるはずの結果をただBが引き継ぐだけ。
- Bの目線で考えても同様
-
- 従って、総体としての移動コストは変わらない。
-
- このことから、与えられた蟻の情報を収集し、
- 落下する方向の端に最も遠い蟻の移動時間が、
- 全ての蟻が棒を渡りきるのに必要な経過時間になる。
-}
import System.Environment

{-
- 蟻さんを表す構造体
-}
data Ant = Ant {
    position :: Int, -- ^ 現在位置(経路の左端から数える)
    velocity :: Int  -- ^ 1秒あたりで進む移動距離(正 東向き : 負 西向き)
} deriving (Show)

{-
 - 蟻さん達の状況をトレースするためのコンテキスト
 - 実質的にはただの蟻さんコレクション
 -}
type AntContext = [Ant]

main = do args <- getArgs
          let maxAndMin = getMaxAndMin args
          putStrLn $ "最長時間=" ++ show (fst maxAndMin) ++ ", 最短時間=" ++ show (snd maxAndMin) 

getMaxAndMin :: [String] -> (Int, Int)
getMaxAndMin (lengthStr:antCount:positionsStr) = (maximum costs, minimum costs)
    where len = read lengthStr :: Int
          positions = map (\ str -> read str :: Int) positionsStr
          costs = listCosts len $ createAntContext positions 

{-
- 指定された蟻さんの配置定義リスト数分だけ、
- それぞれにかかる移動終了のステップ数(時間)を計算します。
-
- 返却されるリストの要素は各蟻さんが棒を渡り切るステップ数です。
-
-}
listCosts
    :: Int -- ^ 棒の長さ
    -> AntContext -- 蟻さんの定義情報リスト
    -> [Int] -- ^ 各蟻さんの定義が経路を渡りきるのに必要なステップ数のリスト
listCosts _ [] = []
listCosts length ctx = map (getCost length) ctx

createAntContext :: [Int] -> AntContext
createAntContext positions = map (\ (pos, vel) -> Ant { position = pos, velocity = vel}) antPatterns
    where antPatterns = zip positions [1,1..] ++ zip positions [(-1),(-1)..]

{- |
- 蟻さん一匹が経路を渡りきるステップ数を返却します。
-
- * 蟻さんが西向きの場合
-   現在の位置[西端からの距離] ÷ 移動速度 = かかるステップ数(時間)
-
- * 蟻さんが東向きの場合
-   (棒の長さ - 現在の位置)[東端までの距離] ÷ 移動速度 = かかるステップ数(時間)
-}
getCost
    :: Int -- ^ 棒の長さ
    -> Ant -- ^ 蟻さん情報
    -> Int -- ^ 現在位置から棒を渡りきるまでのステップ数
getCost length ant
    | v == 0 = 0
    | v < 0 = p `div` (negate v)
    | v > 0 = ((length - 1) - p) `div` v
    where v = velocity ant
          p = position ant
