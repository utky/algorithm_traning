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
- 落下する方向の端に最も遠い蟻の移動距離が、
- 全体としての落下時間になる。
-}

{-
- 蟻さんを表す構造体
-}
data Ant = Ant {
    position :: Int, -- ^ 現在位置(経路の左端から数える)
    velocity :: Velocity  -- ^ 1秒あたりで進む移動距離(正 東向き : 負 西向き)
} deriving (Eq, Show)

{-
- 蟻さんが進むスピードと方向の定義(暫定は2価しか持たない)
-}
data Velocity = West -1 | East 1

{-
 - 蟻さん達の状況をトレースするためのコンテキスト
 - 実質的にはただの蟻さんコレクション
 -}
type AntContext = [Ant]

{-
- 指定された蟻さんの配置パターンリスト数分だけ、
- それぞれにかかる移動終了のステップ数を計算します。
-}
listCost
    :: Int
	-> [AntContext]
	-> [(Int, AntContext)]
listCost _ [] = []
listCost length ctx:restCtxs = ((getMaxCost length ctx), ctx) : listCost length restCtxs


{- |
 - 蟻さんをそれぞれ一歩分移動させます。
 -
 - 一歩移動した後の位置は以下のように計算されます。
 -
 - position + velocity
 -
 - この関数では上記の計算を全ての愛rさんに適用します。
 -}
step :: AntContext -> AntContext
step ctx = map (\ ant -> Ant {position=(position ant) + (velocity ant), velocity=(velocity ant)}) ctx

{- |
 - 条件に該当する蟻さんの進行方向を逆転させます。
 - 進行方向の逆転は蟻さんの持つvelocity値の符号を反転させることで表現します。
 -
 - 進行方向反転の条件は蟻さんの「衝突」です。
 -
 - 衝突の条件
 - 同一positionの蟻さんがいる場合
 - positionの差が1で、少ない方のvelocityが正かつ大きい方のvelocityが負の場合
 -
 -}
turn :: AntContext -> AntContext

{-
 - 二匹の蟻さんが衝突しているかを判定します。
 -
 - 衝突の条件
 - 同一positionの蟻さんがいる場合
 - positionの差が1で、少ない方のvelocityが正かつ大きい方のvelocityが負の場合
 -}
isConflict :: Ant a => a -> a -> Bool
isConflict lhs rhs
    | interval == 1 && (velocity lhs) < 0 && (velocity rhs) > 0 = True
    | interval == -1 && (velocity lhs) > 0 && (velocity rhs) < 0 = True
	| otherwise = isSamePosition lhs rhs
    where interval = (position lhs) - (position rhs)

{-
 - 蟻さんの位置比較簡略化のための関数
 - 二匹の蟻さんが現在同じ位置で衝突中かを判定します。
 -}
isSamePosition :: Ant a => a -> a -> Bool
isSamePosition lhs rhs = (position lhs) == (position rhs)


{-
- 蟻さんの各配置から全ての蟻さんが経路を渡りきるステップ数を返却します
-}
getMaxCost 
    :: Int
	-> AntContext
	-> Int
getMaxCost length ctx = max $ map (getCost length) ctx

{-
- 蟻さん一匹が経路を渡りきるステップ数を返却します
-}
getCost
    :: Int
	-> Ant
	-> Int
getCost length ant
    | v == 0 = 0
	| v < 0 = p / (negate v) + 1
	| v > 0 = (length - p) / v + 1
	where v = velocity ant
	      p = position ant
