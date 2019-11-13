

 
-- データ型と値コンストラクタに同じ名前を使いました。
-- 同じ名前にしておくことに当別な意味はないのですが、
-- 値コンストラクタが一つしか無いデータ型はそうするのが慣例です。
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs $ (x1 - x2) * (y1 - y2)


-- レコード構文はフィールドを取得する関数も自動で作ってくれる
-- firstName :: Person -> String 的な
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)

hoge = Person "bar" "hoge" 1 12.1 "fuga" "piyo"
fuga = Person {firstName = "bar", lastName =  "hoge", age = 1, height =  12.1, phoneNumber = "fuga", flavor = "piyo"}
piyo = hoge{age=39}

-- データ型
-- 型コンストラクタ
-- 値コンストラクタ
-- 型クラス

-- 先に定義されている値コンストラクタのほうが小さい
data Junban = First | Second | Third deriving (Eq, Ord)

junban1 = First < Second
junban2 = First > Second
junban3 = Second < Third
junban4 = First < Third

-- Maybe a は常に Nothing < Just something
-- Just a < Just b の場合は値を比較する
jun1 = Nothing < Just (-1)
jun2 = Just 123 > Just 124


data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Eq, Ord, Show, Read, Bounded, Enum)
minDay = minBound :: Day
maxDay = maxBound :: Day
succWed = succ Wed
predWed = pred Wed
dlst = [Mon .. Sun] ::[Day]

-- typeは型シノニム
-- 型同義名の定義をしているだけ
type WeekDay = Day

-- 型シノニムも多層化できる
type AssocList k v = [(k, v)]
type AssocListInt v = [(Int, v)]

-- 型シノニムで部分適用
data Synon a b = Synon a b
type IntSynon = Synon Int

-- Cons a (List a) の List a はデータ型、 Cons a Int と同じ感じ、のはず
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- 値コンストラクタを中置関数にする場合、':'で始まらないと駄目
infixr 5 :-: -- 結合性宣言、優先順位
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- :-:が値コンストラクタなのでパターンマッチが可能
infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-:xs) ^++ ys = x :-: (xs ^++ ys)

