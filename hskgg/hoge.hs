
 
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


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

nums = [8,2,4,9,1,3]
sampleTree = foldr treeInsert EmptyTree nums

-- 型クラスの定義
class Eq2 a where
  (^==) :: a -> a -> Bool
  (^/=) :: a -> a -> Bool
  x ^== y = not (x ^/= y)
  x ^/= y = not (x ^== y)


data TrafficLight = Red | Yellow | Green

-- Eq型クラスのインスタンス
instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "this is Rded"
    show Yellow = "this is Yellow"
    show Green = "this is Green"

-- 型クラスのサブクラス
-- eg Num
-- class (Eq a) => Num s where ...

-- 多層型を型クラスのインスタンスにする
-- eg. Maybe
-- instance Eq (Maybe m) where ...
-- mに対する等値比較も必要なのでmに対してクラス制約をつける
-- instance (Eq m) => Eq (Maybe m) where ...

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno (Just _) = True

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True



{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b

Functorが要求しているのは型コンストラクタ
型注釈の f a で具体型になるようなやつ
Maybe とか [] とか
-}

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right)
        = Node (f x) (fmap f left) (fmap f right)

{-
*Main> :k Int
Int :: *

* は具体型を表す記号
}


