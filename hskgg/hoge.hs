

 
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
