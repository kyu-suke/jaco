main :: IO ()
main = do
       print $ aaa (+1) [[9],[21]]
       print 1


aaa :: (a-> b) -> [[a]] -> [[b]]
aaa f []           = []
aaa f [[]]           = [[]]
aaa f ((x : xs):xxs) = ((f x:map f xs):aaa f xxs)

