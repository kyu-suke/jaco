import Data.Char

main = do
  putStrLn "Hello World!"
  let a = [(x, y) | x <- [1..3], y <- [x..3]]
  putStrLn $ show a

  putStrLn $ show $ prime 3
  putStrLn $ show $ prime 6

  p (primes 40)

p :: Show a => a -> IO()
p = putStrLn . show

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime x = factors x == [1, x]

primes :: Int -> [Int]
primes x = [xs | xs <- [2..x], prime xs]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let i = chr (ord 'a' + i)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let $ ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n s = [shift n ss | ss <- s]
