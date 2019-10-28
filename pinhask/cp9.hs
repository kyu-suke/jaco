
type IO a = World -> (a, World)

(>>=) :: IO a -> (a -> IO b) -> IO b
f >>= g = \world -> case f world of
                      (v, world') -> g v world'

echo :: IO ()
echo = do c <- getChar
          putChar '\n'
		  putChar c
          putChar '\n'
