
type Parser a = String -> [(a, String)]

return_ :: a -> Parser a
return_ v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
                 [] -> []
                 (x:xs) -> [(x,xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
                    [] -> []
                    [(v, out)] -> parse (f v) out

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
                    [] -> parse q inp
                    [(v, out)] -> [(v, out)]

--
p :: Parser (Char, Char)
p = do x <- item
       item
       y <- item
       return_ (x, y)

--sat :: (Char -> Bool) -> Parser Char
--sat p = do
--          x <- item
--          if p x then return_ x else failure
