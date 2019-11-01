module OneTen where

import Data.Array
import Data.Maybe
import Data.Ring
import Data.Foldable
import Data.Functor
import Data.Eq
import Data.Semigroup
import Data.String as S
import Data.Foldable

import Control.Semigroupoid

hoge :: Int
hoge = 1

fuga :: Int
fuga = 2

myLast :: forall a. Array a -> Maybe a
myLast = head <<< reverse

myButLast :: forall a. Array a -> Maybe a
myButLast a = case init a of
                Just b -> last b
                _ -> Nothing

elementAt :: forall a. Array a -> Int -> Maybe a
elementAt x 1 = head x
elementAt x i = case uncons x of
                    Just {head: _, tail: xs} -> elementAt xs (i - 1)
                    _ -> Nothing

elementAt' :: forall a. Array a -> Int -> Maybe a
elementAt' a i = a !! (i - 1)

myLength :: forall a. Array a -> Int
myLength = sum <<< map (\x -> 1)

myReverse :: forall a. Array a -> Array a
myReverse a = case uncons a of
                Just {head: x, tail: []} -> [x]
                Just {head: x, tail: xs} -> snoc (myReverse xs) x
                _ -> []

isPalindrome :: forall a. Eq a => Array a -> Boolean
isPalindrome a = a == reverse a


-- data NestedList a = Elem a | List [NestedList a]
data NestedList a = Elem a | List (Array (NestedList a))

flatten :: forall a. NestedList a -> Array a
flatten (Elem a) = [a]
flatten (List a) = case uncons a of
                      -- (NestedList a : [NestedList a])                  (List [NestedList a])
                      Just {head: x, tail: xs} -> (flatten x) <> (flatten (List xs))
                      Nothing -> []

compress :: Array Char -> String
compress a = foldl (\x -> \y -> x <> S.fromCodePointArray [S.codePointFromChar y]) "" a

pack :: Array Char -> Array String
pack a = foldl (\x -> \y -> x <> S.fromCodePointArray [S.codePointFromChar y]) [] a



