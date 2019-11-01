module OneTen where

import Data.Array
import Data.Maybe
import Control.Semigroupoid
import Effect.Exception

hoge :: Int
hoge = 1

fuga :: Int
fuga = 2

myLast :: forall a. Array a -> Maybe a
myLast = head <<< reverse 
