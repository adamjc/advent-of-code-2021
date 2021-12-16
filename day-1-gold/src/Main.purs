module Main
  ( main
  , zip3
  )
  where

import Prelude

import Data.Array (cons, catMaybes, foldl, head, null, tail, zip, (..))
import Data.Int (decimal, fromStringAs)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "src/input"
  let
    as = toInts $ split (Pattern "\n") input
    bs = fromMaybe [] $ tail as
    cs = fromMaybe [] $ tail bs
    result = zip3 as bs cs
  logShow $ result

zip3 :: Array Int -> Array Int -> Array Int -> Array (Array Int)
zip3 as bs cs =
  if (null as || null bs || null cs)
    then []
  else
    cons
      ([fromHead as, fromHead bs, fromHead cs])
      (zip3 (fromTail as) (fromTail bs) (fromTail cs))

fromHead :: Array Int -> Int
fromHead a = fromMaybe 0 $ head a

fromTail :: Array Int -> Array Int
fromTail a = fromMaybe [] $ tail a

toInts :: Array String -> Array Int
toInts arr = catMaybes $ map (fromStringAs decimal) arr
