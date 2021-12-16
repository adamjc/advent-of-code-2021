module Main ( main ) where

import Prelude

import Data.Array (catMaybes, foldl, tail, zip)
import Data.Int (decimal, fromStringAs)
import Data.Maybe (fromMaybe)
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
    cs = map asMinusBs $ zip as bs
    result = foldl (+) 0 cs
  logShow result

asMinusBs :: Tuple Int Int -> Int
asMinusBs (Tuple a b) = if a > b then 1 else 0

toInts :: Array String -> Array Int
toInts arr = catMaybes $ map (fromStringAs decimal) arr
