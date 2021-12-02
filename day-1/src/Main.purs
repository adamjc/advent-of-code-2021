module Main where

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
    lines = split (Pattern "\n") input
    nums = toInts lines
    nums2 = fromMaybe [] $ tail nums
    both = zip nums2 nums
    resultish = map sub both
    result = foldl (+) 0 resultish
  logShow result

sub :: Tuple Int Int -> Int
sub (Tuple a b) =
  if a > b
    then 1
    else 0

toInts :: Array String -> Array Int
toInts arr = catMaybes $ map (fromStringAs decimal) arr
