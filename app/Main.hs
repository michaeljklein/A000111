module Main where

import Lib

main :: IO ()
main =
  mapM_ print . fmap (fmap (length . show)) $ (a000111s' :: [(Int, Integer)])

