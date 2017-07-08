module Main where

import Graphics.Rendering.Plot.Light.IO.Text

import qualified Data.Attoparsec.Text as A
import qualified Data.Text.IO as T


fname = "data/forex"

main = do
  d <- T.readFile fname
  let pd = A.parseOnly parseFxDataset d
  case pd of Left e -> error e
             Right datarows -> pure datarows
