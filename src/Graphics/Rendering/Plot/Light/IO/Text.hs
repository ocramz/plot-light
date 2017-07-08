module Graphics.Rendering.Plot.Light.IO.Text where

import Data.Text
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Internal.Types as AP (Parser)
import Data.Scientific

import Control.Applicative ((<|>))
import Data.Time (Day, TimeOfDay)
import qualified Attoparsec.Time as AT

import Data.TimeSeries.Forex



space, comma :: A.Parser Char
space = A.char ' '
comma = A.char ','

-- | Parse a row of numbers, separated by `sep`
rowNums :: AP.Parser Text s -> AP.Parser Text [Scientific]
rowNums sep = A.sepBy A.scientific sep

rowNumSpace :: AP.Parser Text [Scientific]
rowNumSpace = rowNums space

-- | parse a grid of numbers, separated by `sep`
gridNum :: AP.Parser Text s -> AP.Parser Text [[Scientific]]
gridNum sep = A.sepBy (rowNums sep) A.endOfLine









-- * Forex dataset

parseFxDataset :: AP.Parser Text [TsPoint (FxRow Scientific)]
parseFxDataset = A.sepBy parseFxRow A.endOfLine

parseFxRow :: AP.Parser Text (TsPoint (FxRow Scientific))
parseFxRow = do
  (d, t) <- parseDateTime
  _ <- comma
  open <- A.scientific
  _ <- comma
  hi <- A.scientific
  _ <- comma
  lo <- A.scientific
  _ <- comma
  close <- A.scientific
  pure $ Tsp (Tick d t) (FxRow open hi lo close)

parseDateTime :: AP.Parser Text (Day, TimeOfDay)
parseDateTime = do
  d <- AT.dayInISO8601
  _ <- space
  t <- AT.timeOfDayInISO8601
  return (d, t)


