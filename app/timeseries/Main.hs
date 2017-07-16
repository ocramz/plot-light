{-# language OverloadedStrings #-}
module Main where

-- import Control.Arrow ((***), (&&&))

import Graphics.Rendering.Plot.Light
import Graphics.Rendering.Plot.Light.PlotTypes
  
import qualified Data.Attoparsec.Text as A
import qualified Attoparsec.Time as AT
import qualified Data.Text as T 
import qualified Data.Text.IO as T (readFile, writeFile)
import Data.Scientific (Scientific, toRealFloat)
import Text.Blaze.Svg.Renderer.String (renderSvg)
import qualified Data.Colour.Names as C

import Control.Applicative ((<|>))
import Data.Time (Day, TimeOfDay)




fname = "data/forex_small"

xPlot = 800
yPlot = 600
fnameOut = "data/forex_plot_3.svg"


main = do
  d <- T.readFile fname
  let pd = A.parseOnly parseFxDataset d
  case pd of Left e -> error e
             Right d -> -- print $ tsAxis (toFloat . rateOpen) xPlot yPlot 3 C.blue (-45) d
               do
               let svg_t = svgHeader (mkFrameOrigin xPlot yPlot) $ tsAxis avgTs xPlot yPlot 3 C.black C.red (-45) d
               -- putStrLn $ renderSvg svg_t
               T.writeFile fnameOut $ T.pack $ renderSvg svg_t

toFloat :: Scientific -> Float
toFloat x = toRealFloat x :: Float


avgTs :: FxRow Scientific -> Float
avgTs x = 0.5 * (h + l) where
  h = toFloat (rateHigh x)
  l = toFloat (rateLow x)





-- Forex time series parsers - related


data FxRow a  = FxRow {
    rateOpen :: a
  , rateHigh :: a
  , rateLow :: a
  , rateClose :: a
               } deriving (Eq, Show)


space, comma :: A.Parser Char
space = A.char ' '
comma = A.char ','

-- | Parse a row of numbers, separated by `sep`
rowNums :: A.Parser s -> A.Parser [Scientific]
rowNums sep = A.sepBy A.scientific sep

rowNumSpace :: A.Parser [Scientific]
rowNumSpace = rowNums space

-- | parse a grid of numbers, separated by `sep`
gridNum :: A.Parser s -> A.Parser [[Scientific]]
gridNum sep = A.sepBy (rowNums sep) A.endOfLine









-- * Forex dataset

parseFxDataset :: A.Parser [TsPoint (FxRow Scientific)]
parseFxDataset = A.sepBy parseFxRow A.endOfLine

parseFxRow :: A.Parser (TsPoint (FxRow Scientific))
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

parseDateTime :: A.Parser (Day, TimeOfDay)
parseDateTime = do
  d <- AT.dayInISO8601
  _ <- space
  t <- AT.timeOfDayInISO8601
  return (d, t)
