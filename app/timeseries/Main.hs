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

fname = "data/forex"

xPlot = 400
yPlot = 300
fnameOut = "data/forex_plot_5.svg"

fdat = FigureData xPlot yPlot 0.1 0.9 0.1 0.85 10


main = do
  dat <- T.readFile fname
  let pd = A.parseOnly parseFxDataset dat
  case pd of Left e -> error e
             Right d -> 
               do
               let
                 lps =  tspToLP fhi (\_ x -> x) <$> d
                 -- figure = tsAxis fdat fop fcl fhi flo 1 C.black (-45) Nothing Nothing lps       
                 figure = tsAxis' fdat flo fhi C.magenta lps
                 svg_t = svgHeader (mkFrameOrigin xPlot yPlot) figure
               -- putStrLn $ renderSvg svg_t
               T.writeFile fnameOut $ T.pack $ renderSvg svg_t
                 where
                   
                   fhi = toFloat . rateHigh
                   flo = toFloat . rateLow
                   fop = toFloat . rateOpen
                   fcl = toFloat . rateClose




avgTs :: FxRow Scientific -> Float
avgTs x = 0.5 * (h + l) where
  h = toFloat (rateHigh x)
  l = toFloat (rateLow x)





-- Forex time series parsers - related









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
