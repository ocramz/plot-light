module Graphics.Rendering.Plot.Light.IO.Text where


import Data.Text
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Internal.Types as AP (Parser)
import Data.Scientific




-- row = many' decimal

space, comma :: A.Parser Char
space = A.char ' '
comma = A.char ','

-- rowNums :: A.Parser Scientific
rowNums :: AP.Parser Text s -> AP.Parser Text [Scientific]
rowNums = A.sepBy A.scientific

rowNumSpace :: AP.Parser Text [Scientific]
rowNumSpace = rowNums space



-- | parse a grid of numbers, separated by `sep`
gridNum :: AP.Parser Text s -> AP.Parser Text [[Scientific]]
gridNum sep = A.sepBy (rowNums sep) A.endOfLine
