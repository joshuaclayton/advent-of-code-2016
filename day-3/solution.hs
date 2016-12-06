#!/usr/bin/env stack
-- stack --install-ghc runghc --package text --package megaparsec --package split

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List as L
import           Data.List.Split (chunksOf)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

data Polygon = Polygon [Side]
newtype Length = Length Integer
data Side = Side Length

main :: IO ()
main = either (printError . show) run . parseInput . T.pack =<< getContents

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e

run :: [Polygon] -> IO ()
run = print . length . filter isValidTriangle

isValidTriangle :: Polygon -> Bool
isValidTriangle (Polygon [Side (Length l1), Side (Length l2), Side (Length l3)]) =
    l1 + l2 > l3 && l2 + l3 > l1 && l1 + l3 > l2
isValidTriangle _ = False

integersToPolygons :: [[Integer]] -> [Polygon]
integersToPolygons = map toPolygon . chunksOf 3 . concat . L.transpose
  where
    toPolygon = Polygon . map (Side . Length)

parseInput :: Text -> Either ParseError [Polygon]
parseInput = fmap integersToPolygons . runParser (integersParser <* eof) "" . T.strip

integersParser :: Parser [[Integer]]
integersParser = integerRowParser `sepBy1` newline

integerRowParser :: Parser [Integer]
integerRowParser = whitespace *> L.integer `sepBy1` whitespace
  where
    whitespace = many $ string " "
