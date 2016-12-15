#!/usr/bin/env stack
-- stack --install-ghc runghc --package text --package megaparsec --package array

import           Control.Arrow ((&&&))
import           Control.Monad (forM_)
import           Data.Array (Array)
import qualified Data.Array as A
import           Data.Function (on)
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

newtype Width = Width { width :: Int } deriving Show
newtype Height = Height { height :: Int } deriving Show
newtype RotateAmount = RotateAmount Int

data RowType = Row Int | Column Int

type Board = Array (Int, Int) Lighting

data Instruction
    = Rect Width Height
    | Rotate RowType RotateAmount

data Lighting = Lit | Unlit

main :: IO ()
main = either (printError . show) run . parseInput . T.pack =<< getContents

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e

run :: [Instruction] -> IO ()
run = printBoardMeta . foldl iterateBoard (newBoard (Width 50) (Height 6))

printBoardMeta :: Board -> IO ()
printBoardMeta b = do
    print $ boardWidth b
    print $ boardHeight b
    putStrLn $ "Lit: " ++ show (litPixels b)
    printBoard b

printBoard :: Board -> IO ()
printBoard b =
    forM_ (groupBy (snd . fst) $ A.assocs b) $ \(_, vals) ->
        putStrLn $ unwords $ map (renderLit . snd) vals
  where
    renderLit Lit = "#"
    renderLit _   = "."

newBoard :: Width -> Height -> Board
newBoard (Width w) (Height h) = A.array ((1,1), (w,h)) buildBase
  where
    buildBase = [((i, j), Unlit) | i <- [1..w], j <- [1..h]]

litPixels :: Board -> Int
litPixels = length . filter isLit . A.elems
  where
    isLit Lit = True
    isLit _ = False

iterateBoard :: Board -> Instruction -> Board
iterateBoard b (Rect w h) = runRect b w h
iterateBoard b (Rotate t a) = runRotate b t a

runRect :: Board -> Width -> Height -> Board
runRect b (Width w) (Height h) = b A.// updates
  where
    updates = [((i, j), Lit) | i <- [1..w], j <- [1..h] ]

runRotate :: Board -> RowType -> RotateAmount -> Board
runRotate b (Row r) (RotateAmount i) = b A.// updates
  where
    width' = width $ boardWidth b
    updates = [((i', r), newValue (i', r)) | i' <- [1..width']]
    newValue (x, y) = b A.! (result', y)
      where
        result = width' + x - i
        result' = if result > width' then result `rem` width' else result
runRotate b (Column c) (RotateAmount i) = b A.// updates
  where
    height' = height $ boardHeight b
    updates = [((c, i'), newValue (c, i')) | i' <- [1..height']]
    newValue (x, y) = b A.! (x, result')
      where
        result = height' + y - i
        result' = if result > height' then result `rem` height' else result

boardWidth :: Board -> Width
boardWidth = Width . fst . snd . A.bounds

boardHeight :: Board -> Height
boardHeight = Height . snd . snd . A.bounds

parseInput :: Text -> Either ParseError [Instruction]
parseInput = runParser (instructionsParser <* eof) "" . T.strip

instructionsParser :: Parser [Instruction]
instructionsParser = instructionParser `sepBy1` newline

instructionParser :: Parser Instruction
instructionParser = rectParser <|> rotateParser

rectParser :: Parser Instruction
rectParser = uncurry Rect <$> (string "rect" *> space *> dimensionParser)

dimensionParser :: Parser (Width, Height)
dimensionParser = do
    w <- fromInteger <$> L.integer <* string "x"
    h <- fromInteger <$> L.integer

    return (Width w, Height h)

rotateParser :: Parser Instruction
rotateParser = do
    rowType <- string "rotate" *> space *> rowTypeParser
    space <* string "by" <* space
    rotateAmount <- RotateAmount . fromInteger <$> L.integer
    return $ Rotate rowType rotateAmount

rowTypeParser :: Parser RowType
rowTypeParser = rowParser <|> columnParser
  where
    rowParser = Row . (+ 1) . fromInteger <$> (string "row y=" *> L.integer)
    columnParser = Column . (+ 1) . fromInteger <$> (string "column x=" *> L.integer)

groupBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupBy f = map (f . head &&& id)
                   . L.groupBy ((==) `on` f)
                   . L.sortBy (compare `on` f)
