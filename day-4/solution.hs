#!/usr/bin/env stack
-- stack --install-ghc runghc --package text --package megaparsec

{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow ((&&&))
import           Control.Monad (void)
import           Data.Function (on)
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

newtype EncryptedName = EncryptedName { encryptedName :: Text }
newtype SectorId = SectorId { sectorId :: Integer }
newtype Checksum = Checksum Text deriving Eq

data Room = Room
    { rEncryptedName :: EncryptedName
    , rSectorId :: SectorId
    , rChecksum :: Checksum
    }

main :: IO ()
main = either (printError . show) run . parseInput . T.pack =<< getContents 

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e

run :: [Room] -> IO ()
run = print . sum . map (sectorId . rSectorId) . filter isValidRoom

parseInput :: Text -> Either ParseError [Room]
parseInput = runParser (roomsParser <* eof) "" . T.strip

roomsParser :: Parser [Room]
roomsParser = roomParser `sepBy1` newline

isValidRoom :: Room -> Bool
isValidRoom r = rChecksum r == generatedChecksum r
  where
    generatedChecksum = frequencyToChecksum . frequency . T.unpack . encryptedName . rEncryptedName

roomParser :: Parser Room
roomParser = do
    encryptedName' <- T.pack <$> many (letterChar <|> char '-')
    sectorId' <- L.integer
    checksum <- T.pack <$> brackets (many letterChar)

    return $ Room (EncryptedName encryptedName') (SectorId sectorId') (Checksum checksum)

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser () -- space consumer
sc = L.space (void $ char ' ') lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

frequencyToChecksum :: [(Char, Int)] -> Checksum
frequencyToChecksum = Checksum . T.pack . go
  where
    go = take 5 . concatMap (L.sort . map fst) . groupByFrequency
    groupByFrequency = L.groupBy ((==) `on` snd) . filter ((/= '-') . fst) . reverse . L.sortOn snd

frequency :: Ord a => [a] -> [(a, Int)]
frequency = map (head &&& length) . L.group . L.sort
