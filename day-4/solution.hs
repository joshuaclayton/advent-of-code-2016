#!/usr/bin/env stack
-- stack --install-ghc runghc --package text --package megaparsec

{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow ((&&&))
import           Control.Monad (void)
import qualified Data.Char as C
import           Data.Function (on)
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

newtype EncryptedName = EncryptedName { encryptedName :: Text } deriving Show
newtype DecryptedName = DecryptedName { decryptedName :: Text } deriving Show
newtype SectorId = SectorId { sectorId :: Integer } deriving Show
newtype Checksum = Checksum Text deriving (Show, Eq)

data Room = Room
    { rEncryptedName :: EncryptedName
    , rSectorId :: SectorId
    , rChecksum :: Checksum
    } deriving Show

main :: IO ()
main = either (printError . show) run . parseInput . T.pack =<< getContents

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e

run :: [Room] -> IO ()
run = print . filter (namedNorthPole . fst) . map decryptRoom . filter isValidRoom

namedNorthPole :: DecryptedName -> Bool
namedNorthPole (DecryptedName n) =
    T.isInfixOf "north" n && T.isInfixOf "pole" n

decryptRoom :: Room -> (DecryptedName, Room)
decryptRoom = decryptedName' &&& id
  where
    decryptedName' = uncurry decryptText . (rSectorId &&& rEncryptedName)

decryptText :: SectorId -> EncryptedName -> DecryptedName
decryptText (SectorId i) =
    DecryptedName . T.concatMap (T.singleton . rotateChar (fromInteger i)) . encryptedName

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

rotateChar :: Int -> Char -> Char
rotateChar _ '-' = ' '
rotateChar i c = go c
  where
    go = C.chr . handleOvershift . (+ (i `rem` 26)) . C.ord
    handleOvershift i'
        | i' > 122 = i' - 26
        | otherwise = i'
