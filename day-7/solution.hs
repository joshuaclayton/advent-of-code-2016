#!/usr/bin/env stack
-- stack --install-ghc runghc --package text --package megaparsec

{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (void)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

data IPv7Sequence
    = NormalSequence Text
    | HypernetSequence Text
    deriving Show

data IPv7 = IPv7 [IPv7Sequence]

newtype ABBA = ABBA Text

main :: IO ()
main = either (printError . show) run . parseInput . T.pack =<< getContents

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e

run :: [IPv7] -> IO ()
run = print . length . filter doesIPSupportTls

isNormalSequence :: IPv7Sequence -> Bool
isNormalSequence (NormalSequence _) = True
isNormalSequence _ = False

normalSequences :: [IPv7Sequence] -> [IPv7Sequence]
normalSequences = filter isNormalSequence

hypernetSequences :: [IPv7Sequence] -> [IPv7Sequence]
hypernetSequences = filter (not . isNormalSequence)

doesIPSupportTls :: IPv7 -> Bool
doesIPSupportTls (IPv7 sequences) = anyNormalSequencesHaveABBA sequences && noHypernetSequencesHaveABBA sequences
  where
    anyNormalSequencesHaveABBA = any doesSequenceIncludeABBA . normalSequences
    noHypernetSequencesHaveABBA = all (not . doesSequenceIncludeABBA) . hypernetSequences

sequenceValue :: IPv7Sequence -> Text
sequenceValue (NormalSequence t) = t
sequenceValue (HypernetSequence t) = t

doesSequenceIncludeABBA :: IPv7Sequence -> Bool
doesSequenceIncludeABBA s = any (\(ABBA t) -> T.isInfixOf t (sequenceValue s)) allAbbas

allAbbas :: [ABBA]
allAbbas = go allCombinations
  where
    go = map (ABBA . T.pack) . filter (not . sameChar)
    sameChar (a:b:_) = a == b
    allCombinations = [ [a,b,b,a] | a <- ['a'..'z'], b <- ['a'..'z'] ]

----------------------------------------------

parseInput :: Text -> Either ParseError [IPv7]
parseInput = runParser (ipV7sParser <* eof) "" . T.strip

ipV7sParser :: Parser [IPv7]
ipV7sParser = ipV7parser `sepBy1` newline

ipV7parser :: Parser IPv7
ipV7parser = IPv7 <$> many (try hypernetSequenceParser <|> normalSequenceParser)

hypernetSequenceParser :: Parser IPv7Sequence
hypernetSequenceParser = (HypernetSequence . T.pack) <$> brackets (some alphaNumChar)

normalSequenceParser :: Parser IPv7Sequence
normalSequenceParser = NormalSequence . T.pack <$> some alphaNumChar

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser () -- space consumer
sc = L.space (void $ char ' ') lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"