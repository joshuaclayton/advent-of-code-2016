#!/usr/bin/env stack
-- stack --install-ghc runghc --package text --package megaparsec

import           Control.Monad (void)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

newtype Times = Times Int deriving Show

data Chunk
    = Pure Text
    | Repeated Text Times
    deriving Show

main :: IO ()
main = either (printError . show) run . parseInput . T.pack =<< getContents

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e

run :: [Chunk] -> IO ()
run = print . sum . map chunkLength

chunkLength :: Chunk -> Int
chunkLength c@(Repeated t (Times x)) =
    x * sum (map chunkLength (parseResult t))
chunkLength c@(Pure t) =
    case parseResult t of
      [Pure t'] -> T.length t'
      rs -> sum $ map chunkLength rs

parseResult :: Text -> [Chunk]
parseResult = right . parseInput
  where
    right (Right v) = v

chunkToText :: Chunk -> Text
chunkToText (Pure t) = t
chunkToText (Repeated t (Times i)) = T.concat $ replicate i t

parseInput :: Text -> Either ParseError [Chunk]
parseInput = runParser (encryptedTextParser <* eof) "" . T.strip

encryptedTextParser :: Parser [Chunk]
encryptedTextParser = many $ repeatedChunk <|> pureChunk

pureChunk :: Parser Chunk
pureChunk = Pure . T.pack <$> some (noneOf "(")

repeatedChunk :: Parser Chunk
repeatedChunk = do
    (length', times) <- parens lengthAndRepeat
    parsedText <- T.pack <$> count length' anyChar
    return $ Repeated parsedText times

lengthAndRepeat :: Parser (Int, Times)
lengthAndRepeat = do
    length' <- fromInteger <$> L.integer
    string "x"
    times <- Times . fromInteger <$> L.integer
    return (length', times)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser () -- space consumer
sc = L.space (void $ char ' ') lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"
