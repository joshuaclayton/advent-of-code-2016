#!/usr/bin/env stack
-- stack --install-ghc runghc --package text --package megaparsec

{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as B16
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype DoorId = DoorId { doorId :: Text } deriving Show
newtype HashResult = HashResult { hashResult :: Text } deriving Show

main :: IO ()
main = either (printError . show) run . parseInput . T.pack =<< getContents

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e

run :: DoorId -> IO ()
run = print . filterToResult . M.catMaybes . generatePasswordCharacters

filterToResult :: [(Int, Char)] -> String
filterToResult = go (map (const Nothing) [1..8])
  where
    go pw vs
        | all M.isJust pw = M.catMaybes pw
        | otherwise = populateList pw vs
    populateList pw (v:vs) = go (updatePasswordList pw v) vs
    updatePasswordList pw (i, c) =
        if M.isJust $ pw !! i
            then pw
            else (element i .~ Just c) pw

generatePasswordCharacters :: DoorId -> [Maybe (Int, Char)]
generatePasswordCharacters (DoorId t) = map go [0..]
  where
    go = nextCharacterInPassword . hashText . combine
    combine = T.append t . T.pack . show

nextCharacterInPassword :: HashResult -> Maybe (Int, Char)
nextCharacterInPassword (HashResult t) =
    if T.isPrefixOf "00000" t && position `elem` validPositions
        then Just (C.digitToInt position, value)
        else Nothing
  where
    position = T.index t 5
    value = T.index t 6
    validPositions = "01234567" :: String

hashText :: Text -> HashResult
hashText = HashResult . T.decodeUtf8 . B16.encode . MD5.hash . T.encodeUtf8

parseInput :: Text -> Either Text DoorId
parseInput = Right . DoorId . T.strip
