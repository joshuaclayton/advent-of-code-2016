#!/usr/bin/env stack
-- stack --install-ghc runghc --package text

{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow ((&&&))
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T

newtype Signal = Signal { signal :: [(Char, Int)] }

main :: IO ()
main = either (printError . show) run . parseInput . T.pack =<< getContents

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e

run :: [Signal] -> IO ()
run = print . map mostFrequentFromSignal

mostFrequentFromSignal :: Signal -> Char
mostFrequentFromSignal = fst . head . L.sortOn snd . signal

parseInput :: Text -> Either Text [Signal]
parseInput = Right . map toSignal . T.transpose . T.lines
  where
    toSignal = Signal . frequency . T.unpack

frequency :: Ord a => [a] -> [(a, Int)]
frequency = map (head &&& length) . L.group . L.sort
