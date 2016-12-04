#!/usr/bin/env stack
-- stack --install-ghc runghc --package text --package megaparsec

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Maybe as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

type Commands = [Command]

data Command
    = MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    deriving Show

data Keypad = Keypad
    { kRows :: [KeypadRow]
    , kWidth :: KeypadWidth
    , kHeight :: KeypadHeight
    , kCenter :: KeypadPosition
    } deriving Show

data Player = Player
    { pKeypad :: Keypad
    , pCurrentPosition :: KeypadPosition
    } deriving Show

newtype KeypadWidth = KeypadWidth { getKeyboardWidth :: Int } deriving Show
newtype KeypadHeight = KeypadHeight { getKeyboardHeight :: Int } deriving Show
newtype KeypadColumn = KeypadColumn Integer deriving Show

type KeypadRow = [KeypadColumn]
data KeypadPosition = KeypadPosition Int Int deriving Show

main :: IO ()
main = either (printError . show) run . parseInput . T.pack =<< getContents

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e

createKeypadFromRows :: [KeypadRow] -> Keypad
createKeypadFromRows krs = Keypad krs width height center
  where
    width = KeypadWidth $ length $ head krs
    height = KeypadHeight $ length krs
    center = M.fromMaybe emptyPosition $ keypadCenter width height
    emptyPosition = KeypadPosition 0 0

isValidPosition :: Keypad -> KeypadPosition -> Bool
isValidPosition k (KeypadPosition x y)
    | x < 0 = False
    | y < 0 = False
    | x + 1 > width = False
    | y + 1 > height = False
    | otherwise = True
  where
    width = getKeyboardWidth $ kWidth k
    height = getKeyboardHeight $ kHeight k

modifyPosition :: KeypadPosition -> Command -> KeypadPosition
modifyPosition (KeypadPosition x y) MoveLeft = KeypadPosition (x - 1) y
modifyPosition (KeypadPosition x y) MoveRight = KeypadPosition (x + 1) y
modifyPosition (KeypadPosition x y) MoveUp = KeypadPosition x (y - 1)
modifyPosition (KeypadPosition x y) MoveDown = KeypadPosition x (y + 1)

movePosition :: Keypad -> KeypadPosition -> Command -> KeypadPosition
movePosition k pos cmd =
    if isValidPosition k newPosition
        then newPosition
        else pos
  where
    newPosition = modifyPosition pos cmd

keypadCenter :: KeypadWidth -> KeypadHeight -> Maybe KeypadPosition
keypadCenter (KeypadWidth w) (KeypadHeight h) =
    case (odd w, odd h) of
        (True, True) -> Just $ KeypadPosition (positionOnCoordinates w) (positionOnCoordinates h)
        (_, _) -> Nothing
  where
    positionOnCoordinates v = round $ (fromIntegral (v + 1)/2) - 1

keypadColumnAtPosition :: Keypad -> KeypadPosition -> Either Text KeypadColumn
keypadColumnAtPosition k (KeypadPosition x y) = Right $ kRows k !! y !! x

run :: [Commands] -> IO ()
run commands = do
    print commands
    let (Right keypad) = parseKeypad "1 2 3\n4 5 6\n7 8 9"
    let player = Player keypad (kCenter keypad)
    let finalPositions = snd $ foldl go (player, []) commands
    print $ map (keypadColumnAtPosition keypad) finalPositions
  where
    go (player, positions) cmds = (newPlayer, positions ++ [newPosition])
      where
        newPlayer = player { pCurrentPosition = newPosition }
        newPosition = foldl (movePosition (pKeypad player)) (pCurrentPosition player) cmds

parseInput :: Text -> Either ParseError [Commands]
parseInput = runParser (commandsParser <* eof) "" . T.strip

commandsParser :: Parser [Commands]
commandsParser = commandParser `sepBy1` newline

commandParser :: Parser Commands
commandParser = many $ upParser <|> downParser <|> leftParser <|> rightParser
  where
    upParser = string "U" *> pure MoveUp
    downParser = string "D" *> pure MoveDown
    leftParser = string "L" *> pure MoveLeft
    rightParser = string "R" *> pure MoveRight

parseKeypad :: Text -> Either ParseError Keypad
parseKeypad = runParser (keypadParser <* eof) ""

keypadParser :: Parser Keypad
keypadParser = createKeypadFromRows <$> keypadRowParser `sepBy1` newline

keypadRowParser :: Parser KeypadRow
keypadRowParser = keypadColumnParser `sepBy1` string " "

keypadColumnParser :: Parser KeypadColumn
keypadColumnParser = KeypadColumn <$> L.integer
