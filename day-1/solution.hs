#!/usr/bin/env stack
-- stack --install-ghc runghc --package text --package megaparsec

import           Control.Monad (void)
import qualified Data.Bifunctor as BF
import           Data.Scientific (toRealFloat)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

main :: IO ()
main = either printError run . parseInput . T.pack =<< getContents

data Direction
    = North
    | South
    | East
    | West
    deriving Show

data Position = Position Integer Integer deriving (Show, Eq)

data Movement
    = MoveLeft Integer
    | MoveRight Integer
    deriving Show

data Player = Player
    { direction :: Direction
    , positions :: [Position]
    , movement :: Maybe Movement
    } deriving Show

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e

run :: [Movement] -> IO ()
run ms = do
    putStrLn $ "Distance: " ++ show (distanceOf playerAfterMovement)
    putStrLn $ "First Repeated Position Distance: " ++ show (repeatedPositionDistanceOf playerAfterMovement)
  where
    distanceOf = manhattanDistanceFromOrigin . last . positions
    repeatedPositionDistanceOf = fmap manhattanDistanceFromOrigin . firstRepeated . positions
    playerAfterMovement = foldl movePlayer initialPlayer ms

manhattanDistanceFromOrigin :: Position -> Integer
manhattanDistanceFromOrigin (Position x y) = abs x + abs y

firstRepeated :: Eq a => [a] -> Maybe a
firstRepeated = go []
  where
    go _ [] = Nothing
    go visited (x:xs') =
        if x `elem` visited
            then Just x
            else go xs' (visited ++ [x])

initialPlayer :: Player
initialPlayer = Player North [Position 0 0] Nothing

movePlayer :: Player -> Movement -> Player
movePlayer p m = p { direction = newDirection, positions = positions p ++ newPositions, movement = Just m }
  where
    newDirection = calculateNewDirection (direction p) m
    newPositions = calculateNewPosition currentPosition newDirection m
    currentPosition = last $ positions p

calculateNewPosition :: Position -> Direction -> Movement -> [Position]
calculateNewPosition (Position startX startY) d m =
    case d of
        North -> [ Position startX (startY + i) | i <- [1..distance] ]
        South -> [ Position startX (startY - i) | i <- [1..distance] ]
        East ->  [ Position (startX + i) startY | i <- [1..distance] ]
        West ->  [ Position (startX - i) startY | i <- [1..distance] ]
  where
    distance = movementDistance m

calculateNewDirection :: Direction -> Movement -> Direction
calculateNewDirection North (MoveLeft _)  = West
calculateNewDirection North (MoveRight _) = East
calculateNewDirection South (MoveLeft _)  = East
calculateNewDirection South (MoveRight _) = West
calculateNewDirection East  (MoveLeft _)  = North
calculateNewDirection East  (MoveRight _) = South
calculateNewDirection West  (MoveLeft _)  = South
calculateNewDirection West  (MoveRight _) = North

movementDistance :: Movement -> Integer
movementDistance (MoveLeft i) = i
movementDistance (MoveRight i) = i

parseInput :: Text -> Either String [Movement]
parseInput = parseOnly (movementsParser <* eof) . T.strip

movementsParser :: Parser [Movement]
movementsParser = movementParser `sepBy1` string ", "

movementParser :: Parser Movement
movementParser = parseLeft <|> parseRight
  where
    parseLeft = MoveLeft <$> (string "L" *> L.integer)
    parseRight = MoveRight <$> (string "R" *> L.integer)

parseOnly :: Parser a -> Text -> Either String a
parseOnly p = BF.first show . runParser p ""
