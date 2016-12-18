#!/usr/bin/env stack
-- stack --install-ghc runghc --package text --package megaparsec --package containers

import           Control.Arrow ((&&&))
import           Control.Monad (void)
import           Data.List (elemIndex)
import qualified Data.List as Li
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Debug.Trace as DT
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

newtype Value = Value Int deriving (Eq, Ord, Show)
newtype BotId = BotId Int deriving (Eq, Ord, Show)
newtype OutputId = OutputId Int deriving (Eq, Ord, Show)
newtype LowRecipient = LowRecipient { lowRecipient :: Recipient } deriving Show
newtype HighRecipient = HighRecipient { highRecipient :: Recipient } deriving Show

data Recipient
    = BotRecipient BotId
    | OutputRecipient OutputId
    deriving Show

data Instruction
    = AssignValue Value BotId
    | AssignRecipient BotId LowRecipient HighRecipient
    deriving Show

data Bot = Bot
    { bId :: BotId
    , bValues :: [Value]
    , bLowRecipient :: Maybe LowRecipient
    , bHighRecipient :: Maybe HighRecipient
    } deriving Show

data Output = Output
    { oId :: OutputId
    , oValues :: [Value]
    } deriving Show

type Bots = Map BotId Bot
type Outputs = Map OutputId Output

data World = World
    { wBots :: Bots
    , wOutputs :: Outputs
    } deriving Show

assignValues :: [Instruction] -> [Instruction]
assignValues = filter isAssignValue

assignRecipients :: [Instruction] -> [Instruction]
assignRecipients = filter (not . isAssignValue)

isAssignValue :: Instruction -> Bool
isAssignValue AssignValue{} = True
isAssignValue _ = False

worldsEnd :: World -> Bool
worldsEnd = not . M.null . worldStopCondition

worldStopCondition :: World -> Bots
worldStopCondition = M.filter (\Bot{ bValues = vs } -> [Value 17, Value 61] == Li.sort vs) . wBots

reevaluateWorld :: World -> World
reevaluateWorld w =
    case (length botsToReevaluate, worldsEnd w) of
        (0, _) -> w
        (_, True) -> w
        (_, _) -> reevaluateWorld $ foldl go w botsToReevaluate
  where
    botsToReevaluate = M.toList $ M.filter canReevaluateBot (wBots w)
    go w' (bId', b) = reassignHigh (reassignLow (clearBotValues w' b) lowValue b) highValue b
      where
        (lowValue, highValue) = ((!! 0) &&& (!! 1)) $ Li.sort $ bValues b

reassignLow :: World -> Value -> Bot -> World
reassignLow w _ Bot{ bLowRecipient = Nothing } = w
reassignLow w v Bot{ bLowRecipient = Just (LowRecipient r) } = repositionRecipient w v r

reassignHigh :: World -> Value -> Bot -> World
reassignHigh w _ Bot{ bHighRecipient = Nothing } = w
reassignHigh w v Bot{ bHighRecipient = Just (HighRecipient r) } = repositionRecipient w v r

clearBotValues :: World -> Bot -> World
clearBotValues w Bot{ bId = bId' } = w { wBots = newBots }
  where
    newBots = M.update (\b -> Just $ b { bValues = [] }) bId' (wBots w)

repositionRecipient :: World -> Value -> Recipient -> World
repositionRecipient w v (BotRecipient bId') = assignValue w v bId'
repositionRecipient w v (OutputRecipient oId') = assignOutput w v oId'

canReevaluateBot :: Bot -> Bool
canReevaluateBot = uncurry (&&) . (hasTwoValues &&& hasRecipients)
  where
    hasTwoValues = (== 2) . length . bValues
    hasRecipients Bot{ bLowRecipient = Just _, bHighRecipient = Just _ } = True
    hasRecipients _ = False

assignValue :: World -> Value -> BotId -> World
assignValue w v bId' = w { wBots = newBots }
  where
    newBots = M.update (\b -> Just $ b { bValues = v : bValues b }) bId' (wBots w)

assignOutput :: World -> Value -> OutputId -> World
assignOutput w v oId' = w { wOutputs = newOutputs }
  where
    newOutputs = M.update (\o -> Just $ o { oValues = v : oValues o }) oId' (wOutputs w)

assignAllValuesInWorld :: World -> [Instruction] -> World
assignAllValuesInWorld w = foldl applyAssignment w . assignValues
  where
    applyAssignment w' (AssignValue v bId') = assignValue w' v bId'

buildWorld :: [Instruction] -> World
buildWorld = foldl applyInstruction (World M.empty M.empty) . assignRecipients
  where
    applyInstruction w (AssignRecipient bId' l@(LowRecipient lr) h@(HighRecipient hr)) =
        createOrUpdateBot worldWithProcessedLowAndHigh bId' l h
      where
        worldWithProcessedLowAndHigh = processRecipient lr $ processRecipient hr w

createOrUpdateBot :: World -> BotId -> LowRecipient -> HighRecipient -> World
createOrUpdateBot w bId' lr hr =
    w { wBots = M.insert bId' (Bot bId' [] (Just lr) (Just hr)) (wBots w) }

processRecipient :: Recipient -> World -> World
processRecipient (BotRecipient bId') w =
    w { wBots = M.insertWith go bId' (buildBot bId') (wBots w) }
  where
    go old@Bot{ bLowRecipient = low, bHighRecipient = high } new =
        case (low, high) of
            (Nothing, Nothing) -> new
            _ -> old
processRecipient (OutputRecipient oId') w =
    w { wOutputs = M.insert oId' (buildOutput oId') (wOutputs w) }

buildBot :: BotId -> Bot
buildBot bId' = Bot
    { bId = bId'
    , bValues = []
    , bLowRecipient = Nothing
    , bHighRecipient = Nothing
    }

buildOutput :: OutputId -> Output
buildOutput oId' = Output
    { oId = oId'
    , oValues = []
    }

main :: IO ()
main = either (printError . show) run . parseInput . T.pack =<< getContents

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e

run :: [Instruction] -> IO ()
run is = print $ worldStopCondition $ reevaluateWorld $ assignAllValuesInWorld (buildWorld is) is

parseInput :: Text -> Either ParseError [Instruction]
parseInput = runParser (instructionsParser <* eof) "" . T.strip

instructionsParser :: Parser [Instruction]
instructionsParser = instructionParser `sepBy1` newline

instructionParser :: Parser Instruction
instructionParser = assignValueParser <|> assignRecipientParser

assignValueParser :: Parser Instruction
assignValueParser =
    AssignValue
    <$> (valueParser <* string " goes to ")
    <*> botIdParser

assignRecipientParser :: Parser Instruction
assignRecipientParser =
    AssignRecipient
    <$> (botIdParser <* string " gives ")
    <*> (lowRecipientParser <* string " and ")
    <*> highRecipientParser

lowRecipientParser :: Parser LowRecipient
lowRecipientParser = LowRecipient <$> (string "low to " *> recipientParser)

highRecipientParser :: Parser HighRecipient
highRecipientParser = HighRecipient <$> (string "high to " *> recipientParser)

recipientParser :: Parser Recipient
recipientParser =
    try (BotRecipient <$> botIdParser)
    <|> OutputRecipient <$> outputIdParser

valueParser :: Parser Value
valueParser = Value . fromInteger <$> (string "value " *> L.integer)

botIdParser :: Parser BotId
botIdParser = BotId . fromInteger <$> (string "bot " *> L.integer)

outputIdParser :: Parser OutputId
outputIdParser = OutputId . fromInteger <$> (string "output " *> L.integer)
