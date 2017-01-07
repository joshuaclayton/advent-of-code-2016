#!/usr/bin/env stack
-- stack --install-ghc runghc --package text --package megaparsec --package mtl

{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (void, mapM_)
import           Control.Monad.State (State, execState, get, put, modify)
import           Data.Bifunctor (first, second)
import           Data.List (genericLength, genericIndex)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

main :: IO ()
main = either (printError . show) run . parseInput . T.pack =<< getContents

parseInput :: Text -> Either ParseError [Instruction]
parseInput = runParser (instructionsParser <* eof) "" . T.strip

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e

run :: [Instruction] -> IO ()
run = print . a . snd . processInstructions buildComputer

data Computer = Computer
    { a :: Register
    , b :: Register
    , c :: Register
    , d :: Register
    } deriving Show

newtype Register = Register Integer deriving Show
newtype Distance = Distance Integer deriving Show
newtype NamedRegister = NamedRegister Char deriving Show
newtype InstructionIndex = InstructionIndex Integer deriving Show

data Value
    = Raw Integer
    | FromRegister NamedRegister
    deriving Show

data Instruction
    = Copy Value NamedRegister
    | IncreaseRegister NamedRegister
    | DecreaseRegister NamedRegister
    | JumpToInstruction Value Distance
    deriving Show

type ProcessInstruction = State (InstructionIndex, Computer)

addIndex :: Integer -> InstructionIndex -> InstructionIndex
addIndex i (InstructionIndex i') = InstructionIndex $ i' + i

processInstructions :: Computer -> [Instruction] -> (InstructionIndex, Computer)
processInstructions c is =
    execState (handleInstructions is) (InstructionIndex 0, c)

handleInstruction :: Instruction -> ProcessInstruction ()
handleInstruction (IncreaseRegister namedRegister) =
    modify (second (incrementRegister namedRegister)) *> incrementInstruction
handleInstruction (DecreaseRegister namedRegister) =
    modify (second (decrementRegister namedRegister)) *> incrementInstruction
handleInstruction (Copy v namedRegister) =
    modify (second (\c' -> updateRegister namedRegister (registerFromValue c' v) c')) *> incrementInstruction
handleInstruction (JumpToInstruction v (Distance d)) = go =<< get
  where
    registerIsZero (Register i) = i == 0
    go (idx, c') =
        if registerIsZero $ registerFromValue c' v
            then incrementInstruction
            else modify (first (addIndex d))

findInstruction :: [Instruction] -> InstructionIndex -> Maybe Instruction
findInstruction is (InstructionIndex i) =
    if i + 1 <= genericLength is
        then Just $ is `genericIndex` i
        else Nothing

handleInstructions :: [Instruction] -> ProcessInstruction ()
handleInstructions [] = return ()
handleInstructions is = do
    (idx, _) <- get
    case findInstruction is idx of
        Just i -> do
            handleInstruction i
            handleInstructions is
        Nothing -> return ()

incrementInstruction :: ProcessInstruction ()
incrementInstruction = modify (first (addIndex 1))

instructionsParser :: Parser [Instruction]
instructionsParser = instructionParser `sepBy1` newline

instructionParser :: Parser Instruction
instructionParser =
    copyInstructionParser
    <|> increaseRegisterParser
    <|> decreaseRegisterParser
    <|> jumpToInstructionParser

copyInstructionParser :: Parser Instruction
copyInstructionParser =
    Copy
    <$> (string "cpy " *> valueParser <* string " ")
    <*> registerParser

increaseRegisterParser :: Parser Instruction
increaseRegisterParser = IncreaseRegister <$> (string "inc " *> registerParser)

decreaseRegisterParser :: Parser Instruction
decreaseRegisterParser = DecreaseRegister <$> (string "dec " *> registerParser)

jumpToInstructionParser :: Parser Instruction
jumpToInstructionParser =
    JumpToInstruction
    <$> (string "jnz " *> valueParser <* string " ")
    <*> distanceParser

valueParser :: Parser Value
valueParser = rawParser <|> fromRegisterParser
  where
    rawParser = Raw <$> L.integer
    fromRegisterParser = FromRegister <$> registerParser

registerParser :: Parser NamedRegister
registerParser = NamedRegister <$> oneOf "abcd"

distanceParser :: Parser Distance
distanceParser = Distance <$> L.signed space L.integer

-- Data construction

newRegister :: Register
newRegister = Register 0

buildComputer :: Computer
buildComputer =
    Computer
        { a = newRegister
        , b = newRegister
        , c = incrementRegister' newRegister
        , d = newRegister
        }

incrementRegister' :: Register -> Register
incrementRegister' (Register i) = Register $ i + 1

decrementRegister' :: Register -> Register
decrementRegister' (Register i) = Register $ i - 1

updateRegister :: NamedRegister -> Register -> Computer -> Computer
updateRegister (NamedRegister 'a') r c' = c' { a = r }
updateRegister (NamedRegister 'b') r c' = c' { b = r }
updateRegister (NamedRegister 'c') r c' = c' { c = r }
updateRegister (NamedRegister 'd') r c' = c' { d = r }

incrementRegister :: NamedRegister -> Computer -> Computer
incrementRegister (NamedRegister 'a') c' = c' { a = incrementRegister' $ a c' }
incrementRegister (NamedRegister 'b') c' = c' { b = incrementRegister' $ b c' }
incrementRegister (NamedRegister 'c') c' = c' { c = incrementRegister' $ c c' }
incrementRegister (NamedRegister 'd') c' = c' { d = incrementRegister' $ d c' }

decrementRegister :: NamedRegister -> Computer -> Computer
decrementRegister (NamedRegister 'a') c' = c' { a = decrementRegister' $ a c' }
decrementRegister (NamedRegister 'b') c' = c' { b = decrementRegister' $ b c' }
decrementRegister (NamedRegister 'c') c' = c' { c = decrementRegister' $ c c' }
decrementRegister (NamedRegister 'd') c' = c' { d = decrementRegister' $ d c' }

registerFromValue :: Computer -> Value -> Register
registerFromValue c' (Raw i) = Register i
registerFromValue c' (FromRegister n) = valueFromRegister n
  where
    valueFromRegister (NamedRegister 'a') = a c'
    valueFromRegister (NamedRegister 'b') = b c'
    valueFromRegister (NamedRegister 'c') = c c'
    valueFromRegister (NamedRegister 'd') = d c'
