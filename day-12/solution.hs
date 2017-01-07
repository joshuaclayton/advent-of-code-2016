#!/usr/bin/env stack
-- stack --install-ghc runghc --package text --package megaparsec --package mtl

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Control.Monad (void, mapM_)
import           Control.Monad.State (State, execState, get, put, modify)
import           Data.Bifunctor (first, second)
import           Data.List (genericLength, genericIndex)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

data Computer = Computer
    { _a :: Register
    , _b :: Register
    , _c :: Register
    , _d :: Register
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

makeLenses ''Computer

main :: IO ()
main = either (printError . show) run . parseInput . T.pack =<< getContents

parseInput :: Text -> Either ParseError [Instruction]
parseInput = runParser (instructionsParser <* eof) "" . T.strip

printError :: String -> IO ()
printError e = do
    putStrLn "There was a problem parsing the input:\n"
    putStrLn e

run :: [Instruction] -> IO ()
run = print . view a . snd . processInstructions buildComputer

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
    modify (second (\c' -> updateRegister namedRegister (registerFromValue v c') c')) *> incrementInstruction
handleInstruction (JumpToInstruction v (Distance d)) = go =<< get
  where
    registerIsZero (Register i) = i == 0
    go (idx, c') =
        if registerIsZero $ registerFromValue v c'
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
        { _a = newRegister
        , _b = newRegister
        , _c = incrementRegister' newRegister
        , _d = newRegister
        }

incrementRegister' :: Register -> Register
incrementRegister' (Register i) = Register $ i + 1

decrementRegister' :: Register -> Register
decrementRegister' (Register i) = Register $ i - 1

updateRegister :: NamedRegister -> Register -> Computer -> Computer
updateRegister namedRegister r = findLens namedRegister .~ r

incrementRegister :: NamedRegister -> Computer -> Computer
incrementRegister r = over (findLens r) incrementRegister'

decrementRegister :: NamedRegister -> Computer -> Computer
decrementRegister r = over (findLens r) decrementRegister'

registerFromValue :: Value -> Computer -> Register
registerFromValue (Raw i) = const $ Register i
registerFromValue (FromRegister n) = view (findLens n)

findLens (NamedRegister 'a') = a
findLens (NamedRegister 'b') = b
findLens (NamedRegister 'c') = c
findLens (NamedRegister 'd') = d
