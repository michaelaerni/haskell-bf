module Brainfuck (
  interpretCode
  )
where

import Data.Char
import NavSeq


-- Interprets a brainf**k program given its code and the user input
interpretCode :: String -> String -> String
interpretCode code input = interpretInstruction startState where
  startState = ExecutionState cleanedCode initialMemory [] input ""
  cleanedCode = createSeq $ cleanupCode code
  initialMemory = createSeq [0]


-- Contains all data associated with the current state of execution
type Memory = NavSeq Int
type Code = NavSeq Char
data ExecutionState = ExecutionState {
  code :: Code,
  mem :: Memory,
  stack :: [Int],
  input :: String,
  output :: String -- Output is stored in reversed order as output is always just appended
  }


interpretInstruction :: ExecutionState -> String
interpretInstruction state = case (isRightEnd.code) state of
  True -> (reverse.output) state
  otherwise -> case (current.code) state of
    '+' -> interpretInstruction $ handlePlus state
    '-' -> interpretInstruction $ handleMinus state
    '>' -> interpretInstruction $ handleMemRight state
    '<' -> interpretInstruction $ handleMemLeft state
    '.' -> interpretInstruction $ handlePoint state
    ',' -> interpretInstruction $ handleComma state
    '[' -> interpretInstruction $ handleOpenLoop state
    ']' -> interpretInstruction $ handleCloseLoop state
    _   -> interpretInstruction $ handleComment state


handlePlus :: ExecutionState -> ExecutionState
handlePlus (ExecutionState code mem stack input output) = ExecutionState (moveRight code) (incrementMemory mem) stack input output

handleMinus :: ExecutionState -> ExecutionState
handleMinus (ExecutionState code mem stack input output) = ExecutionState (moveRight code) (decrementMemory mem) stack input output

handleMemRight :: ExecutionState -> ExecutionState
handleMemRight (ExecutionState code mem stack input output) =
  let newMem = moveRight mem in
  case isRightEnd newMem of
    -- Extend memory to the right if necessary
    True -> ExecutionState (moveRight code) (appendRight 0 newMem) stack input output
    otherwise -> ExecutionState (moveRight code) newMem stack input output

handleMemLeft :: ExecutionState -> ExecutionState
handleMemLeft (ExecutionState code mem stack input output) =
  case isLeftEnd mem of
    True -> error "Negative memory pointer index"
    otherwise -> ExecutionState (moveRight code) (moveLeft mem) stack input output

handlePoint :: ExecutionState -> ExecutionState
handlePoint (ExecutionState code mem stack input output) = ExecutionState (moveRight code) mem stack input newOutput where
  newOutput = (chr $ current mem) : output

handleComma :: ExecutionState -> ExecutionState
handleComma (ExecutionState _ _ _ [] _) = error "Input is read but no input is available anymore"
handleComma (ExecutionState code mem stack (char:input) output) = ExecutionState (moveRight code) (insert (ord char) mem) stack input output

handleComment :: ExecutionState -> ExecutionState
handleComment (ExecutionState code mem stack input output) = ExecutionState (moveRight code) mem stack input output

handleOpenLoop :: ExecutionState -> ExecutionState
handleOpenLoop (ExecutionState code mem stack input output) =
  let closingOffset = matchBracket code in
  if (current mem) == 0
  then ExecutionState (moveRightMany (closingOffset + 1) code) mem stack input output
  else ExecutionState (moveRight code) mem (closingOffset : stack) input output

handleCloseLoop :: ExecutionState -> ExecutionState
handleCloseLoop (ExecutionState _ _ [] _ _) = error "Closing loop character found but no open loop on the stack anymore"
handleCloseLoop (ExecutionState code mem (offset:stack) input output) = ExecutionState (moveLeftMany offset code) mem stack input output


incrementMemory :: Memory -> Memory
incrementMemory = addToMemory 1

decrementMemory :: Memory -> Memory
decrementMemory = addToMemory (-1)

addToMemory :: Int -> Memory -> Memory
addToMemory val mem = insert (current mem + val) mem


matchBracket :: Code -> Int
matchBracket code = matchBracket' (moveRight code) 1 1 where
  matchBracket' code open offset
    | open == 0 = offset - 1
    | isRightEnd code = error "No maching ] for ["
    | current code == '[' = matchBracket' (moveRight code) (open + 1) (offset + 1)
    | current code == ']' = matchBracket' (moveRight code) (open - 1) (offset + 1)
    | otherwise = matchBracket' (moveRight code) open (offset + 1)


applyMany :: Int -> (a -> a) -> a -> a
applyMany c f x = foldr (\f acc -> f acc) x $ replicate c f

moveRightMany :: Int -> NavSeq a -> NavSeq a
moveRightMany count = applyMany count moveRight

moveLeftMany :: Int -> NavSeq a -> NavSeq a
moveLeftMany count = applyMany count moveLeft


cleanupCode :: String -> String
cleanupCode = filter (`elem` interpretableChars) where
  interpretableChars = ['+', '-', '>', '<', '.', ',', '[', ']']
