module Brainfuck (
  interpretCode
  )
where

import Data.IORef
import Data.Char
import System.IO

-- Interprets a brainf**k program given its code and the memory cell count
interpretCode :: String -> Int -> IO ()
interpretCode code memsize = do
  -- Disable console IO buffering to avoid deadlocks
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  -- Clean the code
  let cleanedCode = cleanupCode code

  -- Initialize the memory and interpret the code
  initMemory memsize >>= \mem -> interpretInstruction $ ExecutionState 0 0 mem [] cleanedCode


-- Contains all data associated with the current state of execution
data ExecutionState = ExecutionState {
  ip :: Int,
  dp :: Int,
  mem :: [IORef Int],
  stack :: [Int],
  code :: String
  }


interpretInstruction :: ExecutionState -> IO ()
interpretInstruction state@(ExecutionState ip dp mem stack code) = case code !!? ip of
  Nothing  -> return () -- All code is executed
  Just '+' -> incrementMemory (mem !! dp) >> interpretInstruction (ExecutionState (ip + 1) dp mem stack code)
  Just '-' -> decrementMemory (mem !! dp) >> interpretInstruction (ExecutionState (ip + 1) dp mem stack code)
  Just '>' -> interpretInstruction $ ExecutionState (ip + 1) (dp + 1) mem stack code
  Just '<' -> interpretInstruction $ ExecutionState (ip + 1) (dp - 1) mem stack code
  Just '.' -> writeChar (mem !! dp) >> interpretInstruction (ExecutionState (ip + 1) dp mem stack code)
  Just ',' -> readChar (mem !! dp) >> interpretInstruction (ExecutionState (ip + 1) dp mem stack code)
  Just '[' -> handleWhileChar state >>= \newState -> interpretInstruction newState
  Just ']' -> interpretInstruction $ ExecutionState (head stack) dp mem (tail stack) code
  Just _   -> interpretInstruction $ ExecutionState (ip + 1) dp mem stack code -- Non-language character, is ignored

readChar :: IORef Int -> IO ()
readChar ref = do
  char <- getChar
  writeIORef ref $ ord char

writeChar :: IORef Int -> IO ()
writeChar ref = do
  char <- readIORef ref
  putStr $ chr char : []

incrementMemory :: IORef Int -> IO ()
incrementMemory = addToMemory 1

decrementMemory :: IORef Int -> IO ()
decrementMemory = addToMemory (-1)

addToMemory :: Int -> IORef Int -> IO ()
addToMemory val ref = do
  old <- readIORef ref
  writeIORef ref $ old + val

-- Initializes a fixed amount of IORefs acting as memory cells
initMemory :: Int -> IO [IORef Int]
initMemory cnt = initMemory' cnt $ return [] where
  initMemory' 0 refs = refs
  initMemory' cnt refs = do
    ref <- newIORef 0
    initMemory' (cnt - 1) $ refs >>= \rs -> return $ ref : rs

handleWhileChar :: ExecutionState -> IO ExecutionState
handleWhileChar (ExecutionState ip dp mem stack code) = do
  let closingIndex = matchBracket ip code
  currentVal <- readIORef (head $ drop dp mem)
  return $ if currentVal == 0
           then ExecutionState (closingIndex + 1) dp mem stack code
           else ExecutionState (ip + 1) dp mem (ip : stack) code

matchBracket :: Int -> String -> Int
matchBracket currentIp code = matchBracket' 1 1 $ drop (currentIp + 1) code where
  matchBracket' ip 0 _ = currentIp + ip - 1
  matchBracket' _ _ [] = error "No matching ']' for '['"
  matchBracket' ip open ('[':code) = matchBracket' (ip + 1) (open + 1) code
  matchBracket' ip open (']':code) = matchBracket' (ip + 1) (open - 1) code
  matchBracket' ip open (c:code) = matchBracket' (ip + 1) open code

-- Safe !! Operator
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i = case drop i xs of
  [] -> Nothing
  xs -> Just $ head xs

-- List of interpretable characters
interpretableChars = ['+', '-', '>', '<', '.', ',', '[', ']']

cleanupCode :: String -> String
cleanupCode = filter (`elem` interpretableChars) 
