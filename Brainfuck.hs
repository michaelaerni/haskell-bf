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
  initMemory memsize (return []) >>= \mem -> interpretInstruction 0 0 mem [] cleanedCode


interpretInstruction :: Int -> Int -> [IORef Int] -> [Int] -> String -> IO ()
interpretInstruction ip dp mem stack code = case code !!? ip of
  Nothing -> return ()
  Just '+' -> incrementMemory (mem !! dp) >> interpretInstruction (ip + 1) dp mem stack code
  Just '-' -> decrementMemory (mem !! dp) >> interpretInstruction (ip + 1) dp mem stack code
  Just '>' -> interpretInstruction (ip + 1) (dp + 1) mem stack code
  Just '<' -> interpretInstruction (ip + 1) (dp - 1) mem stack code
  Just '.' -> writeChar (mem !! dp) >> interpretInstruction (ip + 1) dp mem stack code
  Just ',' -> readChar (mem !! dp) >> interpretInstruction (ip + 1) dp mem stack code
  Just '[' -> handleWhileChar ip dp mem stack code
  Just ']' -> interpretInstruction (head stack) dp mem (tail stack) code
  Just _   -> interpretInstruction (ip + 1) dp mem stack code -- Non-language character, is ignored

readChar :: IORef Int -> IO ()
readChar ref = do
  chr <- getChar
  writeIORef ref $ ord chr

writeChar :: IORef Int -> IO ()
writeChar ref = do
  char <- readIORef ref
  putStr $ chr char : []

incrementMemory :: IORef Int -> IO ()
incrementMemory ref = do
  val <- readIORef ref
  writeIORef ref $ val + 1

decrementMemory :: IORef Int -> IO ()
decrementMemory ref = do
  val <- readIORef ref
  writeIORef ref $ val - 1

-- Initializes a fixed amount of IORefs acting as memory cells
initMemory :: Int -> IO [IORef Int] -> IO [IORef Int]
initMemory 0 refs = refs
initMemory cnt refs = do
  ref <- newIORef 0
  initMemory (cnt - 1) $ refs >>= \rs -> return $ ref : rs

handleWhileChar :: Int -> Int -> [IORef Int] -> [Int] -> String -> IO ()
handleWhileChar ip dp mem stack code = do
  let closingIndex = matchBracket ip code
  currentVal <- readIORef (head $ drop dp mem)
  if currentVal == 0 then interpretInstruction (closingIndex + 1) dp mem stack code else interpretInstruction (ip + 1) dp mem (ip : stack) code

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
