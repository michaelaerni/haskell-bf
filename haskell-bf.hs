import Brainfuck
import System.Environment

handleArgs :: [String] -> Maybe (String, String)
handleArgs [path, input] = Just (path, input)
handleArgs _ = Nothing

interpretFile :: String -> String -> IO String
interpretFile path input = do
  code <- readFile path
  return $ interpretCode code input

main = do
  args <- getArgs
  case handleArgs args of
    Nothing -> putStrLn "Usage: haskell-bf [source] [input]"
    Just (file, input) -> interpretFile file input >>= putStr
