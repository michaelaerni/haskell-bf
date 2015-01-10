import Brainfuck
import Safe
import System.Environment

handleArgs :: [String] -> Maybe (String, Int)
handleArgs (path:memsize:_) = case (readMay memsize) of
  Nothing -> Nothing
  Just size -> Just (path, size)
handleArgs _ = Nothing

interpretFile :: String -> Int -> IO ()
interpretFile path memsize = readFile path >>= \code -> interpretCode code memsize

main = do
  args <- getArgs
  case handleArgs args of
    Nothing -> putStrLn "Usage: haskell-bf [source] [memsize]"
    Just (file, memsize) -> interpretFile file memsize
