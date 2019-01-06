module Main where

import Data.Char (isSpace)
import Data.List (isSuffixOf)
import Text.ParserCombinators.ReadP (ReadP, between, string, skipSpaces, munch1, readP_to_S)

main :: IO ()
main = unfoldContent getContents

unfoldContent :: IO String -> IO ()
unfoldContent input = mapM_ lineProc . lines =<< input 

lineProc :: String -> IO ()
lineProc ln = case readP_to_S pFilePath ln of
  [(fp, _)]
    | ".hs" `isSuffixOf` fp -> putStrLn "```haskell" >> (putStr =<< content) >> putStrLn "```"
    | otherwise             -> unfoldContent content
    where
      content = readFile fp
  _                         -> putStrLn ln

pFilePath :: ReadP FilePath
pFilePath = between (string "<!--")
                    (string "-->")
                    (between skipSpaces skipSpaces (munch1 (not . isSpace)))
