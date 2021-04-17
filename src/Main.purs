module Main where

import Prelude

import Data.Array (splitAt)
import Effect (Effect)
import Effect.Console (log)
import Finder (searchForFiles)
import Node.Process (argv)
import ReadmeGen (generateReadmeFile)

data Command = Find String | GenerateReadme | InvalidCommand

getCommand :: Array String -> Command
getCommand ["find", keyword] = Find keyword
getCommand ["generate"] = GenerateReadme
getCommand _ = InvalidCommand

process :: Command -> Effect Unit
process (Find keyword) = searchForFiles keyword
process GenerateReadme = generateReadmeFile
process InvalidCommand = log "Invalid command. Try 'find $keyword' or 'generate'"

stripArgs :: Array String -> Array String
stripArgs xs = (splitAt 2 xs).after

main :: Effect Unit
main = do
  allArguments <- argv
  strippedArgs <- pure $ stripArgs allArguments
  command <- pure $ getCommand strippedArgs
  process command
