module Mapper where

import Prelude

import Data.Array (difference, filter, nub, sort)
import Data.Maybe (Maybe(..))
import Data.String (contains, drop, joinWith, lastIndexOf, take)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines)
import Data.Traversable (sequence)
-- import Debug (trace)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir)
import Node.Path (FilePath)

hasMD :: String -> Boolean
hasMD = contains (Pattern ".md")

filteredLines :: String -> Array String
filteredLines contents = filter hasMD (lines contents)

extractFileName :: String -> Maybe String
extractFileName line = do
  dropStart <- lastIndexOf (Pattern "(") line
  dropEnd <- lastIndexOf (Pattern ")") line
  trimOne <- pure $ drop (dropStart+1) line
  trimTwo <- pure $ take (dropEnd - dropStart - 1) trimOne
  pure trimTwo

seq :: String -> Maybe (Array String)
seq contents = sequence $ map extractFileName $ filteredLines contents

sortedUniq :: Maybe (Array String) -> Array String
sortedUniq Nothing = []
sortedUniq (Just ls) = sort $ nub ls

arrayToString :: Array String -> String
arrayToString ls = joinWith "\n" ls

getFilesInDir :: FilePath -> Effect (Array FilePath)
getFilesInDir dir = do
  files <- readdir dir
  filtered <- pure $ filter (\x -> contains (Pattern (".md")) x && (not $ contains (Pattern "readme.md")) x) files
  pure filtered

readmePath :: String
readmePath = "../../Dropbox/_notes/readme.md"

dirPath :: String
dirPath = "../../Dropbox/_notes"

main :: Effect Unit
main = do
  a <- readTextFile UTF8 readmePath
  as <- pure $ sortedUniq $ seq a
  filesInDir <- getFilesInDir dirPath
  diff1 <- pure $ difference as filesInDir
  diff2 <- pure $ difference filesInDir as
  -- _ <- trace (length diff) \_ -> pure diff
  log "\nIn readme but not in folder"
  log (arrayToString $ diff1)
  log "\n\nIn folder but not in readme"
  log (arrayToString $ diff2)
