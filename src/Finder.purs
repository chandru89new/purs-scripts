module Finder (searchForFiles) where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.String (Pattern(..),joinWith, split)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Buffer (toString)
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(..))
import Node.Process (argv)

findByKeyword :: Effect Unit
findByKeyword = do
  keyWord <- getKeywordFromProcess
  case keyWord of
    Nothing -> (log "No keyword given")
    Just a -> searchForFiles a

getKeywordFromProcess :: Effect (Maybe String)
getKeywordFromProcess = do
  arguments <- argv
  pure (arguments !! 2)


searchForFiles :: String -> Effect Unit
searchForFiles searchString = 
  let
    command = "grep -r . --exclude-dir=.git -e " <> searchString
  in
    do
      bufEither <- try $ execSync command defaultExecSyncOptions
      bufString <- case bufEither of
        Right bf -> toString UTF8 bf
        Left _ -> pure $ "No result found"
      formatted <- pure $ formatLines bufString
      log formatted

formatLines :: String -> String
formatLines str =
  joinWith "\n\n" $ split (Pattern "\n") str