module ReadmeGen where

import Prelude

import Data.Array (filter, find, foldr, length, sortBy)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (contains, joinWith, replace, split, toLower)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Utils (lines)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Foreign.Object (fromFoldable, toUnfoldable)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir, writeTextFile)

generateReadmeFile :: Effect Unit
generateReadmeFile = do
  files <- readdir "."
  mdFiles <- pure $ filter (contains (Pattern ".md")) files
  tuples <- mkTuplesFromAllFiles mdFiles
  tagsData <- pure $ generateTagsData tuples
  sortedTagsData <- pure $ sortTagsData tagsData
  contentToWrite <- pure $ formatTagsData sortedTagsData
  writeTextFile UTF8 "readme.md" contentToWrite

initObject :: Object.Object (Array String)
initObject = Object.empty

generateTagsData :: Array (Tuple String (Array String)) -> Object.Object (Array String)
generateTagsData list = foldr u initObject list
  where
    u filetag obj =
      let
        tagsInFile = snd filetag
        filename = fst filetag
      in
        foldr (updateTagData filename) obj tagsInFile

updateTagData :: String -> String -> Object.Object (Array String) -> Object.Object (Array String)
updateTagData file tag obj = 
  let
    val = Object.lookup tag obj
  in
    case val of
      Nothing -> Object.insert tag [file] obj
      Just arr -> Object.insert tag (arr <> [file]) obj

convertTagStringIntoTagsArray :: String -> Array String
convertTagStringIntoTagsArray str = 
  let
    strWithoutTagKeyword = replace (Pattern "tags: ") (Replacement "") str
    tags = split (Pattern ",") $ joinWith "," $ split (Pattern ", ") strWithoutTagKeyword
    filteredTags = filter (\x -> x /= "") tags 
  in
    if length filteredTags == 0
      then ["misc"]
      else filteredTags

type FileName = String

readTagLineFromFile :: FileName -> Effect String
readTagLineFromFile fname = do
  fileContents <- readTextFile UTF8 fname
  linesInFile <- pure $ lines fileContents
  lineWhichContainsTags <- pure $ findTagLine linesInFile
  pure lineWhichContainsTags


findTagLine :: (Array String) -> String
findTagLine linesInFile = 
  let 
    line = find (\x -> contains (Pattern "Tags: ") x || contains (Pattern "tags: ") x) linesInFile
  in
    case line of
      Just l -> toLower l
      Nothing -> ""

mkFilenameTagTuple :: FileName -> String -> Tuple FileName (Array String)
mkFilenameTagTuple fname tagString = 
  Tuple fname $ convertTagStringIntoTagsArray tagString


mkTuplesFromAllFiles :: Array FileName -> Effect (Array (Tuple FileName (Array String)))
mkTuplesFromAllFiles files = sequence $ map fn files
  where
    fn file = do
      tagString <- readTagLineFromFile file
      pure $ mkFilenameTagTuple file tagString

formatTagsData :: Object.Object (Array String) -> String
formatTagsData obj = foldr toTagSection initString (Object.keys obj)
  where
    initString = "# Contents \n\n"
    toTagSection :: String -> String -> String
    toTagSection key text =
      let
        fileList = fromMaybe [] (Object.lookup key obj)
        fileListAsString = joinWith "\n- " $ map (\file -> "[" <> file <> "](" <> file <> ")" ) $ fileList
      in
        text <> "**" <> key <> "**" <> "\n- " <> fileListAsString <> "\n\n"

sortTagsData :: Object.Object (Array String) -> Object.Object (Array String)
sortTagsData obj =
  let
    arrayFromObj = toUnfoldable obj
    sortedArrayFromObj = sortTags arrayFromObj
  in
    fromFoldable sortedArrayFromObj

-- sometuples = [
--   (Tuple "yacht" []),
--   (Tuple "zebra" []),
--   (Tuple "banana" []),
--   (Tuple "apple" [])
-- ]

sortTags :: Array (Tuple String (Array String)) -> Array (Tuple String (Array String))
sortTags [] = []
sortTags xs = sortBy sortFn xs
  where
    sortFn (Tuple a _) (Tuple b _) = if (a < b) then GT else LT