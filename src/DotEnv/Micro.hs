-- | Tiny library for handling environment variables stored in @.env@ files.
--
-- == File format
--
-- @.env@ files are plain text UTF-8 files with rows of the form:
--
-- @
-- KEY=VALUE
-- @
--
-- The rows are separated by newline and the @KEY@s must not contain any equal sign.
--
-- The @VALUE@ strings on the other hand can contain equal signs (the string is ingested up to the newline).
--
-- NB: Currently this library does /not/ support variables.
--
-- == Important
--
-- Add the paths of your @.env@ files to the @.gitignore@ file or equivalent, so that they are not checked into source control.
module DotEnv.Micro (loadDotEnv) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Ord (Down(..))
import System.Environment (lookupEnv, setEnv)
import qualified Text.ParserCombinators.ReadP as P (ReadP, readP_to_S, char, munch, sepBy1)

-- directory
import System.Directory (doesFileExist)

-- | Load, parse and apply a @.env@ file
--
-- NB : does not overwrite any preexisting env vars.
--
-- NB2 : if the given @.env@ file is not found or cannot be parsed the program crashes with @fail@.
loadDotEnv :: MonadIO m =>
              Maybe FilePath -- ^ defaults to @.env@ in the Cabal project base directory if Nothing.
           -> m ()
loadDotEnv mfp = liftIO $ do
  let
    fpath = fromMaybe ".env" mfp
  ok <- doesFileExist fpath
  if ok
    then
    do
      mp <- parseDotEnv <$> readFile fpath
      case mp of
        Just es -> setEnvs es
        Nothing -> fail $ unwords ["dotenv: cannot parse", fpath]
    else
    do
      fail $ unwords ["dotenv:", fpath, "file not found"]

setEnvs :: MonadIO m => [(String, String)] -> m ()
setEnvs = traverse_ insf
  where
    insf (k, v) = liftIO $ do
      me <- lookupEnv k
      case me of
        Just _ -> do
          putStrLn $ unwords ["Variable", k, "already set in environment"]
        Nothing -> do
          setEnv k v
          putStrLn $ unwords ["dotenv:", k, "set"] -- DEBUG

parseDotEnv :: String -- ^ contents of the @.env@ file
            -> Maybe [(String, String)]
parseDotEnv = parse1 keyValues

keyValues :: P.ReadP [(String, String)]
keyValues = P.sepBy1 keyValue (P.char '\n') <* P.char '\n'

keyValue :: P.ReadP (String, String)
keyValue = do
  k <- keyP
  void $ P.char '='
  v <- valueP
  pure (k, v)

keyP, valueP :: P.ReadP String
keyP = P.munch (/= '=')
valueP = P.munch (/= '\n')

-- parse :: P.ReadP b -> String -> Maybe b
-- parse p str = fst <$> (listToMaybe $ P.readP_to_S p str)

parse1 :: Foldable t => P.ReadP (t a) -> String -> Maybe (t a)
parse1 p str = fmap fst $ listToMaybe $ sortOn (Down . length . fst) (P.readP_to_S p str)
