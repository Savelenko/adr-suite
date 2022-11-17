module Main where

import Prelude

import ADR (ADRNumber(..), lastNumber, nextNumber)
import ADR as ADR
import ArgParse.Basic (ArgParser, anyNotFlag, choose, command, flag, flagHelp, flagInfo, fromRecord, optional, parseArgs, printArgError)
import Data.Array (drop, filter)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap, for_)
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (ala)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path as Path
import Node.Process as Process
import Status (Status(..), withStatus)
import Template (run)

applyOptionally :: forall a. Maybe (a -> a) -> a -> a
applyOptionally f = ala Endo foldMap f

main :: Effect Unit
main = do
  arguments <- Process.argv
  directory <- Process.cwd
  let command = parseArgs "adr-suite" "Commands and flags:" argumentParser (drop 2 arguments)
  case command of
    Right (New { title, verbose, accepted }) -> do
      allADRFiles <- filter (Path.extname >>> eq ".md") <$> FS.readdir directory
      let
        newNumber = fromMaybe (ADRNumber 0) $ nextNumber <$> lastNumber allADRFiles
        name = run ADR.nameTemplate newNumber title
        adr = directory <> Path.sep <> name <> ".md"
      adrExists <- FS.exists adr
      when (not adrExists) do
        for_ verbose $ const $ log $ "Creating ADR " <> adr
        let
          contents =
            run ADR.template newNumber title
            # applyOptionally (accepted $> withStatus Accepted)
        FS.writeTextFile UTF8 adr contents
        pure unit
    Right (List { verbose }) -> do
      allADRFiles <- filter (Path.extname >>> eq ".md") <$> FS.readdir directory
      for_ allADRFiles log
      for_ (verbose *> lastNumber allADRFiles) \last ->
        log $ "Next ADR number will be " <> show (nextNumber last)
    Left error ->
      log $ printArgError error

data Action =
    List {      
      verbose :: Maybe Unit
    }
  | New {
      title :: String,
      verbose :: Maybe Unit,
      accepted :: Maybe Unit
    }

argumentParser :: ArgParser Action
argumentParser =
  flagHelp
  *> flagInfo ["-v", "--version"] "Display version" "0.0.0"
  *> choose "command" [
      command ["new", "n", "add"] "Create a new ADR" $
        New <$> fromRecord {
          title : anyNotFlag "TITLE" "ADR title",
          verbose : optional $ flag ["--verbose", "-v"] "Print what happens when creating a new ADR",
          accepted : optional $ flag ["--Accepted", "-A"] "Set ADR status to Accepted"
        },
      command ["list", "l"] "List all MarkDown files" $
        List <$> fromRecord {
          verbose : optional $ flag ["--verbose", "-v"] "Print additional details when listing ADRs"
        }
     ]
