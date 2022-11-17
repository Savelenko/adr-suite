module ADR where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Traversable (maximum, sequence)
import Template (Template, later, now, (&))
import Node.Path (FilePath)
import Parsing (runParser)
import Parsing.String.Basic (intDecimal)

{----------------------------------------------------------------------------------------------------------------------}

newtype ADRNumber = ADRNumber Int

derive instance Newtype ADRNumber _

instance Show ADRNumber where
  show (ADRNumber number) = padded where
    padded | number < 10 = "000" <> show number -- e.g. 0003
    padded | number < 100 = "00" <> show number
    padded | number < 1000 = "0" <> show number
    padded | otherwise = show number

nextNumber :: ADRNumber -> ADRNumber
nextNumber = over ADRNumber (_ + 1)

lastNumber :: Array FilePath -> Maybe ADRNumber
lastNumber files =
  case files <#> flip runParser intDecimal # filter isRight # sequence of
    Left _ -> Nothing -- should not be the case dure to `filter isRight` above 
    Right [] -> Nothing
    Right numbers -> ADRNumber <$> maximum numbers

{----------------------------------------------------------------------------------------------------------------------}

nameTemplate :: forall r. Template r (ADRNumber -> String -> r)
nameTemplate =
  later (\(number :: ADRNumber) -> show number) & now " - " & later identity

template :: forall r. Template r (ADRNumber -> String -> r)
template =
  now "# " & nameTemplate & now "\n" &
  now "\n" &
  now "Date: today\n" &
  now "\n" &
  now "## Status\n" &
  now "\n" &
  now "Pending\n" &
  now "\n" &
  now "## Context\n" &
  now "\n" &
  now "## Decision\n" &
  now "\n" &
  now "## Consequences\n" &
  now "\n"

-- IDEA: Is it possible to define a PS-specific "holey monoid" which "builds" a record parameter instead of a curried
-- function?