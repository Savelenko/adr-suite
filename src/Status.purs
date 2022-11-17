module Status where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Parsing (Parser, runParser)
import Parsing.String (anyTill, rest, string)
import Template (Template, later, now, run, (&))

{----------------------------------------------------------------------------------------------------------------------}

data Status =
    Pending
  | Accepted

derive instance Generic Status _

instance Show Status where
  show = genericShow

{----------------------------------------------------------------------------------------------------------------------}

-- | A parser which turns the contents of an ADR into a template. When the template is applied to a `Status`, it will
-- | produce the same ADR but including the status.
statusTemplate :: forall r. Parser String (Template r (Status -> r))
statusTemplate = do
  Tuple beforeStatus _ <- anyTill (string "## Status")
  -- TODO: Use `match` with `string`?
  afterStatus <- rest
  let
    template =
      now beforeStatus &
      now "## Status\n" &
      later (\(s :: Status) -> show s) &
      now afterStatus
  pure template

type ADR = String

withStatus :: Status -> ADR -> ADR
withStatus status adr =
  case runParser adr statusTemplate of
    Left _ -> adr
    Right template -> run template status

-- TODO: When setting status to Accepted, remove status Pending, if present.