-- | Shared input validation and length units used by every calculator in
-- | `TrainHorn.Calc`.
-- |
-- | Every check returns `Left message` for invalid input instead of throwing,
-- | so callers can show the message to a user or chain checks with `do`
-- | notation in the `Either String` monad.
module TrainHorn.Calc.Validate
  ( LengthUnit(..)
  , parseUnit
  , metersPerFoot
  , toMeters
  , fromMeters
  , finite
  , positive
  , nonNegative
  , log10
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Number as Number
import Data.String (trim, toLower)

-- | A length unit for distances and cable runs: international feet or metres.
data LengthUnit = Feet | Meters

derive instance eqLengthUnit :: Eq LengthUnit

-- | Renders the unit as its short label, `"ft"` or `"m"`.
instance showLengthUnit :: Show LengthUnit where
  show Feet = "ft"
  show Meters = "m"

-- | Parses a unit label. Surrounding spaces and letter case are ignored, so
-- | `"ft"`, `" FT "` and `"m"` are accepted; anything else is a `Left`.
parseUnit :: String -> Either String LengthUnit
parseUnit raw = case toLower (trim raw) of
  "ft" -> Right Feet
  "m" -> Right Meters
  _ -> Left ("unit must be one of ft, m; got " <> show raw)

-- | The exact length of one international foot in metres (0.3048).
metersPerFoot :: Number
metersPerFoot = 0.3048

-- | Converts a length in the given unit to metres.
toMeters :: LengthUnit -> Number -> Number
toMeters Feet v = v * metersPerFoot
toMeters Meters v = v

-- | Converts a length in metres to the given unit.
fromMeters :: LengthUnit -> Number -> Number
fromMeters Feet v = v / metersPerFoot
fromMeters Meters v = v

-- | `Right x` when `x` is finite (not NaN or infinite); the first argument
-- | names the parameter in the error message.
finite :: String -> Number -> Either String Number
finite name v
  | Number.isFinite v = Right v
  | otherwise = Left (name <> " must be finite, got " <> show v)

-- | `Right x` when `x` is finite and greater than 0.
positive :: String -> Number -> Either String Number
positive name v = do
  x <- finite name v
  if x > 0.0 then Right x else Left (name <> " must be greater than 0, got " <> show x)

-- | `Right x` when `x` is finite and 0 or greater.
nonNegative :: String -> Number -> Either String Number
nonNegative name v = do
  x <- finite name v
  if x >= 0.0 then Right x else Left (name <> " must be 0 or greater, got " <> show x)

-- | Base-10 logarithm.
log10 :: Number -> Number
log10 x = Number.log x / Number.ln10

