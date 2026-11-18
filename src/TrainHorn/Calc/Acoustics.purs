-- | Sound pressure level (SPL) versus distance for a horn in open air.
-- |
-- | A horn is treated as a point source in a free field. The level falls by
-- | `20 * log10(d2 / d1)` dB (inverse-square law), with an optional
-- | atmospheric absorption term:
-- |
-- | ```
-- | L(d) = L_ref - 20 * log10(d / d_ref) - alpha * (d - d_ref) / 1000
-- | ```
-- |
-- | where `alpha` is in dB per kilometre and the distances in the absorption
-- | term are in metres.
-- |
-- | Limits: no ground reflection, walls, wind or barriers; not valid very
-- | close to the horn. Results are estimates, not measurements, and are not a
-- | legal noise assessment.
module TrainHorn.Calc.Acoustics
  ( airAbsorptionDbPerKm
  , splAtDistance
  , splAtDistanceWithAbsorption
  , distanceForSpl
  , distanceForSplWithAbsorption
  , distanceRatioLoss
  , doublingLoss
  , combineLevels
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (maximum, sum)
import Data.Maybe (Maybe(..))
import Data.Number (isFinite, pow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import TrainHorn.Calc.Validate (LengthUnit(..), finite, log10, nonNegative, positive, toMeters)

-- | Approximate atmospheric absorption in dB/km at 10 degC and 70 % relative
-- | humidity (an ISO 9613-2 table condition, from the ISO 9613-1 equations),
-- | keyed by octave-band centre frequency in Hz. Train horn chords sit mostly
-- | between about 250 Hz and 600 Hz, where absorption is only 1-2 dB/km.
-- | Rounded, typical values: use them as a rough guide only.
airAbsorptionDbPerKm :: Array (Tuple Int Number)
airAbsorptionDbPerKm =
  [ Tuple 63 0.1
  , Tuple 125 0.4
  , Tuple 250 1.0
  , Tuple 500 1.9
  , Tuple 1000 3.7
  , Tuple 2000 9.7
  , Tuple 4000 32.8
  , Tuple 8000 117.0
  ]

-- | Free-field SPL at `distance` from a level `sourceDb` known at
-- | `refDistance` (same unit for both distances):
-- | `sourceDb - 20 * log10(distance / refDistance)`.
-- |
-- | ```purescript
-- | splAtDistance 150.0 100.0 10.0 == Right 130.0  -- 150 dB at 10 ft -> 130 dB at 100 ft
-- | ```
splAtDistance :: Number -> Number -> Number -> Either String Number
splAtDistance sourceDb distance refDistance =
  splAtDistanceWithAbsorption sourceDb distance refDistance Feet 0.0

-- | SPL at `distance` with optional air absorption in dB/km (`>= 0`, see
-- | `airAbsorptionDbPerKm`). The unit applies to both distances and is used
-- | to convert them to metres for the absorption term.
splAtDistanceWithAbsorption :: Number -> Number -> Number -> LengthUnit -> Number -> Either String Number
splAtDistanceWithAbsorption sourceDb distance refDistance unit absorption = do
  src <- finite "source_db" sourceDb
  d <- positive "distance" distance
  ref <- positive "ref_distance" refDistance
  alpha <- nonNegative "absorption_db_per_km" absorption
  pure (splUnchecked src d ref unit alpha)

splUnchecked :: Number -> Number -> Number -> LengthUnit -> Number -> Number
splUnchecked src d ref unit alpha =
  let
    level = src - 20.0 * log10 (d / ref)
  in
    if alpha == 0.0 then level
    else level - alpha * (toMeters unit d - toMeters unit ref) / 1000.0

-- | Free-field distance at which the level falls to `targetDb`; the inverse
-- | of `splAtDistance`: `refDistance * 10 ^ ((sourceDb - targetDb) / 20)`.
-- | The result is in the same unit as `refDistance`.
distanceForSpl :: Number -> Number -> Number -> Either String Number
distanceForSpl sourceDb targetDb refDistance =
  distanceForSplWithAbsorption sourceDb targetDb refDistance Feet 0.0

-- | Distance at which the level falls to `targetDb` with air absorption.
-- | With absorption there is no elementary closed form, so the root is found
-- | by bisection between the free-field answer and `refDistance` (the level
-- | is strictly decreasing with distance), to a relative width of 1e-12.
distanceForSplWithAbsorption :: Number -> Number -> Number -> LengthUnit -> Number -> Either String Number
distanceForSplWithAbsorption sourceDb targetDb refDistance unit absorption = do
  src <- finite "source_db" sourceDb
  target <- finite "target_db" targetDb
  ref <- positive "ref_distance" refDistance
  alpha <- nonNegative "absorption_db_per_km" absorption
  let freeField = ref * pow 10.0 ((src - target) / 20.0)
  if freeField == 0.0 || not (isFinite freeField) then
    Left "source_db and target_db are too far apart to give a usable distance"
  else if alpha == 0.0 || freeField == ref then
    Right freeField
  else
    Right (bisect src target ref unit alpha (min freeField ref) (max freeField ref) 0)

bisect :: Number -> Number -> Number -> LengthUnit -> Number -> Number -> Number -> Int -> Number
bisect src target ref unit alpha lo hi i
  | i >= 200 = 0.5 * (lo + hi)
  | otherwise =
      let
        mid = 0.5 * (lo + hi)
        Tuple lo' hi' =
          if splUnchecked src mid ref unit alpha - target > 0.0 then Tuple mid hi else Tuple lo mid
      in
        if hi' - lo' <= 1.0e-12 * hi' then 0.5 * (lo' + hi')
        else bisect src target ref unit alpha lo' hi' (i + 1)

-- | Free-field level change in dB when the distance is multiplied by
-- | `ratio` (`> 0`): `20 * log10(ratio)`. A ratio of 10 gives 20 dB.
distanceRatioLoss :: Number -> Either String Number
distanceRatioLoss ratio = do
  r <- positive "ratio" ratio
  pure (20.0 * log10 r)

-- | Free-field loss for each doubling of distance: `20 * log10(2)`, about
-- | 6.02 dB.
doublingLoss :: Number
doublingLoss = 20.0 * log10 2.0

-- | Combines incoherent sources (for example two horns) into one level:
-- | `10 * log10(sum(10 ^ (L_i / 10)))`. Two equal sources add about 3.01 dB,
-- | not double the decibels. An empty array is a `Left`.
combineLevels :: Array Number -> Either String Number
combineLevels levels = do
  values <- traverse (finite "level") levels
  case maximum values of
    Nothing -> Left "combine_levels needs at least one level"
    Just peak ->
      -- Factor out the peak to avoid overflow with very large dB values.
      Right (peak + 10.0 * log10 (sum (map (\v -> pow 10.0 ((v - peak) / 10.0)) values)))

