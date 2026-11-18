-- | Copper wire gauge (AWG) selection for 12 V / 24 V horn and compressor
-- | circuits.
-- |
-- | Resistances are computed from the AWG definition, not typed in from a
-- | table:
-- |
-- | * diameter (ASTM B258): `d(n) = 0.005 in * 92 ^ ((36 - n) / 39)`, where
-- |   1/0, 2/0, 3/0, 4/0 are n = 0, -1, -2, -3;
-- | * annealed copper at 20 degC (100 % IACS): `rho = 1.7241e-8 ohm*m`;
-- | * `R = rho * L / A`, with `R_T = R_20 * (1 + 0.00393 * (T - 20))`.
-- |
-- | The results agree with published solid-copper tables to within about 1 %
-- | (10 AWG is 0.999 ohm per 1000 ft). Current flows out and back, so
-- | `V_drop = I * R_per_length * 2 * one_way_length`.
-- |
-- | Scope: this suggests the smallest gauge that meets a voltage-drop budget.
-- | It does not check ampacity or fuse sizing; always fuse to protect the
-- | wire you choose.
module TrainHorn.Calc.Electrical
  ( awgSizes
  , copperResistivityOhmM
  , copperTempCoeffPerC
  , awgDiameterMM
  , awgAreaMM2
  , awgResistanceOhmPer1000ft
  , voltageDrop
  , WireSuggestion
  , suggestAwg
  ) where

import Prelude

import Data.Array (findMap)
import Data.Either (Either(..))
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (pi, pow)
import Data.String (Pattern(..), Replacement(..), replaceAll, toUpper, trim)
import Data.Traversable (traverse)
import TrainHorn.Calc.Validate (LengthUnit, finite, metersPerFoot, positive, toMeters)

-- | Annealed copper resistivity, 100 % IACS, at 20 degC, in ohm*m.
copperResistivityOhmM :: Number
copperResistivityOhmM = 1.7241e-8

-- | Temperature coefficient of resistance of annealed copper near 20 degC.
copperTempCoeffPerC :: Number
copperTempCoeffPerC = 0.00393

-- | Gauges considered by `suggestAwg`, thinnest to thickest: the sizes
-- | commonly sold as automotive primary wire and battery cable. Odd sizes
-- | still work in the other functions.
awgSizes :: Array String
awgSizes = [ "18", "16", "14", "12", "10", "8", "6", "4", "2", "1", "1/0", "2/0", "3/0", "4/0" ]

awgNumber :: String -> Either String Int
awgNumber awg =
  let
    text = trim (replaceAll (Pattern "AWG") (Replacement "") (toUpper (trim awg)))
  in
    case text of
      "1/0" -> Right 0
      "0" -> Right 0
      "2/0" -> Right (-1)
      "00" -> Right (-1)
      "3/0" -> Right (-2)
      "000" -> Right (-2)
      "4/0" -> Right (-3)
      "0000" -> Right (-3)
      _ -> case fromString text of
        Nothing -> Left ("unknown AWG size " <> show awg <> "; use 1-40 or 1/0..4/0")
        Just n
          | n < 1 || n > 40 -> Left ("AWG size must be between 4/0 and 40, got " <> show awg)
          | otherwise -> Right n

-- | Bare conductor diameter in millimetres of a solid AWG size. The label
-- | may be `"10"`, `"10 AWG"`, `"6awg"`, `"1/0"`, `"00"`, `"0000"`, etc.
awgDiameterMM :: String -> Either String Number
awgDiameterMM awg = do
  n <- awgNumber awg
  pure (0.127 * pow 92.0 (toNumber (36 - n) / 39.0)) -- 0.005 in = 0.127 mm

-- | Conductor cross-section area in mm^2: `pi * d^2 / 4`.
awgAreaMM2 :: String -> Either String Number
awgAreaMM2 awg = do
  d <- awgDiameterMM awg
  pure (pi * d * d / 4.0)

-- | Copper resistance in ohms per 1000 ft at the given conductor
-- | temperature in degC (-60 to 200; use 20.0 for the table value).
awgResistanceOhmPer1000ft :: String -> Number -> Either String Number
awgResistanceOhmPer1000ft awg temperatureC = do
  t <- finite "temperature_c" temperatureC
  if t < -60.0 || t > 200.0 then Left "temperature_c must be between -60 and 200 degC"
  else do
    area <- awgAreaMM2 awg
    let r20 = copperResistivityOhmM * 304.8 / (area * 1.0e-6) -- 1000 ft = 304.8 m
    pure (r20 * (1.0 + copperTempCoeffPerC * (t - 20.0)))

-- | Volts lost in a two-conductor copper run:
-- | `amps * R_per_ft * 2 * oneWayLength`.
-- |
-- | Arguments: amps (`> 0`), one-way length (`> 0`), AWG label, length
-- | unit, conductor temperature in degC.
voltageDrop :: Number -> Number -> String -> LengthUnit -> Number -> Either String Number
voltageDrop amps oneWayLength awg unit temperatureC = do
  i <- positive "amps" amps
  len <- positive "one_way_length" oneWayLength
  r <- awgResistanceOhmPer1000ft awg temperatureC
  let feet = toMeters unit len / metersPerFoot
  pure (i * r * (2.0 * feet) / 1000.0)

-- | Result of `suggestAwg`: the chosen gauge, its drop in volts and percent,
-- | and the inputs it was computed for.
type WireSuggestion =
  { awg :: String
  , voltageDropV :: Number
  , voltageDropPct :: Number
  , allowedDropPct :: Number
  , systemVoltage :: Number
  , amps :: Number
  , oneWayLength :: Number
  , unit :: LengthUnit
  }

-- | Smallest copper AWG from `awgSizes` whose round-trip voltage drop stays
-- | within `maxDropPct` percent of `systemVoltage`.
-- |
-- | Arguments: amps, one-way length, system volts (`> 0`, typically 12 or
-- | 24), allowed drop in percent (`0 < x <= 50`, 3 is common), length unit,
-- | conductor temperature in degC. A `Left` also results if even 4/0 cannot
-- | meet the budget.
-- |
-- | ```purescript
-- | map _.awg (suggestAwg 30.0 10.0 12.0 3.0 Feet 20.0) == Right "6"
-- | ```
suggestAwg :: Number -> Number -> Number -> Number -> LengthUnit -> Number -> Either String WireSuggestion
suggestAwg amps oneWayLength systemVoltage maxDropPct unit temperatureC = do
  i <- positive "amps" amps
  len <- positive "one_way_length" oneWayLength
  volts <- positive "system_voltage" systemVoltage
  pct <- positive "max_drop_pct" maxDropPct
  if pct > 50.0 then Left "max_drop_pct must be 50 or less"
  else do
    drops <- traverse (\g -> map { awg: g, drop: _ } (voltageDrop i len g unit temperatureC)) awgSizes
    let allowed = volts * pct / 100.0
    case findMap (\c -> if c.drop <= allowed then Just c else Nothing) drops of
      Nothing -> Left
        ( "no gauge up to 4/0 keeps the drop within " <> show pct <> "% ("
            <> show i
            <> " A over "
            <> show len
            <> " "
            <> show unit
            <> "); shorten the run, raise the system voltage or use parallel conductors"
        )
      Just c -> Right
        { awg: c.awg
        , voltageDropV: c.drop
        , voltageDropPct: c.drop / volts * 100.0
        , allowedDropPct: pct
        , systemVoltage: volts
        , amps: i
        , oneWayLength: len
        , unit
        }

