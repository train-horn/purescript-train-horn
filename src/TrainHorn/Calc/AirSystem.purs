-- | Onboard air system: tank runtime and compressor recovery.
-- |
-- | A tank of volume `V` at absolute pressure `P` holds the same air as
-- | `V * P / P_atm` at atmospheric pressure ("free air", Boyle's law). Gauge
-- | pressures (psig) are made absolute by adding `P_atm`, so the free air
-- | released between two gauge pressures is
-- |
-- | ```
-- | V_free = V_tank * (P_start - P_end) / P_atm
-- | ```
-- |
-- | Horn consumption and compressor output are rated in CFM of free air, so
-- | `seconds = V_free / cfm * 60`.
-- |
-- | Assumptions: isothermal expansion (fast blasts cool the air and give a
-- | little less, so treat results as an upper estimate); the horn draws its
-- | rated CFM at every pressure above the cut-off; the compressor is off while
-- | the horn sounds; line, valve and leak losses are ignored.
module TrainHorn.Calc.AirSystem
  ( atmosphericPsi
  , cubicFeetPerGallon
  , freeAirVolumeCuFt
  , tankRuntimeSeconds
  , blastCount
  , compressorRecoverySeconds
  , compressorRecoveryLinearSeconds
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Int (floor)
import Data.Number (log)
import TrainHorn.Calc.Validate (nonNegative, positive)

-- | Standard atmosphere at sea level in psi (14.696). Pass a lower value for
-- | altitude.
atmosphericPsi :: Number
atmosphericPsi = 14.696

-- | Cubic feet in one US liquid gallon: 231 in^3 / 1728 in^3.
cubicFeetPerGallon :: Number
cubicFeetPerGallon = 231.0 / 1728.0

pressures :: String -> Number -> String -> Number -> Either String { high :: Number, low :: Number }
pressures highName high lowName low = do
  h <- nonNegative highName high
  l <- nonNegative lowName low
  if h <= l then
    Left (highName <> " (" <> show h <> " psi) must be greater than " <> lowName <> " (" <> show l <> " psi)")
  else
    Right { high: h, low: l }

freeAir :: Number -> String -> Number -> String -> Number -> Number -> Either String Number
freeAir tankGallons highName high lowName low atm = do
  gal <- positive "tank_gallons" tankGallons
  p <- pressures highName high lowName low
  a <- positive "atmospheric_psi" atm
  pure (gal * cubicFeetPerGallon * (p.high - p.low) / a)

-- | Free air in cubic feet (at atmospheric pressure) released while a tank
-- | of `tankGallons` US gallons drops from `startPsi` to `endPsi` gauge:
-- | `tankGallons * 0.13368 * (startPsi - endPsi) / atmPsi`.
-- |
-- | Arguments: tank gallons (`> 0`), start psi, end psi (`>= 0`, below
-- | start), atmospheric psi (`> 0`, usually `atmosphericPsi`).
freeAirVolumeCuFt :: Number -> Number -> Number -> Number -> Either String Number
freeAirVolumeCuFt tankGallons startPsi endPsi atm =
  freeAir tankGallons "start_psi" startPsi "end_psi" endPsi atm

-- | Seconds of continuous horn time from `startPsi` down to `cutoffPsi`
-- | for a horn drawing `hornCfm` of free air: `freeAirVolumeCuFt / hornCfm * 60`.
-- |
-- | ```purescript
-- | tankRuntimeSeconds 5.0 150.0 110.0 10.0 atmosphericPsi  -- Right 10.9156...
-- | ```
tankRuntimeSeconds :: Number -> Number -> Number -> Number -> Number -> Either String Number
tankRuntimeSeconds tankGallons startPsi cutoffPsi hornCfm atm = do
  cfm <- positive "horn_cfm" hornCfm
  v <- freeAir tankGallons "start_psi" startPsi "cutoff_psi" cutoffPsi atm
  pure (v / cfm * 60.0)

-- | Whole blasts of `blastSeconds` that fit in `runtimeSeconds`. A 1e-9
-- | epsilon keeps exact divisions such as `0.9 / 0.3` from rounding down.
blastCount :: Number -> Number -> Either String Int
blastCount runtimeSeconds blastSeconds = do
  run <- nonNegative "runtime_seconds" runtimeSeconds
  blast <- positive "blast_seconds" blastSeconds
  pure (floor (run / blast + 1.0e-9))

-- | Seconds for a compressor delivering a constant `compressorCfm` of free
-- | air to refill the tank from `cutInPsi` to `cutOutPsi`:
-- | `V_free / compressorCfm * 60`. Use the CFM rated at (or near) your
-- | cut-out pressure; a 0 psi rating gives an optimistic time.
-- |
-- | Arguments: tank gallons, cut-in psi, cut-out psi (`>` cut-in),
-- | compressor CFM, atmospheric psi.
compressorRecoverySeconds :: Number -> Number -> Number -> Number -> Number -> Either String Number
compressorRecoverySeconds tankGallons cutInPsi cutOutPsi compressorCfm atm = do
  cfm <- positive "compressor_cfm" compressorCfm
  v <- freeAir tankGallons "cut_out_psi" cutOutPsi "cut_in_psi" cutInPsi atm
  pure (v / cfm * 60.0)

-- | Recovery time when compressor output falls linearly with pressure, from
-- | `cfmAtZeroPsi` at 0 psi to `compressorCfm` at `ratedPsi`:
-- | `q(p) = q0 + b * p`. Integrating `dt = (V_tank / P_atm) dp / q(p)` gives
-- |
-- | ```
-- | t = V_tank / P_atm * ln(q(p_out) / q(p_in)) / b * 60     (b /= 0)
-- | ```
-- |
-- | Arguments: tank gallons, cut-in psi, cut-out psi, compressor CFM at the
-- | rated pressure, CFM at 0 psi (`>=` compressor CFM), rated psi (`> 0`),
-- | atmospheric psi.
compressorRecoveryLinearSeconds
  :: Number -> Number -> Number -> Number -> Number -> Number -> Number -> Either String Number
compressorRecoveryLinearSeconds tankGallons cutInPsi cutOutPsi compressorCfm cfmAtZeroPsi ratedPsi atm = do
  cfm <- positive "compressor_cfm" compressorCfm
  v <- freeAir tankGallons "cut_out_psi" cutOutPsi "cut_in_psi" cutInPsi atm
  q0 <- positive "cfm_at_zero_psi" cfmAtZeroPsi
  pRated <- positive "rated_psi" ratedPsi
  if q0 < cfm then
    Left "cfm_at_zero_psi must be at least compressor_cfm (output falls with pressure)"
  else do
    let
      slope = (cfm - q0) / pRated -- cfm per psi, <= 0
      qIn = q0 + slope * cutInPsi
      qOut = q0 + slope * cutOutPsi
    if qOut <= 0.0 then
      Left
        ( "the linear flow model gives no output at " <> show cutOutPsi
            <> " psi; check cfm_at_zero_psi, compressor_cfm and rated_psi"
        )
    else if slope == 0.0 then
      Right (v / q0 * 60.0)
    else
      Right (tankGallons * cubicFeetPerGallon / atm * log (qOut / qIn) / slope * 60.0)
