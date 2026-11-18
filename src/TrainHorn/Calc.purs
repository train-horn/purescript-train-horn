-- | Small, dependency-light calculators for train horn setups on trucks and
-- | other vehicles: how loud a horn is at a distance, how long an air tank
-- | lasts and refills, and which copper wire gauge keeps the voltage drop in
-- | budget.
-- |
-- | This module re-exports everything from:
-- |
-- | * `TrainHorn.Calc.Acoustics`: SPL at a distance (inverse-square law with
-- |   optional air absorption), its inverse, the per-doubling loss and the
-- |   addition of incoherent levels;
-- | * `TrainHorn.Calc.AirSystem`: free-air volume (Boyle's law), tank
-- |   runtime, blast count and compressor recovery time;
-- | * `TrainHorn.Calc.Electrical`: AWG diameter, area, resistance, voltage
-- |   drop and the smallest gauge for a drop budget;
-- | * `TrainHorn.Calc.Validate`: length units and input checks.
-- |
-- | Every function that takes user input returns `Either String Number` (or
-- | `Either String` of a record or `Int`): `Left` carries a readable message
-- | for invalid input such as a zero distance, a non-finite number or a
-- | start pressure below the cut-off.
-- |
-- | It is a faithful port of the Python package `trainhorn-calc` and is
-- | tested against the same reference values to within 1e-9. Browser
-- | versions of these calculators: [interactive calculators](https://train-horn.com/tools/).
-- |
-- | Results are physics-based estimates, not measurements, and not legal
-- | advice on horn use.
-- |
-- | ```purescript
-- | import TrainHorn.Calc
-- |
-- | splAtDistance 150.0 100.0 10.0                 -- Right 130.0
-- | tankRuntimeSeconds 5.0 150.0 110.0 10.0 atmosphericPsi  -- Right 10.91...
-- | map _.awg (suggestAwg 30.0 10.0 12.0 3.0 Feet 20.0)    -- Right "6"
-- | ```
module TrainHorn.Calc
  ( module TrainHorn.Calc.Acoustics
  , module TrainHorn.Calc.AirSystem
  , module TrainHorn.Calc.Electrical
  , module TrainHorn.Calc.Validate
  ) where

import TrainHorn.Calc.Acoustics (airAbsorptionDbPerKm, combineLevels, distanceForSpl, distanceForSplWithAbsorption, distanceRatioLoss, doublingLoss, splAtDistance, splAtDistanceWithAbsorption)
import TrainHorn.Calc.AirSystem (atmosphericPsi, blastCount, compressorRecoveryLinearSeconds, compressorRecoverySeconds, cubicFeetPerGallon, freeAirVolumeCuFt, tankRuntimeSeconds)
import TrainHorn.Calc.Electrical (WireSuggestion, awgAreaMM2, awgDiameterMM, awgResistanceOhmPer1000ft, awgSizes, copperResistivityOhmM, copperTempCoeffPerC, suggestAwg, voltageDrop)
import TrainHorn.Calc.Validate (LengthUnit(..), finite, fromMeters, log10, metersPerFoot, nonNegative, parseUnit, positive, toMeters)
