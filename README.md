# purescript-train-horn

Small calculators for train horn setups on trucks and other vehicles, in PureScript:

- **Sound level vs distance**: SPL at a distance (inverse-square law, optional air absorption), the inverse (distance for a target level), the ~6.02 dB loss per doubling of distance, and the addition of incoherent levels (two equal horns add ~3.01 dB).
- **Air system**: free-air volume (Boyle's law), tank runtime for a horn's CFM, whole blast count, and compressor recovery time (constant-flow or linear-flow model).
- **Wiring**: AWG diameter, area and copper resistance computed from the ASTM B258 definition, round-trip voltage drop, and the smallest gauge that keeps the drop within a budget.

Every function that takes user input returns `Either String Number` (or `Either String` of an `Int` / record), so invalid input such as a zero distance, NaN, or a start pressure below the cut-off is a `Left` with a readable message, never an exception.

This is a faithful port of the Python package [trainhorn-calc](https://github.com/train-horn/trainhorn-calc) and is tested against the same reference vectors to within 1e-9.

## Install

```sh
spago install train-horn
```

## Usage

```purescript
import TrainHorn.Calc

-- 150 dB rated at 10 ft -> level at 100 ft
splAtDistance 150.0 100.0 10.0                               -- Right 130.0

-- where does it fall to 90 dB with 1.9 dB/km air absorption?
distanceForSplWithAbsorption 150.0 90.0 10.0 Feet 1.9          -- Right 6491.256...

-- two identical horns
combineLevels [ 100.0, 100.0 ]                                -- Right 103.0103...

-- 5 gal tank, 150 -> 110 psi, horn draws 10 CFM
tankRuntimeSeconds 5.0 150.0 110.0 10.0 atmosphericPsi         -- Right 10.9157...
tankRuntimeSeconds 5.0 150.0 110.0 10.0 atmosphericPsi >>= \t -> blastCount t 2.0  -- Right 5

-- refill 2 gal from 110 to 150 psi at 1.0 CFM
compressorRecoverySeconds 2.0 110.0 150.0 1.0 atmosphericPsi   -- Right 43.66...

-- 30 A compressor, 10 ft one-way run, 12 V, 3 % drop budget
map _.awg (suggestAwg 30.0 10.0 12.0 3.0 Feet 20.0)            -- Right "6"

-- invalid input
splAtDistance 150.0 0.0 10.0    -- Left "distance must be greater than 0, got 0.0"
```

## Modules

| Module | Contents |
| --- | --- |
| `TrainHorn.Calc` | Re-exports everything below |
| `TrainHorn.Calc.Acoustics` | `splAtDistance`, `splAtDistanceWithAbsorption`, `distanceForSpl`, `distanceForSplWithAbsorption`, `distanceRatioLoss`, `doublingLoss`, `combineLevels`, `airAbsorptionDbPerKm` |
| `TrainHorn.Calc.AirSystem` | `freeAirVolumeCuFt`, `tankRuntimeSeconds`, `blastCount`, `compressorRecoverySeconds`, `compressorRecoveryLinearSeconds`, `atmosphericPsi` |
| `TrainHorn.Calc.Electrical` | `awgDiameterMM`, `awgAreaMM2`, `awgResistanceOhmPer1000ft`, `voltageDrop`, `suggestAwg`, `awgSizes` |
| `TrainHorn.Calc.Validate` | `LengthUnit` (`Feet`, `Meters`), `parseUnit`, unit conversion and input checks |

## Assumptions and limits

- Acoustics: point source in a free field; no ground reflection, walls, wind or barriers. Absorption values are rounded typical figures (10 degC, 70 % RH). Results are estimates, not measurements, and not a legal noise assessment.
- Air: isothermal expansion (an upper estimate for fast blasts); line, valve and leak losses ignored.
- Wiring: voltage drop only. It does not check ampacity or fuse sizing; always fuse to protect the wire you choose.

## Tests

```sh
spago test
```

186 checks: the reference vectors of the Python core and its Go port (SPL, distance inverse with bisection, level addition, Boyle runtime, constant and linear compressor recovery against numeric integration, AWG diameter/area/resistance, voltage drop, minimal gauge selection) plus invalid-input cases.

Browser versions of these calculators are at [train-horn.com/tools](https://train-horn.com/tools/).

## License

MIT
