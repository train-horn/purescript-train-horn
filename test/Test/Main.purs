-- | Test suite: ports the reference vectors of the Python core
-- | (`trainhorn_calc`) and its Go port, checked to within 1e-9.
module Test.Main (main) where

import Prelude

import Data.Array (concat, concatMap, filter, findIndex, index, length, mapMaybe)
import Data.Either (Either(..), isLeft)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (abs, infinity, nan)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import TrainHorn.Calc (LengthUnit(..), atmosphericPsi, awgAreaMM2, awgDiameterMM, awgResistanceOhmPer1000ft, awgSizes, blastCount, combineLevels, compressorRecoveryLinearSeconds, compressorRecoverySeconds, distanceForSpl, distanceForSplWithAbsorption, distanceRatioLoss, doublingLoss, freeAirVolumeCuFt, parseUnit, splAtDistance, splAtDistanceWithAbsorption, suggestAwg, tankRuntimeSeconds, voltageDrop)

-- A check is its name and, when it fails, the reason.
type Check = { name :: String, failure :: Maybe String }

tol :: Number
tol = 1.0e-9

ok :: String -> Check
ok name = { name, failure: Nothing }

bad :: String -> String -> Check
bad name why = { name, failure: Just why }

near :: String -> Either String Number -> Number -> Number -> Check
near name got want eps = case got of
  Left e -> bad name ("unexpected Left: " <> e)
  Right x
    | abs (x - want) <= eps -> ok name
    | otherwise -> bad name ("got " <> show x <> ", want " <> show want <> " (eps " <> show eps <> ")")

invalid :: forall a. String -> Either String a -> Check
invalid name r = if isLeft r then ok name else bad name "expected Left (invalid input)"

truth :: String -> Boolean -> Check
truth name b = if b then ok name else bad name "condition is false"

eqInt :: String -> Either String Int -> Int -> Check
eqInt name got want = case got of
  Right n | n == want -> ok name
  _ -> bad name ("got " <> show got <> ", want " <> show want)

-- Both sides must be Right; compares them with `near`.
near2 :: String -> Either String Number -> Either String Number -> Number -> Check
near2 name a b eps = case b of
  Right w -> near name a w eps
  Left e -> bad name ("unexpected Left: " <> e)

unit' :: String -> LengthUnit
unit' s = case parseUnit s of
  Right u -> u
  Left _ -> Feet

-- ---------------------------------------------------------------- acoustics

splCases :: Array { src :: Number, d :: Number, ref :: Number, u :: LengthUnit, alpha :: Number, want :: Number }
splCases =
  [ { src: 150.0, d: 100.0, ref: 10.0, u: Feet, alpha: 0.0, want: 130.0 }
  , { src: 118.0, d: 10.0, ref: 10.0, u: Feet, alpha: 0.0, want: 118.0 }
  , { src: 150.0, d: 5.0, ref: 10.0, u: Feet, alpha: 0.0, want: 156.02059991327963 }
  , { src: 120.0, d: 10.0, ref: 1.0, u: Meters, alpha: 0.0, want: 100.0 }
  , { src: 140.0, d: 300.0, ref: 10.0, u: Feet, alpha: 0.0, want: 110.45757490560675 }
  , { src: 140.0, d: 300.0, ref: 10.0, u: Meters, alpha: 0.0, want: 110.45757490560675 }
  , { src: 150.0, d: 1010.0, ref: 10.0, u: Meters, alpha: 0.0, want: 109.91357252434715 }
  , { src: 150.0, d: 1010.0, ref: 10.0, u: Meters, alpha: 2.0, want: 107.91357252434715 }
  , { src: 150.0, d: 3290.84, ref: 10.0, u: Feet, alpha: 0.0, want: 99.65386465010951 }
  , { src: 150.0, d: 3290.84, ref: 10.0, u: Feet, alpha: 10.0, want: 89.65386433010951 }
  , { src: 150.0, d: 37.5, ref: 10.0, u: Feet, alpha: 0.0, want: 138.51937464544562 }
  , { src: 153.2, d: 2500.0, ref: 10.0, u: Feet, alpha: 0.0, want: 105.24119982655924 }
  , { src: 150.0, d: 3.0, ref: 10.0, u: Meters, alpha: 3.7, want: 160.48347490560676 }
  , { src: 150.0, d: 50.0, ref: 10.0, u: Meters, alpha: 3.7, want: 135.87259991327963 }
  , { src: 150.0, d: 800.0, ref: 10.0, u: Meters, alpha: 3.7, want: 109.01520026016112 }
  , { src: 150.0, d: 5000.0, ref: 10.0, u: Meters, alpha: 3.7, want: 77.55759991327963 }
  , { src: 150.0, d: 1609.344, ref: 10.0, u: Feet, alpha: 1.9, want: 104.9408101986925 }
  , { src: 150.0, d: 100.0, ref: 10.0, u: unit' " FT ", alpha: 0.0, want: 130.0 }
  ]

splChecks :: Array Check
splChecks = concatMap one splCases
  where
  one c =
    [ near ("splAtDistanceWithAbsorption " <> show c.src <> " " <> show c.d <> " " <> show c.u <> " a=" <> show c.alpha)
        (splAtDistanceWithAbsorption c.src c.d c.ref c.u c.alpha)
        c.want
        tol
    ]
      <>
        if c.alpha == 0.0 then [ near ("splAtDistance " <> show c.src <> " " <> show c.d) (splAtDistance c.src c.d c.ref) c.want tol ]
        else []

absorptionChecks :: Array Check
absorptionChecks =
  [ near "extra loss over 1 km at 2 dB/km"
      (sub <$> splAtDistanceWithAbsorption 150.0 1010.0 10.0 Meters 0.0 <*> splAtDistanceWithAbsorption 150.0 1010.0 10.0 Meters 2.0)
      2.0
      tol
  , near "feet converted to metres for absorption"
      (sub <$> splAtDistanceWithAbsorption 150.0 3290.84 10.0 Feet 0.0 <*> splAtDistanceWithAbsorption 150.0 3290.84 10.0 Feet 10.0)
      (10.0 * (3290.84 - 10.0) * 0.3048 / 1000.0)
      tol
  ]

distanceCases :: Array { src :: Number, target :: Number, ref :: Number, u :: LengthUnit, alpha :: Number, want :: Number }
distanceCases =
  [ { src: 150.0, target: 130.0, ref: 10.0, u: Feet, alpha: 0.0, want: 100.0 }
  , { src: 150.0, target: 90.0, ref: 10.0, u: Feet, alpha: 0.0, want: 10000.0 }
  , { src: 150.0, target: 90.0, ref: 10.0, u: Feet, alpha: 1.9, want: 6491.256148481227 }
  , { src: 150.0, target: 160.0, ref: 10.0, u: Feet, alpha: 1.9, want: 3.163719354263083 }
  , { src: 150.0, target: 100.0, ref: 1.0, u: Meters, alpha: 3.7, want: 280.7075555048921 }
  , { src: 118.0, target: 98.0, ref: 10.0, u: Feet, alpha: 0.0, want: 100.0 }
  ]

distanceChecks :: Array Check
distanceChecks =
  map
    ( \c -> near ("distanceForSplWithAbsorption " <> show c.src <> " -> " <> show c.target <> " a=" <> show c.alpha)
        (distanceForSplWithAbsorption c.src c.target c.ref c.u c.alpha)
        c.want
        (tol * max 1.0 c.want)
    )
    distanceCases
    <> map roundTrip [ { src: 150.0, d: 37.5 }, { src: 118.0, d: 2.0 }, { src: 153.2, d: 2500.0 }, { src: 100.0, d: 10.0 } ]
    <> map roundTripAbs [ 3.0, 50.0, 800.0, 5000.0 ]
    <>
      [ truth "absorption shortens the distance"
          ( case distanceForSplWithAbsorption 150.0 90.0 10.0 Feet 1.9, distanceForSpl 150.0 90.0 10.0 of
              Right a, Right b -> a < b
              _, _ -> false
          )
      ]
  where
  roundTrip c = near ("round trip " <> show c.d) (splAtDistance c.src c.d 10.0 >>= \t -> distanceForSpl c.src t 10.0) c.d 1.0e-6
  roundTripAbs d =
    near ("round trip with absorption " <> show d)
      (splAtDistanceWithAbsorption 150.0 d 10.0 Meters 3.7 >>= \t -> distanceForSplWithAbsorption 150.0 t 10.0 Meters 3.7)
      d
      (d * 1.0e-9)

lossChecks :: Array Check
lossChecks =
  [ near "doublingLoss" (Right doublingLoss) 6.020599913279624 tol
  , near "distanceRatioLoss 10" (distanceRatioLoss 10.0) 20.0 tol
  , near "combine 100+100" (combineLevels [ 100.0, 100.0 ]) 103.01029995663981 tol
  , near "combine 150" (combineLevels [ 150.0 ]) 150.0 tol
  , near "combine 120+90" (combineLevels [ 120.0, 90.0 ]) 120.00434077479319 tol
  , near "combine 3x150" (combineLevels [ 150.0, 150.0, 150.0 ]) 154.77121254719663 tol
  ]

acousticsInvalid :: Array Check
acousticsInvalid =
  [ invalid "distance 0" (splAtDistance 150.0 0.0 10.0)
  , invalid "negative distance" (splAtDistance 150.0 (-5.0) 10.0)
  , invalid "ref 0" (splAtDistance 150.0 10.0 0.0)
  , invalid "unit yd" (parseUnit "yd")
  , invalid "NaN" (splAtDistance nan 10.0 10.0)
  , invalid "Inf" (splAtDistance infinity 10.0 10.0)
  , invalid "negative absorption" (splAtDistanceWithAbsorption 150.0 10.0 10.0 Feet (-1.0))
  , invalid "no levels" (combineLevels [])
  , invalid "NaN level" (combineLevels [ 100.0, nan ])
  , invalid "ratio 0" (distanceRatioLoss 0.0)
  , invalid "too far apart" (distanceForSpl 150.0 (-1.0e6) 10.0)
  , truth "parseUnit accepts m" (parseUnit "m" == Right Meters)
  ]

-- ---------------------------------------------------------------- air system

airChecks :: Array Check
airChecks =
  [ near "free air 1 gal" (freeAirVolumeCuFt 1.0 14.696 0.0 atmosphericPsi) 0.13368055555555555 tol
  , near "free air 5 gal" (freeAirVolumeCuFt 5.0 150.0 110.0 atmosphericPsi) 1.8192781104457751 tol
  , near "runtime 5 gal 10 cfm" (tankRuntimeSeconds 5.0 150.0 110.0 10.0 14.696) 10.91566866267465 tol
  , near "runtime 2 gal 5 cfm" (tankRuntimeSeconds 2.0 150.0 120.0 5.0 14.696) 6.549401197604791 tol
  , near "runtime at altitude" (tankRuntimeSeconds 5.0 150.0 110.0 10.0 12.2) 13.148907103825138 tol
  , near2 "runtime scales with tank" (tankRuntimeSeconds 4.0 150.0 120.0 5.0 atmosphericPsi) (map (_ * 2.0) (tankRuntimeSeconds 2.0 150.0 120.0 5.0 atmosphericPsi)) tol
  , eqInt "blasts 10/2" (blastCount 10.0 2.0) 5
  , eqInt "blasts 10.9/2" (blastCount 10.9 2.0) 5
  , eqInt "blasts 0/1.5" (blastCount 0.0 1.5) 0
  , eqInt "blasts 0.9/0.3" (blastCount (0.3 * 3.0) 0.3) 3
  , near "recovery 2 gal" (compressorRecoverySeconds 2.0 110.0 150.0 1.0 atmosphericPsi) 43.662674650698605 tol
  , near "recovery 3 gal" (compressorRecoverySeconds 3.0 120.0 150.0 2.5 atmosphericPsi) 19.64820359281437 tol
  , near2 "recovery equals runtime at same CFM" (compressorRecoverySeconds 3.0 120.0 150.0 2.5 atmosphericPsi) (tankRuntimeSeconds 3.0 150.0 120.0 2.5 atmosphericPsi) tol
  , near "flat linear curve" (compressorRecoveryLinearSeconds 2.0 110.0 150.0 1.0 1.0 150.0 atmosphericPsi) 43.662674650698605 tol
  , near "falling linear curve" (compressorRecoveryLinearSeconds 2.0 110.0 150.0 1.0 2.0 150.0 atmosphericPsi) 38.70512365385508 tol
  , near "linear curve vs numeric integration" (compressorRecoveryLinearSeconds 2.0 110.0 150.0 1.0 2.0 150.0 atmosphericPsi) numeric 1.0e-4
  ]
  where
  -- Midpoint rule over 20000 steps, as in the core's test.
  steps = 20000
  v = 2.0 * 231.0 / 1728.0 / 14.696
  dp = 40.0 / toNumber steps
  numeric = 60.0 * go 0 0.0
  go i acc
    | i >= steps = acc
    | otherwise = go (i + 1) (acc + v * dp / (2.0 - (110.0 + (toNumber i + 0.5) * dp) / 150.0))

airInvalid :: Array Check
airInvalid =
  [ invalid "tank 0" (tankRuntimeSeconds 0.0 150.0 110.0 10.0 atmosphericPsi)
  , invalid "start below cutoff" (tankRuntimeSeconds 5.0 110.0 150.0 10.0 atmosphericPsi)
  , invalid "start equals cutoff" (tankRuntimeSeconds 5.0 150.0 150.0 10.0 atmosphericPsi)
  , invalid "negative cutoff" (tankRuntimeSeconds 5.0 150.0 (-1.0) 10.0 atmosphericPsi)
  , invalid "cfm 0" (tankRuntimeSeconds 5.0 150.0 110.0 0.0 atmosphericPsi)
  , invalid "atm 0" (freeAirVolumeCuFt 5.0 150.0 110.0 0.0)
  , invalid "recovery cut-in above cut-out" (compressorRecoverySeconds 2.0 150.0 110.0 1.0 atmosphericPsi)
  , invalid "q0 below rated" (compressorRecoveryLinearSeconds 2.0 110.0 150.0 1.0 0.5 150.0 atmosphericPsi)
  , invalid "no output at cut-out" (compressorRecoveryLinearSeconds 2.0 110.0 150.0 0.1 5.0 100.0 atmosphericPsi)
  , invalid "rated 0" (compressorRecoveryLinearSeconds 2.0 110.0 150.0 1.0 2.0 0.0 atmosphericPsi)
  , invalid "blast 0" (blastCount 10.0 0.0)
  , invalid "negative runtime" (blastCount (-1.0) 1.0)
  ]

-- ---------------------------------------------------------------- electrical

awgCases :: Array { awg :: String, diam :: Number, area :: Number, r :: Number }
awgCases =
  [ { awg: "18", diam: 1.0236873428326523, area: 0.8230468337313146, r: 6.384881861674867 }
  , { awg: "14", diam: 1.627726633791505, area: 2.080907717098377, r: 2.5253675388006465 }
  , { awg: "12", diam: 2.052525388493948, area: 3.3087728761114765, r: 1.5882192573386384 }
  , { awg: "10", diam: 2.5881867280128636, area: 5.261154954510373, r: 0.9988409095411369 }
  , { awg: "8", diam: 3.2636432058836333, area: 8.365564060081022, r: 0.6281772229892056 }
  , { awg: "7", diam: 3.6648500338805654, area: 10.548781512773461, r: 0.49816718581541203 }
  , { awg: "6", diam: 4.11537809850691, area: 13.301767890969138, r: 0.3950645390202436 }
  , { awg: "4", diam: 5.189395968020595, area: 21.15063942544283, r: 0.24845853093587858 }
  , { awg: "1/0", diam: 8.251462802171464, area: 53.47512073211765, r: 0.09827106003790216 }
  , { awg: "4/0", diam: 11.684000000000001, area: 107.21930257703055, r: 0.049012227030898296 }
  ]

-- Published solid-copper table values (ohm / 1000 ft at 20 degC), within 1 %.
published :: Array { awg :: String, ohms :: Number }
published =
  [ { awg: "14", ohms: 2.525 }
  , { awg: "12", ohms: 1.588 }
  , { awg: "10", ohms: 0.9989 }
  , { awg: "8", ohms: 0.6282 }
  , { awg: "4", ohms: 0.2485 }
  , { awg: "1/0", ohms: 0.09827 }
  ]

awgChecks :: Array Check
awgChecks =
  concatMap
    ( \c ->
        [ near ("diameter " <> c.awg) (awgDiameterMM c.awg) c.diam tol
        , near ("area " <> c.awg) (awgAreaMM2 c.awg) c.area tol
        , near ("resistance " <> c.awg) (awgResistanceOhmPer1000ft c.awg 20.0) c.r tol
        ]
    )
    awgCases
    <> map (\p -> near ("published " <> p.awg) (awgResistanceOhmPer1000ft p.awg 20.0) p.ohms (p.ohms * 0.01)) published
    <> [ near "three gauges double the area" (div <$> awgAreaMM2 "7" <*> awgAreaMM2 "10") 2.0 0.01 ]
    <> map
      (\p -> truth ("label " <> show p.a <> " == " <> show p.b) (isRight' (awgDiameterMM p.b) && awgDiameterMM p.a == awgDiameterMM p.b))
      [ { a: "1/0", b: "0" }, { a: "4/0", b: "0000" }, { a: "10", b: "10 AWG" }, { a: "2/0", b: " 00 " }, { a: "6", b: "6awg" } ]
    <>
      [ near "10 AWG at 70 C" (awgResistanceOhmPer1000ft "10" 70.0) 1.1951131482659703 tol
      , near "4 AWG at -20 C" (awgResistanceOhmPer1000ft "4" (-20.0)) 0.20940084987275848 tol
      ]
  where
  isRight' = not <<< isLeft

dropChecks :: Array Check
dropChecks =
  [ near "drop 20 A 10 ft 10 AWG" (voltageDrop 20.0 10.0 "10" Feet 20.0) 0.39953636381645474 tol
  , near "drop 20 A 3.048 m 10 AWG" (voltageDrop 20.0 3.048 "10" Meters 20.0) 0.39953636381645474 tol
  , near "drop 30 A 10 ft 6 AWG" (voltageDrop 30.0 10.0 "6" Feet 20.0) 0.23703872341214613 tol
  ]

suggestCases :: Array { amps :: Number, len :: Number, volts :: Number, pct :: Number, awg :: String, dropV :: Number, dropPct :: Number }
suggestCases =
  [ { amps: 30.0, len: 10.0, volts: 12.0, pct: 3.0, awg: "6", dropV: 0.23703872341214613, dropPct: 1.9753226951012177 }
  , { amps: 30.0, len: 10.0, volts: 24.0, pct: 3.0, awg: "10", dropV: 0.5993045457246821, dropPct: 2.4971022738528417 }
  , { amps: 5.0, len: 3.0, volts: 12.0, pct: 3.0, awg: "18", dropV: 0.191546455850246, dropPct: 1.5962204654187166 }
  , { amps: 15.0, len: 20.0, volts: 12.0, pct: 3.0, awg: "6", dropV: 0.23703872341214613, dropPct: 1.9753226951012177 }
  , { amps: 40.0, len: 6.0, volts: 24.0, pct: 3.0, awg: "10", dropV: 0.47944363657974565, dropPct: 1.9976818190822736 }
  , { amps: 100.0, len: 15.0, volts: 12.0, pct: 3.0, awg: "1/0", dropV: 0.2948131801137065, dropPct: 2.456776500947554 }
  , { amps: 30.0, len: 10.0, volts: 24.0, pct: 2.0, awg: "8", dropV: 0.3769063337935234, dropPct: 1.570443057473014 }
  ]

suggestChecks :: Array Check
suggestChecks = concatMap one suggestCases
  where
  one c =
    let
      label = "suggestAwg " <> show c.amps <> " A " <> show c.len <> " ft " <> show c.volts <> " V " <> show c.pct <> "%"
    in
      case suggestAwg c.amps c.len c.volts c.pct Feet 20.0 of
        Left e -> [ bad label ("unexpected Left: " <> e) ]
        Right s ->
          [ truth (label <> " gauge " <> s.awg <> " == " <> c.awg) (s.awg == c.awg)
          , near (label <> " drop V") (Right s.voltageDropV) c.dropV tol
          , near (label <> " drop %") (Right s.voltageDropPct) c.dropPct tol
          , truth (label <> " within budget, unit and budget kept") (s.voltageDropPct <= c.pct && s.unit == Feet && s.allowedDropPct == c.pct)
          , truth (label <> " is minimal") (minimal c s.awg)
          ]
  minimal c g = case findIndex (_ == g) awgSizes of
    Just i | i > 0 -> case index awgSizes (i - 1) of
      Just thinner -> case voltageDrop c.amps c.len thinner Feet 20.0 of
        Right d -> d / c.volts * 100.0 > c.pct
        Left _ -> false
      Nothing -> false
    _ -> true

electricalInvalid :: Array Check
electricalInvalid =
  map (\b -> invalid ("awg " <> show b) (awgDiameterMM b)) [ "5/0", "abc", "-1", "41", "", "0.5" ]
    <>
      [ invalid "amps 0" (suggestAwg 0.0 10.0 12.0 3.0 Feet 20.0)
      , invalid "max drop 0" (suggestAwg 10.0 10.0 12.0 0.0 Feet 20.0)
      , invalid "max drop 60" (suggestAwg 10.0 10.0 12.0 60.0 Feet 20.0)
      , invalid "negative voltage" (suggestAwg 10.0 10.0 (-12.0) 3.0 Feet 20.0)
      , invalid "impossible load" (suggestAwg 1000.0 200.0 12.0 3.0 Feet 20.0)
      , invalid "temperature 500" (awgResistanceOhmPer1000ft "10" 500.0)
      ]

-- ---------------------------------------------------------------- runner

main :: Effect Unit
main = do
  let
    checks = concat
      [ splChecks
      , absorptionChecks
      , distanceChecks
      , lossChecks
      , acousticsInvalid
      , airChecks
      , airInvalid
      , awgChecks
      , dropChecks
      , suggestChecks
      , electricalInvalid
      ]
    failures = filter (\c -> c.failure /= Nothing) checks
  for_ failures \c -> log ("FAIL " <> c.name <> ": " <> show (mapMaybe identity [ c.failure ]))
  log (show (length checks - length failures) <> " of " <> show (length checks) <> " checks passed")
  if length failures == 0 then pure unit else throw (show (length failures) <> " check(s) failed")
